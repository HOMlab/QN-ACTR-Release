package jmt.engine.log;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.util.Locale;
import java.util.Map;

/**
 * <p><b>Name:</b> CSVLogger</p> 
 * <p><b>Description:</b> 
 * A logger implementation that writes its output on a given CSV file. This class is thread-safe, so multiple 
 * threads may concurrently write on the same output file sharing the same CSVLogger instance.
 * </p>
 * <p><b>Date:</b> 11/dic/2009
 * <b>Time:</b> 17.16.14</p>
 * @author Bertoli Marco
 * @version 1.0
 */
public class CSVLogger {
	private File file;
	private boolean initialized;
	private boolean append;
	private String[] columns;
	private String colSep;
	private NumberFormat numberFormat;
	private BufferedWriter writer;
	
	/**
	 * Creates a new CSVLogger that writes to the given file
	 * @param file the file we should write to
	 * @param columns the columns for the log file
	 * @param append true to append to an existing file. False to create a new file
	 * @param colSep the column separator
	 * @param digitSep the decimal digits separator
	 */
	public CSVLogger(File file, String[] columns, boolean append, String colSep, char digitSep) {
		this.file = file;
		this.initialized = false;
		this.append = append;
		this.columns = columns;
		this.colSep = colSep;
		
		DecimalFormatSymbols dfs = new DecimalFormatSymbols(Locale.US);
		dfs.setDecimalSeparator(digitSep);
		numberFormat = new DecimalFormat("#.#", dfs);
		numberFormat.setMaximumFractionDigits(340);
		numberFormat.setMaximumIntegerDigits(340);
	}
		
	/**
	 * Logs the given values. Only the actual writing to the stream is synchronized. This method is designed
	 * so that each log line will never be mixed with another one.
	 * @param values a map with the value for each column
	 * @param defaultValues a map with default values for the log. Whenever a value is not found
	 * in values map, it is looked in defaultValues map.
	 * @throws IOException if a problem is risen opening or writing to the log file
	 */

	public void log(Map<String, ?> values, Map<String, ?> defaultValues) throws IOException {
		if (!isInitialized()) {
			init();
		}
		StringBuilder logLine = new StringBuilder(100);
		for (int i=0; i<columns.length; i++) {
			if (i > 0) {
				logLine.append(colSep);
			}
			Object value = values.get(columns[i]);
			if (isEmpty(value)) {
				value = defaultValues.get(columns[i]);
			}
			if (!isEmpty(value)) {
				logLine.append(toString(value));
			}
		}
		
		// Finally writes to the stream
		synchronized(this) {
			writer.newLine();
			writer.append(logLine);
		}
	}
	
	
	
	/**
	 * Initialize this logger. This method has no effect if the logger is already initialized.
	 * @throws IOException if an IO problem is risen opening the log file.
	 */
	private synchronized void init() throws IOException {
		if (!isInitialized()) {
			boolean needHeader = true;
			if (append && file.exists() && file.length() > 0) {
				needHeader = false;
			}
			initialized = true;
			writer = new BufferedWriter(new FileWriter(file, append));
			
			// Write header if needed
			if (needHeader) {
				for (int i=0; i<columns.length; i++) {
					if (i > 0) {
						writer.write(colSep);
					}
					writer.write(columns[i]);
				}
			}
		}
	}

	/**
	 * Closes this logger, flushing and closing every file stream opened by it.
	 * @throws IOException if an IO problem is risen closing the log file.
	 */
	public synchronized void close() throws IOException {
		JSimLoggerFactory.remove(this);
	}
	
	/**
	 * Disposes this logger object, closing the writer.
	 * @throws IOException if an IO problem is risen closing the log file.
	 */
	synchronized void dispose() throws IOException {
		if (isInitialized()) {
			writer.flush();
			writer.close();
			initialized = false;
		}
	}
	
	/**
	 * @return the file this logger is writing on.
	 */
	public File getFile() {
		return file;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#finalize()
	 */
	@Override
	protected void finalize() throws Throwable {
		close();
	}
	
	/**
	 * @return true if this logger was initialized and running. False otherwise.
	 */
	public boolean isInitialized() {
		return initialized;
	}
	
	/**
	 * Tells if the given object is a null or empty string
	 * @param obj the object
	 * @return true if obj is null or an empty string.
	 */
	private boolean isEmpty(Object obj) {
		if (obj == null) {
			return true;
		} else if (obj instanceof CharSequence) {
			return ((CharSequence)obj).length() == 0;
		} else {
			return false;
		}
	}
	
	/**
	 * Converts the given object to a string with the correct digit separator.
	 * @param obj the object, may not be null
	 * @return the string
	 */
	private String toString(Object obj) {
		if (obj instanceof Number) {
			return numberFormat.format(((Number) obj).doubleValue());
		} else {
			return String.valueOf(obj);
		}
	}
}
