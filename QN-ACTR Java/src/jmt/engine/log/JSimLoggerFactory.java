/**
  * Copyright (C) 2009, Laboratorio di Valutazione delle Prestazioni - Politecnico di Milano

  * This program is free software; you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation; either version 2 of the License, or
  * (at your option) any later version.

  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.

  * You should have received a copy of the GNU General Public License
  * along with this program; if not, write to the Free Software
  * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
  */
package jmt.engine.log;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 * <p><b>Name:</b> JSimLoggerFactory</p> 
 * <p><b>Description:</b> 
 * Provides a factory for loggers used to write to a CSV file during the simulation
 * </p>
 * <p><b>Date:</b> 11/dic/2009
 * <b>Time:</b> 17.09.03</p>
 * @author Bertoli Marco
 * @version 1.0
 */
public class JSimLoggerFactory {
	private static Map<File, CSVLogger> csvLoggerCache = new HashMap<File, CSVLogger>();
	
	/**
	 * Returns an instance of CSVLogger on a given file 
	 * @param file the file we should write to
	 * @param columns the columns for the log file
	 * @param append true to append to an existing file. False to create a new file
	 * @param colSep the column separator
	 * @param digitSep the decimal digits separator
	 * @return the logger instance
	 */
	public static CSVLogger getCSVLogger(File file, String[] columns, boolean append, String colSep, char digitSep) {
		CSVLogger ret = csvLoggerCache.get(file);
		if (ret == null) {
			synchronized (JSimLoggerFactory.class) {
				ret = csvLoggerCache.get(file);
				if (ret == null) {
					ret = new CSVLogger(file, columns, append, colSep, digitSep);
					csvLoggerCache.put(file, ret);
				}
			}
		}
		return ret;
	}
	
	/**
	 * Removes a logger from the list of all loggers and disposes it.
	 * @param logger the logger that should be removed
	 */
	static void remove(CSVLogger logger) throws IOException {
		File file = logger.getFile();
		if (csvLoggerCache.containsKey(file)) {
			synchronized (JSimLoggerFactory.class) {
				if (csvLoggerCache.containsKey(file)) {
					csvLoggerCache.remove(file);
					logger.dispose();
				}
			}
		}
	}
	
	/**
	 * Closes all the loggers built by this factory.
	 * @throws IOException if an IO problem occurrs
	 */
	public static synchronized void closeAllLoggers() throws IOException {
		ArrayList<CSVLogger> loggers = new ArrayList<CSVLogger>();
		loggers.addAll(csvLoggerCache.values());
		for (CSVLogger logger : loggers) {
			logger.close();
		}
	}
}
