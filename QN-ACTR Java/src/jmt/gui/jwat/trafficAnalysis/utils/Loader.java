/**
 * Copyright (C) 2006, Laboratorio di Valutazione delle Prestazioni - Politecnico di Milano

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
package jmt.gui.jwat.trafficAnalysis.utils;

import java.awt.Component;
import java.awt.HeadlessException;
import java.io.File;
import java.util.Vector;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.filechooser.FileFilter;

/**
 * <p>Title: Model Loader</p>
 * <p>Description: This class provides unified load/save functionality
 * for every JMT application</p>
 *
 * @author Bertoli Marco
 *         Date: 15-feb-2006
 *         Time: 15.36.51
 */
public class Loader {
	/**
	 * Filters for input files
	 */
	public static final JmtFileFilter LOG = new JmtFileFilter(".txt", "log file");

	/**
	 * Constants used for output
	 */
	public static final int SUCCESS = 0;
	public static final int CANCELLED = 1;
	public static final int FAILURE = 2;
	public static final int WARNING = 3;

	/**
	* Failure motivations
	*/
	protected static final String FAIL_UNKNOWN = "Unknown input file format";
	protected static final String FAIL_CONVERSION = "Input file is recognized but cannot be converted "
			+ "to work within this application. Please open it with ";

	protected JmtFileChooser dialog;

	protected JmtFileFilter defaultFilter;

	// Motivation of last failure
	protected String failureMotivation;

	// Warnings found during last conversion
	protected Vector warnings = new Vector();

	private File f = null;

	/**
	 * Initialize a new ModelLoader with specified default file filter (chosen between
	 * constrants in this class)
	 * @param defaultFilter default file filter for current application (used to detect application type)
	 */
	public Loader(JmtFileFilter defaultFilter) {
		this.defaultFilter = defaultFilter;
		// Initialize filechooser dialog
		dialog = new JmtFileChooser(defaultFilter);
	}

	/**
	 * Gets the motivation of last failure
	 * @return the motivation of last failure
	 */
	public String getFailureMotivation() {
		return failureMotivation;
	}

	/**
	 * Gets a vector containing warnings of last performed operation
	 * @return a Vector of String with every found warning
	 */
	public Vector getLastWarnings() {
		return warnings;
	}

	// --- Methods used to load models ------------------------------------------------------------
	/**
	 * Shows a Load window and loads chosen model inside specified model data file
	 * @param modelData data file where information should be stored. Note that <b>its type
	 * must be compatible with defaultFilter chosen in the constructor</b>, otherwise a
	 * ClassCastException will be thrown
	 * @param parent parent component of loading window
	 * @return SUCCESS on success, CANCELLED if loading is cancelled,
	 * FAILURE if an error occurs and WARNING is one or more warnings are generated due to
	 * format conversion
	 * @throws ClassCastException if modelData is not of instance of the correct class
	 * @see #getFailureMotivation getFailureMotivation()
	 */
	public int loadModel(Component parent) {
		warnings.clear();
		int status;
		status = this.showOpenDialog(parent);
		if (status == JFileChooser.CANCEL_OPTION) {
			return CANCELLED;
		} else if (status == JFileChooser.ERROR_OPTION) {
			failureMotivation = "Error selecting input file";
			return FAILURE;
		}

		f = dialog.getSelectedFile();

		try {
			if (defaultFilter == LOG) {

			}

		} catch (Exception e) {
			e.printStackTrace();
			failureMotivation = e.getClass().getName() + ": " + e.getMessage();
			return FAILURE;
		}
		// If no warnings were found, report success
		if (warnings.size() > 0) {
			return WARNING;
		} else {
			return SUCCESS;
		}
	}

	/**
	 * Returns name of selected file for i/o operations
	 * @return name of selected file for open or save
	 */
	public File getSelectedFile() {
		return dialog.getSelectedFile();
	}

	// --------------------------------------------------------------------------------------------

	// --- Methods to show dialogs ----------------------------------------------------------------
	/**
	 * Adds all filters to current dialog
	 */
	protected void addAllFilters() {
		dialog.setAcceptAllFileFilterUsed(true);
		dialog.addChoosableFileFilter(LOG);
		dialog.setFileFilter(defaultFilter);
	}

	/**
	 * Shows open file dialog
	 * @param parent parent component for this dialog
	 * @return   the return state of the file chooser on popdown:
	 * <ul>
	 * <li>JFileChooser.CANCEL_OPTION
	 * <li>JFileChooser.APPROVE_OPTION
	 * <li>JFileCHooser.ERROR_OPTION if an error occurs or the
	 *			dialog is dismissed
	 * </ul>
	 * @exception HeadlessException if GraphicsEnvironment.isHeadless()
	 * returns true.
	 */
	protected int showOpenDialog(Component parent) {
		addAllFilters();
		return dialog.showOpenDialog(parent);
	}

	// --------------------------------------------------------------------------------------------

	// --- Inner classes --------------------------------------------------------------------------
	/**
	 * Custom file chooser class
	 */
	protected static class JmtFileChooser extends JFileChooser {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		protected JmtFileFilter defaultFilter;

		/**
		 * Creates a File chooser in the appropriate directory user deafault.
		 * @param defaultFilter default file filter
		 */
		public JmtFileChooser(JmtFileFilter defaultFilter) {
			super(new File(System.getProperty("user.dir")));
			this.defaultFilter = defaultFilter;
		}

		/**
		 * Overrides default method to provide a warning if saving over an existing file
		 */
		@Override
		public void approveSelection() {
			// Gets the choosed file name
			String name = getSelectedFile().getName();
			String parent = getSelectedFile().getParent();
			if (getDialogType() == OPEN_DIALOG) {
				if (defaultFilter.accept(getSelectedFile())) {
					super.approveSelection();
				} else {
					JOptionPane.showMessageDialog(this, "Invalid Extension\nSelect only *.log files", "Invalid Extension", JOptionPane.ERROR_MESSAGE);
				}

			}

		}
	}

	/**
	 * Inner class used to create simple file filters with only extension check
	 */
	protected static class JmtFileFilter extends FileFilter {
		private String extension, description;

		/**
		 * Creates a new filefilter with specified extension and description
		 * @param extension extension of this filter (for example ".jmt")
		 * @param description description of this filter
		 */
		public JmtFileFilter(String extension, String description) {
			this.extension = extension;
			this.description = description;
		}

		/**
		 * Whether the given file is accepted by this filter.
		 */
		@Override
		public boolean accept(File f) {
			String name = f.getName().toLowerCase();
			return name.endsWith(extension) || f.isDirectory();
		}

		/**
		 * The description of this filter
		 * @see javax.swing.filechooser.FileView#getName
		 */
		@Override
		public String getDescription() {
			return description + " (*" + extension + ")";
		}

		/**
		 * Gets extension of this filter
		 * @return extension of this filter
		 */
		public String getExtension() {
			return extension;
		}
	}
	// --------------------------------------------------------------------------------------------
}
