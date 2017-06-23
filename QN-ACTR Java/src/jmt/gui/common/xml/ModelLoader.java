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

/**
 * 2013. Modified for QN-Java project
 * 
 * [2013-06-28]  change openModel dialog default folder.
 * 
 */


package jmt.gui.common.xml;

import java.awt.Component;
import java.awt.HeadlessException;
import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Vector;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.filechooser.FileFilter;

import jmt.framework.xml.XMLUtils;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.definitions.CommonModel;
import jmt.gui.common.definitions.ModelConverter;
import jmt.gui.common.definitions.StoredResultsModel;
import jmt.gui.exact.ExactModel;
import jmt.gui.jaba.JabaModel;

import org.w3c.dom.Document;

import qnactr.sim.GlobalUtilities;

/**
 * <p>Title: Model Loader</p>
 * <p>Description: This class provides unified load/save functionality
 * for every JMT application</p>
 *
 * @author Bertoli Marco
 *         Date: 15-feb-2006
 *         Time: 15.36.51
 */
public class ModelLoader {
	/**
	 * Filters for input files
	 */
	public static final JmtFileFilter JMODEL = new JmtFileFilter(".jsimg;.jmodel", "JSIMgraph data file");
	public static final JmtFileFilter JSIM = new JmtFileFilter(".jsimw;.jsim", "JSIMwiz data file");
	public static final JmtFileFilter JMVA = new JmtFileFilter(".jmva", "JMVA data file");
	public static final JmtFileFilter JABA = new JmtFileFilter(".jaba", "JABA data file");
	public static final JmtFileFilter XML = new JmtFileFilter(".xml", "Engine XML i/o file");
	public static final JmtFileFilter ALL = new JmtFileFilter(".jsimg;.jsimw;.jmva;.jaba;.xml;.jsim;.jmodel", "All JMT data files");

	public static final JmtFileFilter ALL_NOTJABA = new JmtFileFilter(".jsimg;.jsimw;.jmva;.xml;.jsim;.jmodel", "All compatible JMT data files");

	/**
	 * Constants used for output
	 */
	public static final int SUCCESS = 0;
	public static final int CANCELLED = 1;
	public static final int FAILURE = 2;
	public static final int WARNING = 3;

	/**
	 * Constants to define xml types
	 */
	protected static final int XML_SIM = 0;
	protected static final int XML_ARCHIVE = 1;
	protected static final int XML_MVA = 2;
	protected static final int XML_JABA = 5;
	protected static final int XML_RES_SIM = 3;
	protected static final int XML_RES_GUI = 4;
	protected static final int FILE_UNKNOWN = 255;

	/**
	 * Failure motivations
	 */
	protected static final String FAIL_UNKNOWN = "Unknown input file format";
	protected static final String FAIL_CONVERSION = "Input file is recognized but cannot be converted "
			+ "to work within this application. Please open it with ";

	// Better to move this element elsewere...
	protected static final String XML_MVA_ROOT = "model";

	protected JmtFileChooser dialog;

	protected JmtFileFilter defaultFilter;

	// Motivation of last failure
	protected String failureMotivation;

	// Warnings found during last conversion
	protected Vector warnings = new Vector();

	protected String fileFormat;

	protected XMLUtils xmlutils = new XMLUtils();

	/**
	 * Initialize a new ModelLoader with specified default file filter (chosen between
	 * constrants in this class)
	 * @param defaultFilter default file filter for current application (used to detect application type)
	 */
	public ModelLoader(JmtFileFilter defaultFilter) {
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

	/**
	 * @return the format of opened input file
	 */
	public String getInputFileFormat() {
		return fileFormat;
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
	public int loadModel(Object modelData, Component parent) {
		warnings.clear();
		int status;
		
	  //System.out.println("ModelLoader.java before showOpenDialog");
		
		status = this.showOpenDialog(parent);
		
	  //System.out.println("ModelLoader.java after showOpenDialog");
		
		if (status == JFileChooser.CANCEL_OPTION) {
			return CANCELLED;
		} else if (status == JFileChooser.ERROR_OPTION) {
			failureMotivation = "Error selecting input file";
			return FAILURE;
		}

		File file = dialog.getSelectedFile();

		try {
			if (defaultFilter == JMODEL || defaultFilter == JSIM) {
				StoredResultsModel srm;
				// Handles loading of JSIM/JMODEL models
				switch (getXmlFileType(file.getAbsolutePath())) {
					case XML_SIM:
						XMLReader.loadModel(file, (CommonModel) modelData);
						fileFormat = CommonConstants.SIMENGINE;
						break;
					case XML_ARCHIVE:
						XMLArchiver.loadModel((CommonModel) modelData, file);
						fileFormat = CommonConstants.JSIM;
						break;
					case XML_MVA:
						ExactModel tmp = new ExactModel();
						tmp.loadDocument(xmlutils.loadXML(file));
						warnings.addAll(ModelConverter.convertJMVAtoJSIM(tmp, (CommonModel) modelData));
						fileFormat = CommonConstants.JMVA;
						break;
					case XML_JABA:
						//TODO implement bridge JABA --> JSIM
						failureMotivation = FAIL_CONVERSION + "JABA.";
						fileFormat = CommonConstants.JABA;
						return FAILURE;
					case XML_RES_SIM:
						srm = new StoredResultsModel();
						XMLResultsReader.loadModel(srm, file);
						((CommonModel) modelData).setSimulationResults(srm);
						warnings.add("Loaded file contained simulation results only. Associated queuing network model is not available. "
								+ "Results can be shown by selecting \"Show Results\" icon.");
						fileFormat = CommonConstants.SIMENGINE;
						break;
					case XML_RES_GUI:
						srm = new StoredResultsModel();
						XMLResultsReader.loadGuiModel(srm, file);
						((CommonModel) modelData).setSimulationResults(srm);
						warnings.add("Loaded file contained simulation results only. Associated queuing network model is not available. "
								+ "Results can be shown by selecting \"Show Results\" icon.");
						fileFormat = CommonConstants.SIMENGINE;
						break;
					default:
						failureMotivation = FAIL_UNKNOWN;
						return FAILURE;
				}
			} else if (defaultFilter == JMVA) {
				// Handles loading of JMVA models
				CommonModel tmp = new CommonModel();
				switch (getXmlFileType(file.getAbsolutePath())) {
					case XML_SIM:
						XMLReader.loadModel(file, tmp);
						warnings.addAll(ModelConverter.convertJSIMtoJMVA(tmp, (ExactModel) modelData));
						fileFormat = CommonConstants.SIMENGINE;
						break;
					case XML_ARCHIVE:
						XMLArchiver.loadModel(tmp, file);
						warnings.addAll(ModelConverter.convertJSIMtoJMVA(tmp, (ExactModel) modelData));
						fileFormat = CommonConstants.JSIM;
						break;
					case XML_JABA:
						//TODO implement bridge JABA --> JMVA
						failureMotivation = FAIL_CONVERSION + "JABA.";
						fileFormat = CommonConstants.JABA;
						return FAILURE;
					case XML_MVA:
						((ExactModel) modelData).loadDocument(xmlutils.loadXML(file));
						fileFormat = CommonConstants.JMVA;
						break;
					case XML_RES_SIM:
					case XML_RES_GUI:
						// This is silly to be opened in JMVA...
						failureMotivation = FAIL_CONVERSION + "JSIM or JMODEL.";
						fileFormat = CommonConstants.SIMENGINE;
						return FAILURE;
					default:
						failureMotivation = FAIL_UNKNOWN;
						return FAILURE;
				}
			} else if (defaultFilter == JABA) {
				// Handles loading of JABA models
				switch (getXmlFileType(file.getAbsolutePath())) {
					case XML_SIM:
						//TODO implement bridge JSIM --> JABA
					case XML_ARCHIVE:
						//TODO implement bridge JSIM --> JABA
						failureMotivation = FAIL_CONVERSION + "JSIM or JMODEL.";
						return FAILURE;
					case XML_MVA:
						//TODO implement bridge JMVA --> JABA
						failureMotivation = FAIL_CONVERSION + "JMVA.";
						return FAILURE;
					case XML_JABA:
						((JabaModel) modelData).loadDocument(xmlutils.loadXML(file));
						break;
					case XML_RES_SIM:
					case XML_RES_GUI:
						// This is silly to be opened in JABA...
						failureMotivation = FAIL_CONVERSION + "JSIM or JMODEL.";
						return FAILURE;
					default:
						failureMotivation = FAIL_UNKNOWN;
						return FAILURE;
				}
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

	// --- Methods used to save models ------------------------------------------------------------

	/**
	 * Saves specified model into specified file or shows save as window if file is null
	 * @param modelData data file where information should be stored. Note that <b>its type
	 * must be compatible with defaultFilter chosen in the constructor</b>, otherwise a
	 * ClassCastException will be thrown
	 * @param parent parent window that will own the save as dialog
	 * @param file location where pecified model must be saved or null if save as must be shown
	 * @return SUCCESS on success, CANCELLED if loading is cancelled,
	 * FAILURE if an error occurs
	 * @throws ClassCastException if modelData is not of instance of the correct class
	 * @see #getFailureMotivation getFailureMotivation()
	 */
	public int saveModel(Object modelData, Component parent, File file) {
		if (file == null) {
			// Shows save as window
			int status;
			status = this.showSaveDialog(parent);
			if (status == JFileChooser.CANCEL_OPTION) {
				return CANCELLED;
			} else if (status == JFileChooser.ERROR_OPTION) {
				failureMotivation = "Error selecting output file";
				return FAILURE;
			}
			file = dialog.getSelectedFile();
		} else
		// Check extension to avoid saving over a converted file
		if (!file.getName().endsWith(defaultFilter.getFirstExtension())) {
			int resultValue = JOptionPane.showConfirmDialog(parent, "<html>File <font color=#0000ff>" + file.getName() + "</font> does not have \""
					+ defaultFilter.getFirstExtension() + "\" extension.<br>Do you want to replace it anyway?</html>", "JMT - Warning",
					JOptionPane.OK_CANCEL_OPTION, JOptionPane.WARNING_MESSAGE);
			if (resultValue != JOptionPane.OK_OPTION) {
				return CANCELLED;
			}
		}

		// Now checks to save correct type of model
		try {
			if (defaultFilter == JMODEL || defaultFilter == JSIM) {
				XMLArchiver.saveModel(file, (CommonModel) modelData);
			} else if (defaultFilter == JMVA) {
				xmlutils.saveXML(((ExactModel) modelData).createDocument(), file);
			} else if (defaultFilter == JABA) {
				xmlutils.saveXML(((JabaModel) modelData).createDocument(), file);
			}
		} catch (Exception e) {
			failureMotivation = e.getClass().getName() + ": " + e.getMessage();
			return FAILURE;
		}
		return SUCCESS;
	}

	// --------------------------------------------------------------------------------------------

	// --- Methods to open and parse files --------------------------------------------------------
	protected int getXmlFileType(String fileName) {
		// Opens without validating (as we don't know document type)
		try {
			Document doc = XMLReader.loadXML(fileName);
			String root = doc.getDocumentElement().getNodeName();
			// Uses root name to determine document type
			if (root.equals(XMLConstantNames.XML_DOCUMENT_ROOT)) {
				return XML_SIM;
			} else if (root.equals(GuiXMLConstants.XML_ARCHIVE_DOCUMENT_ROOT)) {
				return XML_ARCHIVE;
			} else if (root.equals(ModelLoader.XML_MVA_ROOT)) {
				// Finds if this is JMVA or JABA model
				String jaba = doc.getDocumentElement().getAttribute("jaba");
				if (jaba != null && jaba.equals("true")) {
					return XML_JABA;
				} else {
					return XML_MVA;
				}
			} else if (root.equals(XMLResultsConstants.XML_DOCUMENT_O_ROOT)) {
				return XML_RES_SIM;
			} else if (root.equals(XMLResultsConstants.XML_DOCUMENT_ROOT)) {
				return XML_RES_GUI;
			} else {
				return FILE_UNKNOWN;
			}
		} catch (Exception e) {
			// If an exception is thrown, reports that format is unknown
			return FILE_UNKNOWN;
		}
	}

	// --------------------------------------------------------------------------------------------

	// --- Methods to show dialogs ----------------------------------------------------------------
	/**
	 * Adds all filters to current dialog
	 */
	protected void addAllFilters() {
		dialog.setAcceptAllFileFilterUsed(true);
		dialog.addChoosableFileFilter(ALL);
		dialog.addChoosableFileFilter(JMODEL);
		dialog.addChoosableFileFilter(JSIM);
		dialog.addChoosableFileFilter(JMVA);
		dialog.addChoosableFileFilter(JABA);
		dialog.addChoosableFileFilter(XML);
		//dialog.setFileFilter(defaultFilter);
		dialog.setFileFilter(ALL);
	}

	/**
	 * Adds only compatible filters to current dialog
	 */
	protected void addCompatibleFilters() {
		dialog.setAcceptAllFileFilterUsed(true);
		dialog.addChoosableFileFilter(ALL);
		if (defaultFilter == JABA) {
			dialog.addChoosableFileFilter(JABA);
			dialog.setFileFilter(JABA);
		} else {
			dialog.addChoosableFileFilter(ALL_NOTJABA);
			dialog.addChoosableFileFilter(JMODEL);
			dialog.addChoosableFileFilter(JSIM);
			dialog.addChoosableFileFilter(JMVA);
			dialog.setFileFilter(ALL_NOTJABA);
		}
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
		
	  //JMT
	  //addCompatibleFilters();
		
		// QN-Java
	  dialog.addChoosableFileFilter(JMODEL);
	  dialog.setFileFilter(JMODEL);
		dialog.setCurrentDirectory(new File(GlobalUtilities.getQNFrameworksFolderURI()));
		
		return dialog.showOpenDialog(parent);
	}

	/**
	 * Shows save file dialog
	 * @param    parent  the parent component of the dialog,
	 *			can be <code>null</code>;
	 *                  see <code>showDialog</code> for details
	 * @return   the return state of the file chooser on popdown:
	 * <ul>
	 * <li>JFileChooser.CANCEL_OPTION
	 * <li>JFileChooser.APPROVE_OPTION
	 * <li>JFileCHooser.ERROR_OPTION if an error occurs or the
	 *			dialog is dismissed
	 * </ul>
	 * @exception HeadlessException if GraphicsEnvironment.isHeadless()
	 * returns true.
	 * @see java.awt.GraphicsEnvironment#isHeadless
	 */
	protected int showSaveDialog(Component parent) {
		dialog.resetChoosableFileFilters();
		dialog.addChoosableFileFilter(defaultFilter);
		dialog.setFileFilter(defaultFilter);
		return dialog.showSaveDialog(parent);
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
				super.approveSelection();
			}
			if (getDialogType() == SAVE_DIALOG) {
				if (!name.toLowerCase().endsWith(defaultFilter.getFirstExtension())) {
					name = name + defaultFilter.getFirstExtension();
					setSelectedFile(new File(parent, name));
				}
				if (getSelectedFile().exists()) {
					int resultValue = JOptionPane.showConfirmDialog(this, "<html>File <font color=#0000ff>" + name
							+ "</font> already exists in this folder.<br>Do you want to replace it?</html>", "JMT - Warning",
							JOptionPane.OK_CANCEL_OPTION, JOptionPane.WARNING_MESSAGE);
					if (resultValue == JOptionPane.OK_OPTION) {
						getSelectedFile().delete();
						super.approveSelection();
					}
				} else {
					super.approveSelection();
				}
			}
		}
	}

	/**
	 * Inner class used to create simple file filters with only extension check
	 */
	protected static class JmtFileFilter extends FileFilter {
		public static final String SEP = ";";
		private String description;
		private String[] extensions;

		/**
		 * Creates a new filefilter with specified dot and comma separated list of 
		 * extensions and specified description
		 * @param extensionList comma separated list of extensions 
		 * of this filter (for example ".jmt;.jmodel")
		 * @param description description of this filter
		 */
		public JmtFileFilter(String extensionList, String description) {
			this.extensions = extensionList.split(SEP);
			this.description = createDescription(description, extensions);
		}

		/**
		 * Whether the given file is accepted by this filter.
		 */
		@Override
		public boolean accept(File f) {
			String name = f.getName().toLowerCase();
			if (f.isDirectory()) {
				return true;
			}
			for (String extension : extensions) {
				if (name.endsWith(extension)) {
					return true;
				}
			}
			return false;
		}

		/**
		 * Creates a suitable description for this filter
		 * @param text text description of the filter
		 * @param extensions extensions to be matched
		 * @return created description
		 */
		private String createDescription(String text, String[] extensions) {
			StringBuffer sb = new StringBuffer();
			sb.append(text);
			sb.append(" (*");
			for (int i = 0; i < extensions.length - 1; i++) {
				sb.append(extensions[i]);
				sb.append("; *");
			}
			sb.append(extensions[extensions.length - 1]);
			sb.append(")");
			return sb.toString();
		}

		/**
		 * The description of this filter
		 * @see javax.swing.filechooser.FileView#getName
		 */
		@Override
		public String getDescription() {
			return description;
		}

		/**
		 * Gets the first extension of this filter
		 * @return extension of this filter
		 */
		public String getFirstExtension() {
			return extensions[0];
		}
	}
	// --------------------------------------------------------------------------------------------
}
