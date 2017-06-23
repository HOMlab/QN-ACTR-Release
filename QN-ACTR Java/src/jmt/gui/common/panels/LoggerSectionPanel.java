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

package jmt.gui.common.panels;

import java.awt.Button;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;

import jmt.engine.log.JSimLogger;
import jmt.engine.log.LoggerParameters;
import jmt.framework.gui.help.HoverHelp;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.definitions.StationDefinition;

/**
 * <p>Title: LogTunnel Extension Panel</p>
 * <p>Description: This class extends the standard "WizardPanel" 
 * by adding logging options.</p>
 *
 * @author Michael Fercu
 *         Date: 20-feb-2008
 *         Time: 20.02.08
 *
 */

public class LoggerSectionPanel extends WizardPanel implements CommonConstants {

	private static final long serialVersionUID = 1L;

	private static final JSimLogger debugLog = JSimLogger.getLogger(JSimLogger.STD_LOGGER);

	private StationDefinition stationData;
	//private ClassDefinition classData;
	private Object stationKey;
	private String loggerStationName;
	private String loggerUniversalLogPath;
	private LoggerParameters loggerParameters;
	private File logFile;
	private boolean preventSaveReplacement;

	/* Constants */
	private static final String NAME_loggerAutoAppendDropdownAsk = "Ask to Replace";
	private static final String NAME_loggerAutoAppendDropdownReplace = "Always Replace";
	private static final String NAME_loggerAutoAppendDropdownAppend = "Append";

	/* GUI Components */

	/* Used to create a dropdown with filenames */
	protected String[] loggerOutputFilenameDropdown, loggerOutputDelimiterDropdown, loggerOutputDecimalSeparatorDropdown, loggerAutoAppendDropdown;

	/* Useful for global log */
	JComboBox autoAppendChooser, delimiterChooser, decimalSeparatorChooser, loggerChooser;
	JLabel labelFileStatus;
	JCheckBox checkExecutionTimestamp;

	HoverHelp help;

	/** Constructor for Logger Panel */
	public LoggerSectionPanel(StationDefinition sd, ClassDefinition cd, Object stationKey) {
		setData(sd, cd, stationKey);
		loggerStationName = stationData.getStationName(this.stationKey);
		loggerOutputFilenameDropdown = new String[] { LoggerParameters.GLOBALLOGNAME, loggerStationName + ".csv" };
		loggerOutputDelimiterDropdown = new String[] { ";", ",", "Tab", "Space" };
		loggerOutputDecimalSeparatorDropdown = new String[] { ".", "," };
		loggerAutoAppendDropdown = new String[3];
		loggerAutoAppendDropdown[LoggerParameters.LOGGER_AR_ASK] = NAME_loggerAutoAppendDropdownAsk;
		loggerAutoAppendDropdown[LoggerParameters.LOGGER_AR_REPLACE] = NAME_loggerAutoAppendDropdownReplace;
		loggerAutoAppendDropdown[LoggerParameters.LOGGER_AR_APPEND] = NAME_loggerAutoAppendDropdownAppend;
		loggerParameters = (LoggerParameters) stationData.getLoggingParameters(this.stationKey);
		loggerUniversalLogPath = sd.getLoggingGlbParameter("path");
		loggerParameters.path = loggerUniversalLogPath;
		logFile = new File(loggerParameters.name);
		initComponents();
	}

	public void setData(StationDefinition sd, ClassDefinition cd, Object stationKey) {
		stationData = sd;
		//    classData = cd;
		this.stationKey = stationKey;
	}

	protected void initComponents() {
		JPanel loggerPanelTop = new JPanel();
		JPanel loggerPanelTopLogfile = new JPanel();
		JPanel loggerPanelTR = new JPanel();
		JPanel loggerPanelMiddle = new JPanel();
		JPanel loggerPanelBottom = new JPanel();
		JPanel mainPanel = new JPanel();

		this.setLayout(new GridBagLayout());
		this.setBorder(new EmptyBorder(0, 0, 0, 0));

		// layout of main pane
		mainPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		mainPanel.setLayout(new GridBagLayout());
		this.add(mainPanel, new GridBagConstraints(0, 0, 1, 1, 1, 1, GridBagConstraints.NORTHWEST, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0),
				0, 0));

		help = new HoverHelp();
		help.getHelpLabel().setBorder(BorderFactory.createEtchedBorder());
		help.getHelpLabel().setMinimumSize(new Dimension(585, 20));
		help.getHelpLabel().setMaximumSize(new Dimension(695, 20));
		this.add(help.getHelpLabel(), new GridBagConstraints(0, 1, 1, 1, 0.1, 0.0, GridBagConstraints.SOUTHWEST, GridBagConstraints.HORIZONTAL,
				new Insets(0, 0, 0, 0), 0, 0));

		GridBagConstraints gbc = new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, 10, 0, new Insets(0, 4, 6, 0), 0, 0);

		//layout of top panel
		loggerPanelTop.setLayout(new GridBagLayout());
		loggerPanelTop.setBorder(new TitledBorder(new EtchedBorder(), "Logging Options"));
		// layout of north: dropdown-box for filename, and status
		loggerPanelTopLogfile.setLayout(new FlowLayout(FlowLayout.LEFT, 0, 0));
		loggerPanelTopLogfile.setBorder(new EmptyBorder(3, 0, 3, 0));
		loggerChooser = new JComboBox(loggerOutputFilenameDropdown);
		loggerPanelTopLogfile.add(new JLabel("Logfile: "));
		loggerPanelTopLogfile.add(loggerChooser);
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.gridx = 0;
		gbc.gridy = 0;
		loggerPanelTop.add(loggerPanelTopLogfile, gbc);
		checkExecutionTimestamp = new JCheckBox();
		checkExecutionTimestamp.setText("Simul. Start Timestamp  ");
		help.addHelp(checkExecutionTimestamp, "Simulation Timestamp is useful to identify the run number when appending.  (e.g. "
				+ "31102008T125959GMT" + ")");
		gbc.gridx = 2;
		gbc.gridy = 0;
		loggerPanelTop.add(checkExecutionTimestamp, gbc);
		/*
		labelFileStatus = new JLabel();
		gbc.fill = GridBagConstraints.REMAINDER; gbc.gridx = 1; gbc.gridy = 0; gbc.gridwidth=3;
		loggerPanelTop.add(labelFileStatus,gbc);
		*/
		// layout of north: checkbox fields
		final JCheckBox checkLoggername = new JCheckBox();
		checkLoggername.setText("Logger Name  ");
		help.addHelp(checkLoggername, "Name of the logger class.  (e.g. " + "Logger0" + ")");
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.gridx = 0;
		gbc.gridy = 1;
		loggerPanelTop.add(checkLoggername, gbc);
		final JCheckBox checkTimeStamp = new JCheckBox();
		checkTimeStamp.setText("Timestamp  ");
		help.addHelp(checkTimeStamp, "Timestamp of arrival of the job (ref. to sim time).  (e.g. 0.7532)");
		gbc.gridx = 1;
		gbc.gridy = 1;
		loggerPanelTop.add(checkTimeStamp, gbc);
		final JCheckBox checkJobID = new JCheckBox();
		checkJobID.setText("Job ID  ");
		help.addHelp(checkJobID, "Unique identification number of the job. (e.g. 153)");
		gbc.gridx = 2;
		gbc.gridy = 1;
		loggerPanelTop.add(checkJobID, gbc);
		final JCheckBox checkJobClass = new JCheckBox();
		checkJobClass.setText("Class ID  ");
		help.addHelp(checkJobClass, "Identifiation number of job class.  (e.g. Class0)");
		gbc.gridx = 0;
		gbc.gridy = 2;
		loggerPanelTop.add(checkJobClass, gbc);
		final JCheckBox checkTimeSameClass = new JCheckBox();
		checkTimeSameClass.setText("Interarrival time (same class)      ");
		help.addHelp(checkTimeSameClass, "Time elapsed since the last arrival of the same class. (e.g. 0.7532)");
		gbc.gridx = 1;
		gbc.gridy = 2;
		loggerPanelTop.add(checkTimeSameClass, gbc);
		final JCheckBox checkTimeAnyClass = new JCheckBox();
		checkTimeAnyClass.setText("Interarrival time (any class)  ");
		help.addHelp(checkTimeAnyClass, "Time elapsed since the last arrival of any class. (e.g. 0.7532)");
		gbc.gridx = 2;
		gbc.gridy = 2;
		loggerPanelTop.add(checkTimeAnyClass, gbc);

		//layout of top-right panel
		loggerPanelTR.setLayout(new GridLayout(4, 2, 0, 6));
		loggerPanelTR.setBorder(new TitledBorder(new EtchedBorder(), "Logfile Options"));
		//layout of top-right: choosers
		JLabel cpLabel = new JLabel("Change Path: ");
		loggerPanelTR.add(cpLabel);
		help.addHelp(cpLabel, "Choose path of the log file");
		final Button filepathButton = new Button("Browse");
		loggerPanelTR.add(filepathButton, gbc);
		autoAppendChooser = new JComboBox(loggerAutoAppendDropdown);
		JLabel owLabel = new JLabel("Overwrite: ");
		loggerPanelTR.add(owLabel);
		help.addHelp(owLabel, "Owerwrite the existing logfile with the same name or append the data to the existing file. ");
		loggerPanelTR.add(autoAppendChooser);
		delimiterChooser = new JComboBox(loggerOutputDelimiterDropdown);
		JLabel delLabel = new JLabel("Delimiter: ");
		loggerPanelTR.add(delLabel);
		help.addHelp(delLabel, "Delimiter between rows");
		loggerPanelTR.add(delimiterChooser);
		decimalSeparatorChooser = new JComboBox(loggerOutputDecimalSeparatorDropdown);
		JLabel dsLabel = new JLabel("Decimal separator: ");
		loggerPanelTR.add(dsLabel);
		help.addHelp(dsLabel, "Decimal separator, choose \".\" for English");
		loggerPanelTR.add(decimalSeparatorChooser);

		//layout of middle panel
		loggerPanelMiddle.setLayout(new GridBagLayout());
		loggerPanelMiddle.setBorder(new TitledBorder(new EtchedBorder(), "Preview"));

		//layout of bottom panel
		loggerPanelBottom.setLayout(new FlowLayout(FlowLayout.LEFT));
		loggerPanelBottom.setBorder(new EtchedBorder());
		labelFileStatus = new JLabel("(unknown)");
		help.addHelp(labelFileStatus, "Displays the file name and file size if it exists, or \"(empty file)\" if it does not exist");
		loggerPanelBottom.add(labelFileStatus, FlowLayout.LEFT);

		// Finishing up layout
		loggerPanelTR.setMinimumSize(new Dimension(130, 130));
		loggerPanelBottom.setMinimumSize(new Dimension(480, 50));
		mainPanel.setMinimumSize(new Dimension(585, 150));
		mainPanel.setMaximumSize(new Dimension(695, 350));
		// Finally add the panels to main pane
		gbc = new GridBagConstraints(0, 0, 1, 1, 0.3, 0.75, GridBagConstraints.NORTHWEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0);
		mainPanel.add(loggerPanelTop, gbc);
		gbc = new GridBagConstraints(1, 0, 1, 1, 0.3, 0.25, GridBagConstraints.NORTHEAST, GridBagConstraints.HORIZONTAL, new Insets(0, 1, 1, 1), 0, 0);
		mainPanel.add(loggerPanelTR, gbc);
		gbc = new GridBagConstraints(0, 2, 2, 1, 0.1, 0.0, GridBagConstraints.SOUTHWEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0);
		mainPanel.add(loggerPanelBottom, gbc);
		gbc = null;

		/* **************** *
		 *  Initial Values  *
		 ****************** */
		loggerChooser.setSelectedIndex(loggerChooserGetIndex());
		loggerChooserLoadValues();
		checkExecutionTimestamp.setSelected(loggerParameters.boolExecTimestamp.booleanValue());
		checkLoggername.setSelected(loggerParameters.boolLoggername.booleanValue());
		checkTimeStamp.setSelected(loggerParameters.boolTimeStamp.booleanValue());
		checkJobID.setSelected(loggerParameters.boolJobID.booleanValue());
		checkJobClass.setSelected(loggerParameters.boolJobClass.booleanValue());
		checkTimeSameClass.setSelected(loggerParameters.boolTimeSameClass.booleanValue());
		checkTimeAnyClass.setSelected(loggerParameters.boolTimeAnyClass.booleanValue());

		/* Event Listeners */
		loggerChooser.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {

				if (loggerChooser.getSelectedIndex() == 0) {
					loggerParameters.name = LoggerParameters.GLOBALLOGNAME;
				} else if (loggerChooser.getSelectedIndex() == 1) {
					loggerParameters.name = loggerStationName + ".csv";
				} else if (loggerChooser.getSelectedIndex() == 2) {
					loggerParameters.name = loggerParameters.name;
				}

				/*
				else if (loggerChooser.getSelectedIndex() == 2)
				{
					String tmpFileName = "";
					JFileChooser fc = new JFileChooser();
				    fc.setFileSelectionMode(JFileChooser.FILES_ONLY );
				    fc.setDialogTitle("Choose File Name of Log...");
				    fc.setCurrentDirectory(new java.io.File(loggerUniversalLogPath));
				    int ret = fc.showSaveDialog(LoggerSectionPanel.this);
				    
				    if (ret == JFileChooser.APPROVE_OPTION) {
				        java.io.File thefile = fc.getSelectedFile();
				        tmpFileName = thefile.getName();
				        //stationData.setLoggingGlbParameter("path", loggerUniversalLogPath);
				    }
				    
					loggerParameters.name = tmpFileName;
				}
				*/
				loggerChooserLoadValues();
			}
		});
		delimiterChooser.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setDelimiterAndReplacement(loggerParameters.isGlobal());
			}
		});
		decimalSeparatorChooser.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setDelimiterAndReplacement(loggerParameters.isGlobal());
			}
		});
		autoAppendChooser.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setDelimiterAndReplacement(loggerParameters.isGlobal());
			}
		});
		checkExecutionTimestamp.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				loggerParameters.boolExecTimestamp = new Boolean(checkExecutionTimestamp.isSelected());
			}
		});
		checkLoggername.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				loggerParameters.boolLoggername = new Boolean(checkLoggername.isSelected());
			}
		});
		checkTimeStamp.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				loggerParameters.boolTimeStamp = new Boolean(checkTimeStamp.isSelected());
			}
		});
		checkJobID.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				loggerParameters.boolJobID = new Boolean(checkJobID.isSelected());
			}
		});
		checkJobClass.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				loggerParameters.boolJobClass = new Boolean(checkJobClass.isSelected());
			}
		});
		checkTimeSameClass.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				loggerParameters.boolTimeSameClass = new Boolean(checkTimeSameClass.isSelected());
			}
		});
		checkTimeAnyClass.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				loggerParameters.boolTimeAnyClass = new Boolean(checkTimeAnyClass.isSelected());
			}
		});
		filepathButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {

				JFileChooser fc = new JFileChooser();
				fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
				fc.setDialogTitle("Choose Save Path for all logfiles...");
				fc.setCurrentDirectory(new java.io.File(loggerUniversalLogPath));
				int ret = fc.showSaveDialog(LoggerSectionPanel.this);

				if (ret == JFileChooser.APPROVE_OPTION) {
					java.io.File directory = fc.getSelectedFile();
					loggerUniversalLogPath = directory.getAbsolutePath();
					loggerParameters.path = loggerUniversalLogPath;
					stationData.setLoggingGlbParameter("path", loggerUniversalLogPath);
				}

				updateFileLabelStatus();
			}
		});

		/* finished initializing components */
	}

	/**
	 * called by the Wizard before when switching to another panel
	 */
	@Override
	public void lostFocus() {
		loggerParameters.path = loggerUniversalLogPath; // temporary fix
		stationData.setLoggingParameters(stationKey, loggerParameters);
	}

	/**
	 * called by the Wizard when the panel becomes active
	 */
	@Override
	public void gotFocus() {
		updateFileLabelStatus();
	}

	/**
	 * @return the panel's name
	 */
	@Override
	public String getName() {
		return "Logger Section";
	}

	/*
	 * Helper functions
	 */
	private int loggerChooserGetIndex() {
		try {

			if (loggerParameters.name.equalsIgnoreCase(LoggerParameters.GLOBALLOGNAME)) {
				return 0;
			} else if (loggerParameters.name.startsWith(loggerStationName)) {
				return 1;
			} else if (loggerParameters.name.length() > 0) {
				loggerChooser.addItem(loggerParameters.name);
				loggerChooser.updateUI();
				return 2;
			} else {
				debugLog.debug("[LoggerSectionPanel].loggerChooserGetIndex parsed \"" + loggerParameters.name + "\" and defaulted to global.");
				return 0;
			}

		} catch (Exception e) {
			debugLog.error("[LoggerSectionPanel].loggerChooserGetIndex parsed \"" + loggerParameters.name + "\" and faulted: " + e.toString());
			return 0;
		}
	}

	private void loggerChooserLoadValues() {
		preventSaveReplacement = true; // keeps the other combo boxes (delim/append) from changing
		//if (loggerParameters.isGlobal() == true) {
		delimiterChooser.setSelectedIndex(getDelimiterIndex(stationData.getLoggingGlbParameter("delim")));
		decimalSeparatorChooser.setSelectedIndex(getDecimalSeparatorIndex(stationData.getLoggingGlbParameter("decimalSeparator")));
		autoAppendChooser.setSelectedIndex(Integer.parseInt(stationData.getLoggingGlbParameter("autoAppend")));
		/*// this code is now redundant; since all delimiters and appenders are global-only
		} else
		{
			delimiterChooser.setSelectedIndex(getDelimiterIndex(loggerParameters.delimiter.toString()));
		    autoAppendChooser.setSelectedIndex(loggerParameters.intAutoAppend.intValue());
		} */
		updateFileLabelStatus();
		preventSaveReplacement = false;
	}

	/* Helper function helpers */

	private int getDelimiterIndex(String s) {
		char c = s.charAt(0);
		String searchString;

		if (c == ' ') {
			searchString = "Space";
		} else if (s.startsWith("\\t") || c == '\t') {
			searchString = "Tab";
		} else if (s.startsWith("\\") == false) {
			searchString = "" + c;
		} else {
			return 0;
		}

		// Case 1: string in drop-down box.
		for (int i = 0; i < loggerOutputDelimiterDropdown.length; i++) {
			if (loggerOutputDelimiterDropdown[i].equals(searchString) == true) {
				return i;
			}
		}

		// Case 2: add a new character to dropdown box (extensibility)
		delimiterChooser.insertItemAt(searchString, loggerOutputDelimiterDropdown.length - 1);
		return (loggerOutputDelimiterDropdown.length - 1);
	}

	private int getDecimalSeparatorIndex(String s) {
		if (s.equals(",")) {
			return 1;
		}
		return 0;
	}

	private void setDelimiterAndReplacement(boolean isGlobal) {
		Character delim;
		Integer autoAppendPolicy;

		// Ignores mistaken change from loggerChooser (when out of focus)
		if (preventSaveReplacement == true) {
			return;
		}

		// Sets the delimiter parameter for local log
		if (loggerOutputDelimiterDropdown[delimiterChooser.getSelectedIndex()].equalsIgnoreCase("Space")) {
			delim = new Character(' ');
		} else if (loggerOutputDelimiterDropdown[delimiterChooser.getSelectedIndex()].equalsIgnoreCase("Tab")) {
			delim = new Character('\t');
		} else {
			delim = new Character(loggerOutputDelimiterDropdown[delimiterChooser.getSelectedIndex()].charAt(0));
		}

		// Set the append/replace parameter for local log
		String tmp = loggerAutoAppendDropdown[autoAppendChooser.getSelectedIndex()];
		if (tmp.equalsIgnoreCase(NAME_loggerAutoAppendDropdownReplace)) {
			autoAppendPolicy = new Integer(LoggerParameters.LOGGER_AR_REPLACE);
		} else if (tmp.equalsIgnoreCase(NAME_loggerAutoAppendDropdownAppend)) {
			autoAppendPolicy = new Integer(LoggerParameters.LOGGER_AR_APPEND);
		} else if (tmp.equalsIgnoreCase(NAME_loggerAutoAppendDropdownAsk)) {
			autoAppendPolicy = new Integer(LoggerParameters.LOGGER_AR_ASK);
		} else {
			autoAppendPolicy = new Integer(LoggerParameters.LOGGER_AR_ASK);
		}
		tmp = null;

		/* MF (Bertoli suggests): global append should be the only mode for 0.7.4 
		if (isGlobal == false)
		{
		
			// If not logging to global.csv, set the delimiter and append policy per logger
			loggerParameters.delimiter = delim;
			loggerParameters.intAutoAppend = autoAppendPolicy;
			*/

		// Finally, set the globally effective 'delimiter' and 'append' mode for logs
		stationData.setLoggingGlbParameter("delim", delim.toString());
		stationData.setLoggingGlbParameter("decimalSeparator", loggerOutputDecimalSeparatorDropdown[decimalSeparatorChooser.getSelectedIndex()]);
		stationData.setLoggingGlbParameter("autoAppend", autoAppendPolicy.toString());
	}

	private String updateFileLabelStatus() {
		String tmpStatus = "";
		String path = loggerUniversalLogPath;

		// Make pathname pretty: remove the leading period, it confuses java.io.File.exists() method.
		if ((path.equals(".") == true) || (path.equals("." + File.separator) == true)) {
			path = "";
		}
		// Add a trailing slash
		if ((path.length() > 0) && (path.endsWith(File.separator) == false) && (path.endsWith("/") == false)) {
			path = path + File.separator;
		}

		try {
			logFile = new File(path + loggerParameters.name);

			if (logFile.exists() == true) {
				if (logFile.length() == 0) {
					tmpStatus = "  (empty file)";
				} else {
					tmpStatus = "  (" + logFile.length() / 1024 + "KB file)";
				}
			} else {
				tmpStatus = "  (does not exist).";
			}

			labelFileStatus.setText("File:  " + logFile.getAbsolutePath() + " " + tmpStatus);
		} catch (Exception e) {
			labelFileStatus.setText("Caught exception: " + e.getCause() + " trying to access " + loggerParameters.name + " File.");
		}

		LoggerSectionPanel.this.doLayout();
		LoggerSectionPanel.this.repaint();

		return tmpStatus;
	}
}
