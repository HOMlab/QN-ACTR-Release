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

package jmt.jmarkov;

import java.awt.Color;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.filechooser.FileFilter;

import jmt.jmarkov.Graphics.LogFile;

/* 1.4 example used by DialogDemo.java. */
class CustomDialog extends JDialog implements ActionListener, PropertyChangeListener {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private int typedValue;
	private String typedText = null;
	private JPanel selectionP;
	private GridBagConstraints c;
	private JTextField textField;
	private JRadioButton nolimitRB, limitedRB;
	//in order to ask the log file will be generated or not
	private JCheckBox logCheckBox;
	private JTextField logTextField;
	private JButton logButton;

	private JRadioButton tabDelimited, commaDelimited, semiColonDelimited;
	private JPanel jp;

	private File file;

	private JOptionPane optionPane;

	private String btnString1 = "Enter";

	/**
	 * Returns null if the typed string was invalid;
	 * otherwise, returns the string as the user entered it.
	 */
	public String getValidatedText() {
		return typedText;
	}

	public int getValidatedValue() {
		return typedValue;
	}

	/** Creates the reusable dialog. */
	public CustomDialog(Frame aFrame) {
		super(aFrame, true);
		setTitle("Enter n. of cust. to simulate");
		textField = new JTextField(10);
		textField.setEnabled(false);
		textField.addKeyListener(new KeyAdapter() {
			@Override
			public void keyTyped(KeyEvent e) {
				char c = e.getKeyChar();
				if (!((c >= '0') && (c <= '9') || (c == KeyEvent.VK_BACK_SPACE) || (c == KeyEvent.VK_DELETE))) {
					getToolkit().beep();
					e.consume();
				}
			}
		});
		textField.setBackground(Color.LIGHT_GRAY);
		nolimitRB = new JRadioButton("Unlimited", true);
		nolimitRB.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				nolimitRB.setSelected(true);
				limitedRB.setSelected(false);
				textField.setEnabled(false);
				textField.setBackground(Color.LIGHT_GRAY);
			}
		});
		limitedRB = new JRadioButton("Limited (Type in)", false);
		limitedRB.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				limitedRB.setSelected(true);
				nolimitRB.setSelected(false);
				textField.setEnabled(true);
				textField.setBackground(Color.WHITE);
			}
		});

		logCheckBox = new JCheckBox("Log file");
		logCheckBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (logCheckBox.isSelected()) {
					logButton.setEnabled(true);
					logTextField.setEnabled(true);
					//					logTextField.setBackground(Color.WHITE);
					logButton.setVisible(true);
					logTextField.setVisible(true);
					jp.setVisible(true);
					pack();

				} else {
					logButton.setEnabled(false);
					logTextField.setEnabled(false);
					// 					logTextField.setBackground(Color.LIGHT_GRAY);
					logButton.setVisible(false);
					logTextField.setVisible(false);
					jp.setVisible(false);
					pack();

				}
			}
		});

		file = new File("log_JMCH.csv");
		logTextField = new JTextField(file.getAbsolutePath().toString(), 30);
		logTextField.setEditable(false);
		logTextField.setEnabled(false);
		logTextField.setBackground(Color.LIGHT_GRAY);

		logButton = new JButton("Browse");
		logButton.setEnabled(false);
		logButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				file = saveFileDialog(file);
				if (file == null) {
					logTextField.setText("");
				} else {
					logTextField.setText(file.getAbsolutePath().toString());
				}
			}
		});

		//adding to panel
		selectionP = new JPanel(new GridBagLayout());
		c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
		c.anchor = GridBagConstraints.LINE_START;
		c.gridx = 0;
		c.gridy = 0;
		selectionP.add(nolimitRB, c);
		c.gridy = 1;
		selectionP.add(limitedRB, c);
		c.gridx = 1;
		selectionP.add(textField, c);

		c.gridx = 0;
		c.gridy = 2;
		selectionP.add(logCheckBox, c);

		c.gridy = 3;
		selectionP.add(logTextField, c);

		c.gridx = 1;
		selectionP.add(logButton, c);

		c.gridy = 4;
		c.gridx = 0;

		tabDelimited = new JRadioButton("Tab");
		tabDelimited.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				tabDelimited.setSelected(true);
				commaDelimited.setSelected(false);
				semiColonDelimited.setSelected(false);
			}
		});

		commaDelimited = new JRadioButton("Comma");
		commaDelimited.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				tabDelimited.setSelected(false);
				commaDelimited.setSelected(true);
				semiColonDelimited.setSelected(false);
			}
		});
		commaDelimited.setSelected(true);

		semiColonDelimited = new JRadioButton("Semicolon");
		semiColonDelimited.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				tabDelimited.setSelected(false);
				commaDelimited.setSelected(false);
				semiColonDelimited.setSelected(true);
			}
		});

		jp = new JPanel();

		jp.add(tabDelimited);
		jp.add(commaDelimited);
		jp.add(semiColonDelimited);
		jp.setVisible(false);

		selectionP.add(jp, c);

		logButton.setVisible(false);
		logTextField.setVisible(false);
		jp.setVisible(false);

		//Create an array of the text and components to be displayed.
		String msgString1 = "Select how many customer";
		String msgString2 = "you want to simulate?";
		Object[] array = { msgString1, msgString2, selectionP };

		//Create an array specifying the number of dialog buttons
		//and their text.
		Object[] options = { btnString1 };

		//Create the JOptionPane.
		optionPane = new JOptionPane(array, JOptionPane.QUESTION_MESSAGE, JOptionPane.YES_NO_OPTION, null, options, options[0]);

		//Make this dialog display it.
		setContentPane(optionPane);

		//Handle window closing correctly.
		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
		addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosing(WindowEvent we) {
				/*
				 * Instead of directly closing the window,
				 * we're going to change the JOptionPane's
				 * value property.
				 */
				optionPane.setValue(new Integer(JOptionPane.CLOSED_OPTION));
			}
		});

		//Ensure the text field always gets the first focus.
		addComponentListener(new ComponentAdapter() {
			@Override
			public void componentShown(ComponentEvent ce) {
				textField.requestFocusInWindow();
			}
		});

		//Register an event handler that puts the text into the option pane.
		textField.addActionListener(this);

		//Register an event handler that reacts to option pane state changes.
		optionPane.addPropertyChangeListener(this);
	}

	/** This method handles events for the text field. */
	public void actionPerformed(ActionEvent e) {
		optionPane.setValue(btnString1);
	}

	/** This method reacts to state changes in the option pane. */
	public void propertyChange(PropertyChangeEvent e) {
		String prop = e.getPropertyName();

		if (isVisible() && (e.getSource() == optionPane)
				&& (JOptionPane.VALUE_PROPERTY.equals(prop) || JOptionPane.INPUT_VALUE_PROPERTY.equals(prop))) {
			Object value = optionPane.getValue();

			if (value == JOptionPane.UNINITIALIZED_VALUE) {
				//ignore reset
				return;
			}

			//Reset the JOptionPane's value.
			//If you don't do this, then if the user
			//presses the same button next time, no
			//property change event will be fired.
			optionPane.setValue(JOptionPane.UNINITIALIZED_VALUE);

			if (btnString1.equals(value)) {
				if (nolimitRB.isSelected()) {
					typedText = "unlimited";
					typedValue = 0;
					clearAndHide();
				} else {
					typedText = textField.getText();
					try {
						typedValue = Integer.parseInt(typedText);
					} catch (NumberFormatException nfe) {
						typedValue = 0;
					}
					if ((typedValue > 100000) || (typedValue < 1)) {
						//text was invalid
						textField.selectAll();
						JOptionPane.showMessageDialog(CustomDialog.this, "Sorry, '" + typedValue + "' " + "isn't a valid response.\n"
								+ "Please enter a number between 1 and 100'000.", "Please try again", JOptionPane.ERROR_MESSAGE);
						typedText = null;
						typedValue = 0;
						textField.requestFocusInWindow();
					} else {
						clearAndHide();
					}
				}
			} else { //user closed dialog or clicked cancel
				typedValue = 0;
				typedText = null;
				clearAndHide();
			}
		}
	}

	/** This method clears the dialog and hides it. */
	public void clearAndHide() {
		textField.setText(null);
		setVisible(false);
	}

	public boolean isLogging() {
		return logCheckBox.isSelected();
	}

	public File getLogFile() {
		if (isLogging() == false) {
			return null;
		}
		return file;
	}

	public int getDelimiterType() {
		if (commaDelimited.isSelected()) {
			return LogFile.COMMA_DELIMITED;
		} else if (tabDelimited.isSelected()) {
			return LogFile.TABBED_DELIMITED;
		} else if (semiColonDelimited.isSelected()) {
			return LogFile.SEMI_COLON_DELIMITED;
		}
		return -1;
	}

	File saveFileDialog(File file) {
		FileFilter fFileFilter = new FileFilter() {
			@Override
			public boolean accept(File f) {
				return f.getName().toLowerCase().endsWith(".csv") || f.isDirectory();
			}

			@Override
			public String getDescription() {
				return "Comma delimited (*.csv)";
			}
		};

		JFileChooser fc = new JFileChooser();

		// Start in current directory
		fc.setCurrentDirectory(file);

		// Set filter for Java source files.
		fc.setFileFilter(fFileFilter);

		// Set to a default name for save.
		fc.setSelectedFile(file);

		// Open chooser dialog
		int result = fc.showSaveDialog(this);

		if (result == JFileChooser.CANCEL_OPTION) {
			return null;
		} else if (result == JFileChooser.APPROVE_OPTION) {
			file = fc.getSelectedFile();
			if (file.exists()) {
				int response = JOptionPane.showConfirmDialog(null, "Overwrite existing file?", "Confirm Overwrite", JOptionPane.OK_CANCEL_OPTION,
						JOptionPane.QUESTION_MESSAGE);
				if (response == JOptionPane.CANCEL_OPTION) {
					return null;
				}
			}
			return file;
		} else {
			return null;
		}
	}
}
