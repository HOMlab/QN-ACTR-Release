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
import java.awt.Label;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.KeyStroke;

/* 1.4 example used by DialogDemo.java. */
class CustomDialogMethod extends JDialog implements ActionListener, PropertyChangeListener {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private int typedValue;
	private String selectedMethod = null;
	private JPanel selectionP;
	private GridBagConstraints c;
	private JTextField textField;
	private JRadioButton mm1RB, mm1kRB, mmnRB, mmnkRB;
	private JOptionPane optionPane;
	private Label numberLabel;

	private String btnString1 = "Enter";
	private String btnString2 = "Cancel";
	private MMQueues frame;

	// maximum number of server (c) 
	private int maximumServer = 30;

	/**
	 * Returns null if the typed string was invalid;
	 * otherwise, returns the string as the user entered it.
	 */
	public String getSelectedMethod() {
		return selectedMethod;
	}

	public int getValidatedValue() {
		return typedValue;
	}

	/** Creates the reusable dialog. */
	public CustomDialogMethod(Frame aFrame) {
		super(aFrame, true);
		frame = (MMQueues) aFrame;
		setTitle("Select The Station Type");
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
		mm1RB = new JRadioButton("M/M/1 Station, 1 Server", true);
		mm1RB.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				mm1RB.setSelected(true);
				mm1kRB.setSelected(false);
				mmnRB.setSelected(false);
				mmnkRB.setSelected(false);
				textField.setEnabled(false);
				textField.setText("");
				textField.setBackground(Color.LIGHT_GRAY);
				numberLabel.setEnabled(false);

			}
		});
		mm1kRB = new JRadioButton("M/M/1/k Finite Capacity Station, 1 Server", false);
		mm1kRB.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				mm1RB.setSelected(false);
				mm1kRB.setSelected(true);
				mmnRB.setSelected(false);
				mmnkRB.setSelected(false);
				textField.setEnabled(false);
				textField.setText("");
				textField.setBackground(Color.LIGHT_GRAY);
				numberLabel.setEnabled(false);
			}
		});

		mmnRB = new JRadioButton("M/M/c Station, c Servers", false);
		mmnRB.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				mm1RB.setSelected(false);
				mm1kRB.setSelected(false);
				mmnRB.setSelected(true);
				mmnkRB.setSelected(false);
				textField.setEnabled(true);
				textField.setBackground(Color.WHITE);
				numberLabel.setEnabled(true);
			}
		});

		mmnkRB = new JRadioButton("M/M/c/k Finite Capacity Station, c Servers", false);
		mmnkRB.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				mm1RB.setSelected(false);
				mm1kRB.setSelected(false);
				mmnRB.setSelected(false);
				mmnkRB.setSelected(true);
				textField.setEnabled(true);
				textField.setBackground(Color.WHITE);
				numberLabel.setEnabled(true);

			}
		});

		//adding to panel
		selectionP = new JPanel(new GridBagLayout());
		c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
		c.anchor = GridBagConstraints.LINE_START;
		c.gridx = 0;
		c.gridy = 0;
		selectionP.add(mm1RB, c);
		c.gridy = 1;
		selectionP.add(mm1kRB, c);
		c.gridy = 2;
		selectionP.add(mmnRB, c);
		c.gridy = 3;
		selectionP.add(mmnkRB, c);

		JPanel numberOfStationPanel = new JPanel(new GridBagLayout());

		c.gridy = 0;
		numberLabel = new Label("     Number of servers c =");
		numberOfStationPanel.add(numberLabel, c);
		c.gridx = 1;
		numberOfStationPanel.add(textField, c);

		c.gridy = 4;
		c.gridx = 0;
		selectionP.add(numberOfStationPanel, c);
		numberLabel.setEnabled(false);

		//Create an array of the text and components to be displayed.
		String msgString1 = "";
		//String msgString2 = "";
		Object[] array = { msgString1, selectionP };

		//Create an array specifying the number of dialog buttons
		//and their text.
		Object[] options = { btnString1 };

		//Create the JOptionPane.
		optionPane = new JOptionPane(array, JOptionPane.DEFAULT_OPTION, JOptionPane.OK_OPTION, null, options, options[0]);
		InputMap im = optionPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
		im.put(KeyStroke.getKeyStroke("pressed ESCAPE"), "none");

		//Make this dialog display it.
		setContentPane(optionPane);

		//Handle window closing correctly.
		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);

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
				if (mm1RB.isSelected()) {
					selectedMethod = "mm1";
					typedValue = 0;
					clearAndHide();
				} else if (mm1kRB.isSelected()) {
					selectedMethod = "mm1k";
					typedValue = 0;
					clearAndHide();
				}

				else {

					if (mmnRB.isSelected()) {
						selectedMethod = "mmn";
					} else if (mmnkRB.isSelected()) {
						selectedMethod = "mmnk";
					}

					try {
						typedValue = Integer.parseInt(textField.getText());
					} catch (NumberFormatException nfe) {
						typedValue = 0;
					}
					if ((typedValue > maximumServer) || (typedValue < 1)) {
						//text was invalid
						textField.selectAll();
						JOptionPane.showMessageDialog(CustomDialogMethod.this, "Sorry, '" + typedValue + "' " + "isn't a valid response.\n"
								+ "Please enter a number between 2 and " + maximumServer + ".", "Please try again", JOptionPane.ERROR_MESSAGE);
						selectedMethod = null;
						typedValue = 0;
						textField.requestFocusInWindow();
					} else {
						clearAndHide();
					}
				}
			} else { //user closed dialog or clicked cancel
				typedValue = 0;
				selectedMethod = null;
				clearAndHide();
			}
		}
	}

	/** This method clears the dialog and hides it. */
	public void clearAndHide() {
		textField.setText(null);
		setVisible(false);
	}

}
