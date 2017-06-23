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

package jmt.gui.common.editors;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dialog;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.util.HashMap;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.TableModelEvent;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import jmt.framework.gui.components.HtmlPanel;
import jmt.framework.gui.components.JMTDialog;
import jmt.framework.gui.table.editors.ButtonCellEditor;
import jmt.framework.gui.table.editors.ComboBoxCellEditor;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.distributions.Distribution;
import jmt.gui.common.resources.JMTImageLoader;
import jmt.gui.common.serviceStrategies.LDStrategy;

/**
 * <p>Title: Load Dependent Service Time Strategy Editor</p>
 * <p>Description: An editor used to parametrize a load dependent service
 * time strategy. This is a modal JDialog.</p>
 * 
 * @author Bertoli Marco
 *         Date: 13-ott-2005
 *         Time: 11.49.53
 */
public class LDStrategyEditor extends JMTDialog implements CommonConstants {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected static final int BORDERSIZE = 20;
	protected static final int MAXRANGES = 1000;
	public static final String HELPFILE = "ParserHelp.html";
	protected static HashMap<String, Distribution> distributions;
	protected LDStrategy strategy;
	protected JTable rangesTable;

	// --- Static methods ------------------------------------------------------------------------------
	/**
	 * Returns a new instance of LDStrategyEditor, given parent container (used to find
	 * top level Dialog or Frame to create this dialog as modal)
	 * @param parent any type of container contained in a Frame or Dialog
	 * @param strategy LDStrategy to be modified
	 * @return new instance of LDStrategyEditor
	 */
	public static LDStrategyEditor getInstance(Container parent, LDStrategy strategy) {
		// Finds top level Dialog or Frame to invoke correct costructor
		while (!(parent instanceof Frame || parent instanceof Dialog)) {
			parent = parent.getParent();
		}

		if (parent instanceof Frame) {
			return new LDStrategyEditor((Frame) parent, strategy);
		} else {
			return new LDStrategyEditor((Dialog) parent, strategy);
		}
	}

	/**
	 * Uses reflection to return an HashMap of distributions suitable for LDStrategy.
	 * (With mean value specifiable) Search's key is distribution name and value is the
	 * Class of found distribution
	 * @return found distributions
	 */
	protected static HashMap<String, Distribution> findDistributions() {
		Distribution[] all = Distribution.findAllWithMean();
		HashMap<String, Distribution> tmp = new HashMap<String, Distribution>();
		for (Distribution element : all) {
			tmp.put(element.getName(), element);
		}
		return tmp;
	}

	// -------------------------------------------------------------------------------------------------

	// --- Constructors --------------------------------------------------------------------------------
	/**
	 * Builds a new LDStrategyEditor Dialog. This dialog is designed to be modal.
	 * @param parent owner Frame for this dialog.
	 * @param strategy Reference to LDStrategy to be edited
	 */
	public LDStrategyEditor(Frame parent, LDStrategy strategy) {
		super(parent, true);
		initData(strategy);
		initGUI();
	}

	/**
	 * Builds a new LDStrategyEditor Dialog. This dialog is designed to be modal.
	 * @param parent owner Dialog for this dialog.
	 * @param strategy Reference to LDStrategy to be edited
	 */
	public LDStrategyEditor(Dialog parent, LDStrategy strategy) {
		super(parent, true);
		initData(strategy);
		initGUI();
	}

	// -------------------------------------------------------------------------------------------------

	// --- Actions performed by buttons and EventListeners ---------------------------------------------
	// When okay button is pressed
	protected AbstractAction okayAction = new AbstractAction("Okay") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Accepts changes and closes this window");
		}

		public void actionPerformed(ActionEvent e) {
			LDStrategyEditor.this.close();
		}
	};

	// When Add Range button is pressed
	protected AbstractAction addRangeAction = new AbstractAction("Add Range") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Adds a new range into this service time strategy");
		}

		public void actionPerformed(ActionEvent e) {
			addRange();
		}
	};

	// deletion of one range
	protected AbstractAction deleteRange = new AbstractAction("") {
		/**
		* 
		*/
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "Deletes this range from current service section");
			putValue(Action.SMALL_ICON, JMTImageLoader.loadImage("Delete"));
		}

		public void actionPerformed(ActionEvent e) {
			int index = rangesTable.getSelectedRow();
			if (index >= 0 && index < rangesTable.getRowCount()) {
				deleteRange(index);
			}
		}
	};

	//Component responsible of setting global number of ranges at once
	protected JSpinner rangesNumSpinner = new JSpinner() {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent e) {
					//stop editing text inside spinner
					try {
						rangesNumSpinner.commitEdit();
					} catch (ParseException pe) {
						//if string does not represent a number, return
						return;
					}
					//new number of ranges
					int x = -1;
					try {
						x = ((Integer) rangesNumSpinner.getValue()).intValue();
					} catch (NumberFormatException nfe) {
					} catch (ClassCastException cce) {
					}
					//if new number is valid, proceed updating number
					if (x != -1) {
						setNumberOfRanges(x);
					} else {
						//otherwise, reset to 0
						rangesNumSpinner.setValue(new Integer(0));
					}
				}
			});
		}
	};

	// -------------------------------------------------------------------------------------------------

	// --- Initialize data structure and layout --------------------------------------------------------
	/**
	 * Initialize this dialog data structures
	 * @param strategy Reference to service strategy to be edited
	 */
	protected void initData(LDStrategy strategy) {
		this.strategy = strategy;
		// If distributions is not already set, sets it!
		if (distributions == null) {
			distributions = findDistributions();
		}
	}

	/**
	 * Inits all gui related stuff
	 */
	protected void initGUI() {
		// Sets default title, close operation and dimensions
		this.centerWindow(640, 480);
		this.setTitle("Editing Load Dependent Service Strategy...");

		JTabbedPane mainPanel = new JTabbedPane();
		this.getContentPane().setLayout(new BorderLayout());
		this.getContentPane().add(mainPanel, BorderLayout.CENTER);

		// Adds bottom_panel to contentpane
		JPanel bottom_panel = new JPanel(new FlowLayout());
		this.getContentPane().add((bottom_panel), BorderLayout.SOUTH);

		// Adds Okay button to bottom_panel
		JButton okaybutton = new JButton(okayAction);
		bottom_panel.add(okaybutton);

		// Creates the Edit panel
		JPanel LDPanel = new JPanel(new BorderLayout());
		LDPanel.setBorder(BorderFactory.createEmptyBorder(BORDERSIZE, BORDERSIZE, BORDERSIZE, BORDERSIZE));
		JPanel upperPanel = new JPanel(new BorderLayout());
		// Adds "Add Range" Button
		JPanel upRightPanel = new JPanel(new BorderLayout());
		JButton addRange = new JButton(addRangeAction);
		addRange.setMinimumSize(DIM_BUTTON_S);
		upRightPanel.add(addRange, BorderLayout.NORTH);
		//build spinner panel
		JPanel spinnerPanel = new JPanel();
		JLabel spinnerDescrLabel = new JLabel("Ranges:");
		rangesNumSpinner.setPreferredSize(DIM_BUTTON_XS);
		spinnerPanel.add(spinnerDescrLabel);
		spinnerPanel.add(rangesNumSpinner);
		upRightPanel.add(spinnerPanel, BorderLayout.SOUTH);
		upperPanel.add(upRightPanel, BorderLayout.EAST);
		upperPanel.add(new JLabel(LDSERVICE_DESCRIPTION), BorderLayout.CENTER);
		upperPanel.add(Box.createVerticalStrut(BORDERSIZE / 2), BorderLayout.SOUTH);
		LDPanel.add(upperPanel, BorderLayout.NORTH);
		// Adds StrategyTable
		rangesTable = new LDStrategyTable();
		LDPanel.add(new JScrollPane(rangesTable), BorderLayout.CENTER);
		// Creates Help panel
		HtmlPanel helpPanel = new HtmlPanel(LDStrategyEditor.class.getResource(HELPFILE));
		helpPanel.setBorder(BorderFactory.createEmptyBorder(BORDERSIZE, BORDERSIZE, BORDERSIZE, BORDERSIZE));

		// Adds Edit and Help tabs to mainPanel
		mainPanel.addTab("Edit", LDPanel);
		mainPanel.addTab("Help", new JScrollPane(helpPanel));

		refreshComponents();
	}

	/**
	 * Sets number of ranges in this strategy
	 * @param newNumber new number of ranges
	 */
	protected void setNumberOfRanges(int newNumber) {
		if (newNumber > MAXRANGES) {
			refreshComponents();
			return;
		}
		/*If new number is lesser than 0, reset to 0*/
		if (newNumber < 1) {
			setNumberOfRanges(1);
			return;
		}
		int oldNumber = strategy.getRangeNumber();
		/*If new number is greater than former one, just add */
		if (newNumber > oldNumber) {
			for (int i = oldNumber; i < newNumber; i++) {
				addRange();
			}
		} else if (newNumber < oldNumber) {
			/*otherwise, just delete*/
			for (int i = oldNumber - 1; i >= newNumber; i--) {
				deleteRange(i);
			}
		}
		refreshComponents();
	}

	/**
	 * Adds a new range to the strategy
	 */
	protected void addRange() {
		if (strategy.getRangeNumber() >= MAXRANGES) {
			return;
		}
		strategy.addRange();
		refreshComponents();
	}

	/**
	 * synchronizes components to display coherently global number of ranges
	 */
	protected void refreshComponents() {
		rangesTable.tableChanged(new TableModelEvent(rangesTable.getModel()));
		try {
			rangesNumSpinner.setValue(new Integer(strategy.getRangeNumber()));
		} catch (NumberFormatException nfe) {
		}
	}

	/**
	 * delete a range from strategy given the index the range to be deleted is displayed at
	 * inside the table.
	 */
	protected void deleteRange(int index) {
		strategy.deleteRange(strategy.getAllRanges()[index]);
		refreshComponents();
	}

	/**
	 * Multiple deletion of ranges. Indexes to ship call to precedent method, are retrieved
	 * through classtable methods (get selected rows)
	 */
	protected void deleteSelectedClasses() {
		int[] rows = rangesTable.getSelectedRows();
		for (int i = rows.length - 1; i >= 0; i--) {
			deleteRange(rows[i]);
		}
	}

	// -------------------------------------------------------------------------------------------------

	//---------------------------- Table containing strategy parameters --------------------------------

	/**
	 * Table that must display all of data about user classes. Customization of table
	 * settings is obtained via inheritation of <code>JTable</code> Class.
	 */
	protected class LDStrategyTable extends JTable {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		/*This button allow a single range to be deleted directly from the table.
		Corresponding value contained into cell must be zero.*/
		public JButton deleteButton = new JButton() {
			/**
			 * 
			 */
			private static final long serialVersionUID = 1L;

			{
				setAction(deleteRange);
				setFocusable(false);
			}
		};
		/** Editor to select distribution type */
		protected ComboBoxCellEditor distributionEditor = ComboBoxCellEditor.getEditorInstance(distributions.keySet().toArray());

		public LDStrategyTable() {
			super();
			this.setModel(new LDStrategyTableModel());
			this.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		}

		/*Set of column dimensions*/
		protected int[] columnSizes = new int[] { 20, 20, 80, 150, 30, 18 };

		//Sets a table model for visualization and editing of data
		public void setModel(LDStrategyTableModel tabMod) {
			super.setModel(tabMod);
			sizeColumnsAndRows();
			setDefaultRenderer(String.class, new GrayCellRenderer());
			setDefaultEditor(Object.class, new jmt.gui.exact.table.ExactCellEditor());
		}

		//returns a component to be contained inside a table column(or cell)
		@Override
		public TableCellRenderer getCellRenderer(int row, int column) {
			// Delete buttons
			if (column == 5 && row > 0) {
				return new ButtonCellEditor(deleteButton);
			} else if (column == 5 && row == 0) {
				return getDefaultRenderer(String.class);
			} else if (column == 2) {
				return ComboBoxCellEditor.getRendererInstance();
			} else {
				return getDefaultRenderer(getModel().getColumnClass(column));
			}
		}

		/*returns customized editor for table cells.*/
		@Override
		public TableCellEditor getCellEditor(int row, int column) {
			if (column == 2) {
				return distributionEditor;
			} else if (column == 5) {
				return new ButtonCellEditor(new JButton(deleteRange));
			} else {
				return super.getCellEditor(row, column);
			}
		}

		//set sizes for columns and rows of this table.
		protected void sizeColumnsAndRows() {
			for (int i = 0; i < columnSizes.length && i < getColumnCount(); i++) {
				this.getColumnModel().getColumn(i).setPreferredWidth(columnSizes[i]);
				if (i == columnSizes.length - 1) {
					//delete button and containing table cells as well, must be square
					this.getColumnModel().getColumn(i).setMaxWidth(columnSizes[i]);
					this.setRowHeight(columnSizes[i]);
				}
			}
		}
	}

	/**
	 * Table data model to implement customized data editing
	 */
	protected class LDStrategyTableModel extends AbstractTableModel {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		//Names of columns contained in table. Columns containing buttons have empty names
		protected String[] columnNames = new String[] { "From", "To", "Distribution", "Mean", "C", "" };

		//Class declarations for this table's columns.
		protected Class[] colClasses = new Class[] { String.class, String.class, JComboBox.class, String.class, String.class, JButton.class };

		/**Creates a new instance of class table model*/
		public LDStrategyTableModel() {
			super();
		}

		/**returns number of rows to be displayed in the table. In this case, global
		 * number of ranges*/
		public int getRowCount() {
			return strategy.getRangeNumber();
		}

		/**Returns total number of columns*/
		public int getColumnCount() {
			return columnNames.length;
		}

		/**Returns name for each column (given its index) to be displayed
		 * inside table header*/
		@Override
		public String getColumnName(int columnIndex) {
			if (columnIndex < columnNames.length) {
				return columnNames[columnIndex];
			} else {
				return null;
			}
		}

		/**Returns class describing data contained in specific column.*/
		@Override
		public Class<Object> getColumnClass(int columnIndex) {
			if (columnIndex < colClasses.length) {
				return colClasses[columnIndex];
			} else {
				return Object.class;
			}
		}

		/**Tells wether data contained in a specific cell(given row and column index)
		 * is editable or not. In this case distribution column is not editable, as
		 * editing functionality is implemented via edit button*/
		@Override
		public boolean isCellEditable(int rowIndex, int columnIndex) {
			if (rowIndex == 0 && (columnIndex == 0 || columnIndex == 5)) {
				return false;
			}
			if (columnIndex == 1) {
				return false;
			} else if (columnIndex == 4 && !distributions.get(getValueAt(rowIndex, 2)).hasC()) {
				return false;
			}
			return true;
		}

		/**retrieves value to be displayed in table cell from the underlying model
		 * data structure implementation.*/
		public Object getValueAt(int rowIndex, int columnIndex) {
			Object key = strategy.getAllRanges()[rowIndex];
			switch (columnIndex) {
				case (0): {
					return new Integer(strategy.getRangeFrom(key));
				}
				case (1): {
					int value = strategy.getRangeTo(key);
					if (value > 0) {
						return new Integer(value);
					} else {
						return "\u221E"; // Infinity character
					}
				}
				case (2): {
					return strategy.getRangeDistribution(key).getName();
				}
				case (3): {
					return strategy.getRangeDistributionMean(key);
				}
				case (4): {
					if (strategy.getRangeDistribution(key).hasC()) {
						return FormatNumber(strategy.getRangeDistribution(key).getC());
					} else {
						return null;
					}
				}
				default: {
					return null;
				}
			}
		}

		/**Puts edited values to the underlying data structure for model implementation*/
		@Override
		public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
			Object key = strategy.getAllRanges()[rowIndex];
			switch (columnIndex) {
				case (0):
					try {
						strategy.setRangeFrom(key, Integer.parseInt((String) aValue));
					} catch (NumberFormatException e) {
					}
					break;
				case (2):
					strategy.setRangeDistribution(key, distributions.get(aValue));
					break;
				case (3):
					strategy.setRangeDistributionMean(key, (String) aValue);
					break;
				case (4):
					try {
						strategy.setRangeDistributionC(key, Double.parseDouble((String) aValue));
					} catch (NumberFormatException nfe) {
					}
					break;
			}

			// If from or distribution has changed, need to repaint the table
			if (columnIndex == 0 || columnIndex == 2) {
				repaint();
			}
		}

		/**
		 * Helper method used to formats given number into string according to default rules.
		 * @param d bouble to be converted
		 * @return string representation of given number
		 */
		protected String FormatNumber(double d) {
			DecimalFormat nf = new DecimalFormat();
			String ret;
			// If module of number is greater than 1e4 or lesser than 1e-4 uses exponential notation
			if (Math.abs(d) >= 1e-4 && Math.abs(d) <= 1e4 || d == 0) {
				nf.applyPattern("#.####");
				ret = nf.format(d);
				if (ret.length() > 7) {
					ret = ret.substring(0, 6);
				}
			} else {
				nf.applyPattern("0.00E00");
				ret = nf.format(d);
			}
			return ret;
		}

	}
	// -------------------------------------------------------------------------------------------------

}
