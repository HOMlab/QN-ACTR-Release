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

package jmt.gui.jsim.panels;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.text.ParseException;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTable;
import javax.swing.KeyStroke;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.TableModelEvent;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import jmt.framework.gui.table.editors.ButtonCellEditor;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.Defaults;
import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.definitions.SimulationDefinition;
import jmt.gui.common.distributions.Distribution;
import jmt.gui.common.editors.DistributionsEditor;
import jmt.gui.common.editors.ImagedComboBoxCellEditorFactory;
import jmt.gui.common.resources.JMTImageLoader;

/**
 * Created by IntelliJ IDEA.
 * User: OrsotronIII
 * Date: 13-mag-2005
 * Time: 14.43.32
 * This panel provides functionality of editing and visualizing data about model's
 * classes.
 * Modified by Bertoli Marco 9-jan-2006  --> ComboBoxCellEditor
 */
public class ClassesPanel extends WizardPanel implements CommonConstants {

	//Interface containing values of main parameters to be set by user
	/*TODO: to enable this functionality, just put a constructor accepting a customized
	*JSIMDefault class as parameter, and then put a form to allow user to generate instances
	*of this class.*/
	//protected Defaults defaults = new Defaults(){};
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	//Table containg class-set data
	protected ClassTable classTable;

	//ComboBox editor for station type
	protected ImagedComboBoxCellEditorFactory comboEditor = new ImagedComboBoxCellEditorFactory();

	//Button that allows to add classes one by one
	protected JButton addClass;

	//Component responsible of setting global number of classes at once
	protected JSpinner classNumSpinner = new JSpinner() {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent e) {
					//stop editing text inside spinner
					try {
						classNumSpinner.commitEdit();
					} catch (ParseException pe) {
						//if string does not represent a number, return
						return;
					}
					//new number of classes
					int x = -1;
					try {
						x = ((Integer) classNumSpinner.getValue()).intValue();
					} catch (NumberFormatException nfe) {
					} catch (ClassCastException cce) {
					}
					//if new number is valid, proceed updating number
					if (x != -1) {
						setNumberOfClasses(x);
					} else {
						//otherwise, reset to 0
						classNumSpinner.setValue(new Integer(0));
					}
				}
			});
		}
	};

	//Interface linking to underlying implementation layer
	protected ClassDefinition data;

	//Index for temporary class name assignment
	protected int classNameIndex = 0;

	//deletion of classes after a multiple selection
	protected AbstractAction deleteSelectedClasses = new AbstractAction("Delete selected classes") {
		/**
		* 
		*/
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0, false));
			putValue(Action.SHORT_DESCRIPTION, "Deletes selected classes from the system");
		}

		public void actionPerformed(ActionEvent e) {
			deleteSelectedClasses();
		}
	};

	//deletion of one class
	protected AbstractAction deleteClass = new AbstractAction("") {
		/**
		* 
		*/
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "Delete");
			putValue(Action.SMALL_ICON, JMTImageLoader.loadImage("Delete"));
		}

		public void actionPerformed(ActionEvent e) {
			int index = classTable.getSelectedRow();
			if (index >= 0 && index < classTable.getRowCount()) {
				deleteClass(index);
			}
		}
	};

	//addition of a class one by one
	protected AbstractAction addNewClass = new AbstractAction("Add Class") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_PLUS, ActionEvent.ALT_MASK));
			putValue(Action.SHORT_DESCRIPTION, "Adds a new class");
		}

		public void actionPerformed(ActionEvent e) {
			addClass();
		}
	};

	//editing of arrival time distribution
	protected AbstractAction editDistribution = new AbstractAction("Edit") {
		/**
		* 
		*/
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "Edits distribution parameters");
		}

		public void actionPerformed(ActionEvent e) {
			// ---- Bertoli Marco ----
			int index = classTable.getSelectedRow();
			if (index >= 0 && index < classTable.getRowCount()) {
				Object key = data.getClassKeys().elementAt(index);
				DistributionsEditor editor = DistributionsEditor.getInstance(ClassesPanel.this.getParent(), (Distribution) data
						.getClassDistribution(key));
				// Sets editor window title
				editor.setTitle("Editing " + data.getClassName(key) + " distribution...");
				// Shows editor window
				editor.show();
				// Sets new Distribution to selected class
				data.setClassDistribution(editor.getResult(), key);

				// Updates table view. This is needed as Distribution is not contained
				// into edited cell (but in its left one)
				classTable.repaint();
			}
			// ---- end ----
		}
	};

	/**Creates a new instance of <code>ClassesPanel</code> given a model definition.*/
	public ClassesPanel(ClassDefinition cd) {
		super();
		classTable = new ClassTable();
		initComponents();
		//forbid column to be moved
		classTable.getTableHeader().setReorderingAllowed(false);
		setData(cd);
	}

	/**Default constructor, used to extend this panel for JModel*/
	public ClassesPanel() {
		super();
	}

	/**Sets data model for this panel.
	 * Instantly all of the panel components are assigned their specific value.
	 * @param cd: data for class definition.*/
	public void setData(ClassDefinition cd) {
		data = cd;
		classTable.setModel(new ClassTableModel());
		classNumSpinner.setValue(new Integer(data.getClassKeys().size()));
		comboEditor.clearCache();
	}

	/**Gets data model for this panel.
	 * @return : data for class definition.*/
	public ClassDefinition getData() {
		return data;
	}

	/**
	 * called by the Wizard when the panel becomes active
	 */
	@Override
	public void gotFocus() {
		comboEditor.clearCache();
	}

	//Builds internal structure of the panel. Sets up layout of components
	protected void initComponents() {
		//create margins for this panel.
		Box vBox = Box.createVerticalBox();
		Box hBox = Box.createHorizontalBox();
		vBox.add(Box.createVerticalStrut(20));
		vBox.add(hBox);
		vBox.add(Box.createVerticalStrut(20));
		hBox.add(Box.createHorizontalStrut(20));

		//build central panel
		JPanel componentsPanel = new JPanel(new BorderLayout());
		//new BoxLayout(componentsPanel, BoxLayout.Y_AXIS);

		//build upper part of central panel
		JPanel upperPanel = new JPanel(new BorderLayout());
		JLabel descrLabel = new JLabel(CLASSES_DESCRIPTION);
		//descrLabel.setMaximumSize(new Dimension(300, 1000));
		upperPanel.add(descrLabel, BorderLayout.CENTER);

		//build upper right corner of the main panel
		JPanel upRightPanel = new JPanel(new BorderLayout());
		addClass = new JButton(addNewClass);
		addClass.setMinimumSize(DIM_BUTTON_S);
		upRightPanel.add(addClass, BorderLayout.CENTER);

		//build spinner panel
		JPanel spinnerPanel = new JPanel();
		JLabel spinnerDescrLabel = new JLabel("Classes:");
		//rangesNumSpinner = new JSpinner();
		classNumSpinner.setPreferredSize(DIM_BUTTON_XS);
		spinnerPanel.add(spinnerDescrLabel);
		spinnerPanel.add(classNumSpinner);

		//add all panels to the mail panel
		upRightPanel.add(spinnerPanel, BorderLayout.SOUTH);
		upperPanel.add(upRightPanel, BorderLayout.EAST);
		componentsPanel.add(upperPanel, BorderLayout.NORTH);
		componentsPanel.add(new JScrollPane(classTable), BorderLayout.CENTER);
		hBox.add(componentsPanel);
		hBox.add(Box.createHorizontalStrut(20));
		this.setLayout(new GridLayout(1, 1));
		this.add(vBox);
	}

	//returns name to be displayed on the tab, when inserted in a wizard tabbed pane
	@Override
	public String getName() {
		return "Classes";
	}

	//adds a new class to the table and, simultaneously to the underlying model data structure
	protected void addClass() {
		data.addClass(Defaults.get("className") + (++classNameIndex), Defaults.getAsInteger("classType").intValue(), Defaults.getAsInteger(
				"classPriority").intValue(), Defaults.getAsInteger("classPopulation"), Defaults.getAsNewInstance("classDistribution"));
		refreshComponents();
	}

	//synchronizes components to display coherently global number of classes
	protected void refreshComponents() {
		classTable.tableChanged(new TableModelEvent(classTable.getModel()));
		try {
			classNumSpinner.setValue(new Integer(data.getClassKeys().size()));
		} catch (NumberFormatException nfe) {
		}
		if (data.getClassKeys().size() >= MAX_NUMBER_OF_CLASSES) {
			addClass.setEnabled(false);
		} else {
			addClass.setEnabled(true);
		}
	}

	/*delete a class from model given the index the class to be deleted is displayed at
	inside the table.*/
	protected void deleteClass(int index) {
		data.deleteClass(data.getClassKeys().get(index));
		refreshComponents();
	}

	/*Multiple deletion of classes. Indexes to ship call to precedent method, are retrieved
	through classtable methods (get selected rows)*/
	protected void deleteSelectedClasses() {
		int[] rows = classTable.getSelectedRows();
		for (int i = rows.length - 1; i >= 0; i--) {
			deleteClass(rows[i]);
		}
	}

	/*Modify global number of classes for this model all at once.*/
	protected void setNumberOfClasses(int newNumber) {
		/*If new number is greater than a certain number, don't do anything and cancel
		number modification inside spinner*/
		if (newNumber > MAX_NUMBER_OF_CLASSES) {
			setNumberOfClasses(MAX_NUMBER_OF_CLASSES);
			return;
		}
		/*If new number is not valid, reset to 0*/
		if (newNumber < 0) {
			setNumberOfClasses(0);
			return;
		}
		int oldNumber = data.getClassKeys().size();
		/*If new number is greater than former one, just add */
		if (newNumber > oldNumber) {
			for (int i = oldNumber; i < newNumber; i++) {
				addClass();
			}
		} else if (newNumber < oldNumber) {
			/*otherwise, just delete*/
			for (int i = oldNumber - 1; i >= newNumber; i--) {
				deleteClass(i);
			}
		}
		refreshComponents();
	}

	//---------------------------- Table containing classes parameters --------------------------
	/*Table that must display all of data about user classes. Customization of table settings is
	obtained via inheritation of <code>JTable</code> Class.*/

	protected class ClassTable extends JTable {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		/* for selection of class types. Must be inserted in ComboBoxCellEditor*/
		protected Object[] classTypes = new Object[] { "Closed", "Open" };

		/*This button allow a single userclass to be deleted directly from the table.
		Corresponding value contained into cell must be zero.*/
		public JButton deleteButton = new JButton() {
			/**
			 * 
			 */
			private static final long serialVersionUID = 1L;

			{
				setAction(deleteClass);
				setFocusable(false);
			}
		};

		/*This button activates the distribution editor. Returning value must be sent
		to underlying model definition, and then diplayed in another column contained*/
		protected JButton editDistributionButton = new JButton() {
			/**
			 * 
			 */
			private static final long serialVersionUID = 1L;

			{
				setText("Edit");
			}
		};

		/*Set of column dimensions*/
		protected int[] columnSizes = new int[] { 120, 50, 50, 50, 150, 38, 18 };

		//Sets a table model for visualization and editing of data
		public void setModel(ClassTableModel tabMod) {
			super.setModel(tabMod);
			sizeColumnsAndRows();
			setDefaultRenderer(String.class, new jmt.gui.exact.table.DisabledCellRenderer());
			setDefaultEditor(Object.class, new jmt.gui.exact.table.ExactCellEditor());
			setRowHeight(ROW_HEIGHT);
		}

		//returns a component to be contained inside a table column(or cell)
		@Override
		public TableCellRenderer getCellRenderer(int row, int column) {
			if (column == 1) {
				return comboEditor.getRenderer();
			} else if (column == 5) {
				/*if distribution column contains null value, no editor must be displayed,
				as this class is a closed one (e.g. described by population)*/
				if (getValueAt(row, column - 1) != null) {
					return new ButtonCellEditor(editDistributionButton);
				} else {
					return getDefaultRenderer(String.class);
				}
			}
			//Addition of column that contains delete buttons
			if (column == 6) {
				return new ButtonCellEditor(deleteButton);
			} else {
				return getDefaultRenderer(getModel().getColumnClass(column));
			}
		}

		/*returns customized editor for table cells.*/
		@Override
		public TableCellEditor getCellEditor(int row, int column) {
			if (column == 1) {
				return comboEditor.getEditor(classTypes);
			} else if (column == 5 && getValueAt(row, column - 1) != null) {
				return new ButtonCellEditor(new JButton(editDistribution));
			} else if (column == 6) {
				return new ButtonCellEditor(new JButton(deleteClass));
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

	//------------------------------------Table model for classes panel --------------------------
	/*Table data model to implement customized data editing*/

	protected class ClassTableModel extends AbstractTableModel {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		//Names of columns contained in table. Columns containing buttons have empty names
		protected String[] columnNames = new String[] { "Name", "Type", "Priority", "Population", "Interarrival Time Distribution", "", "" };

		//Class declarations for this table's columns.
		protected Class[] colClasses = new Class[] { String.class, JComboBox.class, String.class, String.class, String.class, Object.class,
				JButton.class };

		/**Creates a new instance of class table model*/
		public ClassTableModel() {
			super();
		}

		/**returns number of rows to be displayed in the table. In this case, global
		 * number of classes*/
		public int getRowCount() {
			if (data.getClassKeys() != null) {
				return data.getClassKeys().size();
			} else {
				return 0;
			}
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
		public Class getColumnClass(int columnIndex) {
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
			if (columnIndex == 3 && (getValueAt(rowIndex, 1).equals("Open"))) {
				return false;
			}
			if (columnIndex == 5 && (getValueAt(rowIndex, 1).equals("Closed"))) {
				return false;
			}
			if (columnIndex == 4) {
				return false;
			}
			return true;
		}

		/**retrieves value to be displayed in table cell from the underlying model
		 * data structure implementation.*/
		public Object getValueAt(int rowIndex, int columnIndex) {
			Object key = data.getClassKeys().get(rowIndex);
			switch (columnIndex) {
				case (0): {
					return data.getClassName(key);
				}
				case (1): {
					int type = data.getClassType(key);
					if (type == CLASS_TYPE_CLOSED) {
						return "Closed";
					} else if (type == CLASS_TYPE_OPEN) {
						return "Open";
					}
				}
				case (2): {
					return new Integer(data.getClassPriority(key));
				}
				case (3): {
					return data.getClassPopulation(key);
				}
				case (4): {
					return data.getClassDistribution(key);
				}
				default: {
					return null;
				}
			}
		}

		/**Puts edited values to the underlying data structure for model implementation*/
		@Override
		public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
			Object key = data.getClassKeys().get(rowIndex);
			switch (columnIndex) {
				case (0): {
					data.setClassName((String) aValue, key);
					break;
				}
				case (1): {
					if ((aValue).equals("Closed")) {
						data.setClassType(CLASS_TYPE_CLOSED, key);
					} else if ((aValue).equals("Open")) {
						data.setClassType(CLASS_TYPE_OPEN, key);
					}
					break;
				}
				case (2): {
					try {
						int priority = Integer.parseInt((String) aValue);
						if (priority >= 0) {
							data.setClassPriority(priority, key);
						}
					} catch (NumberFormatException nfe) {
					}
					break;
				}
				case (3): {
					try {
						Integer population = Integer.decode((String) aValue);
						if (population.intValue() > 0) {
							data.setClassPopulation(population, key);
						}
					} catch (NumberFormatException nfe) {
					}
					break;
				}/*case(5):{
					                   data.setClassDistribution(aValue, key);
					                   break;
					               }*/
			}
			/*if editing cell belongs to class type column, i must update population and
			distribution cells*/
			if (columnIndex == 1) {
				repaint();
			}
		}

	}

	/**
	 * called by the Wizard before when switching to another panel
	 */
	@Override
	public void lostFocus() {
		// Aborts editing of table
		TableCellEditor editor = classTable.getCellEditor();
		if (editor != null) {
			editor.stopCellEditing();
		}
		// Preloads jobs
		((SimulationDefinition) data).manageJobs();
	}

}
