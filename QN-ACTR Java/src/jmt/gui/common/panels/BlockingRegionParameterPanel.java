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

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.SpringLayout;
import javax.swing.border.EtchedBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.TableModelEvent;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import jmt.framework.gui.layouts.SpringUtilities;
import jmt.framework.gui.table.editors.ComboBoxCellEditor;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.Defaults;
import jmt.gui.common.definitions.BlockingRegionDefinition;
import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.editors.GrayCellRenderer;
import jmt.gui.common.editors.ImagedComboBoxCellEditorFactory;

/**
 * <p>Title: Blocking Region Parameter Panel</p>
 * <p>Description: This panel is used to parametrize a blocking region</p>
 *
 * @author Bertoli Marco
 *         Date: 30-mar-2006
 *         Time: 16.15.46
 */
public class BlockingRegionParameterPanel extends WizardPanel implements CommonConstants {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected ClassDefinition cd;
	protected BlockingRegionDefinition brd;
	protected Object key;
	protected BlockingTable table;
	// Name of blocking region
	protected JTextField name;
	// Global number of jobs
	protected JSpinner number;
	protected JCheckBox inf;
	// Panels for input data
	protected JPanel globalPanel, classPanel;
	/** Used to display classes with icon */
	protected ImagedComboBoxCellEditorFactory classEditor;

	/**
	 * Builds a new Blocking Region Parameter Panel
	 * @param cd class definition data structure
	 * @param brd blocking region definition data structure
	 * @param key search's key for given blocking region
	 */
	public BlockingRegionParameterPanel(ClassDefinition cd, BlockingRegionDefinition brd, Object key) {
		this.cd = cd;
		this.brd = brd;
		this.key = key;
		classEditor = new ImagedComboBoxCellEditorFactory(cd);
		initComponents();
		setData(cd, brd, key);
		addActions();
	}

	/**
	 * Sets data for this panel
	 * @param cd class definition data structure
	 * @param brd blocking region definition data structure
	 * @param key search's key for given blocking region
	 */
	public void setData(ClassDefinition cd, BlockingRegionDefinition brd, Object key) {
		this.cd = cd;
		this.brd = brd;
		this.key = key;
		classPanel.setBorder(BorderFactory.createTitledBorder(new EtchedBorder(), "Class specific " + brd.getRegionName(key) + " Properties"));
		globalPanel.setBorder(BorderFactory.createTitledBorder(new EtchedBorder(), "Global " + brd.getRegionName(key) + " Properties"));
		classEditor.setData(cd);
		updateData();
	}

	/**
	 * Initialize graphic components of this panel
	 */
	protected void initComponents() {
		setLayout(new BorderLayout(5, 5));
		// Panel with global region parameters
		globalPanel = new JPanel(new SpringLayout());
		globalPanel.setBorder(BorderFactory.createTitledBorder(new EtchedBorder(), "Global " + brd.getRegionName(key) + " Properties"));
		// Enter region name
		JLabel label = new JLabel("Region name: ");
		name = new JTextField();
		label.setLabelFor(name);
		globalPanel.add(label);
		globalPanel.add(name);
		// Enter global number of customers
		label = new JLabel("Region capacity: ");
		JPanel tmpPanel = new JPanel(new BorderLayout(6, 6));
		number = new JSpinner();
		inf = new JCheckBox("Infinite");
		tmpPanel.add(number, BorderLayout.CENTER);
		tmpPanel.add(inf, BorderLayout.EAST);
		label.setLabelFor(tmpPanel);
		globalPanel.add(label);
		globalPanel.add(tmpPanel);

		SpringUtilities.makeCompactGrid(globalPanel, 2, 2, 20, 4, 20, 10);
		add(globalPanel, BorderLayout.NORTH);
		// Panel with class dependent parameters
		classPanel = new JPanel(new BorderLayout());
		classPanel.setBorder(BorderFactory.createTitledBorder(new EtchedBorder(), "Class specific " + brd.getRegionName(key) + " Properties"));
		table = new BlockingTable();
		classPanel.add(new WarningScrollTable(table, WARNING_CLASS));
		add(classPanel, BorderLayout.CENTER);
	}

	/**
	 * Adds action listeners to created components
	 */
	protected void addActions() {
		// Name change
		inputListener listener = new inputListener();
		name.addKeyListener(listener);
		name.addFocusListener(listener);

		// Global customer actions
		number.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				Object value = number.getValue();
				if (value instanceof Integer) {
					int num = ((Integer) value).intValue();
					if (num > 0) {
						brd.setRegionCustomerConstraint(key, (Integer) value);
					}
					// Shows stored value
					number.setValue(brd.getRegionCustomerConstraint(key));
				}
			}
		});
		inf.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (inf.isSelected()) {
					number.setValue(new Float(Float.POSITIVE_INFINITY));
					number.setEnabled(false);
					brd.setRegionCustomerConstraint(key, new Integer(-1));
				} else {
					// Uses default value if finite, otherwise initialize with one.
					Integer num = Defaults.getAsInteger("blockingMaxJobs");
					if (num.intValue() <= 0) {
						num = new Integer(1);
					}
					number.setValue(num);
					number.setEnabled(true);
					brd.setRegionCustomerConstraint(key, num);
				}
			}
		});

	}

	/**
	 * @return the panel's name
	 */
	@Override
	public String getName() {
		return "Finite Capacity Region Parameters";
	}

	/**
	 * called by the Wizard before when switching to another panel
	 */
	@Override
	public void lostFocus() {
		// Aborts editing of table
		TableCellEditor editor = table.getCellEditor();
		if (editor != null) {
			editor.stopCellEditing();
		}
	}

	/**
	 * called by the Wizard when the panel becomes active
	 */
	@Override
	public void gotFocus() {
		// Fires a table change event
		table.tableChanged(new TableModelEvent(table.getModel()));
		classEditor.clearCache();
	}

	/**
	 * This method will update data inside table and input components
	 */
	protected void updateData() {
		// Name of region and global number of customers
		name.setText(brd.getRegionName(key));
		Integer customer = brd.getRegionCustomerConstraint(key);
		if (customer.intValue() > 0) {
			number.setValue(customer);
			inf.setSelected(false);
			number.setEnabled(true);
		} else {
			number.setValue(new Float(Float.POSITIVE_INFINITY));
			inf.setSelected(true);
			number.setEnabled(false);
		}
	}

	/**
	 * Sets if global panel is visible (default true)
	 * @param value true if panel is visible
	 */
	public void setGlobalVisible(boolean value) {
		globalPanel.setVisible(value);
	}

	/**
	 * Sets if class panel is visible (default true)
	 * @param value true if panel is visible
	 */
	public void setClassVisible(boolean value) {
		classPanel.setVisible(value);
	}

	/**
	 * Listener used to change region name (associated to name JTextFields).
	 * Parameters are set when JTextField loses focus or ENTER key is pressed.
	 */
	protected class inputListener implements KeyListener, FocusListener {
		/**
		 * Update station's name
		 */
		protected void updateValues() {
			brd.setRegionName(key, name.getText());
			name.setText(brd.getRegionName(key));
		}

		public void focusLost(FocusEvent e) {
			updateValues();
		}

		public void keyPressed(KeyEvent e) {
			if (e.getKeyCode() == KeyEvent.VK_ENTER) {
				updateValues();
				e.consume();
			}
		}

		public void focusGained(FocusEvent e) {
		}

		public void keyReleased(KeyEvent e) {
		}

		public void keyTyped(KeyEvent e) {
		}
	}

	/**
	 * Inner class used to parametrize blocking region class constraints
	 */
	protected class BlockingTable extends JTable {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		protected String[] dropRules = new String[] { Boolean.TRUE.toString(), Boolean.FALSE.toString() };
		protected TableCellRenderer gray = new GrayCellRenderer();

		int[] columnSizes = new int[] { 200, 60, 20, 50 };

		public BlockingTable() {
			super();
			setModel(new BlockingTableModel());
			sizeColumns();
			setRowHeight(ROW_HEIGHT);
		}

		@Override
		public TableCellRenderer getCellRenderer(int row, int column) {
			if (column == 0) {
				return classEditor.getRenderer();
			} else if (column == 1) {
				return gray;
			} else if (column == 3) {
				return ComboBoxCellEditor.getRendererInstance();
			} else {
				return super.getCellRenderer(row, column);
			}
		}

		@Override
		public TableCellEditor getCellEditor(int row, int column) {
			if (column == 3) {
				return ComboBoxCellEditor.getEditorInstance(dropRules);
			} else {
				return super.getCellEditor(row, column);
			}
		}

		private void sizeColumns() {
			for (int i = 0; i < columnSizes.length && i < getColumnCount(); i++) {
				this.getColumnModel().getColumn(i).setPreferredWidth(columnSizes[i]);
			}
		}
	}

	/**
	 * Table model for blocking table
	 */
	protected class BlockingTableModel extends AbstractTableModel {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		private Class[] columnClasses = new Class[] { String.class, String.class, Boolean.class, String.class };
		private String[] columnNames = new String[] { "Class", "Capacity", "\u221e", "Drop" };

		public int getRowCount() {
			return cd.getClassKeys().size();
		}

		public int getColumnCount() {
			return 4;
		}

		@Override
		public String getColumnName(int columnIndex) {
			return columnNames[columnIndex];
		}

		@Override
		public Class<Integer> getColumnClass(int columnIndex) {
			return columnClasses[columnIndex];
		}

		@Override
		public boolean isCellEditable(int rowIndex, int columnIndex) {
			// Drop rule, infinity constraint are editables. Number value is editable if infinity is uncecked
			return columnIndex == 3 || columnIndex == 2 || (columnIndex == 1 && !((Boolean) getValueAt(rowIndex, columnIndex + 1)).booleanValue());
		}

		public Object getValueAt(int rowIndex, int columnIndex) {
			Object classKey = cd.getClassKeys().get(rowIndex);
			Integer num;
			switch (columnIndex) {
				case (0):
					return classKey;
				case (1):
					num = brd.getRegionClassCustomerConstraint(key, classKey);
					if (num.intValue() > 0) {
						return num;
					} else {
						return "\u221e"; // Infinity symbol
					}
				case (2):
					num = brd.getRegionClassCustomerConstraint(key, classKey);
					return (num.intValue() <= 0) ? Boolean.TRUE : Boolean.FALSE;
				case (3):
					return brd.getRegionClassDropRule(key, classKey).toString();
			}
			return null;
		}

		/**Puts edited values to the underlying data structure for model implementation*/
		@Override
		public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
			Object classKey = cd.getClassKeys().get(rowIndex);
			Integer value;
			switch (columnIndex) {
				case (1):
					try {
						value = Integer.decode((String) aValue);
						if (value.intValue() > 0) {
							brd.setRegionClassCustomerConstraint(key, classKey, value);
						}
					} catch (NumberFormatException e) {
						// Do nothing
					}
					break;
				case (2):
					boolean val = ((Boolean) aValue).booleanValue();
					if (val) {
						brd.setRegionClassCustomerConstraint(key, classKey, new Integer(-1));
					} else {
						value = Defaults.getAsInteger("blockingMaxJobsPerClass");
						if (value.intValue() <= 0) {
							value = new Integer(1);
						}
						brd.setRegionClassCustomerConstraint(key, classKey, value);
					}
					break;
				case (3):
					brd.setRegionClassDropRule(key, classKey, Boolean.valueOf((String) aValue));
					break;
			}
			repaint();
		}

	}

}
