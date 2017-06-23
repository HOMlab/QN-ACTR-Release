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
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.ParseException;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.DefaultCellEditor;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import jmt.framework.gui.table.editors.ButtonCellEditor;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.Defaults;
import jmt.gui.common.definitions.BlockingRegionDefinition;
import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.definitions.StationDefinition;
import jmt.gui.common.editors.GrayCellRenderer;
import jmt.gui.common.resources.JMTImageLoader;

/**
 * <p>Title: All Blocking Region Panel</p>
 * <p>Description: This is the tabbed pane shown in JSIM for the specification of blocking
 * regions.</p>
 *
 * @author Bertoli Marco
 *         Date: 5-mag-2006
 *         Time: 15.41.01
 */
public class AllBlockingRegionsPanel extends WizardPanel implements CommonConstants {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private ClassDefinition cd;
	private StationDefinition sd;
	private BlockingRegionDefinition bd;
	// Global components
	private JSpinner regionsNumSpinner;
	private JButton addRegion;
	private JTable regions;
	private int counter = 1; // Used for name generation
	private BlockingStationPanel blockingPanel;
	private JPanel emptyPanel = new JPanel();

	/**
	 * called by the Wizard before when switching to another panel
	 */
	@Override
	public void lostFocus() {
		// Aborts editing of table
		TableCellEditor editor = regions.getCellEditor();
		if (editor != null) {
			editor.stopCellEditing();
		}
		if (blockingPanel != null) {
			blockingPanel.lostFocus();
		}
	}

	/**
	 * called by the Wizard when the panel becomes active
	 */
	@Override
	public void gotFocus() {
		if (blockingPanel != null) {
			blockingPanel.gotFocus();
		}
		// Selects first blocking region if none selected and if present
		if (regions.getRowCount() > 0 && regions.getSelectedRow() < 0) {
			regions.getSelectionModel().setSelectionInterval(0, 0);
		}
	}

	/**
	 * @return the panel's name
	 */
	@Override
	public String getName() {
		return "Finite Capacity Regions";
	}

	/**
	 * Creates a new BlockingRegionsPanel with given data structures
	 * @param cd class definition data structure
	 * @param sd station definition data structure
	 * @param bd blocking region definition data structure
	 */
	public AllBlockingRegionsPanel(ClassDefinition cd, StationDefinition sd, BlockingRegionDefinition bd) {
		this.cd = cd;
		this.sd = sd;
		this.bd = bd;
		initComponents();
		initActions();
	}

	/**
	 * Changes data structure references of this panel
	 * @param cd
	 * @param sd
	 * @param bd
	 */
	public void setData(ClassDefinition cd, StationDefinition sd, BlockingRegionDefinition bd) {
		this.cd = cd;
		this.sd = sd;
		this.bd = bd;
		counter = 1;
		// Updates number of regions shown
		update();
	}

	/**
	 * Initialize all components of this panel
	 */
	private void initComponents() {
		setLayout(new BorderLayout(5, 5));
		this.setBorder(new EmptyBorder(20, 20, 20, 20));
		// Builds upper panel
		JPanel upperPanel = new JPanel(new BorderLayout());
		JLabel description = new JLabel(BLOCKING_DESCRIPTION);
		upperPanel.add(description, BorderLayout.CENTER);

		//build upper right corner of the main panel
		JPanel upRightPanel = new JPanel(new BorderLayout());
		addRegion = new JButton("Add Region");
		addRegion.setMinimumSize(DIM_BUTTON_S);
		upRightPanel.add(addRegion, BorderLayout.CENTER);
		upperPanel.add(upRightPanel, BorderLayout.EAST);

		//build spinner panel
		JPanel spinnerPanel = new JPanel();
		JLabel spinnerDescrLabel = new JLabel("Regions:");
		regionsNumSpinner = new JSpinner();
		regionsNumSpinner.setPreferredSize(DIM_BUTTON_XS);
		spinnerPanel.add(spinnerDescrLabel);
		spinnerPanel.add(regionsNumSpinner);
		upRightPanel.add(spinnerPanel, BorderLayout.SOUTH);

		add(upperPanel, BorderLayout.NORTH);

		// Creates blocking regions list
		regions = new RegionTable();
		regions.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

		JScrollPane jsp = new JScrollPane(regions, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
				ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		jsp.setPreferredSize(new Dimension(220, 200));
		add(jsp, BorderLayout.WEST);
		update();
	}

	/**
	 * Initialize actions for every component of this window
	 */
	private void initActions() {
		// Sets add button action
		addRegion.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addRegion();
				update();
			}
		});

		// Sets spinner actions
		regionsNumSpinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				//stop editing text inside spinner
				try {
					regionsNumSpinner.commitEdit();
				} catch (ParseException pe) {
					//if string does not represent a number, return
					return;
				}
				Object value = regionsNumSpinner.getValue();
				if (value instanceof Integer) {
					int val = ((Integer) value).intValue();
					// Avoid placement of thousands of regions
					if (val > MAX_NUMBER_OF_REGIONS) {
						val = MAX_NUMBER_OF_REGIONS;
					}
					// If value is valid
					if (val >= 0) {
						int diff = val - bd.getRegionKeys().size();
						// Avoid ciclic updates whenever no differences are inserted
						if (diff == 0) {
							return;
						}

						while (diff > 0) {
							// Add new region
							addRegion();
							diff--;
						}
						while (diff < 0) {
							// Removes last region
							bd.deleteBlockingRegion(bd.getRegionKeys().lastElement());
							diff++;
						}
					}
					update();
				}
			}
		});

		// Sets table selection action
		regions.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			/**
			 * Called whenever the value of the selection changes.
			 *
			 * @param e the event that characterizes the change.
			 */
			public void valueChanged(ListSelectionEvent e) {
				// Retrives blocking region key
				Object key = regions.getValueAt(regions.getSelectedRow(), 0);
				// If no elements are selected, removes old blocking panel

				if (key == null) {
					if (blockingPanel != null) {
						blockingPanel.lostFocus();
						// Hides blocking panel before removing it to avoid validation bugs under Windows
						blockingPanel.setVisible(false);
						AllBlockingRegionsPanel.this.remove(blockingPanel);
						AllBlockingRegionsPanel.this.add(emptyPanel);
						blockingPanel = null;
						AllBlockingRegionsPanel.this.validate();
					}
					return;
				}

				// A new blocking panel has to be created
				if (blockingPanel == null) {
					AllBlockingRegionsPanel.this.remove(emptyPanel);
					blockingPanel = new BlockingStationPanel(cd, sd, bd, key);
					AllBlockingRegionsPanel.this.add(blockingPanel, BorderLayout.CENTER);
					blockingPanel.gotFocus();
					AllBlockingRegionsPanel.this.validate();
				}
				// Modify existing panel
				else {
					// Lose focus on old data
					blockingPanel.lostFocus();
					blockingPanel.setData(cd, sd, bd, key);
					// Got focus for new data
					blockingPanel.gotFocus();
				}
			}
		});
	}

	/**
	 * Adds a new blocking region to current model
	 */
	private void addRegion() {
		if (bd.getRegionKeys().size() < MAX_NUMBER_OF_REGIONS) {
			bd.addBlockingRegion(Defaults.get("blockingRegionName") + counter++, Defaults.get("blockingRegionType"));
		}
	}

	/**
	 * Updates region spinner and region list to reflect current blocking regions
	 */
	private void update() {
		regionsNumSpinner.setValue(new Integer(bd.getRegionKeys().size()));
		regions.tableChanged(new TableModelEvent(regions.getModel()));
		regions.repaint();
		if (bd.getRegionKeys().size() >= MAX_NUMBER_OF_REGIONS) {
			addRegion.setEnabled(false);
		} else {
			addRegion.setEnabled(true);
		}

		// Selects first blocking region if none selected and if present
		if (regions.getSelectedRow() < 0) {
			regions.getSelectionModel().setSelectionInterval(0, 0);
		}
	}

	protected class RegionTable extends JTable {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		/** Cell renderer for region names */
		private BlockingElementRenderer blockRenderer;
		/** Cell renderer for deletion */
		private ButtonCellEditor deleteRenderer;
		/** Cell renderer for capacity */
		private TableCellRenderer gray = new GrayCellRenderer();

		/** Button to delete selected region */
		private JButton delete = new JButton(new AbstractAction() {
			/**
			 * 
			 */
			private static final long serialVersionUID = 1L;
			{
				putValue(Action.SHORT_DESCRIPTION, "Delete");
				putValue(Action.SMALL_ICON, JMTImageLoader.loadImage("Delete"));
			}

			/**
			 * Invoked when an action occurs.
			 */
			public void actionPerformed(ActionEvent e) {
				int index = regions.getSelectedRow();
				if (index >= 0 && index < regions.getRowCount()) {
					Object key = regions.getValueAt(index, 0);
					bd.deleteBlockingRegion(key);
					AllBlockingRegionsPanel.this.update();
					// Select next or prev element
					if (regions.getRowCount() > index) {
						regions.getSelectionModel().setSelectionInterval(index, index);
					} else if (index - 1 >= 0) {
						regions.getSelectionModel().setSelectionInterval(index - 1, index - 1);
					} else {
						regions.getSelectionModel().clearSelection();
					}
				}
			}
		});

		/**
		 * Constructs a new RegionTable
		 */
		public RegionTable() {
			super(new RegionTableModel());
			blockRenderer = new BlockingElementRenderer();
			delete.setFocusable(false);
			deleteRenderer = new ButtonCellEditor(delete);
			setRowHeight(ROW_HEIGHT);

			// Sets column size
			getColumnModel().getColumn(0).setPreferredWidth(120);
			getColumnModel().getColumn(1).setPreferredWidth(50);
			getColumnModel().getColumn(2).setMaxWidth(20);
			getColumnModel().getColumn(3).setMaxWidth(ROW_HEIGHT);
			getColumnModel().setColumnSelectionAllowed(false);
		}

		/**
		 * Returns an appropriate renderer for the cell specified by this row and
		 * column. If the <code>TableColumn</code> for this column has a non-null
		 * renderer, returns that.  If not, finds the class of the data in
		 * this column (using <code>getColumnClass</code>)
		 * and returns the default renderer for this type of data.
		 *
		 * @param row    the row of the cell to render, where 0 is the first row
		 * @param column the column of the cell to render,
		 *               where 0 is the first column
		 * @return the assigned renderer; if <code>null</code>
		 *         returns the default renderer
		 *         for this type of object
		 */
		@Override
		public TableCellRenderer getCellRenderer(int row, int column) {
			switch (column) {
				case 0:
					return blockRenderer;
				case 1:
					return gray;
				case 2:
					return super.getDefaultRenderer(Boolean.class);
				case 3:
					return deleteRenderer;
				default:
					return null;
			}
		}

		/**
		 * Returns an appropriate editor for the cell specified by
		 * <code>row</code> and <code>column</code>. If the
		 * <code>TableColumn</code> for this column has a non-null editor,
		 * returns that.  If not, finds the class of the data in this
		 * column (using <code>getColumnClass</code>)
		 * and returns the default editor for this type of data.
		 *
		 * @param row    the row of the cell to edit, where 0 is the first row
		 * @param column the column of the cell to edit,
		 *               where 0 is the first column
		 * @return the editor for this cell;
		 *         if <code>null</code> return the default editor for
		 *         this type of cell
		 */
		@Override
		public TableCellEditor getCellEditor(int row, int column) {
			switch (column) {
				case 0:
					return new BlockingTableEditor();
				case 1:
					return super.getDefaultEditor(String.class);
				case 2:
					return super.getDefaultEditor(Boolean.class);
				case 3:
					return new ButtonCellEditor(delete);
				default:
					return null;
			}
		}
	}

	protected class RegionTableModel extends AbstractTableModel {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		/**
		 * Returns the number of columns in the model. A
		 * <code>JTable</code> uses this method to determine how many columns it
		 * should create and display by default.
		 *
		 * @return the number of columns in the model
		 * @see #getRowCount
		 */
		public int getColumnCount() {
			return 4;
		}

		/**
		 * Returns a default name for the column using spreadsheet conventions:
		 * A, B, C, ... Z, AA, AB, etc.  If <code>column</code> cannot be found,
		 * returns an empty string.
		 *
		 * @param column the column being queried
		 * @return a string containing the default name of <code>column</code>
		 */
		@Override
		public String getColumnName(int column) {
			switch (column) {
				case 0:
					return "Name";
				case 1:
					return "Capacity";
				case 2:
					return "\u221e";
				case 3:
					return "";
				default:
					return null;
			}
		}

		/**
		 * Returns the number of rows in the model. A
		 * <code>JTable</code> uses this method to determine how many rows it
		 * should display.  This method should be quick, as it
		 * is called frequently during rendering.
		 *
		 * @return the number of rows in the model
		 * @see #getColumnCount
		 */
		public int getRowCount() {
			return bd.getRegionKeys().size();
		}

		/**
		 * Returns the value for the cell at <code>columnIndex</code> and
		 * <code>rowIndex</code>.
		 *
		 * @param    rowIndex    the row whose value is to be queried
		 * @param    columnIndex the column whose value is to be queried
		 * @return the value Object at the specified cell
		 */
		public Object getValueAt(int rowIndex, int columnIndex) {
			// Try catch for concurrency problems when deleting regions
			try {
				Object key = bd.getRegionKeys().get(rowIndex);
				switch (columnIndex) {
					case 0:
						return key;
					case 1:
						if (bd.getRegionCustomerConstraint(key).intValue() > 0) {
							return bd.getRegionCustomerConstraint(key);
						} else {
							return "\u221e";
						}
					case 2:
						return Boolean.valueOf(bd.getRegionCustomerConstraint(key).intValue() <= 0);
					default:
						return null;
				}
			} catch (Exception e) {
				return null;
			}
		}

		/**
		 * This empty implementation is provided so users don't have to implement
		 * this method if their data model is not editable.
		 *
		 * @param aValue      value to assign to cell
		 * @param rowIndex    row of cell
		 * @param columnIndex column of cell
		 */
		@Override
		public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
			Integer value;
			Object key = bd.getRegionKeys().get(rowIndex);
			switch (columnIndex) {
				case (1):
					try {
						value = Integer.decode((String) aValue);
						if (value.intValue() > 0) {
							bd.setRegionCustomerConstraint(key, value);
						}
					} catch (NumberFormatException e) {
						// Do nothing
					}
					break;
				case (2):
					boolean val = ((Boolean) aValue).booleanValue();
					if (val) {
						bd.setRegionCustomerConstraint(key, new Integer(-1));
					} else {
						value = Defaults.getAsInteger("blockingMaxJobs");
						if (value.intValue() <= 0) {
							value = new Integer(1);
						}
						bd.setRegionCustomerConstraint(key, value);
					}
					break;
			}
			repaint();
		}

		/**
		 * Cell is editable
		 *
		 * @param rowIndex    the row being queried
		 * @param columnIndex the column being queried
		 * @return false
		 */
		@Override
		public boolean isCellEditable(int rowIndex, int columnIndex) {
			return columnIndex != 1 || getValueAt(rowIndex, 2).equals(Boolean.FALSE);
		}
	}

	/**
	 * Inner class used to paint the blocking region's list elements
	 */
	private class BlockingElementRenderer implements TableCellRenderer {
		/**
		* Returns the component used for drawing the cell.  This method is
		* used to configure the renderer appropriately before drawing.
		*
		* @param    table        the <code>JTable</code> that is asking the
		* renderer to draw; can be <code>null</code>
		* @param    value        the value of the cell to be rendered.  It is
		* up to the specific renderer to interpret
		* and draw the value.  For example, if
		* <code>value</code>
		* is the string "true", it could be rendered as a
		* string or it could be rendered as a check
		* box that is checked.  <code>null</code> is a
		* valid value
		* @param    isSelected    true if the cell is to be rendered with the
		* selection highlighted; otherwise false
		* @param    hasFocus    if true, render cell appropriately.  For
		* example, put a special border on the cell, if
		* the cell can be edited, render in the color used
		* to indicate editing
		* @param    row     the row index of the cell being drawn.  When
		* drawing the header, the value of
		* <code>row</code> is -1
		* @param    column     the column index of the cell being drawn
		*/
		public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
			JLabel label = new JLabel(bd.getRegionName(value), JMTImageLoader.loadImage("BlockingCombo"), SwingConstants.LEFT);
			label.setOpaque(true);
			label.setBorder(new LineBorder(hasFocus ? Color.BLUE : Color.WHITE));
			label.setBackground(isSelected ? table.getSelectionBackground() : Color.WHITE);
			label.setForeground(isSelected ? table.getSelectionForeground() : Color.BLACK);
			label.setFont(isSelected ? label.getFont().deriveFont(Font.BOLD) : label.getFont().deriveFont(Font.ROMAN_BASELINE));
			return label;
		}
	}

	/**
	 * Custom editor for blocking region name as the table contains search's key and not name.
	 */
	private class BlockingTableEditor extends DefaultCellEditor {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		JTextField field;
		Object key; // Search's key for blocking region

		/**
		 * Constructs a <code>BlockingTableEditor</code>.
		 *
		 */
		public BlockingTableEditor() {
			super(new JTextField());
		}

		/**
		 * Sets an initial <code>value</code> for the editor.  This will cause
		 * the editor to <code>stopEditing</code> and lose any partially
		 * edited value if the editor is editing when this method is called. <p>
		 * <p/>
		 * Returns the component that should be added to the client's
		 * <code>Component</code> hierarchy.  Once installed in the client's
		 * hierarchy this component will then be able to draw and receive
		 * user input.
		 *
		 * @param    table        the <code>JTable</code> that is asking the
		 * editor to edit; can be <code>null</code>
		 * @param    value        the value of the cell to be edited; it is
		 * up to the specific editor to interpret
		 * and draw the value.  For example, if value is
		 * the string "true", it could be rendered as a
		 * string or it could be rendered as a check
		 * box that is checked.  <code>null</code>
		 * is a valid value
		 * @param    isSelected    true if the cell is to be rendered with
		 * highlighting
		 * @param    row the row of the cell being edited
		 * @param    column the column of the cell being edited
		 * @return the component for editing
		 */
		@Override
		public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {
			field = (JTextField) super.getTableCellEditorComponent(table, value, isSelected, row, column);
			key = value;
			field.setText(bd.getRegionName(key));
			return field;
		}

		/**
		 * Returns the value contained in the editor. and sets name of the blocking region
		 *
		 * @return the value contained in the editor
		 */
		@Override
		public Object getCellEditorValue() {
			if (!field.getText().equals("")) {
				bd.setRegionName(key, field.getText());
			}
			return key;
		}
	}
}
