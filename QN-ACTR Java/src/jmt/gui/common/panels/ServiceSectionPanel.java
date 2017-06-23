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

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.JTable;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.TableModelEvent;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import jmt.framework.gui.table.editors.ButtonCellEditor;
import jmt.framework.gui.table.editors.ComboBoxCellEditor;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.Defaults;
import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.definitions.StationDefinition;
import jmt.gui.common.distributions.Distribution;
import jmt.gui.common.editors.DistributionsEditor;
import jmt.gui.common.editors.ImagedComboBoxCellEditorFactory;
import jmt.gui.common.editors.LDStrategyEditor;
import jmt.gui.common.serviceStrategies.LDStrategy;
import jmt.gui.common.serviceStrategies.ZeroStrategy;

/**
 * Created by IntelliJ IDEA.
 * User: orsotronIII
 * Date: 30-giu-2005
 * Time: 14.01.23
 * Modified by Bertoli Marco 7-oct-2005  --> Added load Dependent
 *                           9-jan-2006  --> ComboBoxCellEditor
 */
public class ServiceSectionPanel extends WizardPanel implements CommonConstants {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected Object stationKey;
	protected StationDefinition data;
	protected ClassDefinition classData;

	private ServiceTable serviceTable;
	private JSpinner serversNumSpinner;

	/** Used to display classes with icon */
	protected ImagedComboBoxCellEditorFactory classEditor;

	//editing of arrival time distribution
	protected AbstractAction editDistribution = new AbstractAction("Edit") {
		/**
		* 
		*/
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "Edits service section parameters");
		}

		public void actionPerformed(ActionEvent e) {
			// ---- Bertoli Marco ----
			int index = serviceTable.getSelectedRow();
			if (index >= 0 && index < serviceTable.getRowCount()) {
				Object key = classData.getClassKeys().elementAt(index);
				Object service = data.getServiceTimeDistribution(stationKey, key);
				// If it's a Distribution, shows Distribution Editor
				if (service instanceof Distribution) {
					DistributionsEditor editor = DistributionsEditor.getInstance(ServiceSectionPanel.this.getParent(), (Distribution) service);
					// Sets editor window title
					editor.setTitle("Editing " + classData.getClassName(key) + " distribution...");
					// Shows editor window
					editor.show();
					// Sets new Distribution to selected class
					data.setServiceTimeDistribution(stationKey, key, editor.getResult());

					// Updates table view. This is needed as Distribution is not contained
					// into edited cell (but in its left one)
					serviceTable.repaint();
				}
				// Otherwise shows LDStrategy Editor
				else {
					LDStrategyEditor editor = LDStrategyEditor.getInstance(ServiceSectionPanel.this.getParent(), (LDStrategy) service);
					// Sets editor window title
					editor.setTitle("Editing " + classData.getClassName(key) + " Load Dependent service strategy...");
					// Shows editor window
					editor.show();
					serviceTable.repaint();
				}
			}
			// ---- end ----
		}
	};

	public ServiceSectionPanel(StationDefinition sd, ClassDefinition cd, Object stationKey) {
		super();
		classEditor = new ImagedComboBoxCellEditorFactory(cd);
		setData(sd, cd, stationKey);
	}

	private void initComponents() {
		//building mainPanel
		this.setLayout(new BorderLayout(5, 5));
		this.setBorder(new EmptyBorder(5, 5, 5, 5));
		//layout of main panel
		JPanel serversNumPanel = new JPanel();
		serversNumPanel.setBorder(new TitledBorder(new EtchedBorder(), "Number of Servers"));
		serversNumPanel.add(new JLabel("Number:"));
		serversNumPanel.add(serversNumSpinner);
		this.add(serversNumPanel, BorderLayout.NORTH);
		WarningScrollTable ServiceSectionTable = new WarningScrollTable(serviceTable, WARNING_CLASS);
		ServiceSectionTable.setBorder(new TitledBorder(new EtchedBorder(), "Service Time Distributions"));
		this.add(ServiceSectionTable, BorderLayout.CENTER);
	}

	public void setData(StationDefinition sd, ClassDefinition cd, Object stationKey) {
		this.removeAll();
		this.stationKey = stationKey;
		data = sd;
		classData = cd;
		serviceTable = new ServiceTable();
		serversNumSpinner = new JSpinner();
		serversNumSpinner.setPreferredSize(DIM_BUTTON_XS);
		classEditor.setData(cd);
		initComponents();
		updateSpinner();
		addManagers();
	}

	private void updateSpinner() {
		serversNumSpinner.setValue(data.getStationNumberOfServers(stationKey));
		if (STATION_TYPE_DELAY.equals(data.getStationType(stationKey))) {
			serversNumSpinner.setValue(new Float(Float.POSITIVE_INFINITY));
			serversNumSpinner.setEnabled(false);
		} else {
			serversNumSpinner.setEnabled(true);
		}
	}

	private void addManagers() {
		serversNumSpinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				if (serversNumSpinner.getValue() instanceof Integer) {
					Integer serverNum = (Integer) serversNumSpinner.getValue();
					if (serverNum.intValue() < 1 && serversNumSpinner.isEnabled()) {
						serverNum = new Integer(1);
						serversNumSpinner.setValue(serverNum);
					}
					data.setStationNumberOfServers(serverNum, stationKey);
				}
			}
		});
	}

	@Override
	public void repaint() {
		if (serviceTable != null) {
			serviceTable.tableChanged(new TableModelEvent(serviceTable.getModel()));
		}
		super.repaint();
	}

	/**
	 * @return the panel's name
	 */
	@Override
	public String getName() {
		return "Service Section";
	}

	/**
	 * called by the Wizard before when switching to another panel
	 */
	@Override
	public void lostFocus() {
		// Aborts editing of table
		TableCellEditor editor = serviceTable.getCellEditor();
		if (editor != null) {
			editor.stopCellEditing();
		}
	}

	/**
	 * called by the Wizard when the panel becomes active
	 */
	@Override
	public void gotFocus() {
		classEditor.clearCache();
	}

	protected class ServiceTable extends JTable {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		/**
		 * This field is used to initialize elements shown on Service type selection - Bertoli Marco
		 */
		protected Object[] serviceType = new Object[] { SERVICE_LOAD_INDEPENDENT, SERVICE_LOAD_DEPENDENT, SERVICE_ZERO };

		JButton editButton = new JButton() {
			/**
			 * 
			 */
			private static final long serialVersionUID = 1L;

			{
				setText("Edit");
			}
		};

		int[] columnSizes = new int[] { 90, 60, 150, 30 };

		public ServiceTable() {
			super();
			setModel(new ServiceTableModel());
			sizeColumns();
			setRowHeight(ROW_HEIGHT);
		}

		@Override
		public TableCellRenderer getCellRenderer(int row, int column) {
			if (column == columnSizes.length - 1 && getValueAt(row, 1) != SERVICE_ZERO) {
				return new ButtonCellEditor(editButton);
			} else if (column == 1) {
				return ComboBoxCellEditor.getRendererInstance();
			} else if (column == 0) {
				return classEditor.getRenderer();
			} else {
				return super.getCellRenderer(row, column);
			}
		}

		@Override
		public TableCellEditor getCellEditor(int row, int column) {
			if (column == 1) {
				return ComboBoxCellEditor.getEditorInstance(serviceType);
			} else if (column == columnSizes.length - 1) {
				return new ButtonCellEditor(new JButton(editDistribution));
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

	protected class ServiceTableModel extends AbstractTableModel {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		private Class[] columnClasses = new Class[] { String.class, JComboBox.class, String.class, Object.class };
		private String[] columnNames = new String[] { "Class", "Strategy", "Service Time Distribution", "" };

		public int getRowCount() {
			return classData.getClassKeys().size();
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
			return ((columnIndex == columnNames.length - 1 && getValueAt(rowIndex, 1) != SERVICE_ZERO) || columnIndex == 1);
		}

		public Object getValueAt(int rowIndex, int columnIndex) {
			Object classKey = classData.getClassKeys().get(rowIndex);
			switch (columnIndex) {
				case (0):
					return classKey;
				case (1):
					// Checks if current service section is load dependent or indipendent
					if (data.getServiceTimeDistribution(stationKey, classKey) instanceof LDStrategy) {
						return SERVICE_LOAD_DEPENDENT;
					} else if (data.getServiceTimeDistribution(stationKey, classKey) instanceof ZeroStrategy) {
						return SERVICE_ZERO;
					} else {
						return SERVICE_LOAD_INDEPENDENT;
					}
				case (2):
					return data.getServiceTimeDistribution(stationKey, classKey);
			}
			return null;
		}

		/**Puts edited values to the underlying data structure for model implementation*/
		@Override
		public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
			Object classKey = classData.getClassKeys().get(rowIndex);
			switch (columnIndex) {
				// Load dependency
				case (1):
					if (SERVICE_LOAD_DEPENDENT.equals(aValue)) {
						// Puts a LDService strategy only if previously it was different
						if (!(data.getServiceTimeDistribution(stationKey, classKey) instanceof LDStrategy)) {
							data.setServiceTimeDistribution(stationKey, classKey, new LDStrategy());
						}
					} else if (SERVICE_ZERO.equals(aValue)) {
						data.setServiceTimeDistribution(stationKey, classKey, new ZeroStrategy());
					} else {
						// Puts default service strategy, only if previously a load dependent was chosen
						if (!(data.getServiceTimeDistribution(stationKey, classKey) instanceof Distribution)) {
							data.setServiceTimeDistribution(stationKey, classKey, Defaults.getAsNewInstance("stationServiceStrategy"));
						}
					}
					repaint();
			}
		}

	}

}
