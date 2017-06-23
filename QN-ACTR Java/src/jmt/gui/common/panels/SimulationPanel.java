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
import java.util.HashMap;

import javax.swing.Box;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.JTable;
import javax.swing.SpinnerNumberModel;
import javax.swing.SpringLayout;
import javax.swing.SwingConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.table.TableCellEditor;

import jmt.framework.gui.layouts.SpringUtilities;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.definitions.GuiInterface;
import jmt.gui.common.definitions.SimulationDefinition;
import jmt.gui.common.definitions.StationDefinition;
import jmt.gui.exact.table.ExactTable;
import jmt.gui.exact.table.ExactTableModel;

/**
 * <p>Title: Simulation & Preloading panel</p>
 * <p>Description: A panel in which simulation parameters and queue preloading can be specified</p>
 * 
 * @author Bertoli Marco
 *         Date: 16-set-2005
 *         Time: 14.01.02
 *
 * Modified by Francesco D'Aquino 11/11/2005
 */
public class SimulationPanel extends WizardPanel implements CommonConstants {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected static final int BORDERSIZE = 20;
	protected static final long MINIMUM_TIME = 5; // Minimum simulation duration
	protected ClassDefinition cd;
	protected StationDefinition sd;
	protected SimulationDefinition simd;
	protected HashMap<Object, Integer> unallocated;

	// Simulation parameters
	protected JCheckBox randomSeed, infDuration, noStatistic;
	protected JSpinner seed, duration, polling, maxSamples;
	protected JTable preloadTable;

	//Francesco D'Aquino
	protected JCheckBox animationEnabler;
	private GuiInterface gui;

	// end Francesco D'Aquino

	/**
	 * Builds a new simulation panel
	 * @param cd a ClassDefinition data structure
	 * @param sd a StationDefinition data structure
	 * @param simd a SimulationDefinition data structure
	 */
	public SimulationPanel(ClassDefinition cd, StationDefinition sd, SimulationDefinition simd, GuiInterface gui) {
		setData(cd, sd, simd);
		//Francesco D'Aquino
		this.gui = gui;
		InitGUI(this.gui);
		// end Francesco D'Aquino
		InitActions();
	}

	/**
	 * Initialize internal data structures
	 * @param cd a ClassDefinition data structure
	 * @param sd a StationDefinition data structure
	 * @param simd a SimulationDefinition data structure
	 */
	public void setData(ClassDefinition cd, StationDefinition sd, SimulationDefinition simd) {
		this.cd = cd;
		this.sd = sd;
		this.simd = simd;
		refreshDataStructures();
	}

	/**
	 * Refresh internal data structures. This method is separate from setData as have to
	 * be called in JSIM at every gotFocus event (or internal data structures will
	 * not be up-to-date)
	 */
	protected void refreshDataStructures() {
		// Creates an hashmap with unallocated jobs for every class closed class
		unallocated = new HashMap<Object, Integer>();
		for (int i = 0; i < cd.getClassKeys().size(); i++) {
			Object key = cd.getClassKeys().get(i);
			if (cd.getClassType(key) == CLASS_TYPE_CLOSED) {
				unallocated.put(key, new Integer(cd.getClassPopulation(key).intValue() - simd.getPreloadedJobsNumber(key).intValue()));
			}
		}
	}

	/**
	 * called by the Wizard when the panel becomes active
	 */
	@Override
	public void gotFocus() {
		refreshDataStructures();
		this.removeAll();
		InitGUI(gui);
		InitActions();
	}

	/**
	 * called by the Wizard before when switching to another panel
	 */
	@Override
	public void lostFocus() {
		// Aborts editing of table
		TableCellEditor editor = preloadTable.getCellEditor();
		if (editor != null) {
			editor.stopCellEditing();
		}
		simd.manageJobs();
	}

	/**
	 * Sets the tab to be shown
	 * @param tabNumber the index of the tab to be shown
	 */
	/*public void setSelectedTab(int tabNumber) {
	    tabbedPane.setSelectedIndex(tabNumber);    
	}*/

	/**
	 * Initialize all GUI related stuff
	 *
	 * Modified by Francesco D'Aquino.
	 * The old version was InitGUI(). The gui parameter is used to insert or not the
	 * JCheckBox used to enable queue animation.
	 */
	protected void InitGUI(GuiInterface gui) {
		// ------------ Francesco D'Aquino  ------------------------------------
		//tabbedPane = new JTabbedPane();
		// ----------- end Francesco D'Aquino ----------------------------------

		// Adds margins and a central main panel
		this.setLayout(new BorderLayout());
		this.add(Box.createVerticalStrut(BORDERSIZE), BorderLayout.NORTH);
		this.add(Box.createVerticalStrut(BORDERSIZE), BorderLayout.SOUTH);
		this.add(Box.createHorizontalStrut(BORDERSIZE), BorderLayout.WEST);
		this.add(Box.createHorizontalStrut(BORDERSIZE), BorderLayout.EAST);
		JPanel mainpanel = new JPanel(new BorderLayout());
		this.add(mainpanel, BorderLayout.CENTER);

		JPanel upperPanel = new JPanel(new BorderLayout());
		JLabel descrLabel = new JLabel(SIMULATION_DESCRIPTION);
		descrLabel.setVerticalAlignment(SwingConstants.NORTH);
		upperPanel.add(descrLabel, BorderLayout.WEST);
		upperPanel.add(Box.createVerticalStrut(BORDERSIZE / 2), BorderLayout.SOUTH);
		mainpanel.add(upperPanel, BorderLayout.NORTH);

		// Adds preloading table
		preloadTable = new PreloadingTable();
		WarningScrollTable preloadPanel = new WarningScrollTable(preloadTable, WARNING_CLASS_STATION);
		//tabbedPane.addTab("Initial state",preloadPanel);
		//ParametricAnalysisPanel parametricAnalysisPanel = new ParametricAnalysisPanel();
		//tabbedPane.addTab("Parametric analysis options",parametricAnalysisPanel);
		//mainpanel.add(tabbedPane, BorderLayout.CENTER);
		mainpanel.add(preloadPanel, BorderLayout.CENTER);

		// Adds simulation parameters
		JPanel simPanel = new JPanel(new SpringLayout());
		JLabel label;
		// Simulation seed
		label = new JLabel("Simulation random seed: ");
		simPanel.add(label);
		seed = new JSpinner();
		seed.setValue(simd.getSimulationSeed());
		label.setLabelFor(seed);
		simPanel.add(seed);
		randomSeed = new JCheckBox("random");
		if (simd.getUseRandomSeed()) {
			randomSeed.setSelected(true);
			seed.setEnabled(false);
			randomSeed.setToolTipText("Uses a random seed to initialize the random number generator");
		}
		simPanel.add(randomSeed);
		// Maximum duration
		label = new JLabel("Maximum duration (sec): ");
		simPanel.add(label);
		duration = new JSpinner(new SpinnerNumberModel(1, .1, Integer.MAX_VALUE, 1));
		duration.setValue(simd.getMaximumDuration());
		label.setLabelFor(duration);
		simPanel.add(duration);
		infDuration = new JCheckBox("infinite");
		infDuration.setToolTipText("Disables the automatic timer used to stop simulation");
		if (simd.getMaximumDuration().longValue() == -1) {
			infDuration.setSelected(true);
			duration.setValue(new Double(600));
			duration.setEnabled(false);
		}
		simPanel.add(infDuration);

		// Maximum number of samples
		label = new JLabel("Maximum number of samples: ");
		simPanel.add(label);
		maxSamples = new JSpinner(new SpinnerNumberModel(500000, 100000, Integer.MAX_VALUE, 50000));
		maxSamples.setValue(simd.getMaxSimulationSamples());
		label.setLabelFor(maxSamples);
		simPanel.add(maxSamples);
		// Adds disable statistic checkbox
		noStatistic = new JCheckBox("no automatic stop");
		noStatistic.setToolTipText("Disable confidence interval/maximum relative error as simulation stopping criteria");
		noStatistic.setSelected(simd.getDisableStatistic().booleanValue());
		simPanel.add(noStatistic);

		//Francesco D'Aquino
		if (gui.isAnimationDisplayable()) {
			animationEnabler = new JCheckBox("animation");
			animationEnabler.setToolTipText("Shows queue animation during simulation");
			if (simd.isAnimationEnabled()) {
				animationEnabler.setSelected(true);
			}
		}
		// end Francesco D'Aquino

		// Maximum duration
		label = new JLabel("Animation update interval (sec): ");
		simPanel.add(label);
		polling = new JSpinner(new SpinnerNumberModel(1, .1, Integer.MAX_VALUE, 1));
		polling.setValue(new Double(simd.getPollingInterval()));
		label.setLabelFor(polling);
		simPanel.add(polling);

		if (gui.isAnimationDisplayable()) {
			simPanel.add(animationEnabler);
		} else {
			simPanel.add(new JPanel());
		}

		SpringUtilities.makeCompactGrid(simPanel, 4, 3, //rows, cols
				16, 6, //initX, initY
				6, 6);//xPad, yPad
		upperPanel.add(simPanel, BorderLayout.CENTER);
	}

	/**
	 * Inits all action listeners related to GUI object
	 */
	protected void InitActions() {
		// Random seed
		randomSeed.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				if (randomSeed.isSelected()) {
					simd.setUseRandomSeed(true);
					seed.setEnabled(false);
				} else {
					simd.setUseRandomSeed(false);
					seed.setEnabled(true);
				}
			}
		});
		// Seed value
		seed.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				Object value = seed.getValue();
				if (value instanceof Long) {
					Long l = (Long) value;
					if (l.longValue() >= 0) {
						simd.setSimulationSeed(l);
					}
				} else if (value instanceof Integer) {
					Integer i = (Integer) value;
					if (i.intValue() >= 0) {
						simd.setSimulationSeed(new Long(i.intValue()));
					}
				}
				seed.setValue(simd.getSimulationSeed());
			}
		});

		// Infinite duration
		infDuration.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				if (infDuration.isSelected()) {
					simd.setMaximumDuration(new Double(-1));
					duration.setEnabled(false);
				} else {
					Object value = duration.getValue();
					if (value instanceof Double) {
						simd.setMaximumDuration((Double) value);
					} else if (value instanceof Integer) {
						simd.setMaximumDuration(new Double(((Integer) value).intValue()));
					}
					duration.setEnabled(true);
				}
			}
		});

		if (animationEnabler != null) {
			animationEnabler.addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent e) {
					if (animationEnabler.isSelected()) {
						simd.setAnimationEnabled(true);
					} else {
						simd.setAnimationEnabled(false);
					}
				}
			});
		}

		// Duration
		duration.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				Object value = duration.getValue();
				if (value instanceof Double) {
					Double d = (Double) value;
					if (d.doubleValue() >= MINIMUM_TIME) {
						simd.setMaximumDuration(d);
					}
				} else if (value instanceof Integer) {
					Integer i = (Integer) value;
					if (i.intValue() >= MINIMUM_TIME) {
						simd.setMaximumDuration(new Double(i.intValue()));
					}
				}
				duration.setValue(simd.getMaximumDuration());
			}
		});

		// Maximum number of samples
		maxSamples.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				Object value = maxSamples.getValue();
				if (value instanceof Integer) {
					Integer i = (Integer) value;
					if (i.intValue() >= 100000) {
						simd.setMaxSimulationSamples(i);
					}
				}
				maxSamples.setValue(simd.getMaxSimulationSamples());
			}
		});

		//Polling interval
		polling.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				Object value = polling.getValue();
				if (value instanceof Double) {
					Double d = (Double) value;
					if (d.doubleValue() > 0) {
						simd.setPollingInterval(d.doubleValue());
					}
				} else if (value instanceof Integer) {
					Integer i = (Integer) value;
					if (i.intValue() > 0) {
						simd.setPollingInterval(i.doubleValue());
					}
				}
				polling.setValue(new Double(simd.getPollingInterval()));
			}
		});

		// Disable statistic
		noStatistic.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				simd.setDisableStatistic(new Boolean(noStatistic.isSelected()));
			}
		});
	}

	/**
	 * Inner class to specify preloading table
	 */
	protected class PreloadingTable extends ExactTable {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public PreloadingTable() {
			super(new PreloadTableModel());
			setDefaultEditor(Integer.class, new jmt.gui.exact.table.ExactCellEditor());
			autoResizeMode = AUTO_RESIZE_OFF;
			setDisplaysScrollLabels(true);

			setRowSelectionAllowed(false);
			setColumnSelectionAllowed(false);
			setClipboardTransferEnabled(false);
		}
	}

	/**
	 * Model for Preload table
	 * Rows represent classes, columns stations.
	 */
	protected class PreloadTableModel extends ExactTableModel {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public PreloadTableModel() {
			prototype = "Station10000";
			rowHeaderPrototype = "Class10000 (Ni = 100)";
		}

		public int getRowCount() {
			return cd.getClassKeys().size();
		}

		public int getColumnCount() {
			return sd.getStationKeysNoSourceSink().size();
		}

		@Override
		public Class<?> getColumnClass(int columnIndex) {
			if (columnIndex >= 0) {
				return Integer.class;
			} else {
				return super.getColumnClass(columnIndex);
			}
		}

		@Override
		public String getColumnName(int index) {
			if (index >= 0 && sd.getStationKeysNoSourceSink().size() > 0) {
				return sd.getStationName(getStationKey(index));
			} else {
				return "";
			}
		}

		@Override
		public Object getPrototype(int i) {
			if (i == -1) {
				return rowHeaderPrototype;
			} else {
				return prototype;
			}
		}

		@Override
		protected Object getValueAtImpl(int rowIndex, int columnIndex) {
			Object row = getClassKey(rowIndex), col = getStationKey(columnIndex);
			return simd.getPreloadedJobs(col, row);
		}

		@Override
		protected Object getRowName(int rowIndex) {
			String className = cd.getClassName(getClassKey(rowIndex));
			Integer population = cd.getClassPopulation(getClassKey(rowIndex));
			if (cd.getClassType(getClassKey(rowIndex)) == CLASS_TYPE_OPEN) {
				return className;
			} else {
				return className + " (Ni = " + population + ")";
			}
		}

		//returns search key of a station given its index in table
		private Object getStationKey(int index) {
			return sd.getStationKeysNoSourceSink().get(index);
		}

		//returns search key of a class given its index in table
		private Object getClassKey(int index) {
			return cd.getClassKeys().get(index);
		}

		@Override
		public void setValueAt(Object value, int rowIndex, int columnIndex) {
			try {
				if (value instanceof Integer || value instanceof String) {
					int i;
					if (value instanceof Integer) {
						i = ((Integer) value).intValue();
					} else {
						i = Integer.parseInt((String) value);
					}

					Object key = getClassKey(rowIndex);
					int oldjobs = simd.getPreloadedJobs(getStationKey(columnIndex), key).intValue();
					if (i >= 0) {
						if (cd.getClassType(key) == CLASS_TYPE_OPEN) {
							simd.setPreloadedJobs(new Integer(i), getStationKey(columnIndex), key);
						} else if (i - oldjobs <= unallocated.get(key).intValue()) {
							simd.setPreloadedJobs(new Integer(i), getStationKey(columnIndex), key);
							int newunallocated = unallocated.get(key).intValue() - i + oldjobs;
							unallocated.put(key, new Integer(newunallocated));
						}
					}
				}
			} catch (NumberFormatException e) {
				// Aborts modification if String is invalid
			}
		}

		@Override
		public void clear(int row, int col) {
			int oldjobs = simd.getPreloadedJobs(getStationKey(col), getClassKey(row)).intValue();
			simd.setPreloadedJobs(new Integer(0), getStationKey(col), getClassKey(row));
			// If class is closed, put back oldjobs into unallocated data structure
			if (cd.getClassType(getClassKey(row)) == CLASS_TYPE_CLOSED) {
				int newunallocated = unallocated.get(getClassKey(row)).intValue() + oldjobs;
				unallocated.put(getClassKey(row), new Integer(newunallocated));
			}
		}

		@Override
		public boolean isCellEditable(int rowIndex, int columnIndex) {
			return true;
		}

	}

	/**
	 * @return name to be displayed on the tab, when inserted in a wizard tabbed pane
	 */
	@Override
	public String getName() {
		return "Simulation";
	}

}
