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
import java.awt.Component;

import javax.swing.JTabbedPane;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.definitions.StationDefinition;

/**
 * Created by IntelliJ IDEA.
 * User: orsotronIII
 * Date: 14-lug-2005
 * Time: 10.35.43
 * To change this template use Options | File Templates.
 */
public class StationParameterPanel extends WizardPanel implements CommonConstants {
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;
	protected StationDefinition stationData;
	protected ClassDefinition classData;
	protected Object stationKey;

	protected JTabbedPane mainPanel;

	protected InputSectionPanel isPane;
	protected ServiceSectionPanel ssPane;
	protected RoutingSectionPanel rsPane;
	protected ForkSectionPanel fsPane;
	protected LoggerSectionPanel lsPane;
	// Current wizard panel
	protected WizardPanel current;

	private TitledBorder title = new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED), "");

	public StationParameterPanel(StationDefinition sd, ClassDefinition cd, Object stationKey) {
		super();
		initComponents();
		setData(sd, cd, stationKey);
	}

	private void initComponents() {
		this.setLayout(new BorderLayout(5, 5));
		mainPanel = new JTabbedPane();
		mainPanel.setBorder(title);

		// Adds a change listener to perform gotFocus() and lostFocus() calls on wizardPanels
		mainPanel.addChangeListener(new ChangeListener() {

			/**
			 * Invoked when the target of the listener has changed its state.
			 *
			 * @param e a ChangeEvent object
			 */
			public void stateChanged(ChangeEvent e) {
				// Lose focus on old panel
				if (current != null) {
					current.lostFocus();
				}
				// gets focus on new panel
				if (mainPanel.getSelectedComponent() != null) {
					current = (WizardPanel) mainPanel.getSelectedComponent();
					current.gotFocus();
				}
			}
		});
		add(mainPanel, BorderLayout.CENTER);
	}

	public void setData(StationDefinition sd, ClassDefinition cd, Object stationKey) {
		stationData = sd;
		classData = cd;
		setData(stationKey);
	}

	public void setData(Object stationKey) {
		this.stationKey = stationKey;
		String type = stationData.getStationType(stationKey);
		// Stores information on previously shown panel
		Component oldComponent;
		oldComponent = mainPanel.getSelectedComponent();

		mainPanel.removeAll();
		if (stationData.getStationName(stationKey) != null) {
			title.setTitle(stationData.getStationName(stationKey) + " Parameters Definiton");
		} else {
			title.setTitle("Parameters Definition");
		}
		if (hasForkSection(type)) {
			if (fsPane == null) {
				fsPane = new ForkSectionPanel(stationData, classData, stationKey);
			} else {
				fsPane.setData(stationData, classData, stationKey);
			}
			mainPanel.add(fsPane, fsPane.getName());

			// If this was previously selected, selects this
			if (oldComponent == fsPane) {
				mainPanel.setSelectedComponent(oldComponent);
			}

		}
		if (hasInputSection(type)) {
			if (isPane == null) {
				isPane = new InputSectionPanel(stationData, classData, stationKey);
			} else {
				isPane.setData(stationData, classData, stationKey);
			}
			mainPanel.add(isPane, isPane.getName());

			// If this was previously selected, selects this
			if (oldComponent == isPane) {
				mainPanel.setSelectedComponent(oldComponent);
			}

		}
		if (hasServiceSection(type)) {
			if (ssPane == null) {
				ssPane = new ServiceSectionPanel(stationData, classData, stationKey);
			} else {
				ssPane.setData(stationData, classData, stationKey);
			}
			mainPanel.add(ssPane, ssPane.getName());

			// If this was previously selected, selects this
			if (oldComponent == ssPane) {
				mainPanel.setSelectedComponent(oldComponent);
			}

		}
		if (hasLoggerSection(type)) {
			if (lsPane == null) {
				lsPane = new LoggerSectionPanel(stationData, classData, stationKey);
			} else {
				lsPane.setData(stationData, classData, stationKey);
			}
			mainPanel.add(lsPane, lsPane.getName());

			// If this was previously selected, selects this
			if (oldComponent == lsPane) {
				mainPanel.setSelectedComponent(oldComponent);
			}

		}
		if (hasRoutingSection(type)) {
			if (rsPane == null) {
				rsPane = new RoutingSectionPanel(stationData, classData, stationKey);
			} else {
				rsPane.setData(stationData, classData, stationKey);
			}
			mainPanel.add(rsPane, rsPane.getName());

			// If this was previously selected, selects this
			if (oldComponent == rsPane) {
				mainPanel.setSelectedComponent(oldComponent);
			}

		}

		// Sets current panel
		current = (WizardPanel) mainPanel.getSelectedComponent();
	}

	private boolean hasInputSection(String type) {
		if (checkNullValues()) {
			return false;
		} else {
			return STATION_TYPE_SERVER.equals(type) || STATION_TYPE_FORK.equals(type);
		}
	}

	private boolean hasServiceSection(String type) {
		if (checkNullValues()) {
			return false;
		} else {
			return STATION_TYPE_SERVER.equals(type) || STATION_TYPE_DELAY.equals(type);
		}
	}

	private boolean hasRoutingSection(String type) {
		if (checkNullValues()) {
			return false;
		} else {
			return STATION_TYPE_SERVER.equals(type) || STATION_TYPE_DELAY.equals(type) || STATION_TYPE_SOURCE.equals(type)
					|| STATION_TYPE_ROUTER.equals(type) || STATION_TYPE_LOGGER.equals(type) || STATION_TYPE_TERMINAL.equals(type)
					|| STATION_TYPE_JOIN.equals(type);
		}
	}

	private boolean hasLoggerSection(String type) {
		if (checkNullValues()) {
			return false;
		} else {
			return STATION_TYPE_LOGGER.equals(type);
		}
	}

	private boolean hasForkSection(String type) {
		if (checkNullValues()) {
			return false;
		} else {
			return STATION_TYPE_FORK.equals(type);
		}
	}

	private boolean checkNullValues() {
		return stationKey == null || stationData == null || classData == null;
	}

	// -------------------------------------------------------------------------
	// ----------------------  Francesco D'Aquino ------------------------------
	// -------------------------------------------------------------------------
	public void showRoutingSectionPanel() {
		mainPanel.setSelectedComponent(rsPane);
	}

	public void showRoutingSectionPanel(Object classKey) {
		rsPane.setData(stationData, classData, stationKey);
		rsPane.setSelectedClass(classKey);
		mainPanel.setSelectedComponent(rsPane);
	}

	public void showQueueSectionPanel() {
		mainPanel.setSelectedComponent(isPane);
	}

	public void showServiceSectionPanel() {
		mainPanel.setSelectedComponent(ssPane);
	}

	/**
	 * @return the panel's name
	 */
	@Override
	public String getName() {
		return "Station Parameters";
	}

	/**
	 * called by the Wizard before when switching to another panel
	 */
	@Override
	public void lostFocus() {
		if (current != null) {
			current.lostFocus();
		}
	}
}
