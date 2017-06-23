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
import java.util.Vector;

import javax.swing.AbstractListModel;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JScrollPane;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.definitions.StationDefinition;
import jmt.gui.common.panels.StationParameterPanel;
import jmt.gui.common.resources.JMTImageLoader;

/**
 * Created by IntelliJ IDEA.
 * User: orsotronIII
 * Date: 14-lug-2005
 * Time: 13.17.44
 * Modified by Bertoli Marco 11-apr-2006
 */
public class AllStationsParametersPanel extends WizardPanel implements CommonConstants {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private StationParameterPanel stationParsPane;
	private JList stationsList;
	private JLabel panelDescription;

	private StationDefinition stationData;
	private ClassDefinition classData;
	private Object selectedKey; // Search's key for selected station

	public AllStationsParametersPanel(StationDefinition sd, ClassDefinition cd) {
		super();
		stationData = sd;
		classData = cd;
		initComponents();
	}

	private void initComponents() {
		setLayout(new BorderLayout(5, 5));
		this.setBorder(new EmptyBorder(20, 20, 20, 20));
		//classesList = new JList(new StationsListModel());
		stationsList = new JList();
		stationsList.setListData(stationData.getStationKeys());
		stationsList.setCellRenderer(new StationElementRenderer());
		stationsList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		panelDescription = new JLabel(STATIONS_PAR_DESCRIPTION);
		JScrollPane jsp = new JScrollPane(stationsList, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
				ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		jsp.setPreferredSize(new Dimension(140, 200));
		add(panelDescription, BorderLayout.NORTH);
		add(jsp, BorderLayout.WEST);
		stationsList.addListSelectionListener(new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				updateParsPane();
			}
		});
	}

	@Override
	public String getName() {
		return "Station Parameters";
	}

	private void updateParsPane() {
		Object stationKey = null;
		stationKey = stationsList.getSelectedValue();
		if (stationParsPane != null) {
			stationParsPane.setData(stationKey);
		} else {
			stationParsPane = new StationParameterPanel(stationData, classData, stationKey);
			add(stationParsPane, BorderLayout.CENTER);
		}
	}

	//----------------------------------- Francesco D'Aquino ----------------------------

	/**
	 * Shows a section panel
	 * @param stationKey the station you are interested to edit
	 * @param classKey the class you are interested to edit, use null if this parameter is not needed
	 * @param index of the panel to show:
	 *          <br> 0 - queue section</br>
	 *          <br> 1 - service section</br>
	 *          <br> 2 - routing section</br>;
	 */
	public void showStationParameterPanel(Object stationKey, Object classKey, int index) {
		if (stationParsPane != null) {
			stationParsPane.setData(stationKey);

		} else {
			stationParsPane = new StationParameterPanel(stationData, classData, stationKey);
			add(stationParsPane, BorderLayout.CENTER);
		}
		if (index == 0) {
			stationParsPane.showQueueSectionPanel();
		} else if (index == 1) {
			stationParsPane.showServiceSectionPanel();
		} else if (index == 2) {
			stationParsPane.showRoutingSectionPanel(classKey);
		}

	}

	// --------------------- end Francesco D'Aquino --------------------------------------

	public void setData(StationDefinition sd, ClassDefinition cd) {
		this.stationData = sd;
		this.classData = cd;
		if (stationParsPane != null) {
			stationParsPane.setData(stationData, classData, null);
		}
		repaint();
	}

	/**
	 * Updates list of stations and selects last selected station
	 */
	@Override
	public void gotFocus() {
		if (stationsList != null) {
			Vector stations = stationData.getStationKeys();
			stationsList.setListData(stations);
			// If old selected key exists selects it, oterwise select first station
			if (stations.contains(selectedKey)) {
				stationsList.setSelectedValue(selectedKey, true);
			} else if (stations.size() > 0) {
				stationsList.setSelectedIndex(0);
				selectedKey = stationsList.getSelectedValue();
			}
		}
	}

	/**
	 * Stores previous selected station
	 */
	@Override
	public void lostFocus() {
		selectedKey = stationsList.getSelectedValue();
	}

	private class StationElementRenderer implements ListCellRenderer {

		public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
			JLabel label = new JLabel(stationData.getStationName(value), JMTImageLoader.loadImage(stationData.getStationType(value) + "Combo"),
					SwingConstants.LEFT);
			label.setOpaque(true);
			label.setBorder(new LineBorder(cellHasFocus ? Color.BLUE : Color.WHITE));
			label.setBackground(isSelected ? list.getSelectionBackground() : Color.WHITE);
			label.setForeground(isSelected ? list.getSelectionForeground() : Color.BLACK);
			label.setFont(isSelected ? label.getFont().deriveFont(Font.BOLD) : label.getFont().deriveFont(Font.ROMAN_BASELINE));
			return label;
		}
	}

	private class StationsListModel extends AbstractListModel {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public int getSize() {
			return stationData.getStationKeys().size();
		}

		public Object getElementAt(int index) {
			return stationData.getStationKeys().get(index);
			//return stationData.getStationName(stationData.getStationKeys().get(index));
		}

	}

}
