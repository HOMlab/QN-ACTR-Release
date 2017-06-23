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

import javax.swing.Box;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;

import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.definitions.SimulationDefinition;
import jmt.gui.common.definitions.StationDefinition;

/**
 * Created by IntelliJ IDEA.
 * User: orsotronIII
 * Date: 29-lug-2005
 * Time: 15.14.21
 * To change this template use Options | File Templates.
 */
public class RSPLPanel extends WizardPanel implements CommonConstants {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private RefSourcePanel rsPanel;

	private ClassDefinition classData;
	private StationDefinition stationData;
	private SimulationDefinition simData;

	public RSPLPanel(ClassDefinition classes, StationDefinition stations, SimulationDefinition sim) {
		super();
		setData(classes, stations, sim);
		initComponents();
		refreshComponents();
	}

	public void setData(ClassDefinition classes, StationDefinition stations, SimulationDefinition sim) {
		classData = classes;
		stationData = stations;
		simData = sim;
		refreshComponents();
	}

	public void initComponents() {
		setLayout(new BorderLayout(5, 5));
		setBorder(new EmptyBorder(20, 20, 20, 20));
		JPanel upperPanel = new JPanel(new BorderLayout());
		JLabel descrLabel = new JLabel(REFSOURCE_DESCRIPTION);
		upperPanel.add(descrLabel, BorderLayout.CENTER);
		upperPanel.add(Box.createVerticalStrut(10), BorderLayout.SOUTH);
		add(upperPanel, BorderLayout.NORTH);

		rsPanel = new RefSourcePanel(classData, stationData, simData);
		add(rsPanel, BorderLayout.CENTER);
	}

	/**
	 * called by the Wizard when the panel becomes active
	 */
	@Override
	public void gotFocus() {
		if (rsPanel != null) {
			rsPanel.gotFocus();
		}
	}

	@Override
	public void repaint() {
		refreshComponents();
		super.repaint();
	}

	private void refreshComponents() {
		if (rsPanel != null) {
			rsPanel.setData(classData, stationData, simData);
		}
	}

	@Override
	public String getName() {
		return "Reference Stations";
	}

	/**
	 * called by the Wizard before when switching to another panel
	 */
	@Override
	public void lostFocus() {
		if (rsPanel != null) {
			rsPanel.lostFocus();
		}
		simData.manageJobs();
	}

}
