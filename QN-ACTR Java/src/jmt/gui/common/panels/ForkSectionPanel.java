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
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.Defaults;
import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.definitions.StationDefinition;

/**
 * <p>Title: ForkSection Panel</p>
 * <p>Description: This panelis used to parametrize fork special behaviour</p>
 *
 * @author Bertoli Marco
 *         Date: 15-mar-2006
 *         Time: 14.25.36
 *         
 *  Modified by Ashanka (Dec 09)
 *  Desc: Minor Cosmetic changes in the label. Changed some Label regarding the Definitions for better understanding of Users.
 */
public class ForkSectionPanel extends WizardPanel implements CommonConstants {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected StationDefinition sd;
	protected ClassDefinition cd;
	protected Object stationKey;
	protected JSpinner numForkSpinner, blockSpinner;

	/**
	 * Sets data for this panel
	 * @param sd station definition data structure
	 * @param cd class definition data structure
	 * @param stationKey search's key for input station
	 */
	public void setData(StationDefinition sd, ClassDefinition cd, Object stationKey) {
		this.removeAll();
		this.stationKey = stationKey;
		this.sd = sd;
		this.cd = cd;
		updateSpinners();
		initComponents();
	}

	/**
	 * Builds a new panel to specify fork section parameters
	 * @param sd station definition data structure
	 * @param cd class definition data structure
	 * @param stationKey search's key for input station
	 */
	public ForkSectionPanel(StationDefinition sd, ClassDefinition cd, Object stationKey) {
		super();
		setData(sd, cd, stationKey);
	}

	/**
	 * Initialize layout
	 */
	private void initComponents() {
		//building mainPanel
		this.setLayout(new GridLayout(2, 1));
		this.setBorder(new EmptyBorder(5, 5, 5, 5));

		//layout of fragNum panel
		JPanel fragnum = new JPanel();
		fragnum.setBorder(new TitledBorder(new EtchedBorder(), "Fork degree"));
		this.add(fragnum);
		JLabel text = new JLabel("Number of Forked-Jobs to be generated on each output link for each input job (customer) to the Fork:");
		text.setLabelFor(numForkSpinner);
		fragnum.add(text);
		fragnum.add(numForkSpinner);

		//layout of block panel
		JPanel block = new JPanel(new BorderLayout(10, 10));
		block.setBorder(new TitledBorder(new EtchedBorder(), "Fork-Join Section Capacity"));
		this.add(block);

		// Adds a checkbox to block panel to select block function
		final JCheckBox check = new JCheckBox();
		check.setText("Enable Finite Capacity: limit maximum number of jobs (customers) inside a fork-join section. ");
		check.setToolTipText("Limit the maximum allowed number of jobs inside a fork-join section. Following jobs will be queued.");

		// Adds a spinner to block panel to select block number
		JLabel label = new JLabel("Capacity (max number of jobs, NOT Forked-Jobs): ");
		label.setLabelFor(blockSpinner);

		// Initial values
		if (sd.getForkBlock(stationKey).intValue() < 0) {
			check.setSelected(false);
			blockSpinner.setValue(new Float(Float.POSITIVE_INFINITY));
			blockSpinner.setEnabled(false);
		} else {
			check.setSelected(true);
			blockSpinner.setValue(sd.getForkBlock(stationKey));
			blockSpinner.setEnabled(true);
		}

		// Adds action listeners
		check.addActionListener(new ActionListener() {

			/**
			 * Toggles block property
			 */
			public void actionPerformed(ActionEvent e) {
				if (check.isSelected()) {
					if (Defaults.getAsInteger("forkBlock").intValue() > 0) {
						sd.setForkBlock(stationKey, Defaults.getAsInteger("forkBlock"));
					} else {
						sd.setForkBlock(stationKey, new Integer(1));
					}
					blockSpinner.setValue(sd.getForkBlock(stationKey));
					blockSpinner.setEnabled(true);
				} else {
					sd.setForkBlock(stationKey, new Integer(-1));
					blockSpinner.setValue(new Float(Float.POSITIVE_INFINITY));
					blockSpinner.setEnabled(false);
				}
			}
		});

		// Creates a temp panel for blockSpinner and its label
		JPanel tmp = new JPanel();
		tmp.add(label);
		tmp.add(blockSpinner);

		block.add(check, BorderLayout.NORTH);
		block.add(tmp, BorderLayout.CENTER);
	}

	/**
	 * Creates spinner to choose number of splits for each output link
	 * and to choose blocking region number of jobs
	 */
	private void updateSpinners() {
		numForkSpinner = new JSpinner();
		numForkSpinner.setPreferredSize(DIM_BUTTON_XS);
		numForkSpinner.setValue(sd.getStationNumberOfServers(stationKey));
		numForkSpinner.setToolTipText("Number of tasks created for each input job on each output link");
		numForkSpinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				if (numForkSpinner.getValue() instanceof Integer) {
					Integer serverNum = (Integer) numForkSpinner.getValue();
					if (serverNum.intValue() < 1) {
						serverNum = new Integer(1);
						numForkSpinner.setValue(serverNum);
					}
					sd.setStationNumberOfServers(serverNum, stationKey);
				}
			}
		});

		blockSpinner = new JSpinner();
		blockSpinner.setPreferredSize(DIM_BUTTON_XS);
		blockSpinner.setToolTipText("Maximum allowed number of jobs inside a fork-join section. Following jobs will be queued.");
		blockSpinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				if (blockSpinner.getValue() instanceof Integer) {
					Integer blockNum = (Integer) blockSpinner.getValue();
					if (blockNum.intValue() < 1) {
						blockNum = new Integer(1);
						blockSpinner.setValue(blockNum);
					}
					sd.setForkBlock(stationKey, blockNum);
				}
			}
		});

	}

	/**
	 * @return the panel's name
	 */
	@Override
	public String getName() {
		return "Fork Section";
	}
}
