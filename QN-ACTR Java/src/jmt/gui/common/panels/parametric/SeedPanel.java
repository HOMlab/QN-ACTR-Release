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
package jmt.gui.common.panels.parametric;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridLayout;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.ScrollPaneConstants;
import javax.swing.SpinnerNumberModel;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.definitions.SimulationDefinition;
import jmt.gui.common.definitions.StationDefinition;
import jmt.gui.common.definitions.parametric.ParametricAnalysis;
import jmt.gui.common.definitions.parametric.ParametricAnalysisChecker;
import jmt.gui.common.definitions.parametric.SeedParametricAnalysis;

/**
 * <p>Title: SeedPanel</p>
 * <p>Description: this is the panel for parametric analysis where all parameters
 * of the model are kept constant, while the simulation seed is modified.</p>
 *
 * @author Francesco D'Aquino
 *         Date: 9-mar-2006
 *         Time: 16.21.06
 */
public class SeedPanel extends ParameterOptionPanel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private JLabel stepsLabel;
	private JSpinner steps;
	private JScrollPane scroll;
	private JTextArea description;
	private JScrollPane descrPane;
	private TitledBorder descriptionTitle;
	private ParametricAnalysisChecker checker;

	private SeedParametricAnalysis SPA;

	public SeedPanel(SeedParametricAnalysis spa, ClassDefinition classDef, StationDefinition stationDef, SimulationDefinition simDef) {
		super();
		SPA = spa;
		super.setOrientation(JSplitPane.HORIZONTAL_SPLIT);
		super.setDividerSize(3);

		DESCRIPTION = "Repeat the simulations changing only the seed of the random number " + "generator.\n\n"
				+ "The seeds used for the repeated simulations are randomly"
				+ " generated, starting from the seed specified in the 'Simulation Parameters' panel.";
		checker = new ParametricAnalysisChecker(cd, sd, simd);
		cd = classDef;
		sd = stationDef;
		simd = simDef;
		initialize();
	}

	public void initialize() {
		JPanel edit = new JPanel(new GridLayout(4, 1, 0, 5));
		stepsLabel = new JLabel("Steps (n. of exec.): ");
		steps = new JSpinner(new SpinnerNumberModel(SPA.getNumberOfSteps(), 2, ParametricAnalysis.MAX_STEP_NUMBER, 1));
		steps.setToolTipText("Sets the number of performed simulations");
		edit.add(stepsLabel);
		edit.add(steps);
		edit.setPreferredSize(new Dimension(130, 88));
		JPanel editLables = new JPanel(new GridLayout(4, 1, 0, 5));
		editLables.add(stepsLabel);
		editLables.setPreferredSize(new Dimension(100, 88));
		JPanel editPanel = new JPanel();
		editPanel.add(editLables);
		editPanel.add(edit);
		editPanel.setBorder(new EmptyBorder(10, 20, 0, 20));
		JPanel cont = new JPanel(new BorderLayout());
		title = new TitledBorder("Simulation seed variation");
		cont.add(editPanel, BorderLayout.CENTER);
		scroll = new JScrollPane(cont);
		scroll.setBorder(title);
		description = new JTextArea(DESCRIPTION);
		description.setOpaque(false);
		description.setEditable(false);
		description.setLineWrap(true);
		description.setWrapStyleWord(true);
		descrPane = new JScrollPane(description);
		descriptionTitle = new TitledBorder(new EtchedBorder(), "Description");
		descrPane.setBorder(descriptionTitle);
		descrPane.setMinimumSize(new Dimension(80, 0));
		scroll.setMinimumSize(new Dimension(360, 0));
		setLeftComponent(scroll);
		setRightComponent(descrPane);
		setListeners();
		this.setBorder(new EmptyBorder(5, 0, 5, 0));
	}

	@Override
	public void setEnabled(boolean enabled) {
		stepsLabel.setEnabled(enabled);
		steps.setEnabled(enabled);
		description.setEnabled(enabled);
		if (!enabled) {
			scroll.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER);
			descrPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER);
		} else {
			scroll.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
			descrPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
		}
		if (!enabled) {
			title.setTitleColor(Color.LIGHT_GRAY);
			descriptionTitle.setTitleColor(Color.LIGHT_GRAY);
		} else {
			title.setTitleColor(DEFAULT_TITLE_COLOR);
			descriptionTitle.setTitleColor(DEFAULT_TITLE_COLOR);
		}
	}

	private void setListeners() {
		steps.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				if (steps.getValue() instanceof Integer) {
					Integer sValue = (Integer) steps.getValue();
					SPA.setNumberOfSteps(sValue.intValue());
				}
				steps.setValue(new Integer(SPA.getNumberOfSteps()));
			}
		});
	}
}
