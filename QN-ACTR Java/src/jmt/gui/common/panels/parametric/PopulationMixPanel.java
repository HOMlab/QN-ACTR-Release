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
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.Vector;

import javax.swing.JComboBox;
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
import jmt.gui.common.definitions.parametric.PopulationMixParametricAnalysis;

/**
 * <p>Title: PopulationMixPanel</p>
 * <p>Description: this is the panel for the parametric analysis where
 * the proportion between two closed classes is modified while global
 * number of jobs is kept constant.</p>
 *
 * @author Francesco D'Aquino
 *         Date: 9-mar-2006
 *         Time: 16.18.40
 */
public class PopulationMixPanel extends ParameterOptionPanel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private JLabel fromLabel;
	private JSpinner from;
	private JLabel toLabel;
	private JSpinner to;
	private JLabel stepsLabel;
	private JSpinner steps;
	private JLabel classChooserLabel;
	private JComboBox classChooser;
	private JScrollPane scroll;
	private JTextArea description;
	private JScrollPane descrPane;
	private TitledBorder descriptionTitle;
	private PopulationMixParametricAnalysis PMPA;

	public PopulationMixPanel(PopulationMixParametricAnalysis pmpa, ClassDefinition classDef, StationDefinition stationDef,
			SimulationDefinition simDef) {
		super();
		PMPA = pmpa;
		super.setOrientation(JSplitPane.HORIZONTAL_SPLIT);
		super.setDividerSize(3);
		DESCRIPTION = "This type of analysis is available for closed models with two classes only "
				+ "(and possibly other open classes) and it applies only to the closed classes.\n\n"
				+ "Repeat the simulation changing the proportion of jobs "
				+ "between the two closed classes, keeping constant the total number of jobs.\n\n"
				+ "The 'From' and 'To' values represent the initial and final values of " + "population mix (ßi = Ni / N) for the chosen class.\n\n"
				+ "Since only integer values are allowed " + "the number of steps that can be practically executed may be very small.";

		sd = stationDef;
		cd = classDef;
		simd = simDef;
		initialize();
	}

	public void initialize() {
		JPanel edit = new JPanel(new GridLayout(4, 1, 0, 5));
		fromLabel = new JLabel("Initial ß: ");
		from = new JSpinner(new SpinnerNumberModel(PMPA.getInitialValue(), 0.000, 1.000, 0.001));
		from.setToolTipText("Sets the initial proportion of jobs");
		toLabel = new JLabel("Final ß: ");
		to = new JSpinner(new SpinnerNumberModel(PMPA.getFinalValue(), 0.000, 1.000, 0.001));
		to.setToolTipText("Sets the final proportion of jobs");
		stepsLabel = new JLabel("Steps (n. of exec.): ");
		int maxSteps = PMPA.searchForAvaibleSteps();
		if (maxSteps > ParametricAnalysis.MAX_STEP_NUMBER) {
			maxSteps = ParametricAnalysis.MAX_STEP_NUMBER;
		}
		steps = new JSpinner(new SpinnerNumberModel(PMPA.getNumberOfSteps(), 2, maxSteps, 1));
		steps.setToolTipText("Sets the number of steps to be performed");
		Vector classes = cd.getClosedClassKeys();
		String[] classNames = new String[classes.size()];
		for (int i = 0; i < classes.size(); i++) {
			classNames[i] = cd.getClassName(classes.get(i));
		}
		classChooserLabel = new JLabel("Class: ");
		classChooser = new JComboBox(classNames);
		classChooser.setToolTipText("Sets the class the inserted values will refer to");
		classChooser.setSelectedItem(cd.getClassName(PMPA.getReferenceClass()));
		edit.add(fromLabel);
		edit.add(from);
		edit.add(toLabel);
		edit.add(to);
		edit.add(stepsLabel);
		edit.add(steps);
		edit.add(classChooserLabel);
		edit.add(classChooser);
		edit.setPreferredSize(new Dimension(130, 88));
		JPanel editLables = new JPanel(new GridLayout(4, 1, 0, 5));
		editLables.add(fromLabel);
		editLables.add(toLabel);
		editLables.add(stepsLabel);
		editLables.add(classChooserLabel);
		editLables.setPreferredSize(new Dimension(100, 88));
		JPanel editPanel = new JPanel();
		editPanel.add(editLables);
		editPanel.add(edit);
		editPanel.setBorder(new EmptyBorder(15, 20, 0, 20));
		JPanel cont = new JPanel(new BorderLayout());
		cont.add(editPanel, BorderLayout.CENTER);
		scroll = new JScrollPane(cont);
		title = new TitledBorder("Type of population mix");
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
		fromLabel.setEnabled(enabled);
		from.setEnabled(enabled);
		toLabel.setEnabled(enabled);
		to.setEnabled(enabled);
		stepsLabel.setEnabled(enabled);
		steps.setEnabled(enabled);
		classChooserLabel.setEnabled(enabled);
		classChooser.setEnabled(enabled);
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
		from.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				if (from.getValue() instanceof Double) {
					Double fValue = (Double) from.getValue();
					Double tValue = (Double) to.getValue();
					SpinnerNumberModel snm = (SpinnerNumberModel) steps.getModel();
					double oldValue = PMPA.getInitialValue();
					PMPA.setInitialValue(fValue.doubleValue());
					int newMaxSteps = PMPA.searchForAvaibleSteps();
					if (newMaxSteps > ParametricAnalysis.MAX_STEP_NUMBER) {
						newMaxSteps = ParametricAnalysis.MAX_STEP_NUMBER;
					}
					if ((fValue.doubleValue() != tValue.doubleValue()) && (newMaxSteps >= ((Integer) snm.getMinimum()).intValue())) {
						int oldSteps = ((Integer) snm.getValue()).intValue();
						snm.setMaximum(new Integer(newMaxSteps));
						if (newMaxSteps < oldSteps) {
							PMPA.setNumberOfSteps(newMaxSteps);
							steps.setValue(new Integer(newMaxSteps));
						}
					} else {
						PMPA.setInitialValue(oldValue);
					}
				}
				from.setValue(new Double(PMPA.getInitialValue()));
			}
		});
		to.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				if (to.getValue() instanceof Double) {
					Double fValue = (Double) from.getValue();
					Double tValue = (Double) to.getValue();
					SpinnerNumberModel snm = (SpinnerNumberModel) steps.getModel();
					double oldValue = PMPA.getFinalValue();
					PMPA.setFinalValue(tValue.doubleValue());
					int newMaxSteps = PMPA.searchForAvaibleSteps();
					if (newMaxSteps > ParametricAnalysis.MAX_STEP_NUMBER) {
						newMaxSteps = ParametricAnalysis.MAX_STEP_NUMBER;
					}
					if ((fValue.doubleValue() != tValue.doubleValue()) && (newMaxSteps >= ((Integer) snm.getMinimum()).intValue())) {
						int oldSteps = ((Integer) snm.getValue()).intValue();
						snm.setMaximum(new Integer(newMaxSteps));
						if (newMaxSteps < oldSteps) {
							PMPA.setNumberOfSteps(newMaxSteps);
							steps.setValue(new Integer(newMaxSteps));
						}
					} else {
						PMPA.setFinalValue(oldValue);
					}
				}
				to.setValue(new Double(PMPA.getFinalValue()));
			}
		});
		steps.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				if (steps.getValue() instanceof Integer) {
					int sValue = ((Integer) steps.getValue()).intValue();
					int sMax = ((Integer) ((SpinnerNumberModel) steps.getModel()).getMaximum()).intValue();
					int sMin = ((Integer) ((SpinnerNumberModel) steps.getModel()).getMinimum()).intValue();
					if ((sValue >= sMin) && (sValue <= sMax)) {
						PMPA.setNumberOfSteps(sValue);
					}
				}
				steps.setValue(new Integer(PMPA.getNumberOfSteps()));
			}
		});
		classChooser.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				String className = (String) classChooser.getSelectedItem();
				Object classKey = null;
				Vector classes = cd.getClassKeys();
				//for cycle used to get the key of the selected class
				for (int i = 0; i < classes.size(); i++) {
					if (cd.getClassName(classes.get(i)).equals(className)) {
						classKey = classes.get(i);
						break;
					}
				}
				PMPA.setReferenceClass(classKey);
				PMPA.setDefaultInitialValue();
				from.setValue(new Double(PMPA.getInitialValue()));
				//SpinnerNumberModel snm = (SpinnerNumberModel) steps.getModel();
				//int oldMax = ((Integer)snm.getMaximum()).intValue();
				//int currentSteps = ((Integer)snm.getValue()).intValue();
				//int newMaximumSteps = PMPA.searchForAvaibleSteps();
				//if (newMaximumSteps > ParametricAnalysis.MAX_STEP_NUMBER) newMaximumSteps = ParametricAnalysis.MAX_STEP_NUMBER;
				//if (newMaximumSteps < oldMax) steps.setModel(new SpinnerNumberModel(newMaximumSteps,2,newMaximumSteps,0.001));
				//else steps.setModel(new SpinnerNumberModel(currentSteps,2,newMaximumSteps,0.001));
			}
		});
	}
}
