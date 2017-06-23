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

import javax.swing.ButtonGroup;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
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
import jmt.gui.common.definitions.parametric.ArrivalRateParametricAnalysis;
import jmt.gui.common.definitions.parametric.ParametricAnalysis;
import jmt.gui.common.definitions.parametric.ParametricAnalysisChecker;

/**
 * <p>Title: ArrivalRatesPanel</p>
 * <p>Description: this is the panel for the parametric analysis where the arrival
 *  rate for open classes is modified.</p>
 *
 * @author Francesco D'Aquino
 *         Date: 9-mar-2006
 *         Time: 16.08.09
 */
public class ArrivalRatesPanel extends ParameterOptionPanel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private JRadioButton allClass;
	private JRadioButton singleClass;
	private JLabel fromLabel;
	private JTextField from;
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
	private ParametricAnalysisChecker checker;
	private String DESCRIPTION_SINGLE;
	private ArrivalRateParametricAnalysis ARPA;

	public ArrivalRatesPanel(ArrivalRateParametricAnalysis arpa, ClassDefinition classDef, StationDefinition stationDef, SimulationDefinition simDef) {
		super();
		ARPA = arpa;
		super.setOrientation(JSplitPane.HORIZONTAL_SPLIT);
		super.setDividerSize(3);

		DESCRIPTION = "Repeat the simulation with different arrival rate for all open " + "classes.\n\n"
				+ "The 'To' value represents the percentage of the final arrival rate with" + " respect to the initial value.\n\n"
				+ "This option will not be available if there is at least one"
				+ " open class with an interarrival time distribution with infinite or null mean value.\n\n";

		DESCRIPTION_SINGLE = "Repeat the simulation with different arrival rates for an open "
				+ "classes, provided that the interarrival time distribution has a finite, not null, mean value. "
				+ "The 'To' value is the final arrival rate.\n\n ";

		cd = classDef;
		sd = stationDef;
		simd = simDef;
		checker = new ParametricAnalysisChecker(cd, sd, simd);
		initialize();
	}

	public void initialize() {
		JPanel radioButtonsPanel = new JPanel(new GridLayout(2, 1));
		allClass = new JRadioButton("Change arrival rates of all open classes");
		allClass.setToolTipText("Change arrival rates of all open classes");
		singleClass = new JRadioButton("Change the arrival rate of one open class");
		singleClass.setToolTipText("Change only the arrival rate of one open class");
		ButtonGroup group = new ButtonGroup();
		group.add(allClass);
		group.add(singleClass);
		radioButtonsPanel.add(allClass);
		radioButtonsPanel.add(singleClass);
		radioButtonsPanel.setBorder(new EmptyBorder(5, 20, 0, 20));
		JPanel edit = new JPanel(new GridLayout(4, 1, 0, 5));
		classChooserLabel = new JLabel("Class: ");
		classChooser = new JComboBox();
		classChooser.setToolTipText("Choose the class whose arrival rate will change");
		if (ARPA.isSingleClass()) {
			fromLabel = new JLabel("From (j/s):");
			from = new JTextField();
			from.setEnabled(false);
			from.setText(Double.toString(ARPA.getInitialValue()));
			toLabel = new JLabel("To (j/s): ");
			to = new JSpinner(new SpinnerNumberModel(ARPA.getFinalValue(), 0.001, Double.MAX_VALUE, 0.001));
			to.setToolTipText("Sets the final arrival rate in job/sec");
			stepsLabel = new JLabel("Steps (n. of exec.): ");
			steps = new JSpinner(new SpinnerNumberModel(ARPA.getNumberOfSteps(), 2, ParametricAnalysis.MAX_STEP_NUMBER, 1));
			steps.setToolTipText("Sets the number of steps to be performed");
			//get the vector containing the keys of the class that can be used to do this type of parametric analysis
			Vector<Object> validC = checker.checkForArrivalRatesParametricSimulationAvaibleClasses();
			String[] classNames = new String[validC.size()];
			for (int i = 0; i < validC.size(); i++) {
				classNames[i] = cd.getClassName(validC.get(i));
			}
			//if there is only one class avaible or if the "all class" simulation is not avaible
			//disable the allClass radio button
			if ((validC.size() < cd.getOpenClassKeys().size()) || (cd.getOpenClassKeys().size() == 1)) {
				allClass.setEnabled(false);
			}
			classChooser.removeAllItems();
			for (String className : classNames) {
				classChooser.addItem(className);
			}
			classChooser.setEnabled(true);
			classChooser.setSelectedItem(cd.getClassName(ARPA.getReferenceClass()));
			singleClass.setSelected(true);
			allClass.setSelected(false);
		} else {
			fromLabel = new JLabel("From (%):");
			from = new JTextField();
			from.setEnabled(false);
			from.setText(Double.toString(ARPA.getInitialValue()));
			toLabel = new JLabel("To (%): ");
			to = new JSpinner(new SpinnerNumberModel(ARPA.getFinalValue(), 0.1, Double.MAX_VALUE, 0.1));
			to.setToolTipText("Sets the final proportion of arrival rate with respect to the initial");
			stepsLabel = new JLabel("Steps (n. of exec.): ");
			steps = new JSpinner(new SpinnerNumberModel(ARPA.getNumberOfSteps(), 2, ParametricAnalysis.MAX_STEP_NUMBER, 1));
			steps.setToolTipText("Sets the number of steps to be performed");
			classChooser.addItem("All open classes");
			classChooser.setEnabled(false);
			singleClass.setSelected(false);
			allClass.setSelected(true);
		}
		from.setBackground(Color.WHITE);
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
		editPanel.setBorder(new EmptyBorder(10, 20, 0, 20));
		JPanel cont = new JPanel(new BorderLayout());
		title = new TitledBorder("Type of arrival rate growth");
		cont.add(radioButtonsPanel, BorderLayout.NORTH);
		cont.add(editPanel, BorderLayout.CENTER);
		scroll = new JScrollPane(cont);
		scroll.setBorder(title);
		description = new JTextArea();
		if (ARPA.isSingleClass()) {
			description.setText(DESCRIPTION_SINGLE);
		} else {
			description.setText(DESCRIPTION);
		}
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
		if (enabled) {
			//if the panel is enabled the allClass radioButton can be enabled only if all of the class have a service
			//time distribution with a mean value, in the reference station contained in STPA.
			Vector<Object> validC = checker.checkForArrivalRatesParametricSimulationAvaibleClasses();
			if ((validC.size() < cd.getOpenClassKeys().size() || (cd.getOpenClassKeys().size() == 1))) {
				allClass.setEnabled(false);
			} else {
				allClass.setEnabled(true);
			}
		} else {
			allClass.setEnabled(false);
		}
		singleClass.setEnabled(enabled);
		fromLabel.setEnabled(enabled);
		from.setEnabled(false);
		toLabel.setEnabled(enabled);
		to.setEnabled(enabled);
		stepsLabel.setEnabled(enabled);
		steps.setEnabled(enabled);
		description.setEnabled(enabled);
		classChooserLabel.setEnabled(enabled);
		if (!enabled) {
			classChooser.setEnabled(enabled);
		} else if (singleClass.isSelected()) {
			classChooser.setEnabled(enabled);
		}
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
		to.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				Object tValue = to.getValue();
				if (tValue instanceof Double) {
					ARPA.setFinalValue(((Double) tValue).doubleValue());
				} else if (tValue instanceof Integer) {
					ARPA.setFinalValue(((Integer) tValue).doubleValue());
				}
				to.setValue(new Double(ARPA.getFinalValue()));
			}
		});
		steps.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				if (steps.getValue() instanceof Integer) {
					Integer sValue = (Integer) steps.getValue();
					ARPA.setNumberOfSteps(sValue.intValue());
				}
				steps.setValue(new Integer(ARPA.getNumberOfSteps()));
			}
		});
		allClass.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED) {
					ARPA.setSingleClass(false);
					fromLabel.setText("From (%):");
					toLabel.setText("To (%): ");
					ARPA.setDefaultInitialValue();
					ARPA.setDefaultFinalValue();
					from.setText(Double.toString(ARPA.getInitialValue()));
					to.setModel(new SpinnerNumberModel(ARPA.getFinalValue(), 0.1, Double.MAX_VALUE, 0.1));
					to.setValue(new Double(ARPA.getFinalValue()));
					to.setToolTipText("Sets the final proportion of arrival rate with respect to the initial");
					classChooser.removeAllItems();
					classChooser.addItem("All open classes");
					classChooser.setEnabled(false);
					description.setText(DESCRIPTION);
				}
			}
		});
		singleClass.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED) {
					ARPA.setSingleClass(true);
					classChooser.setEnabled(true);
					//get the vector containing the keys of the valid classes. A class is valid if its service
					//time distribution is load independent (inside the reference station of the STPA) and has a mean value.
					Vector<Object> validC = checker.checkForArrivalRatesParametricSimulationAvaibleClasses();
					String[] classNames = new String[validC.size()];
					for (int i = 0; i < validC.size(); i++) {
						classNames[i] = cd.getClassName(validC.get(i));
					}
					ItemListener listener = classChooser.getItemListeners()[0];
					classChooser.removeItemListener(listener);
					classChooser.removeAllItems();
					for (String className : classNames) {
						classChooser.addItem(className);
					}
					classChooser.addItemListener(listener);
					//if no classes where previously associated, associate
					//the first one
					if (!validC.contains(ARPA.getReferenceClass())) {
						ARPA.setReferenceClass(validC.get(0));
					}
					classChooser.setSelectedItem(cd.getClassName(ARPA.getReferenceClass()));
					fromLabel.setText("From (j/s):");
					toLabel.setText("To (j/s): ");
					ARPA.setDefaultInitialValue();
					ARPA.setDefaultFinalValue();
					from.setText(Double.toString(ARPA.getInitialValue()));
					to.setModel(new SpinnerNumberModel(ARPA.getFinalValue(), 0.001, Double.MAX_VALUE, 0.001));
					to.setValue(new Double(ARPA.getFinalValue()));
					to.setToolTipText("Sets the final arrival rate in job/sec");
					description.setText(DESCRIPTION_SINGLE);
				}
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
				//Set the default value for initial and final value
				ARPA.setReferenceClass(classKey);
				ARPA.setDefaultInitialValue();
				ARPA.setDefaultFinalValue();
				from.setText(Double.toString(ARPA.getInitialValue()));
				to.setValue(new Double(ARPA.getFinalValue()));
			}
		});
	}
}
