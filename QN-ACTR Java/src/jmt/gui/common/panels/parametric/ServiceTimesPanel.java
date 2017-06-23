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
import jmt.gui.common.definitions.parametric.ParametricAnalysis;
import jmt.gui.common.definitions.parametric.ParametricAnalysisChecker;
import jmt.gui.common.definitions.parametric.ServiceTimesParametricAnalysis;

/**
 * <p>Title: ServiceTimesPanel</p>
 * <p>Description: this is the panel for he parametric analysis where
 * the service time inside a station is modified.</p>
 *
 * @author Francesco D'Aquino
 *         Date: 9-mar-2006
 *         Time: 16.19.35
 */
public class ServiceTimesPanel extends ParameterOptionPanel {
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
	private JLabel stationChooserLabel;
	private JComboBox stationChooser;
	private JScrollPane scroll;
	private JTextArea description;
	private JScrollPane descrPane;
	private TitledBorder descriptionTitle;
	private ParametricAnalysisChecker checker;
	private String DESCRIPTION_SINGLE;

	private ServiceTimesParametricAnalysis STPA;

	public ServiceTimesPanel(ServiceTimesParametricAnalysis stpa, ClassDefinition classDef, StationDefinition stationDef, SimulationDefinition simDef) {
		super();
		STPA = stpa;
		super.setOrientation(JSplitPane.HORIZONTAL_SPLIT);
		super.setDividerSize(3);
		DESCRIPTION = "Repeat the simulation with different service times of a station for" + " the jobs of all the classes.\n\n"
				+ "The 'To' value represents the proportion of the final service time with" + " respect to the initial value.\n\n"
				+ "The 'all class proportionally' option"
				+ " will not be enabled for those stations where at least one of the two following conditions is true:\n"
				+ " - there is at least one class with a service time distribution with infinite or null mean value;\n"
				+ " - there is at least one load dependent class";

		DESCRIPTION_SINGLE = "Repeat the simulation changing the service time of a station for" + " one class only.\n\n"
				+ "The 'To' value represents the final mean value of service time distribution.";

		cd = classDef;
		sd = stationDef;
		simd = simDef;
		checker = new ParametricAnalysisChecker(cd, sd, simd);
		initialize();
	}

	public void initialize() {
		JPanel radioButtonsPanel = new JPanel(new GridLayout(2, 1));
		allClass = new JRadioButton("Change service time of all classes");
		allClass.setToolTipText("Change service time of all classes");
		singleClass = new JRadioButton("Change service time of one class");
		singleClass.setToolTipText("Change only the service time of one class");
		ButtonGroup group = new ButtonGroup();
		group.add(allClass);
		group.add(singleClass);
		allClass.setSelected(true);
		radioButtonsPanel.add(allClass);
		radioButtonsPanel.add(singleClass);
		radioButtonsPanel.setBorder(new EmptyBorder(5, 20, 0, 20));
		JPanel edit = new JPanel(new GridLayout(5, 1, 0, 5));
		ParametricAnalysisChecker checker = new ParametricAnalysisChecker(cd, sd, simd);
		Vector<Object> avaibleS = checker.checkForServiceTimesParametricAnalysisAvaibleStations();
		String[] stationNames = new String[avaibleS.size()];
		for (int i = 0; i < avaibleS.size(); i++) {
			stationNames[i] = sd.getStationName(avaibleS.get(i));
		}
		classChooserLabel = new JLabel("Class:");
		classChooser = new JComboBox();
		classChooser.setToolTipText("Choose the class whose service time mean value inside the selected station will change");
		stationChooser = new JComboBox(stationNames);
		stationChooser.setToolTipText("Choose the station whose service time mean value will be change");
		description = new JTextArea();
		description.setOpaque(false);
		description.setEditable(false);
		description.setLineWrap(true);
		description.setWrapStyleWord(true);
		descrPane = new JScrollPane(description);
		descriptionTitle = new TitledBorder(new EtchedBorder(), "Description");
		descrPane.setBorder(descriptionTitle);
		descrPane.setMinimumSize(new Dimension(80, 0));
		//if the reference station of the STPA is still avaible
		if (avaibleS.contains(STPA.getReferenceStation())) {
			stationChooser.setSelectedItem(sd.getStationName(STPA.getReferenceStation()));
		}
		//else select another one
		else {
			STPA.setReferenceStation(avaibleS.get(0));
			stationChooser.setSelectedIndex(0);
		}
		if (STPA.isSingleClass()) {
			fromLabel = new JLabel("From (s):");
			from = new JTextField();
			from.setEnabled(false);
			from.setText(Double.toString(STPA.getInitialValue()));
			toLabel = new JLabel("To (s):");
			to = new JSpinner(new SpinnerNumberModel(STPA.getFinalValue(), 0.001, Double.MAX_VALUE, 0.001));
			to.setToolTipText("Sets the final service time mean value expressed in seconds");
			stepsLabel = new JLabel("Steps (n. of exec.): ");
			steps = new JSpinner(new SpinnerNumberModel(STPA.getNumberOfSteps(), 2, ParametricAnalysis.MAX_STEP_NUMBER, 1));
			steps.setToolTipText("Sets the number of steps to be performed");
			//get the vector containing the keys of the class that can be used to do this type of parametric analysis
			Vector<Object> validC = checker.checkForServiceTimesParametricSimulationAvaibleClasses(STPA.getReferenceStation());
			String[] classeNames = new String[validC.size()];
			for (int i = 0; i < validC.size(); i++) {
				classeNames[i] = cd.getClassName(validC.get(i));
			}
			classChooser.removeAllItems();
			for (String classeName : classeNames) {
				classChooser.addItem(classeName);
			}
			classChooser.setEnabled(true);
			classChooser.setSelectedItem(cd.getClassName(STPA.getReferenceClass()));
			singleClass.setSelected(true);
			if ((validC.size() < cd.getClassKeys().size()) || (cd.getClosedClassKeys().size() == 1)) {
				allClass.setEnabled(false);
			}
			allClass.setSelected(false);
			description.setText(DESCRIPTION_SINGLE);
		} else {
			fromLabel = new JLabel("From (%):");
			from = new JTextField();
			from.setEnabled(false);
			from.setText(Double.toString(STPA.getInitialValue()));
			toLabel = new JLabel("To (%):");
			to = new JSpinner(new SpinnerNumberModel(STPA.getFinalValue(), 0.1, Double.MAX_VALUE, 0.1));
			to.setToolTipText("Sets the final proportion of service time with respect to the initial");
			stepsLabel = new JLabel("Steps (n. of exec.): ");
			steps = new JSpinner(new SpinnerNumberModel(STPA.getNumberOfSteps(), 2, ParametricAnalysis.MAX_STEP_NUMBER, 1));
			steps.setToolTipText("Sets the number of steps to be performed");
			classChooser.setEnabled(false);
			singleClass.setSelected(false);
			allClass.setSelected(true);
			description.setText(DESCRIPTION);
			classChooser.addItem("All classes");
		}
		from.setBackground(Color.WHITE);
		stationChooserLabel = new JLabel("Station:");
		edit.add(fromLabel);
		edit.add(from);
		edit.add(toLabel);
		edit.add(to);
		edit.add(stepsLabel);
		edit.add(steps);
		edit.add(classChooserLabel);
		edit.add(stationChooser);
		edit.add(classChooser);
		edit.setPreferredSize(new Dimension(130, 108));
		JPanel editLables = new JPanel(new GridLayout(5, 1, 0, 5));
		editLables.add(fromLabel);
		editLables.add(toLabel);
		editLables.add(stepsLabel);
		editLables.add(stationChooserLabel);
		editLables.add(classChooserLabel);
		editLables.setPreferredSize(new Dimension(100, 108));
		JPanel editPanel = new JPanel();
		editPanel.add(editLables);
		editPanel.add(edit);
		editPanel.setBorder(new EmptyBorder(10, 20, 0, 20));
		JPanel cont = new JPanel(new BorderLayout());
		cont.add(radioButtonsPanel, BorderLayout.NORTH);
		cont.add(editPanel, BorderLayout.CENTER);
		scroll = new JScrollPane(cont);
		title = new TitledBorder("Type of service time growth");
		scroll.setBorder(title);
		//description = new JTextArea(DESCRIPTION);
		scroll.setMinimumSize(new Dimension(360, 0));
		setLeftComponent(scroll);
		setRightComponent(descrPane);
		this.setBorder(new EmptyBorder(5, 0, 5, 0));
		setListeners();
	}

	@Override
	public void setEnabled(boolean enabled) {
		if (enabled) {
			//if the panel is enabled the allClass radioButton can be enabled only if all of the class have a service
			//time distribution with a mean value, in the reference station contained in STPA.
			Vector<Object> validC = checker.checkForServiceTimesParametricSimulationAvaibleClasses(STPA.getReferenceStation());
			if ((validC.size() < cd.getClassKeys().size()) || (cd.getClassKeys().size() == 1)) {
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
		stationChooserLabel.setEnabled(enabled);
		stationChooser.setEnabled(enabled);
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
					STPA.setFinalValue(((Double) tValue).doubleValue());
				} else if (tValue instanceof Integer) {
					STPA.setFinalValue(((Integer) tValue).doubleValue());
				}
				to.setValue(new Double(STPA.getFinalValue()));
			}
		});
		steps.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				if (steps.getValue() instanceof Integer) {
					Integer sValue = (Integer) steps.getValue();
					STPA.setNumberOfSteps(sValue.intValue());
				}
				steps.setValue(new Integer(STPA.getNumberOfSteps()));
			}
		});
		allClass.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED) {
					STPA.setSingleClass(false);
					fromLabel.setText("From (%):");
					toLabel.setText("To (%): ");
					STPA.setDefaultInitialValue();
					STPA.setDefaultFinalValue();
					from.setText(Double.toString(STPA.getInitialValue()));
					to.setModel(new SpinnerNumberModel(STPA.getFinalValue(), 0.1, Double.MAX_VALUE, 0.1));
					to.setValue(new Double(STPA.getFinalValue()));
					to.setToolTipText("Sets the final proportion of service time with respect to the initial");
					classChooser.removeAllItems();
					classChooser.addItem("All classes");
					classChooser.setEnabled(false);
					description.setText(DESCRIPTION);
				}
			}
		});
		singleClass.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED) {
					STPA.setSingleClass(true);
					//get the vector containing the keys of the valid classes. A class is valid if its service
					//time distribution is load independent (inside the reference station of the STPA) and has a mean value.
					Vector<Object> validC = checker.checkForServiceTimesParametricSimulationAvaibleClasses(STPA.getReferenceStation());
					String[] classNames = new String[validC.size()];
					for (int i = 0; i < validC.size(); i++) {
						classNames[i] = cd.getClassName(validC.get(i));
					}
					classChooser.setEnabled(true);
					ItemListener listener = classChooser.getItemListeners()[0];
					classChooser.removeItemListener(listener);
					classChooser.removeAllItems();
					for (String className : classNames) {
						classChooser.addItem(className);
					}
					classChooser.addItemListener(listener);
					//if no classes where previously associated, associate
					//the first one
					if (!validC.contains(STPA.getReferenceClass())) {
						STPA.setReferenceClass(validC.get(0));
					}
					classChooser.setSelectedItem(cd.getClassName(STPA.getReferenceClass()));
					fromLabel.setText("From (s):");
					toLabel.setText("To (s):");
					STPA.setDefaultInitialValue();
					STPA.setDefaultFinalValue();
					from.setText(Double.toString(STPA.getInitialValue()));
					to.setModel(new SpinnerNumberModel(STPA.getFinalValue(), 0.001, Double.MAX_VALUE, 0.001));
					to.setValue(new Double(STPA.getFinalValue()));
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
				STPA.setReferenceClass(classKey);
				STPA.setDefaultInitialValue();
				STPA.setDefaultFinalValue();
				from.setText(Double.toString(STPA.getInitialValue()));
				to.setValue(new Double(STPA.getFinalValue()));
			}
		});
		stationChooser.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				String stationName = (String) stationChooser.getSelectedItem();
				STPA.setReferenceStation(getStationKey(stationName));
				//get the vector containing the keys of the valid classes. A class is valid if its service
				//time distribution is load independent (inside the reference station of the STPA) and has a mean value.
				Vector<Object> validC = checker.checkForServiceTimesParametricSimulationAvaibleClasses(STPA.getReferenceStation());
				if (STPA.isSingleClass()) {
					String[] classes = new String[validC.size()];
					for (int i = 0; i < validC.size(); i++) {
						classes[i] = cd.getClassName(validC.get(i));
					}
					//if the reference station of the STPA is no more valid get the first avaible
					if (!validC.contains(STPA.getReferenceClass())) {
						STPA.setReferenceClass(validC.get(0));
					}
					STPA.setDefaultInitialValue();
					STPA.setDefaultFinalValue();
					from.setText(Double.toString(STPA.getInitialValue()));
					to.setValue(new Double(STPA.getFinalValue()));
					ItemListener listener = classChooser.getItemListeners()[0];
					classChooser.removeItemListener(listener);
					classChooser.removeAllItems();
					for (String classe : classes) {
						classChooser.addItem(classe);
					}
					classChooser.setEnabled(true);
					classChooser.setSelectedItem(cd.getClassName(STPA.getReferenceClass()));
					classChooser.addItemListener(listener);
					singleClass.setSelected(true);
					allClass.setSelected(false);
					//if the number of avaible classes is less than the total number of classes the all class service
					//time parametric analysis is no more avaible.
					if (validC.size() < cd.getClassKeys().size()) {
						allClass.setEnabled(false);
					} else {
						allClass.setEnabled(true);
					}
				} else {
					//if the number of avaible classes is less than the total number of classes the all class service
					//time parametric analysis is no more avaible...
					if (validC.size() < cd.getClassKeys().size()) {
						STPA.setSingleClass(true);
						String[] classes = new String[validC.size()];
						for (int i = 0; i < validC.size(); i++) {
							classes[i] = cd.getClassName(validC.get(i));
						}
						//set the reference class to the STPA...
						STPA.setReferenceClass(validC.get(0));
						STPA.setDefaultInitialValue();
						STPA.setDefaultFinalValue();
						from.setText(Double.toString(STPA.getInitialValue()));
						to.setModel(new SpinnerNumberModel(STPA.getFinalValue(), 0.001, Double.MAX_VALUE, 0.001));
						to.setValue(new Double(STPA.getFinalValue()));
						//... and set the STPA to be single class
						singleClass.setSelected(true);
						allClass.setEnabled(false);
						description.setText(DESCRIPTION_SINGLE);
					} else {
						allClass.setEnabled(true);
						classChooser.setEnabled(false);
						singleClass.setSelected(false);
						allClass.setSelected(true);
					}
				}
			}
		});
	}
}
