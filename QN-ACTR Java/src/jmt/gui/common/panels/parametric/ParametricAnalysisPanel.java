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
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;

import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.definitions.GuiInterface;
import jmt.gui.common.definitions.SimulationDefinition;
import jmt.gui.common.definitions.StationDefinition;
import jmt.gui.common.definitions.parametric.ArrivalRateParametricAnalysis;
import jmt.gui.common.definitions.parametric.NumberOfCustomerParametricAnalysis;
import jmt.gui.common.definitions.parametric.ParametricAnalysisChecker;
import jmt.gui.common.definitions.parametric.ParametricAnalysisDefinition;
import jmt.gui.common.definitions.parametric.ParametricAnalysisModelFactory;
import jmt.gui.common.definitions.parametric.PopulationMixParametricAnalysis;
import jmt.gui.common.definitions.parametric.SeedParametricAnalysis;
import jmt.gui.common.definitions.parametric.ServiceTimesParametricAnalysis;

/**
 * <p>Title: ParametricAnalysisPanel</p>
 * <p>Description: with this panel user can select the type of parametric analysis
 * . This panel contains a <code>ParameterOptionPanel</code> that changes each time
 * user selects a different parametric analysis type.</p>
 *
 * @author Francesco D'Aquino
 *         Date: 7-mar-2006
 *         Time: 13.12.42
 */

public class ParametricAnalysisPanel extends WizardPanel implements CommonConstants {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	Color DEFAULT_TITLE_COLOR = new TitledBorder("").getTitleColor();
	private String[] parameters = { "                   " };
	private ParameterOptionPanel parameterOptionPanel;
	private JCheckBox enabler;
	private JPanel upperPanel;
	private JPanel chooserPanel;
	private JPanel enablerPanel;
	private JComboBox chooser;
	private TitledBorder tb;
	ClassDefinition cd;
	StationDefinition sd;
	SimulationDefinition simd;
	GuiInterface gui;

	public ParametricAnalysisPanel(ClassDefinition cd, StationDefinition sd, SimulationDefinition simd, GuiInterface gui) {
		this.cd = cd;
		this.sd = sd;
		this.simd = simd;
		this.gui = gui;
		initGui();
		setListeners();
	}

	public void initGui() {
		enabler = new JCheckBox("Enable what-if analysis");
		enabler.setToolTipText("Enable or disable what-if analysis");
		enablerPanel = new JPanel(new BorderLayout());
		enablerPanel.add(enabler, BorderLayout.WEST);
		//enablerPanel.setBorder(new EmptyBorder(30,10,10,10));
		upperPanel = new JPanel(new BorderLayout());
		chooserPanel = new JPanel();
		tb = new TitledBorder("Parameter selection for the control of repeated executions");
		chooserPanel.setBorder(tb);
		chooser = new JComboBox(parameters);
		chooser.setPreferredSize(DIM_BUTTON_L);
		chooser.setToolTipText("Choose the what-if analysis to be performed");
		chooserPanel.add(chooser, BorderLayout.NORTH);
		JLabel description = new JLabel(PARAMETRIC_ANALYSIS_DESCRIPTION);
		JPanel northPanel = new JPanel(new BorderLayout());
		northPanel.setBorder(new EmptyBorder(0, 0, 15, 0));
		northPanel.add(description, "Center");
		northPanel.add(enablerPanel, "East");
		upperPanel.add(northPanel, BorderLayout.NORTH);
		upperPanel.add(chooserPanel, BorderLayout.SOUTH);
		this.setLayout(new BorderLayout());
		this.setBorder(new EmptyBorder(20, 20, 20, 20));
		chooser.removeAllItems();
		ParametricAnalysisChecker pac = new ParametricAnalysisChecker(cd, sd, simd);
		if (!pac.canBeEnabled()) {
			enabler.setEnabled(false);
			parameterOptionPanel = createPanel(null);
			simd.setParametricAnalysisModel(null);
			simd.setParametricAnalysisEnabled(false);
		} else {
			enabler.setEnabled(true);
			enabler.setSelected(simd.isParametricAnalysisEnabled());
			ParametricAnalysisDefinition pad = simd.getParametricAnalysisModel();
			parameters = pac.getRunnableParametricAnalysis();
			for (String parameter : parameters) {
				chooser.addItem(parameter);
			}
			String temp = parameters[0];
			if (pad == null) {
				pad = ParametricAnalysisModelFactory.createParametricAnalysisModel(temp, cd, sd, simd);
				simd.setParametricAnalysisModel(pad);
			} else {
				int code = pad.checkCorrectness(true); //check correctness of the PAD and if it is possible corrects it
				if (code != 2) {
					chooser.setSelectedItem(pad.getType());
				} else { //the old type of parametric analysis is no more avaible
					pad = ParametricAnalysisModelFactory.createParametricAnalysisModel(temp, cd, sd, simd);
					simd.setParametricAnalysisModel(pad);
				}
			}
			parameterOptionPanel = createPanel(pad);
		}
		parameterOptionPanel.setBorder(new EmptyBorder(10, 0, 0, 0));
		this.add(upperPanel, BorderLayout.NORTH);
		this.add(parameterOptionPanel, BorderLayout.CENTER);
		this.setEnabled(enabler.isSelected());
	}

	@Override
	public void setEnabled(boolean enabled) {
		if (!enabled) {
			chooser.setEnabled(false);
			enablerPanel.setEnabled(false);
			chooserPanel.setEnabled(false);
			upperPanel.setEnabled(false);
			parameterOptionPanel.setEnabled(false);
			tb.setTitleColor(Color.LIGHT_GRAY);
			parameterOptionPanel.repaint();
		} else {
			chooser.setEnabled(true);
			enablerPanel.setEnabled(true);
			chooserPanel.setEnabled(true);
			upperPanel.setEnabled(true);
			parameterOptionPanel.setEnabled(true);
			tb.setTitleColor(DEFAULT_TITLE_COLOR);
			parameterOptionPanel.repaint();
		}
	}

	/**
	 * Sets the listeners to enabler and chooser
	 */
	private void setListeners() {
		enabler.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED) {
					setEnabled(true);
					simd.setParametricAnalysisEnabled(true);
				} else {
					setEnabled(false);
					simd.setParametricAnalysisEnabled(false);
				}
			}
		});
		chooser.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				String param = (String) chooser.getSelectedItem();
				if (parameterOptionPanel != null) {
					remove(parameterOptionPanel);
					ParametricAnalysisDefinition temp = ParametricAnalysisModelFactory.createParametricAnalysisModel(param, cd, sd, simd);
					simd.setParametricAnalysisModel(temp);
					simd.setSaveChanged();
					parameterOptionPanel = createPanel(temp);
					add(parameterOptionPanel, BorderLayout.CENTER);
					doLayout();
					parameterOptionPanel.validate();
				}
			}
		});
	}

	/**
	 * Creates the choosen parameter option panel
	 * @param pad the instance of <code>ParameterAnalysisDefinition</code> model.
	 * @return  the <code>ParameterOptionPanel</code> corresponding to the <code>ParameterAnalysisDefinition</code>
	 * passed as parameter
	 */
	protected ParameterOptionPanel createPanel(ParametricAnalysisDefinition pad) {
		ParameterOptionPanel pop;
		if (pad == null) {
			pop = new EmptyPanel();
		} else {
			if (pad instanceof NumberOfCustomerParametricAnalysis) {
				pop = new NumberOfCustomersPanel((NumberOfCustomerParametricAnalysis) pad, cd, sd, simd, gui);
			} else if (pad instanceof PopulationMixParametricAnalysis) {
				pop = new PopulationMixPanel((PopulationMixParametricAnalysis) pad, cd, sd, simd);
			} else if (pad instanceof ServiceTimesParametricAnalysis) {
				pop = new ServiceTimesPanel((ServiceTimesParametricAnalysis) pad, cd, sd, simd);
			} else if (pad instanceof ArrivalRateParametricAnalysis) {
				pop = new ArrivalRatesPanel((ArrivalRateParametricAnalysis) pad, cd, sd, simd);
			} else if (pad instanceof SeedParametricAnalysis) {
				pop = new SeedPanel((SeedParametricAnalysis) pad, cd, sd, simd);
			} else {
				pop = null;
			}
		}
		return pop;
	}

	/**
	 * @return the panel's name
	 */
	@Override
	public String getName() {
		return "What-if analysis";
	}

	public void setData(ClassDefinition cd, StationDefinition sd, SimulationDefinition simd) {
		this.cd = cd;
		this.sd = sd;
		this.simd = simd;
		this.removeAll();
		this.initGui();
		this.setListeners();
		this.doLayout();
		this.validate();
		this.repaint();
	}

	/*
	private void checkIfCanBeEnabled() {
	    ParametricAnalysisChecker pac = new ParametricAnalysisChecker(cd,sd,simd);
	    if (!pac.canBeEnabled()) {
	        enabler.setEnabled(false);
	        parameterOptionPanel = createPanel(null);
	        simd.setParametricAnalysisModel(null);
	        simd.setParametricAnalysisEnabled(false);
	    }
	    else {
	        enabler.setEnabled(true);
	        enabler.setSelected(simd.isParametricAnalysisEnabled());
	        ParametricAnalysisDefinition pad = simd.getParametricAnalysisModel();
	        ItemListener i = chooser.getItemListeners()[0];
	        chooser.removeItemListener(i);
	        chooser.removeAllItems();
	        parameters = pac.getRunnableParametricAnalysis();
	        for (int k=0;k<parameters.length;k++) chooser.addItem(parameters[k]);
	        chooser.addItemListener(i);
	        String temp = parameters[0];
	        if (pad == null) {
	            pad = ParametricAnalysisModelFactory.createParametricAnalysisModel(temp,cd,sd,simd);
	            simd.setParametricAnalysisModel(pad);
	        }
	        /*else {
	            int code = pad.checkCorrectness(true);  //check correctness of the PAD and if it is possible corrects it
	            if (code != 2) chooser.setSelectedItem(pad.getType());
	            else {    //the old type of parametric analysis is no more avaible
	                pad = ParametricAnalysisModelFactory.createParametricAnalysisModel(temp,cd,sd,simd);
	                simd.setParametricAnalysisModel(pad);
	            }
	        }
	        remove(parameterOptionPanel);
	        parameterOptionPanel = createPanel(pad);
	        //simd.setParametricAnalysisModel(temp);
	        //parameterOptionPanel = createPanel(temp);
	        add(parameterOptionPanel,BorderLayout.CENTER);
	        doLayout();
	        parameterOptionPanel.validate();
	    }
	}*/

	/**
	 * Called in JSIM when the What-if analysis panel is selected
	 */
	@Override
	public void gotFocus() {
		this.removeAll();
		initGui();
		setListeners();
	}

}
