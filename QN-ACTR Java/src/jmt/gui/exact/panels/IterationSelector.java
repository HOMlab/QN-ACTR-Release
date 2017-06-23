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
package jmt.gui.exact.panels;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.text.DecimalFormat;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.JTabbedPane;
import javax.swing.SwingConstants;
import javax.swing.border.EtchedBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.exact.ExactConstants;
import jmt.gui.exact.ExactModel;

/**
 * <p>Title: Iteration Selector Panel</p>
 * <p>Description: This panel is used to select iteration value
 * for what-if analysis. This is designed to hold other ResultsPanel</p>
 *
 * @author Bertoli Marco
 *         Date: 6-giu-2006
 *         Time: 15.52.27
 */
public class IterationSelector extends WizardPanel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	// Vector with all solution panels
	private Vector<SolutionPanel> panels = new Vector<SolutionPanel>();
	// Data structure
	private ExactModel model;
	// Dimension of spinner
	final static Dimension DIM_SPINNER = new Dimension(60, 20);

	private JTabbedPane tabber = new JTabbedPane();
	private JLabel description = new JLabel();

	// Formats numbers
	private DecimalFormat doubleFormatter = new DecimalFormat("#0.0####");
	private DecimalFormat intFormatter = new DecimalFormat("#0");

	/**
	 * Creates a bew Iteration Selector panel, given target ExactModel
	 * @param em reference to data structure
	 */
	public IterationSelector(ExactModel em) {
		model = em;
		initComponents();
	}

	/**
	 * Adds a solutionPanel to be notified when selected iteration changes.
	 * This component is placed in the north of given panel.
	 * @param s solutionPanel to be added
	 */
	public void addSolutionPanel(SolutionPanel s) {
		panels.add(s);
		tabber.add(s);
	}

	/**
	 * Selects iteration i in all given panels
	 * @param i iteration to be selected
	 */
	private void select(int i) {
		Iterator<SolutionPanel> it = panels.iterator();
		while (it.hasNext()) {
			it.next().setIteration(i);
		}
		double value = model.getWhatIfValues()[i];
		// Now adjusts description label
		if (model.getWhatIfType().equals(ExactConstants.WHAT_IF_ARRIVAL)) {
			if (model.getWhatIfClass() < 0) {
				description.setText("Arrival rates \u03bbi of each open class (" + doubleFormatter.format(value * 100) + "% of initial value)");
			} else {
				description.setText("Arrival rate for class " + model.getClassNames()[model.getWhatIfClass()] + " \u03bbi = "
						+ doubleFormatter.format(value) + " [job/s]");
			}
		} else if (model.getWhatIfType().equals(ExactConstants.WHAT_IF_CUSTOMERS)) {
			if (model.getWhatIfClass() < 0) {
				description.setText("Number of customers Ni of each closed class (" + doubleFormatter.format(value * 100) + "% of initial value)");
			} else {
				description.setText("Number of customers for " + model.getClassNames()[model.getWhatIfClass()] + " Ni = "
						+ intFormatter.format(value));
			}
		} else if (model.getWhatIfType().equals(ExactConstants.WHAT_IF_DEMANDS)) {
			if (model.getWhatIfClass() < 0) {
				description.setText("Service demands Di for all classes (" + doubleFormatter.format(value * 100) + "% of initial value) at "
						+ model.getStationNames()[model.getWhatIfStation()]);
			} else {
				description.setText("Service demand for " + model.getClassNames()[model.getWhatIfClass()] + " at "
						+ model.getStationNames()[model.getWhatIfStation()] + " Di = " + doubleFormatter.format(value) + " [s]");
			}
		} else if (model.getWhatIfType().equals(ExactConstants.WHAT_IF_MIX)) {
			description
					.setText("Population mix \u03b2i = " + doubleFormatter.format(value) + " for " + model.getClassNames()[model.getWhatIfClass()]);
		}
	}

	/**
	 * Initialize all gui objects
	 */
	private void initComponents() {
		setLayout(new BorderLayout());
		JPanel topPanel = new JPanel(new GridLayout(2, 1));
		topPanel.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.RAISED));
		JLabel label = new JLabel("Execution number: ");
		final JSpinner spinner = new JSpinner();
		spinner.setPreferredSize(DIM_SPINNER);
		spinner.setValue(new Integer(1));
		label.setLabelFor(spinner);
		// Pack spinner and its label on top, while description is on the second line
		JPanel tmp = new JPanel();
		tmp.add(label);
		tmp.add(spinner);
		topPanel.add(tmp);
		description.setHorizontalAlignment(SwingConstants.CENTER);
		topPanel.add(description);
		// Adds state change to spinner.
		spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				int i = ((Integer) spinner.getValue()).intValue();
				if (i <= 0) {
					i = 1;
				}
				if (i > model.getWhatIfValues().length) {
					i = model.getWhatIfValues().length;
				}
				select(i - 1);
				spinner.setValue(new Integer(i));
			}
		});

		add(topPanel, BorderLayout.NORTH);
		add(tabber, BorderLayout.CENTER);
		select(0);
	}

	/**
	 * @return the panel's name
	 */
	@Override
	public String getName() {
		return "Textual Results";
	}
}
