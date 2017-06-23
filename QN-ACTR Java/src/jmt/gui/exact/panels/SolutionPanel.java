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
import java.awt.Color;
import java.awt.Font;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.event.TableModelEvent;

import jmt.framework.gui.help.HoverHelp;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.exact.ExactModel;
import jmt.gui.exact.ExactWizard;
import jmt.gui.exact.table.ExactTableModel;
import jmt.gui.exact.table.ResultsTable;

/**

 * @author alyf (Andrea Conti)
 * Date: 11-set-2003
 * Time: 23.48.19

 * Modified by Bertoli Marco 6-jun-2006 --> What-if analysis

 */

/**
 * generic solution panel
 */
public abstract class SolutionPanel extends WizardPanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected String helpText, name;

	protected ExactWizard ew;
	protected HoverHelp help;

	protected ExactModel data;

	protected int classes, stations;
	protected String[] classNames;
	protected String[] stationNames;
	protected boolean hasResults;
	protected boolean resultsOK;
	protected boolean isSingle;

	protected ResultsTable table;
	protected JLabel statusLabel = new JLabel();
	/** Number of iteration to be shown. Used by What-if analysis */
	protected int iteration = 0;

	public SolutionPanel(ExactWizard ew) {
		this.ew = ew;
		help = ew.getHelp();

		/* sync status with data object */
		sync();
		initComponents();
	}

	/**
	 * gets status from data object
	 */
	protected void sync() {
		/* arrays are copied to ensure data object consistency is preserved */
		data = ew.getData();
		classes = data.getClasses();
		stations = data.getStations();
		classNames = data.getClassNames();
		stationNames = data.getStationNames();
		hasResults = data.hasResults();
		resultsOK = data.areResultsOK();

		isSingle = (classes == 1);
		statusLabel.setVisible(!resultsOK);
	}

	/**
	 * Set up the panel contents and layout
	 */
	protected void initComponents() {
		table = new ResultsTable(getTableModel(), help);

		statusLabel.setForeground(Color.red);
		statusLabel.setFont(new Font("Arial", Font.BOLD, 14));
		statusLabel.setText("WARNING: parameters have been changed since this solution was computed!");
		statusLabel.setHorizontalAlignment(SwingConstants.CENTER);
		help.addHelp(statusLabel, "This solution is not current with the parameters of the model. Click solve to compute a new solution.");

		JPanel intPanel = new JPanel(new BorderLayout(10, 10));

		JScrollPane jsp = new JScrollPane(table, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		JLabel descrLabel = new JLabel(getDescriptionMessage());

		intPanel.add(descrLabel, BorderLayout.NORTH);
		intPanel.add(jsp, BorderLayout.CENTER);

		setLayout(new BorderLayout());
		add(intPanel, BorderLayout.CENTER);
		setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));

	}

	@Override
	public void gotFocus() {
		sync();
		table.updateStructure();
	}

	@Override
	public void help() {
		JOptionPane.showMessageDialog(this, helpText, "Help", JOptionPane.INFORMATION_MESSAGE);
	}

	@Override
	public String getName() {
		return name;
	}

	/**Returns table model that must describe data contained in this panel's table.
	 * @return :this panel's table model.
	 * */
	protected abstract ExactTableModel getTableModel();

	/**Returns this panel's purpose description.
	 * @return :message describing data contained in this panel.*/
	protected abstract String getDescriptionMessage();

	/**
	 * Sets iteration number for results to be shown. This is used for
	 * what-if analysis. Default is first iteration (0)
	 * @param iteration number of iteration to be shown. Must be between 0
	 * and [number of iterations]-1
	 */
	public void setIteration(int iteration) {
		this.iteration = iteration;
		// Refresh table
		if (table != null) {
			table.tableChanged(new TableModelEvent(table.getModel()));
		}
	}

}
