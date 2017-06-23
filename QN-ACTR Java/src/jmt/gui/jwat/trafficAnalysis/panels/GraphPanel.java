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
package jmt.gui.jwat.trafficAnalysis.panels;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.text.DecimalFormat;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTable;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import jmt.engine.jwat.trafficAnalysis.BurstEngine;
import jmt.engine.jwat.trafficAnalysis.ModelTrafficAnalysis;
import jmt.engine.jwat.trafficAnalysis.OnResetModel;
import jmt.engine.jwat.trafficAnalysis.OnSetParamtersListener;
import jmt.engine.jwat.trafficAnalysis.TrafficAnalysisSession;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.jwat.JWATConstants;
import jmt.gui.jwat.MainJwatWizard;
import jmt.gui.jwat.trafficAnalysis.utils.NewPlot;

/**
 * <p>Title: Graph Panel</p>
 * <p>Description: This panelis used to display the burstiness factors
 * in a graph.
 * 
 * @author Marco Rosini
 *         Date: 27-07-2006
 *         Time: 11.01.29
 */
public class GraphPanel extends WizardPanel implements JWATConstants {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	public static final String[] INDICES_TYPES = { " b ", " a " };
	public static final String DESCRIPTION_GRAPH = "<html><body align=\"left\"><font size=\"4\"><b>Burst values</b>"
			+ "</font><font size=\"3\"><br>Computed burstiness factors for all the epochs. Left-click and drag on the graph to zoom "
			+ "it, right-click to save it in EPS or PNG format.</body></html>";

	// Plot
	private NewPlot graph;
	// Performance index selector
	private JComboBox index;
	// Bounds for graph
	private JSpinner Xmin, Xmax, Ymin, Ymax;
	// Tells if spinner update is forced. This is needed to avoid that updates made by
	// code will be interpreted as updated made by user.
	private boolean forcedUpdate = false;
	// Table used to select performance indices to be plotted
	private JTable table;
	// Scrollpane used for table
	private JScrollPane tableScrollPane;
	// Dimension of bounds spinners
	final static Dimension DIM_SPINNER = new Dimension(60, 20);
	// Current performance index
	private String currentIndex = " b ";
	private MainJwatWizard ew;
	private Vector columnHeads;
	private Vector<Vector<Comparable>> rows;
	private JPanel left, mainPanel;
	private double[] resA, resB;

	private BurstEngine engine = null;

	/**
	 * Builds a new GraphPanel, given an exact model data structure
	 * @param model reference to data structure
	 */
	public GraphPanel(MainJwatWizard ew) {
		this.ew = ew;
		((TrafficAnalysisSession) ew.getSession()).addSetParamsListener(new OnSetParamtersListener() {
			public void ParamsSetted() {
				System.err.println("Graph Panel reset");
				engine = ((TrafficAnalysisSession) GraphPanel.this.ew.getSession()).getEngine();
				GraphPanel.this.removeAll();
				initGraphics();
				setTable();
			}
		});
		((ModelTrafficAnalysis) ew.getModel()).addResetModelListener(new OnResetModel() {
			public void modelResetted() {
				GraphPanel.this.removeAll();
			}
		});
	}

	/**
	 * Initialize GUI of this panel
	 */
	private void initGraphics() {
		setLayout(new BorderLayout(10, 10));
		setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));
		mainPanel = new JPanel(new BorderLayout(5, 5));
		mainPanel.setBorder(BorderFactory.createEtchedBorder());

		// Adds description label
		JLabel descrLabel = new JLabel(DESCRIPTION_GRAPH);
		add(descrLabel, BorderLayout.NORTH);
		add(mainPanel, BorderLayout.CENTER);

		// Creates left panel with options
		left = new JPanel(new BorderLayout(3, 3));
		// Adds performance index selection
		JPanel indexPanel = new JPanel();
		JLabel pIndex = new JLabel("Burstiness factor: ");
		index = new JComboBox(INDICES_TYPES);
		pIndex.setLabelFor(index);
		JPanel formulaPanel = new JPanel();
		JPanel formula2Panel = new JPanel();
		JLabel formula = new JLabel("b = (number of epochs for which lambda(k)>lambda) / n");
		JLabel formula2 = new JLabel("a = lambda+ / lambda");
		formulaPanel.add(formula);
		formula2Panel.add(formula2);
		indexPanel.add(pIndex);
		indexPanel.add(index);
		JPanel totPanel = new JPanel(new BorderLayout(4, 4));
		totPanel.add(indexPanel, BorderLayout.NORTH);
		totPanel.add(formulaPanel, BorderLayout.CENTER);
		totPanel.add(formula2Panel, BorderLayout.SOUTH);
		left.add(totPanel, BorderLayout.NORTH);
		// left.add(formulaPanel, BorderLayout.NORTH);
		// Adds panel for bounds selection
		JPanel boundsPanel = new JPanel(new GridLayout(2, 4, 1, 1));
		boundsPanel.add(new JLabel("Xmin: ", SwingConstants.RIGHT));
		Xmin = new JSpinner(new SpinnerNumberModel(0.0, 0, engine.getEpochRange(), 0.01));
		Xmin.setPreferredSize(DIM_SPINNER);
		boundsPanel.add(Xmin);
		boundsPanel.add(new JLabel("Xmax: ", SwingConstants.RIGHT));
		Xmax = new JSpinner(new SpinnerNumberModel(0.0, 0, engine.getEpochRange(), 0.01));
		Xmax.setPreferredSize(DIM_SPINNER);
		boundsPanel.add(Xmax);
		boundsPanel.add(new JLabel("Ymin: ", SwingConstants.RIGHT));
		Ymin = new JSpinner(new SpinnerNumberModel(0.0, 0.0, 1e10, 0.01));
		Ymin.setPreferredSize(DIM_SPINNER);
		boundsPanel.add(Ymin);
		boundsPanel.add(new JLabel("Ymax: ", SwingConstants.RIGHT));
		Ymax = new JSpinner(new SpinnerNumberModel(0.0, 0.0, 1e10, 0.01));
		Ymax.setPreferredSize(DIM_SPINNER);
		boundsPanel.add(Ymax);
		left.add(boundsPanel, BorderLayout.SOUTH);

		mainPanel.add(left, BorderLayout.WEST);

		// Puts graph in the right panel
		// Creates label for X-axis
		String xLabel = "";
		graph = new NewPlot();
		xLabel = "Number of epochs";
		graph.setXLabel(xLabel);
		graph.setXRange(0, 30);
		graph.setYLabel("Burstiness factor " + currentIndex);
		mainPanel.add(graph, BorderLayout.CENTER);
		columnHeads = new Vector();
		columnHeads.add("Epoch");
		columnHeads.add("Factor b");
		columnHeads.add("Factor a");
		table = new JTable(rows, columnHeads);

		tableScrollPane = new JScrollPane(table);
		tableScrollPane.setPreferredSize(new Dimension(160, tableScrollPane.getPreferredSize().height));
		left.add(tableScrollPane, BorderLayout.CENTER);
		updateSpinners();
		addActions();

	}

	/**
	 * Is the method invoked to set the value in the table
	 */

	public void setTable() {
		DecimalFormat df = new DecimalFormat("0.00000000");
		rows = new Vector<Vector<Comparable>>();
		Vector<Comparable> line;
		double[] resultB = engine.getData("b");
		double[] resultA = engine.getData("a");
		resA = new double[resultA.length - 1];
		resB = new double[resultB.length - 1];
		int k = 0;
		int count = 2;
		for (int i = 1; i < engine.getEpochRange(); i++) {
			line = new Vector<Comparable>();
			line.add(new Integer(count));
			line.add(new Double(df.format(resultB[i])));
			line.add(new Double(df.format(resultA[i])));
			resB[k] = resultB[i];
			resA[k] = resultA[i];
			rows.add(line);
			count++;
			k++;
		}

		left.remove(tableScrollPane);
		table = new JTable(rows, columnHeads);
		table.setEnabled(false);
		table.getColumnModel().getColumn(0).setMaxWidth(50);
		table.getColumnModel().getColumn(1).setPreferredWidth(65);
		table.getColumnModel().getColumn(2).setPreferredWidth(65);
		table.setRowHeight(18);
		tableScrollPane = new JScrollPane(table);
		tableScrollPane.setPreferredSize(new Dimension(160, tableScrollPane.getPreferredSize().height));
		left.add(tableScrollPane, BorderLayout.CENTER);
		mainPanel.updateUI();
		paintIndex(1);

	}

	/**
	 * Updates values in spinners used to select ranges to be shown in graph
	 */
	private void updateSpinners() {
		// Check for special value used if graph is empty
		if (graph.getXRange()[0] != Double.MAX_VALUE) {
			Xmin.setValue(new Double(graph.getXRange()[0]));
			Xmax.setValue(new Double(graph.getXRange()[1]));
			Ymin.setValue(new Double(graph.getYRange()[0]));
			Ymax.setValue(new Double(graph.getYRange()[1]));
		} else {
			Xmin.setValue(new Double(0.0));
			Xmax.setValue(new Double(0.0));
			Ymin.setValue(new Double(0.0));
			Ymax.setValue(new Double(0.0));
		}
	}

	/**
	 * Used when a spinne value is updated
	 */
	private void setBounds() {
		double xmin, xmax, ymin, ymax;
		Object val = Xmin.getValue();
		if (val instanceof Number) {
			xmin = ((Number) val).doubleValue();
		} else {
			xmin = graph.getXRange()[0];
		}
		val = Xmax.getValue();
		if (val instanceof Number) {
			xmax = ((Number) val).doubleValue();
		} else {
			xmax = graph.getXRange()[1];
		}
		val = Ymin.getValue();
		if (val instanceof Number) {
			ymin = ((Number) val).doubleValue();
		} else {
			ymin = graph.getYRange()[0];
		}
		val = Ymax.getValue();
		if (val instanceof Number) {
			ymax = ((Number) val).doubleValue();
		} else {
			ymax = graph.getYRange()[1];
		}
		// Sets bounds
		graph.setXRange(xmin, xmax);
		graph.setYRange(ymin, ymax);
		graph.repaint();
	}

	/**
	 * This function must be called each time selected performance
	 * index changes
	 */
	private void updateIndex() {
		String current = (String) index.getSelectedItem();

		currentIndex = current;
		if (((ModelTrafficAnalysis) ew.getModel()).getMatrix() != null) {
			paintIndex(1);
			// Updates graph
		}
		graph.setYLabel("Burstiness factor " + current);
		graph.repaint();
	}

	/**
	 * Adds action listeners to GUI components
	 */
	private void addActions() {
		// Listener used for bounds spinners
		ChangeListener boundsListener = new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				if (!forcedUpdate) {
					setBounds();
					updateSpinners();
				}
			}
		};
		Xmin.addChangeListener(boundsListener);
		Xmax.addChangeListener(boundsListener);
		Ymin.addChangeListener(boundsListener);
		Ymax.addChangeListener(boundsListener);
		// Listener for index selection comboBox
		index.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				updateIndex();
			}
		});

		// Adds a listener to the graph to detect zoom events
		graph.addRescaleListener(new NewPlot.RescaleListener() {
			public void Rescaled() {
				forcedUpdate = true;
				updateSpinners();
				forcedUpdate = false;
			}
		});
	}

	/**
	 * Paints performance index at specified row
	 * @param rowNum row number of index to be painted
	 */
	private void paintIndex(int rowNum) {
		double[] xAxis = new double[resB.length];
		int z = 2;
		for (int i = 0; i < resB.length; i++) {
			xAxis[i] = z;
			z++;
		}
		graph.setXAxis(xAxis);

		// Clears previous graph

		if (resB.length < -1 || resA.length < -1) {
			// Resets view
			autosizeGraph();
			return;
		}

		// Factor B
		if (currentIndex.equals(INDICES_TYPES[0])) {
			graph.clear(rowNum);
			graph.draw(rowNum, resB);
		}
		// Factor A
		if (currentIndex.equals(INDICES_TYPES[1])) {
			graph.clear(rowNum);
			graph.draw(rowNum, resA);
		}

		// Resets view
		autosizeGraph();
	}

	/**
	 * AutoResizes graph window
	 */
	private void autosizeGraph() {
		graph.fillPlot();
	}

	/**
	 * @return the panel's name
	 */
	@Override
	public String getName() {
		return "Burst Values - Graphics";
	}

	@Override
	public void lostFocus() {
		ew.setLastPanel(TRAFFIC_GRAPH_PANEL);
	}

}
