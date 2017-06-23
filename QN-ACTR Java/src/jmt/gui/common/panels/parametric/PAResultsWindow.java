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
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Graphics2D;
import java.awt.GridLayout;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.Vector;

import javax.imageio.ImageIO;
import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;
import javax.swing.SpringLayout;
import javax.swing.SwingUtilities;
import javax.swing.border.EmptyBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;

import jmt.engine.QueueNet.SimConstants;
import jmt.framework.gui.components.JMTFrame;
import jmt.framework.gui.graph.MeasureValue;
import jmt.framework.gui.graph.PAPlot;
import jmt.framework.gui.layouts.SpringUtilities;
import jmt.gui.common.definitions.MeasureDefinition;
import jmt.gui.common.definitions.PAResultsModel;
import jmt.gui.common.definitions.ResultsConstants;
import jmt.gui.common.definitions.SimulationDefinition;
import jmt.gui.common.definitions.parametric.ArrivalRateParametricAnalysis;
import jmt.gui.common.definitions.parametric.NumberOfCustomerParametricAnalysis;
import jmt.gui.common.definitions.parametric.ParametricAnalysis;
import jmt.gui.common.definitions.parametric.ParametricAnalysisDefinition;
import jmt.gui.common.definitions.parametric.PopulationMixParametricAnalysis;
import jmt.gui.common.definitions.parametric.SeedParametricAnalysis;
import jmt.gui.common.definitions.parametric.ServiceTimesParametricAnalysis;
import jmt.gui.common.resources.JMTImageLoader;
import jmt.gui.common.xml.XMLConstantNames;
import jmt.gui.exact.table.ExactTable;
import jmt.gui.exact.table.ExactTableModel;

/**
 * <p>Title: PAResultsWindow </p>
 * <p>Description: this is a JFrame used to show results from a parametric analysis simulation.
 * Performance indexes are grouped by type inside a JTabbedPane. Performance indexes of the
 * same type are showed one below the other. <br>
 * It contains some subclasses used for several purpose: <br>
 *   - <code>PAMeasurePanel</code>: it is a JPanel that contains the table with results and the
 *                     plot <br>
 *   - <code>ZoomedFrame</code>: it is a JFrame used to show a single performance index <br>
 *   - <code>PlotImagesFileChooser</code>: a file chooser used to save plot images <br>
 *   - <code>PlotImagesFileFilter</code>: a file filter used to filter unsupported image type <br>
 *   - <code>PlotPopupMenu</code>: a JPopupMenu used to zoom inside and outside the plot and save images <br></p>
 *
 * @author Francesco D'Aquino
 *         Date: 28-gen-2006
 *         Time: 10.48.11
 *         
 * Modified by Ashanka (Aug 09):
 * Desc: The code to include the changes for label changes from 
 *       1. Queue Length to Customer Number 
 *       2. Number of Customers to System Customer Number 
 * 
 * Modified by Ashanka (Sep 09):
 * Desc: The code to include the changes for label changes from 
 *       1. Customer Number to Number of Customers
 *       2. System Customer Number to System Number of Customers.
 *       
 * Modified by Ashanka (Nov 09):
 * Desc: Added the description of the Drop Rate
 * 
 * Modified by Ashanka (Nov 09):
 * Desc: Appended the values of various measures to the tool tip.
 * 
 * Modified by Ashanka (May 2010): 
 * Patch: Multi-Sink Perf. Index 
 * Description: Added new Performance index for the capturing the 
 * 				1. global response time (ResponseTime per Sink)
 *              2. global throughput (Throughput per Sink)
 *              each sink per class.
 */
public class PAResultsWindow extends JMTFrame implements ResultsConstants, ParametricAnalysis {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	public static final int SPINNER_WIDTH = 65;
	public static final double PLOT_ZOOM_FACTOR = 0.5;
	public static final double SPINNER_PRECISION = 0.001;

	private ParametricAnalysisDefinition pad;
	private PAResultsModel results;

	/**
	 * Creates a new PAResultsWindow
	 * @param pad parametric analysis definition model
	 * @param resModel parametric analysis results data structure
	 */
	public PAResultsWindow(ParametricAnalysisDefinition pad, PAResultsModel resModel) {
		this.results = resModel;
		this.pad = pad;
		initGUI();
	}

	/**
	 * Initialize all gui-related stuff
	 */
	private void initGUI() {
		// Sets default title, close operation and dimensions
		this.setDefaultCloseOperation(JDialog.HIDE_ON_CLOSE);
		this.setTitle("Simulation Results...");
		this.setIconImage(JMTImageLoader.loadImage("Results").getImage());
		this.centerWindow(800, 600);

		// Creates all tabs
		JTabbedPane mainPanel = new JTabbedPane();
		this.getContentPane().setLayout(new BorderLayout());
		this.getContentPane().add(mainPanel, BorderLayout.CENTER);
		addTabPane(mainPanel, "Number of Customers", DESCRIPTION_QUEUELENGTHS, results.getQueueLengthMeasures());
		addTabPane(mainPanel, "Queue Time", DESCRIPTION_QUEUETIMES, results.getQueueTimeMeasures());
		addTabPane(mainPanel, "Residence Time", DESCRIPTION_RESIDENCETIMES, results.getResidenceTimeMeasures());
		addTabPane(mainPanel, "Response Time", DESCRIPTION_RESPONSETIMES, results.getResponseTimeMeasures());
		addTabPane(mainPanel, "Utilization", DESCRIPTION_UTILIZATIONS, results.getUtilizationMeasures());
		addTabPane(mainPanel, "Throughput", DESCRIPTION_THROUGHPUTS, results.getThroughputMeasures());
		addTabPane(mainPanel, "Drop Rate", DESCRIPTION_DROPRATE, results.getDropRateMeasures());
		addTabPane(mainPanel, "System Throughput", DESCRIPTION_SYSTEMTHROUGHPUTS, results.getSystemThroughputMeasures());
		addTabPane(mainPanel, "System Response Time", DESCRIPTION_SYSTEMRESPONSETIMES, results.getSystemResponseTimeMeasures());
		addTabPane(mainPanel, "System Drop Rate", DESCRIPTION_DROPRATE, results.getSystemDropRateMeasures());
		addTabPane(mainPanel, "System Number of Customers", DESCRIPTION_CUSTOMERNUMBERS, results.getCustomerNumberMeasures());
		//Added by ASHANKA START
		//Adds the System Power panel in the results window.
		addTabPane(mainPanel, "System Power", DESCRIPTION_SYSTEMPOWER, results.getSystemPowerMeasures());
		//Added by ASHANKA STOP
		addTabPane(mainPanel, SimulationDefinition.MEASURE_R_PER_SINK, DESCRIPTION_RESPONSETIME_SINK, results.getResponsetimePerSinkMeasures());
		addTabPane(mainPanel, SimulationDefinition.MEASURE_X_PER_SINK, DESCRIPTION_THROUGHPUT_SINK, results.getThroughputPerSinkMeasures());
	}

	public String getXLabel() {
		if (pad instanceof NumberOfCustomerParametricAnalysis) {
			return "N";
		} else if (pad instanceof PopulationMixParametricAnalysis) {
			return "ß of " + pad.getReferenceClassName();
		} else if (pad instanceof SeedParametricAnalysis) {
			return "Step";
		} else if (pad instanceof ArrivalRateParametricAnalysis) {
			ArrivalRateParametricAnalysis arpa = (ArrivalRateParametricAnalysis) pad;
			if (arpa.isSingleClass()) {
				return arpa.getReferenceClassName() + " arrival rate [j/s]";
			} else {
				return "Ratio between assumed arrival rate and the initial one [%]";
			}
		} else if (pad instanceof ServiceTimesParametricAnalysis) {
			ServiceTimesParametricAnalysis stpa = (ServiceTimesParametricAnalysis) pad;
			if (stpa.isSingleClass()) {
				return stpa.getReferenceClassName() + " service time at " + stpa.getReferenceStationName() + " [s]";
			} else {
				return "Ratio between assumed service time at " + stpa.getReferenceStationName() + " and the initial one [%]";
			}
		} else {
			return "Not defined";
		}
	}

	public String getYLabel(int measureIndex) {
		int measureType = results.getMeasureType(measureIndex);
		String subfix = "";
		if ((measureType == SimConstants.QUEUE_LENGTH) || (measureType == SimConstants.SYSTEM_JOB_NUMBER)) {
			subfix = " (j)";
		} else if ((measureType == SimConstants.QUEUE_TIME) || (measureType == SimConstants.RESIDENCE_TIME)
				|| (measureType == SimConstants.RESPONSE_TIME) || (measureType == SimConstants.SYSTEM_RESPONSE_TIME)) {
			subfix = " (s)";
		} else if ((measureType == SimConstants.SYSTEM_THROUGHPUT) || (measureType == SimConstants.THROUGHPUT)) {
			subfix = " (j/s)";
		} else if (measureType == SimConstants.UTILIZATION) {
			subfix = " ";
		}
		String name = results.getName(measureIndex) + subfix;
		name = name.replaceFirst("Network_", "");
		name = name.replaceFirst("All classes_", "");
		return name;
	}

	/**
	 * Creates a new tabbed pane that shows specified measures and adds it to
	 * specified JTabPane. If measures indexes vector is null or empty no panel is added.
	 * @param parent panel where newly created tab should be added
	 * @param name name of the panel to be added
	 * @param description description to be shown into the panel
	 * @param indexes array with all measures indexes to be shown in this panel
	 */
	private void addTabPane(JTabbedPane parent, String name, String description, int[] indexes) {
		// If no measure are present, don't add corresponding tab
		if (indexes != null && indexes.length > 0) {
			JPanel tabPanel = new JPanel(new BorderLayout());
			// Adds margins
			tabPanel.add(Box.createVerticalStrut(BORDERSIZE), BorderLayout.NORTH);
			tabPanel.add(Box.createVerticalStrut(BORDERSIZE), BorderLayout.SOUTH);
			tabPanel.add(Box.createHorizontalStrut(BORDERSIZE), BorderLayout.WEST);
			tabPanel.add(Box.createHorizontalStrut(BORDERSIZE), BorderLayout.EAST);
			JPanel mainpanel = new JPanel(new BorderLayout());
			tabPanel.add(mainpanel, BorderLayout.CENTER);
			// Adds tab description
			JPanel upperPanel = new JPanel(new BorderLayout());
			JLabel descrLabel = new JLabel(description);
			upperPanel.add(descrLabel, BorderLayout.NORTH);
			upperPanel.add(Box.createVerticalStrut(BORDERSIZE / 2), BorderLayout.SOUTH);
			mainpanel.add(upperPanel, BorderLayout.NORTH);
			// Adds panel with measures
			JPanel scroll = new JPanel(new GridLayout(indexes.length, 1, 1, 1));
			// Adds all measures to this panel
			for (int indexe : indexes) {
				scroll.add(new PAMeasurePanel(results, indexe));//,thisMeasureValues));
			}
			mainpanel.add(new JScrollPane(scroll), BorderLayout.CENTER);

			// Adds tab to parent tabbed pane
			parent.addTab(name, tabPanel);
		}
	}

	/**
	 * Gets the title of the table showing results
	 * @return a <code>String</code> containing the title
	 */
	/*private String getTableTitle() {
	        if (pad.getType().equals(PA_TYPE_NUMBER_OF_CUSTOMERS)) {
	            return "<html><b>Total number of jobs</b></html>";
	        }
	        else if (pad.getType().equals(PA_TYPE_POPULATION_MIX)) {
	            String className = pad.getReferenceClassName();
	            return "<html><b>Proportion of jobs from " + className + " with respect to total number of jobs</b></html>";
	        }
	        else if (pad.getType().equals(PA_TYPE_SERVICE_TIMES)) {
	            ServiceTimesParametricAnalysis stpa = (ServiceTimesParametricAnalysis)pad;
	            String className = stpa.getReferenceClassName();
	            String stationName = stpa.getReferenceStationName();
	            if (stpa.isSingleClass()) {
	                return "<html><b>Proportion of service time mean value of " + className + " at " + stationName + " with respect to the initial value</b></html>";
	            }
	            else return "<html><b>Proportion of service time mean value at " + stationName + " with respect to the initial value</b></html>";
	        }
	        else if (pad.getType().equals(PA_TYPE_ARRIVAL_RATE)){
	            ArrivalRateParametricAnalysis arpa = (ArrivalRateParametricAnalysis)pad;
	            String className = arpa.getReferenceClassName();
	            if (arpa.isSingleClass()) {
	                return "<html><b>Proportion of arrival rate mean value of " + className + " with respect to the initial value</b></html>";
	            }
	            else return "<html><b>Proportion of arrival rate mean value with respect to the initial value</b></html>";
	        }
	        else if (pad.getType().equals(PA_TYPE_SEED)) {
	            return "<html><b>Simulation number</b></html>";
	        }
	        else return "Not defined";
	    }
	    */

	/**
	 * Inner class to create a panel that holds all the computed performance indices
	 */
	protected class PAMeasurePanel extends JPanel implements PlotContainer {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		protected PAResultsModel rm;
		protected int measureIndex;
		protected Vector values;
		protected ZoomedFrame zoomedFrame;
		protected JCheckBox boundsEnabler;
		protected JSpinner xMin;
		protected JSpinner xMax;
		protected JSpinner yMin;
		protected JSpinner yMax;
		protected PAPlot graph;
		protected double XMIN;
		protected double XMAX;
		protected double YMIN;
		protected double YMAX;

		public PAMeasurePanel(PAResultsModel rm, int measureIndex) {//, Vector values){ //, Vector validities) {
			this.rm = rm;
			this.measureIndex = measureIndex;
			this.values = rm.getValues(measureIndex);
			createPanel();
			addListeners();
		}

		/**
		 * Used to create the panel holding all measure's data
		 */
		protected void createPanel() {
			JTextField samples;
			JPanel graphPanel;
			JLabel textState;
			this.setLayout(new BorderLayout(5, 5));
			this.setBorder(BorderFactory.createRaisedBevelBorder());

			//Adds mainPanel with all informations on this measure
			JLabel label;
			JTextField field;
			JPanel mainPanel = new JPanel(new SpringLayout());
			//Vector temp = rm.getParameterValues();
			//Create graph and initialize ranges
			graph = new PAPlot(values, rm.getParameterValues(), getXLabel(), getYLabel(measureIndex));
			graph.drawPlot(true);
			XMIN = graph.getPlotXMin();
			XMAX = graph.getPlotXMax();
			//If simulation was stopped andanly one result was calculated the XMIN andX YMAX
			//may equal, the same for YMIN and YMAX. In this case adjust the XMIN, XMAX, YMIN,
			//and YMAX values
			if (XMAX == XMIN) {
				if (XMIN != 0) {
					XMIN -= SPINNER_PRECISION;
				}
				XMAX += SPINNER_PRECISION;
			}
			YMIN = graph.getPlotYMin();
			YMAX = graph.getPlotYMax();
			if (YMAX == YMIN) {
				if (YMIN != 0) {
					YMIN -= SPINNER_PRECISION;
				}
				YMAX += SPINNER_PRECISION;
			}
			graph.setXRange(XMIN, XMAX);
			graph.setYRange(YMIN, YMAX);

			// Station name
			label = new JLabel("Station Name: ");
			if (rm.getNodeType(measureIndex).equalsIgnoreCase(XMLConstantNames.NODETYPE_REGION)) {
				label.setText("Region Name: ");
			}
			field = new JTextField();
			field.setEditable(false);
			field.setMaximumSize(new Dimension(field.getMaximumSize().width, field.getMinimumSize().height));
			label.setLabelFor(field);
			field.setText(rm.getStationName(measureIndex));
			field.setToolTipText("Name of the station: " + field.getText());
			mainPanel.add(label);
			mainPanel.add(field);
			// Class name
			label = new JLabel("Class Name: ");
			field = new JTextField();
			field.setEditable(false);
			field.setMaximumSize(new Dimension(field.getMaximumSize().width, field.getMinimumSize().height));
			label.setLabelFor(field);
			field.setText(rm.getClassName(measureIndex));
			field.setToolTipText("Name of the class: " + field.getText());
			mainPanel.add(label);
			mainPanel.add(field);
			// Alpha/Precision
			label = new JLabel("Conf.Int/Max Rel.Err (0-1): ");
			field = new JTextField();
			field.setEditable(false);
			field.setMaximumSize(new Dimension(field.getMaximumSize().width, field.getMinimumSize().height));
			label.setLabelFor(field);
			field.setText(rm.getAlpha(measureIndex) + " / " + rm.getPrecision(measureIndex)); // AnalyzedSamples
			field.setToolTipText("Confidence Interval and Maximum Relative Error requested for this measure: " + field.getText());
			mainPanel.add(label);
			mainPanel.add(field);
			//Samples
			label = new JLabel("Samples: ");
			samples = new JTextField();
			samples.setEditable(false);
			samples.setMaximumSize(new Dimension(samples.getMaximumSize().width, samples.getMinimumSize().height));
			label.setLabelFor(samples);
			samples.setText("" + values.size());
			samples.setToolTipText("Number of samples: " + samples.getText());
			mainPanel.add(label);
			mainPanel.add(samples);
			//xMin
			label = new JLabel("X min:");
			if (XMAX > SPINNER_PRECISION) {
				xMin = new JSpinner(new SpinnerNumberModel(XMIN, 0, XMAX - SPINNER_PRECISION, SPINNER_PRECISION));
			} else {
				xMin = new JSpinner(new SpinnerNumberModel(XMIN, 0, XMAX, SPINNER_PRECISION));
			}
			xMin.setToolTipText("Sets the smallest x represented");
			xMin.setMaximumSize(new Dimension(40, 18));
			label.setLabelFor(xMin);
			mainPanel.add(label);
			mainPanel.add(xMin);
			//xMax
			label = new JLabel("X max:");
			xMax = new JSpinner(new SpinnerNumberModel(XMAX, XMIN + SPINNER_PRECISION, XMAX, SPINNER_PRECISION));
			xMax.setToolTipText("Sets the largest x represented");
			xMax.setMaximumSize(new Dimension(40, 18));
			label.setLabelFor(xMax);
			mainPanel.add(label);
			mainPanel.add(xMax);
			//yMin
			label = new JLabel("Y min:");
			if (YMAX - SPINNER_PRECISION > YMIN) {
				yMin = new JSpinner(new SpinnerNumberModel(YMIN, 0, YMAX - SPINNER_PRECISION, SPINNER_PRECISION));
			} else {
				yMin = new JSpinner(new SpinnerNumberModel(YMIN, 0, YMAX, SPINNER_PRECISION));
			}
			yMin.setToolTipText("Sets the smallest y represented");
			yMin.setMaximumSize(new Dimension(40, 18));
			label.setLabelFor(yMin);
			mainPanel.add(label);
			mainPanel.add(yMin);
			//yMax
			label = new JLabel("Y max:");
			if (YMIN + SPINNER_PRECISION < YMAX) {
				yMax = new JSpinner(new SpinnerNumberModel(YMAX, YMIN + SPINNER_PRECISION, YMAX, SPINNER_PRECISION));
			} else {
				yMax = new JSpinner(new SpinnerNumberModel(YMAX, YMIN, YMAX, SPINNER_PRECISION));
			}
			yMax.setToolTipText("Sets the largest y represented");
			yMax.setMaximumSize(new Dimension(40, 18));
			label.setLabelFor(yMax);
			mainPanel.add(label);
			mainPanel.add(yMax);
			//Bounds enabler
			//label = new JLabel("Show confidence interval range:");
			boundsEnabler = new JCheckBox("Show confidence interval range");
			boundsEnabler.setToolTipText("Enable or disable bounds representation");
			boundsEnabler.setSelected(true);
			boundsEnabler.addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent e) {
					if (boundsEnabler.isSelected()) {
						graph.clear();
						graph.drawPlot(true);
					} else {
						graph.clear();
						graph.drawPlot(false);
					}
					graph.repaint();
				}
			});
			//mainPanel.add(label);
			//mainPanel.add(boundsEnabler);

			SpringUtilities.makeCompactGrid(mainPanel, 8, 2, //rows, cols
					16, 16, //initX, initY
					16, 10);//xPad, yPad

			JPanel pivotPanel = new JPanel(new BorderLayout());

			JPanel tempMainPanel = new JPanel(new BorderLayout());
			JLabel mainPanelTitle = new JLabel("<html><b>Description</b></html>");
			JPanel mainPanelTitlePanel = new JPanel();
			mainPanelTitlePanel.add(mainPanelTitle);
			tempMainPanel.add(mainPanelTitlePanel, BorderLayout.NORTH);
			tempMainPanel.add(mainPanel, BorderLayout.CENTER);
			//Add mainPanel
			pivotPanel.add(tempMainPanel, BorderLayout.WEST);

			//Create values table and add to JScrollPane
			ValuesTable table = new ValuesTable(values, rm.getMeasureType(measureIndex));//,validities);
			JScrollPane jsp = new JScrollPane(table);
			jsp.setBorder(new EmptyBorder(10, 10, 10, 10));
			jsp.setPreferredSize(new Dimension(Integer.MAX_VALUE, 105));

			JPanel tempJspPanel = new JPanel(new BorderLayout());
			//JLabel tableTitle = new JLabel(getTableTitle());
			JLabel tableTitle = new JLabel("<html><b>Simulation results table</b></html>");
			JPanel tempTitlePanel = new JPanel();
			tempTitlePanel.add(tableTitle);
			tempJspPanel.add(tempTitlePanel, BorderLayout.NORTH);
			tempJspPanel.add(jsp, BorderLayout.CENTER);

			textState = new JLabel();
			textState.setText("<html>The values in <font color=\"red\"> red </font> could not be computed with the requested precision</html>");
			JPanel textStatePanel = new JPanel(new BorderLayout());
			JPanel tempTextStatePanel = new JPanel(new FlowLayout());
			tempTextStatePanel.add(textState);
			textStatePanel.add(tempTextStatePanel, BorderLayout.CENTER);
			textStatePanel.setBorder(BorderFactory.createEmptyBorder(0, BORDERSIZE / 2, BORDERSIZE / 2, BORDERSIZE / 2));
			JPanel tablePanel = new JPanel(new BorderLayout());
			tablePanel.add(textStatePanel, BorderLayout.SOUTH);
			tablePanel.add(tempJspPanel, BorderLayout.CENTER);
			tablePanel.setBorder(new EmptyBorder(10, 0, 5, 0));

			//Add graph to pivot panel
			graphPanel = new JPanel(new BorderLayout());
			graphPanel.add(graph, BorderLayout.CENTER);
			graph.add(boundsEnabler, BorderLayout.SOUTH);
			graphPanel.setBorder(new EmptyBorder(0, 10, 10, 20));
			JLabel graphTitle = new JLabel("<html><b>Plot</b></html>");
			JPanel graphTitlePanel = new JPanel();
			graphTitlePanel.add(graphTitle);
			JPanel tempGraphPanel = new JPanel(new BorderLayout());
			tempGraphPanel.add(graphTitlePanel, BorderLayout.NORTH);
			tempGraphPanel.add(graph, BorderLayout.CENTER);

			pivotPanel.add(tempGraphPanel, BorderLayout.CENTER);

			//Create a second pivot panel, add the first pivot to it
			JPanel pivotPanel2 = new JPanel(new BorderLayout());
			pivotPanel2.add(pivotPanel, BorderLayout.CENTER);

			//Add the JScrollPane to pivotPanel2
			pivotPanel2.add(tablePanel, BorderLayout.SOUTH);

			//Add pivotPanel2 to the panel
			add(pivotPanel2, BorderLayout.CENTER);
		}

		/**
		 * Gives a reference to <code>this</code>
		 * @return a reference to <code>this</code> PAMeasurePanel
		 */
		public PAMeasurePanel getReference() {
			return this;
		}

		/**
		 * Gets a reference to the PAPlot contained
		 * @return a reference to the plot
		 */
		public PAPlot getPlot() {
			return graph;
		}

		/**
		 * Restores the original ranges
		 */
		public void resizePlot() {
			double[] xRange = { XMIN, XMAX };
			double[] yRange = { YMIN, YMAX };
			xMin.setValue(new Double(xRange[0]));
			xMax.setValue(new Double(xRange[1]));
			yMin.setValue(new Double(yRange[0]));
			yMax.setValue(new Double(yRange[1]));
			graph.repaint();
		}

		/**
		 * Zooms in by PLOT_ZOOM_FACTOR factor
		 */
		public void zoomIn() {
			double[] xRange = graph.getXRange();
			double[] yRange = graph.getYRange();
			double width = xRange[1] - xRange[0];
			double height = yRange[1] - yRange[0];
			double newWidth = (xRange[1] - xRange[0]) * PLOT_ZOOM_FACTOR;
			double newHeight = (yRange[1] - yRange[0]) * PLOT_ZOOM_FACTOR;
			double newXMin = xRange[0] + (width - newWidth) / 2;
			//The next 7 lines check that the new range is valid
			if (newXMin < XMIN) {
				newXMin = XMIN;
			}
			double newXMax = xRange[0] + (width - newWidth) / 2 + newWidth;
			if (newXMax > XMAX) {
				newXMax = XMAX;
			}
			double newYMin = yRange[0] + (height - newHeight) / 2;
			if (newYMin < YMIN) {
				newYMin = YMIN;
			}
			double newYMax = yRange[0] + (height - newHeight) / 2 + newHeight;
			if (newYMax > YMAX) {
				newYMax = YMAX;
			}
			graph.setXRange(newXMin, newXMax);
			graph.setYRange(newYMin, newYMax);
			xMin.setValue(new Double(newXMin));
			xMax.setValue(new Double(newXMax));
			yMin.setValue(new Double(newYMin));
			yMax.setValue(new Double(newYMax));
			graph.repaint();
		}

		/**
		 * Zooms in by PLOT_ZOOM_FACTOR factor
		 */
		public void zoomOut() {
			double[] xRange = graph.getXRange();
			double[] yRange = graph.getYRange();
			double width = xRange[1] - xRange[0];
			double height = yRange[1] - yRange[0];
			double newWidth = (xRange[1] - xRange[0]) * (1 / PLOT_ZOOM_FACTOR);
			double newHeight = (yRange[1] - yRange[0]) * (1 / PLOT_ZOOM_FACTOR);
			double newXMin = xRange[0] - (-width + newWidth) / 2;
			//The next 7 lines check that the new range is valid
			if (newXMin < XMIN) {
				newXMin = XMIN;
			}
			double newXMax = xRange[0] - (-width + newWidth) / 2 + newWidth;
			if (newXMax > XMAX) {
				newXMax = XMAX;
			}
			double newYMin = yRange[0] - (-height + newHeight) / 2;
			if (newYMin < YMIN) {
				newYMin = YMIN;
			}
			double newYMax = yRange[0] - (-height + newHeight) / 2 + newHeight;
			if (newYMax > YMAX) {
				newYMax = YMAX;
			}
			graph.setXRange(newXMin, newXMax);
			graph.setYRange(newYMin, newYMax);
			xMin.setValue(new Double(newXMin));
			xMax.setValue(new Double(newXMax));
			yMin.setValue(new Double(newYMin));
			yMax.setValue(new Double(newYMax));
			graph.repaint();
		}

		/**
		 * Adds listeners to components
		 */
		protected void addListeners() {
			graph.addMouseListener(new MouseAdapter() {
				public void mouseReleased(MouseEvent e) {
					double[] xRange = graph.getXRange();
					double[] yRange = graph.getYRange();
					if ((xRange[0] >= 0) && (xRange[1] <= XMAX)) {
						xMin.setValue(new Double(xRange[0]));
						xMax.setValue(new Double(xRange[1]));
					} else {
						double min = xRange[0], max = xRange[1];
						if (min < 0) {
							min = 0;
						}
						if (max > XMAX) {
							max = XMAX;
						}
						xMin.setValue(new Double(min));
						xMax.setValue(new Double(max));
						graph.setXRange(min, max);
					}
					if ((yRange[0] >= 0) && (yRange[1] <= YMAX)) {
						yMin.setValue(new Double(yRange[0]));
						yMax.setValue(new Double(yRange[1]));
					} else {
						double min = yRange[0], max = yRange[1];
						if (min < 0) {
							min = 0;
						}
						if (max > YMAX) {
							max = YMAX;
						}
						yMin.setValue(new Double(min));
						yMax.setValue(new Double(max));
						graph.setYRange(min, max);
					}
					graph.repaint();
				}

				/**
				 * If the mouse is clicked twice open the related ZoomedFrame
				 * @param e a MouseEvent
				 */
				public void mouseClicked(MouseEvent e) {
					if (e.getClickCount() == 2) {
						if (zoomedFrame == null) {
							zoomedFrame = new ZoomedFrame(rm, measureIndex, values, pad.getParameterValues());
						}
						zoomedFrame.show();
					}
					if (SwingUtilities.isRightMouseButton(e)) {
						PlotPopupMenu plotPopup = new PlotPopupMenu(getReference());
						plotPopup.show(graph, e.getPoint().x, e.getPoint().y);
					}
				}
			});
			xMin.addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent e) {
					if (xMin.getValue() instanceof Double) {
						double XMin = ((Double) xMin.getValue()).doubleValue();
						SpinnerNumberModel snm = (SpinnerNumberModel) xMax.getModel();
						snm.setMinimum(new Double(XMin + SPINNER_PRECISION));
						double[] range = graph.getXRange();
						graph.setXRange(XMin, range[1]);
						graph.repaint();
					}
				}
			});
			xMax.addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent e) {
					if (xMax.getValue() instanceof Double) {
						double XMax = ((Double) xMax.getValue()).doubleValue();
						SpinnerNumberModel snm = (SpinnerNumberModel) xMin.getModel();
						snm.setMaximum(new Double(XMax - SPINNER_PRECISION));
						double[] range = graph.getXRange();
						graph.setXRange(range[0], XMax);
						graph.repaint();
					}
				}
			});
			yMin.addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent e) {
					if (yMin.getValue() instanceof Double) {
						double YMin = ((Double) yMin.getValue()).doubleValue();
						SpinnerNumberModel snm = (SpinnerNumberModel) yMax.getModel();
						snm.setMinimum(new Double(YMin + SPINNER_PRECISION));
						double[] range = graph.getYRange();
						graph.setYRange(YMin, range[1]);
						graph.repaint();
					}
				}
			});
			yMax.addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent e) {
					if (yMax.getValue() instanceof Double) {
						double YMax = ((Double) yMax.getValue()).doubleValue();
						SpinnerNumberModel snm = (SpinnerNumberModel) yMin.getModel();
						snm.setMaximum(new Double(YMax - SPINNER_PRECISION));
						double[] range = graph.getYRange();
						graph.setYRange(range[0], YMax);
						graph.repaint();
					}
				}
			});
		}

	}

	/**
	 * Specifies some basic functions performed by those components that contain
	 * a Plot object
	 */
	public interface PlotContainer {
		/**
		 * Gets the PAPlot object
		 * @return  the plot
		 */
		public PAPlot getPlot();

		/**
		 * Restores the x and y ranges to their original values
		 */
		public void resizePlot();

		/**
		 * Zooms in by PLOT_ZOOM_FACTOR factor
		 */
		public void zoomIn();

		/**
		 * Zooms in by PLOT_ZOOM_FACTOR factor
		 */
		public void zoomOut();

	}

	/**
	 * A class representing a frame containing information about a single measure
	 */
	protected class ZoomedFrame extends JFrame implements PlotContainer {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		MeasureDefinition md;
		int measureIndex;
		Vector values;
		//Vector validities;
		Vector<Number> parameterValues;
		protected JCheckBox boundsEnabler;
		protected JSpinner xMin;
		protected JSpinner xMax;
		protected JSpinner yMin;
		protected JSpinner yMax;
		protected PAPlot graph;
		protected double XMIN;
		protected double XMAX;
		protected double YMIN;
		protected double YMAX;

		public ZoomedFrame(MeasureDefinition md, int measureIndex, Vector values, Vector<Number> parameterValues) {
			super(md.getName(measureIndex));
			this.measureIndex = measureIndex;
			this.values = values;
			this.md = md;
			this.parameterValues = parameterValues;
			initialize();
			addListeners();
			this.setIconImage(JMTImageLoader.loadImage("Results").getImage());
		}

		/**
		 * Initializes all gui-related stuff
		 */
		public void initialize() {
			JTextField samples;
			JPanel graphPanel;
			JLabel textState;
			this.getContentPane().setLayout(new BorderLayout(5, 5));

			//Adds mainPanel with all informations on this measure
			JLabel label;
			JTextField field;
			JPanel mainPanel = new JPanel(new SpringLayout());
			// Station name
			label = new JLabel("Station Name: ");
			field = new JTextField();
			field.setEditable(false);
			field.setMaximumSize(new Dimension(field.getMaximumSize().width, field.getMinimumSize().height));
			label.setLabelFor(field);
			field.setText(md.getStationName(measureIndex));
			field.setToolTipText("Name of the station");
			mainPanel.add(label);
			mainPanel.add(field);
			// Class name
			label = new JLabel("Class Name: ");
			field = new JTextField();
			field.setEditable(false);
			field.setMaximumSize(new Dimension(field.getMaximumSize().width, field.getMinimumSize().height));
			label.setLabelFor(field);
			field.setText(md.getClassName(measureIndex));
			field.setToolTipText("Name of the class");
			mainPanel.add(label);
			mainPanel.add(field);
			// Alpha/Precision
			label = new JLabel("Conf.Int/Max Rel.Err: ");
			field = new JTextField();
			field.setEditable(false);
			field.setMaximumSize(new Dimension(field.getMaximumSize().width, field.getMinimumSize().height));
			label.setLabelFor(field);
			field.setText(md.getAlpha(measureIndex) + " / " + md.getPrecision(measureIndex)); // AnalyzedSamples
			field.setToolTipText("Confidence Interval and Maximum Relative Error requested for this measure");
			mainPanel.add(label);
			mainPanel.add(field);
			label = new JLabel("Samples: ");
			samples = new JTextField();
			samples.setEditable(false);
			samples.setMaximumSize(new Dimension(samples.getMaximumSize().width, samples.getMinimumSize().height));
			label.setLabelFor(samples);
			samples.setText("" + values.size());
			samples.setToolTipText("Number of samples");
			mainPanel.add(label);
			mainPanel.add(samples);

			//Create the plot and initialize ranges
			graph = new PAPlot(values, parameterValues, getXLabel(), getYLabel(measureIndex));
			graph.drawPlot(true);
			XMIN = graph.getPlotXMin();
			XMAX = graph.getPlotXMax();
			YMIN = graph.getPlotYMin();
			YMAX = graph.getPlotYMax();
			//If simulation was stopped andanly one result was calculated the XMIN andX YMAX
			//may equal, the same for YMIN and YMAX. In this case adjust the XMIN, XMAX, YMIN,
			//and YMAX values
			if (XMAX == XMIN) {
				XMIN -= SPINNER_PRECISION;
				XMAX += SPINNER_PRECISION;
			}
			YMIN = graph.getPlotYMin();
			YMAX = graph.getPlotYMax();
			if (YMAX == YMIN) {
				YMIN -= SPINNER_PRECISION;
				YMAX += SPINNER_PRECISION;
			}
			graph.setXRange(XMIN, XMAX);
			graph.setYRange(YMIN, YMAX);

			//xMin
			label = new JLabel("X min:");
			xMin = new JSpinner(new SpinnerNumberModel(XMIN, 0, XMAX - SPINNER_PRECISION, SPINNER_PRECISION));
			xMin.setToolTipText("Sets the smallest x represented");
			xMin.setMaximumSize(new Dimension(40, 18));
			label.setLabelFor(xMin);
			mainPanel.add(label);
			mainPanel.add(xMin);
			//xMax
			label = new JLabel("X max:");
			xMax = new JSpinner(new SpinnerNumberModel(XMAX, XMIN + SPINNER_PRECISION, XMAX, SPINNER_PRECISION));
			xMax.setToolTipText("Sets the largest x represented");
			xMax.setMaximumSize(new Dimension(40, 18));
			label.setLabelFor(xMax);
			mainPanel.add(label);
			mainPanel.add(xMax);
			//yMin
			label = new JLabel("Y min:");
			yMin = new JSpinner(new SpinnerNumberModel(YMIN, 0, YMAX - SPINNER_PRECISION, SPINNER_PRECISION));
			yMin.setToolTipText("Sets the smallest y represented");
			yMin.setMaximumSize(new Dimension(40, 18));
			label.setLabelFor(yMin);
			mainPanel.add(label);
			mainPanel.add(yMin);
			//yMax
			label = new JLabel("Y max:");
			yMax = new JSpinner(new SpinnerNumberModel(YMAX, YMIN + SPINNER_PRECISION, YMAX, SPINNER_PRECISION));
			yMax.setToolTipText("Sets the largest y represented");
			yMax.setMaximumSize(new Dimension(40, 18));
			label.setLabelFor(yMax);
			mainPanel.add(label);
			mainPanel.add(yMax);
			//Bounds enabler
			//label = new JLabel("Show confidence interval range:");
			boundsEnabler = new JCheckBox("Show confidence interval range");
			boundsEnabler.setToolTipText("Enable or disable bounds representation");
			boundsEnabler.setSelected(true);
			boundsEnabler.addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent e) {
					if (boundsEnabler.isSelected()) {
						graph.clear();
						graph.drawPlot(true);
					} else {
						graph.clear();
						graph.drawPlot(false);
					}
					graph.repaint();
				}
			});
			//mainPanel.add(label);
			//mainPanel.add(boundsEnabler);

			SpringUtilities.makeCompactGrid(mainPanel, 8, 2, //rows, cols
					16, 16, //initX, initY
					16, 10);//xPad, yPad

			JPanel pivotPanel = new JPanel(new BorderLayout());
			JPanel tempMainPanel = new JPanel(new BorderLayout());
			JLabel mainPanelTitle = new JLabel("<html><b>Description</b></html>");
			JPanel mainPanelTitlePanel = new JPanel();
			mainPanelTitlePanel.add(mainPanelTitle);
			tempMainPanel.add(mainPanelTitlePanel, BorderLayout.NORTH);
			tempMainPanel.add(mainPanel, BorderLayout.CENTER);
			pivotPanel.add(tempMainPanel, BorderLayout.WEST);

			//Create values table and add to JScrollPane
			ValuesTable table = new ValuesTable(values, md.getMeasureType(measureIndex));//,validities);
			JScrollPane jsp = new JScrollPane(table);
			jsp.setBorder(new EmptyBorder(10, 10, 10, 10));
			jsp.setPreferredSize(new Dimension(Integer.MAX_VALUE, 102));
			JPanel tempJspPanel = new JPanel(new BorderLayout());
			//JLabel tableTitle = new JLabel(getTableTitle());
			JLabel tableTitle = new JLabel("<html><b>Simulation results</b></html>");
			JPanel tempTitlePanel = new JPanel();
			tempTitlePanel.add(tableTitle);
			tempJspPanel.add(tempTitlePanel, BorderLayout.NORTH);
			tempJspPanel.add(jsp, BorderLayout.CENTER);
			textState = new JLabel();
			textState.setText("<html>The values in <font color=\"red\"> red </font> could not be computed with the requested precision</html>");
			JPanel textStatePanel = new JPanel(new BorderLayout());
			JPanel tempTextStatePanel = new JPanel(new FlowLayout());
			tempTextStatePanel.add(textState);
			textStatePanel.add(tempTextStatePanel, BorderLayout.CENTER);
			textStatePanel.setBorder(BorderFactory.createEmptyBorder(0, BORDERSIZE / 2, BORDERSIZE / 2, BORDERSIZE / 2));
			JPanel tablePanel = new JPanel(new BorderLayout());
			tablePanel.add(textStatePanel, BorderLayout.SOUTH);
			tablePanel.add(tempJspPanel, BorderLayout.CENTER);
			tablePanel.setBorder(new EmptyBorder(10, 0, 5, 0));

			//Add graph to panel
			graphPanel = new JPanel(new BorderLayout());
			graphPanel.add(graph, BorderLayout.CENTER);
			graph.add(boundsEnabler, BorderLayout.SOUTH);
			graphPanel.setBorder(new EmptyBorder(0, 10, 10, 20));
			JLabel graphTitle = new JLabel("<html><b>Plot</b></html>");
			JPanel graphTitlePanel = new JPanel();
			graphTitlePanel.add(graphTitle);
			JPanel tempGraphPanel = new JPanel(new BorderLayout());
			tempGraphPanel.add(graphTitlePanel, BorderLayout.NORTH);
			tempGraphPanel.add(graph, BorderLayout.CENTER);

			pivotPanel.add(tempGraphPanel, BorderLayout.CENTER);

			//Create a second pivot panel, add the first pivot to it
			JPanel pivotPanel2 = new JPanel(new BorderLayout());
			pivotPanel2.add(pivotPanel, BorderLayout.CENTER);

			//Add the JScrollPane to pivotPanel2
			pivotPanel2.add(tablePanel, BorderLayout.SOUTH);

			//Add pivotPanel2 to the panel
			this.getContentPane().add(pivotPanel2, BorderLayout.CENTER);

			Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
			Dimension size = new Dimension(800, 600);
			this.setSize(size);
			this.setLocation((screenSize.width - size.width) / 2, (screenSize.height - size.height) / 2);
		}

		/**
		 * Gets a reference to the plot
		 * @return the plot object
		 */
		public PAPlot getPlot() {
			return graph;
		}

		/**
		 * Forces the plot to get its original axis range
		 */
		public void resizePlot() {
			double[] xRange = { XMIN, XMAX };
			double[] yRange = { YMIN, YMAX };
			xMin.setValue(new Double(xRange[0]));
			xMax.setValue(new Double(xRange[1]));
			yMin.setValue(new Double(yRange[0]));
			yMax.setValue(new Double(yRange[1]));
			graph.repaint();
		}

		/**
		 * Zooms inside by the factor PLOT_ZOOM_FACTOR
		 */
		public void zoomIn() {
			double[] xRange = graph.getXRange();
			double[] yRange = graph.getYRange();
			double width = xRange[1] - xRange[0];
			double height = yRange[1] - yRange[0];
			double newWidth = (xRange[1] - xRange[0]) * PLOT_ZOOM_FACTOR;
			double newHeight = yRange[1] - yRange[0] * PLOT_ZOOM_FACTOR;
			double newXMin = xRange[0] + (width - newWidth) / 2;
			//The next 7 lines check if the new range is valid
			if (newXMin < XMIN) {
				newXMin = XMIN;
			}
			double newXMax = xRange[0] + (width - newWidth) / 2 + newWidth;
			if (newXMax > XMAX) {
				newXMax = XMAX;
			}
			double newYMin = yRange[0] + (height - newHeight) / 2;
			if (newYMin < YMIN) {
				newYMin = YMIN;
			}
			double newYMax = yRange[0] + (height - newHeight) / 2 + newHeight;
			if (newYMax > YMAX) {
				newYMax = YMAX;
			}
			graph.setXRange(newXMin, newXMax);
			graph.setYRange(newYMin, newYMax);
			xMin.setValue(new Double(newXMin));
			xMax.setValue(new Double(newXMax));
			yMin.setValue(new Double(newYMin));
			yMax.setValue(new Double(newYMax));
			graph.repaint();
		}

		/**
		 * Zooms outside by the factor PLOT_ZOOM_FACTOR
		 */
		public void zoomOut() {
			double[] xRange = graph.getXRange();
			double[] yRange = graph.getYRange();
			double width = xRange[1] - xRange[0];
			double height = yRange[1] - yRange[0];
			double newWidth = (xRange[1] - xRange[0]) * (1 / PLOT_ZOOM_FACTOR);
			double newHeight = (yRange[1] - yRange[0]) * (1 / PLOT_ZOOM_FACTOR);
			double newXMin = xRange[0] - (-width + newWidth) / 2;
			if (newXMin < XMIN) {
				newXMin = XMIN;
			}
			double newXMax = xRange[0] - (-width + newWidth) / 2 + newWidth;
			if (newXMax > XMAX) {
				newXMax = XMAX;
			}
			double newYMin = yRange[0] - (-height + newHeight) / 2;
			if (newYMin < YMIN) {
				newYMin = YMIN;
			}
			double newYMax = yRange[0] - (-height + newHeight) / 2 + newHeight;
			if (newYMax > YMAX) {
				newYMax = YMAX;
			}
			graph.setXRange(newXMin, newXMax);
			graph.setYRange(newYMin, newYMax);
			xMin.setValue(new Double(newXMin));
			xMax.setValue(new Double(newXMax));
			yMin.setValue(new Double(newYMin));
			yMax.setValue(new Double(newYMax));
			graph.repaint();
		}

		/**
		 * Gets a reference to <code>this</code>
		 * @return a reference to <code>this</code> ZoomedFrame object
		 */
		public ZoomedFrame getReference() {
			return this;
		}

		/**
		 * Adds all listeners to objects
		 */
		protected void addListeners() {
			graph.addMouseListener(new MouseAdapter() {
				public void mouseReleased(MouseEvent e) {
					double[] xRange = graph.getXRange();
					double[] yRange = graph.getYRange();
					//if the new x range is valid...
					if ((xRange[0] >= 0) && (xRange[1] <= XMAX)) {
						xMin.setValue(new Double(xRange[0])); // -> set it
						xMax.setValue(new Double(xRange[1]));
					} else {
						// ... else where it's not compatible restore the original value
						double min = xRange[0], max = xRange[1];
						if (min < 0) {
							min = 0;
						}
						if (max > XMAX) {
							max = XMAX;
						}
						xMin.setValue(new Double(min));
						xMax.setValue(new Double(max));
						graph.setXRange(min, max);
					}
					//the same as for the x range
					if ((yRange[0] >= 0) && (yRange[1] <= YMAX)) {
						yMin.setValue(new Double(yRange[0]));
						yMax.setValue(new Double(yRange[1]));
					} else {
						double min = yRange[0], max = yRange[1];
						if (min < 0) {
							min = 0;
						}
						if (max > YMAX) {
							max = YMAX;
						}
						yMin.setValue(new Double(min));
						yMax.setValue(new Double(max));
						graph.setYRange(min, max);
					}
					graph.repaint();
				}

				public void mouseClicked(MouseEvent e) {
					//show a popup menù where you can zomm in and out and save
					//the plot to an image
					if (SwingUtilities.isRightMouseButton(e)) {
						PlotPopupMenu plotPopup = new PlotPopupMenu(getReference());
						plotPopup.show(graph, e.getPoint().x, e.getPoint().y);
					}
				}
			});
			xMin.addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent e) {
					if (xMin.getValue() instanceof Double) {
						double XMin = ((Double) xMin.getValue()).doubleValue();
						//the xMax spinner now has a new minimum value, set it
						SpinnerNumberModel snm = (SpinnerNumberModel) xMax.getModel();
						snm.setMinimum(new Double(XMin + SPINNER_PRECISION));
						double[] range = graph.getXRange();
						//set the new range
						graph.setXRange(XMin, range[1]);
						graph.repaint();
					}
				}
			});
			xMax.addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent e) {
					if (xMax.getValue() instanceof Double) {
						double XMax = ((Double) xMax.getValue()).doubleValue();
						//the xMin spinner now has a new maximum value, set it
						SpinnerNumberModel snm = (SpinnerNumberModel) xMin.getModel();
						snm.setMaximum(new Double(XMax - SPINNER_PRECISION));
						double[] range = graph.getXRange();
						//set the new range
						graph.setXRange(range[0], XMax);
						graph.repaint();
					}
				}
			});
			yMin.addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent e) {
					if (yMin.getValue() instanceof Double) {
						double YMin = ((Double) yMin.getValue()).doubleValue();
						//the yMax spinner now has a new minimum value, set it
						SpinnerNumberModel snm = (SpinnerNumberModel) yMax.getModel();
						snm.setMinimum(new Double(YMin + SPINNER_PRECISION));
						double[] range = graph.getYRange();
						//set the new range
						graph.setYRange(YMin, range[1]);
						graph.repaint();
					}
				}
			});
			yMax.addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent e) {
					if (yMax.getValue() instanceof Double) {
						double YMax = ((Double) yMax.getValue()).doubleValue();
						//the yMin spinner now has a new maximum value, set it
						SpinnerNumberModel snm = (SpinnerNumberModel) yMin.getModel();
						snm.setMaximum(new Double(YMax - SPINNER_PRECISION));
						double[] range = graph.getYRange();
						//set the new range
						graph.setYRange(range[0], YMax);
						graph.repaint();
					}
				}
			});
		}
	}

	/**
	 * Inner class to show parametric analysis results
	 */
	protected class ValuesTable extends ExactTable {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		Vector values;
		//Vector validities;
		DefaultTableCellRenderer dtcr = new DefaultTableCellRenderer();

		public ValuesTable(Vector values, int measureType) {//, Vector validities) {
			super(new ValuesTableModel(values, measureType));//,validities));
			this.values = values;
			//this.validities = validities;
			autoResizeMode = AUTO_RESIZE_OFF;

			setRowSelectionAllowed(true);
			setColumnSelectionAllowed(true);
			setClipboardTransferEnabled(true);
			setFillCellsEnabled(false);
			this.setRowHeaderWidth(100);
		}

		/**
		 * The original getCellRenderer method is overwritten, since the table
		 * displays in red the values that could not be calculated with the requested
		 * precision
		 * @param row the row of the cell
		 * @param column the column of the cell
		 * @return a the TableCellRenderer for the requested cell (row,column)
		 */
		public TableCellRenderer getCellRenderer(int row, int column) {
			dtcr.setHorizontalAlignment(DefaultTableCellRenderer.CENTER);
			//Component c = null;
			Component c;
			if (column < values.size()) {
				c = dtcr.getTableCellRendererComponent(this, values.get(column), false, false, row, column);
				//if (!((Boolean)(validities.get(column))).booleanValue()) {
				if (!((PAResultsModel.MeasureValueImpl) values.get(column)).isValid()) {
					c.setForeground(Color.RED);
				} else {
					c.setForeground(Color.BLACK);
				}

			} else {
				c = dtcr.getTableCellRendererComponent(this, "-", false, false, row, column);
				c.setForeground(Color.BLACK);
			}

			return dtcr;
		}
	}

	/**
	 * Model for ValuesTable
	 * Columns represent the values assumed by the varying parameter.
	 */
	protected class ValuesTableModel extends ExactTableModel {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		private DecimalFormat hexFormat = new DecimalFormat("0.000E0");
		private DecimalFormat threeDecFormat = new DecimalFormat("0.000");
		private DecimalFormat twoDecFormat = new DecimalFormat("0.00");

		private String getFormattedVal(double value) {
			if ((value < 1E-3) || (value > 1E4 - 1)) {
				return hexFormat.format(value);
			} else {
				return threeDecFormat.format(value);
			}
		}

		Vector values;
		int measureType;

		//Vector validities;
		public ValuesTableModel(Vector values, int measureType) {//, Vector validities) {
			this.values = values;
			this.measureType = measureType;
			//this.validities = validities;
			prototype = "XXXX.XXX XXX";
			rowHeaderPrototype = "Upper bound";
		}

		public Object getPrototype(int i) {
			if (i == -1) {
				return rowHeaderPrototype;
			} else {
				return prototype;
			}
		}

		public int getRowCount() {
			return 3;
		}

		/**
		 * @return the object at (rowIndex, columnIndex)
		 */
		protected Object getValueAtImpl(int rowIndex, int columnIndex) {
			String toReturn;
			//if the value that should be contained in the requested cell is avaible
			if (columnIndex < values.size()) {
				//upper bound cells
				if (rowIndex == 1) {
					double upper = ((MeasureValue) values.get(columnIndex)).getUpperBound();
					double value = ((MeasureValue) values.get(columnIndex)).getMeanValue();
					//if the upper bound is 0 but not the value, or if the upperbound is infinite
					//show only a "-"
					if (((upper == 0) && (value != 0)) || (Double.isInfinite(upper))) {
						toReturn = "-";
					}
					//else show the formatted value
					else {
						toReturn = getFormattedVal(upper);
					}
				}
				//value cells, always show the formatted value
				else if (rowIndex == 0) {
					double value = ((MeasureValue) values.get(columnIndex)).getMeanValue();
					toReturn = getFormattedVal(value);
				}
				//lower bound cells
				else {
					double upper = ((MeasureValue) values.get(columnIndex)).getUpperBound();
					double lower = ((MeasureValue) values.get(columnIndex)).getLowerBound();
					double value = ((MeasureValue) values.get(columnIndex)).getMeanValue();
					//as for upper bounds, show a value only if it is meaningful
					if (((upper == 0) && (value != 0)) || (Double.isInfinite(upper))) {
						toReturn = "-";
					} else {
						toReturn = getFormattedVal(lower);
					}
				}
			} else {
				toReturn = "-";
			}
			return toReturn;
		}

		public int getColumnCount() {
			return pad.getNumberOfSteps();
		}

		/**
		 * @return the header for row <code>rowIndex</code>
		 */
		protected Object getRowName(int rowIndex) {
			String subfix = "";
			if ((measureType == SimConstants.QUEUE_LENGTH) || (measureType == SimConstants.SYSTEM_JOB_NUMBER)) {
				subfix = " (j)";
			} else if ((measureType == SimConstants.QUEUE_TIME) || (measureType == SimConstants.RESIDENCE_TIME)
					|| (measureType == SimConstants.RESPONSE_TIME) || (measureType == SimConstants.SYSTEM_RESPONSE_TIME)) {
				subfix = " (s)";
			} else if ((measureType == SimConstants.SYSTEM_THROUGHPUT) || (measureType == SimConstants.THROUGHPUT)) {
				subfix = " (j/s)";
			} else if (measureType == SimConstants.UTILIZATION) {
				subfix = " ";
			}
			if (rowIndex == 0) {
				return ("Mean value" + subfix);
			} else if (rowIndex == 1) {
				//Modified by ASHANKA START
				//This Label has been modified to
				//let the users know that the Min and Max is 
				//Conf Interval Range and not sample Min or Max.
				//return ("Max" + subfix);
				return ("Max" + subfix + " (Conf Int)");
				//Modified by ASHANKA STOP
			} else {
				//Modified by ASHANKA START
				//return ("Min" + subfix);
				return ("Min" + subfix + " (Conf Int)");
				//This Label has been modified to
				//let the users know that the Min and Max is 
				//Conf Interval Range and not sample Min or Max.
				//Modified by ASHANKA STOP
			}
		}

		/**
		 * Gets the name of the column, given the column index
		 * @param index the index of the column to give the name
		 * @return the column name
		 */
		public String getColumnName(int index) {
			String columnName = "NA";
			//if single class return the value, else return the percentage
			if (pad.getType().equals(PA_TYPE_ARRIVAL_RATE)) {
				ArrivalRateParametricAnalysis arpa = (ArrivalRateParametricAnalysis) pad;
				Vector<Number> assumedValues = arpa.getParameterValues();
				if (arpa.isSingleClass()) {
					Object temp = assumedValues.get(index);
					double val = ((Double) temp).doubleValue();
					columnName = threeDecFormat.format(val) + " j/s";
				} else {
					Object temp = assumedValues.get(index);
					double val = ((Double) temp).doubleValue();
					columnName = twoDecFormat.format(val) + " %";
				}
			}
			//if single class return the value, else return the percentage
			else if (pad.getType().equals(PA_TYPE_SERVICE_TIMES)) {
				ServiceTimesParametricAnalysis stpa = (ServiceTimesParametricAnalysis) pad;
				Vector<Number> assumedValues = stpa.getParameterValues();
				if (stpa.isSingleClass()) {
					Object temp = assumedValues.get(index);
					double val = ((Double) temp).doubleValue();
					columnName = threeDecFormat.format(val) + " s";
				} else {
					Object temp = assumedValues.get(index);
					double val = ((Double) temp).doubleValue();
					columnName = twoDecFormat.format(val) + " %";
				}
			}
			//for "number of customers" return the number of customers
			else if (pad.getType().equals(PA_TYPE_NUMBER_OF_CUSTOMERS)) {
				Vector<Number> assumedValues = pad.getParameterValues();
				int val = ((Double) (assumedValues.get(index))).intValue();
				columnName = "N = " + Integer.toString(val);
			}
			//for population mix parametric analysis return the value of ß
			else if (pad.getType().equals(PA_TYPE_POPULATION_MIX)) {
				Vector<Number> assumedValues = pad.getParameterValues();
				DecimalFormat threeDec = new DecimalFormat("0.000");
				double value = ((Double) assumedValues.get(index)).doubleValue();
				columnName = "ß = " + threeDec.format(value);
			}
			// if it is a seed parametric analysis just enumerate columns
			else if (pad.getType().equals(PA_TYPE_SEED)) {
				columnName = Integer.toString(index);
				return columnName;
			}
			return columnName;
		}
	}

	/**
	 * A simple JPopupMenu used to manage operations on plot. It gives the
	 * choice to zoom in and out on the plot, restore original view and save plot to
	 * images (in EPS or PNG format)
	 */
	protected class PlotPopupMenu extends JPopupMenu {
		public JMenuItem restore;
		public JMenuItem zoomIn;
		public JMenuItem zoomOut;
		public JMenuItem saveAs;
		public PlotContainer parent;

		public PlotPopupMenu(PlotContainer parent) {
			restore = new JMenuItem("Original view");
			zoomIn = new JMenuItem("Zoom in");
			zoomOut = new JMenuItem("Zoom out");
			saveAs = new JMenuItem("Save as..");
			this.add(restore);
			this.add(zoomIn);
			this.add(zoomOut);
			this.addSeparator();
			this.add(saveAs);
			this.parent = parent;
			addListeners();
		}

		public void addListeners() {
			restore.addActionListener(new AbstractAction() {
				public void actionPerformed(ActionEvent e) {
					parent.resizePlot();
				}
			});

			zoomIn.addActionListener(new AbstractAction() {
				public void actionPerformed(ActionEvent e) {
					parent.zoomIn();
				}
			});

			zoomOut.addActionListener(new AbstractAction() {
				public void actionPerformed(ActionEvent e) {
					parent.zoomOut();
				}
			});

			saveAs.addActionListener(new AbstractAction() {
				public void actionPerformed(ActionEvent e) {
					PlotImagesFileFilter PNGfilter = new PlotImagesFileFilter(".png", "Portable Network Graphics images");
					PlotImagesFileFilter EPSfilter = new PlotImagesFileFilter(".eps", "Encapsulated Post Script images");
					PlotImagesFileChooser fileChooser = new PlotImagesFileChooser(PNGfilter);
					fileChooser.setFileFilter(PNGfilter);
					fileChooser.addChoosableFileFilter(EPSfilter);
					int r = fileChooser.showSaveDialog((Component) parent);
					if (r == JFileChooser.APPROVE_OPTION) {
						File file = fileChooser.getSelectedFile();
						if (fileChooser.getFileFilter().equals(EPSfilter)) {
							PAPlot plot = parent.getPlot();
							try {
								FileOutputStream fileStream = new FileOutputStream(file);
								plot.export(fileStream);
								fileStream.close();
							} catch (FileNotFoundException fnf) {
								JOptionPane.showMessageDialog(fileChooser, "File not found", "JMT - Error", JOptionPane.ERROR_MESSAGE);
							} catch (IOException ioe) {
								JOptionPane.showMessageDialog(fileChooser, "I/O exception", "JMT - Error", JOptionPane.ERROR_MESSAGE);
							}
						} else {
							PAPlot plot = parent.getPlot();
							BufferedImage image = plot.exportImage();
							try {
								int targetType = BufferedImage.TYPE_INT_RGB;
								BufferedImage originalImage = convertType(image, targetType);
								ImageIO.write(originalImage, "png", file);
								ImageIO.createImageOutputStream(file).close();
							} catch (IOException fnf) {
								JOptionPane.showMessageDialog(null, "File not found");
							}
						}
					}
				}

			});
		}

		BufferedImage convertType(BufferedImage src, int targetType) {
			if (src.getType() == targetType) {
				return src;
			}
			BufferedImage tgt = new BufferedImage(src.getWidth(), src.getHeight(), targetType);
			Graphics2D g = tgt.createGraphics();
			g.drawRenderedImage(src, null);
			g.dispose();
			return tgt;
		}
	}

	/**
	 * Custom file chooser class
	 */
	protected static class PlotImagesFileChooser extends JFileChooser {
		protected PlotImagesFileFilter defaultFilter;

		/**
		 * Creates a File chooser in the appropriate directory user deafault.
		 * @param defaultFilter default file filter
		 */
		public PlotImagesFileChooser(PlotImagesFileFilter defaultFilter) {
			super(new File(System.getProperty("user.dir")));
			this.defaultFilter = defaultFilter;
		}

		/**
		 * Overrides default method to provide a warning if saving over an existing file
		 */
		public void approveSelection() {
			// Gets the choosed file name
			String name = getSelectedFile().getName();
			String parent = getSelectedFile().getParent();
			if (getDialogType() == OPEN_DIALOG) {
				super.approveSelection();
			}
			if (getDialogType() == SAVE_DIALOG) {
				PlotImagesFileFilter used = ((PlotImagesFileFilter) this.getFileFilter());
				if (!name.toLowerCase().endsWith(used.getExtension())) {
					name = name + used.getExtension();
					setSelectedFile(new File(parent, name));
				}
				if (getSelectedFile().exists()) {
					int resultValue = JOptionPane.showConfirmDialog(this, "<html>File <font color=#0000ff>" + name
							+ "</font> already exists in this folder.<br>Do you want to replace it?</html>", "JMT - Warning",
							JOptionPane.OK_CANCEL_OPTION, JOptionPane.WARNING_MESSAGE);
					if (resultValue == JOptionPane.OK_OPTION) {
						getSelectedFile().delete();
						super.approveSelection();
					}
				} else {
					super.approveSelection();
				}
			}
		}
	}

	/**
	 * Inner class used to create simple file filters with only extension check
	 */
	protected static class PlotImagesFileFilter extends javax.swing.filechooser.FileFilter {
		private String extension, description;

		/**
		 * Creates a new filefilter with specified extension and description
		 * @param extension extension of this filter (for example ".jmt")
		 * @param description description of this filter
		 */
		public PlotImagesFileFilter(String extension, String description) {
			this.extension = extension;
			this.description = description;
		}

		/**
		 * Whether the given file is accepted by this filter.
		 */
		public boolean accept(File f) {
			String name = f.getName().toLowerCase();
			return name.endsWith(extension) || f.isDirectory();
		}

		/**
		 * The description of this filter
		 * @see javax.swing.filechooser.FileView#getName
		 */
		public String getDescription() {
			return description + " (*" + extension + ")";
		}

		/**
		 * Gets extension of this filter
		 * @return extension of this filter
		 */
		public String getExtension() {
			return extension;
		}
	}
}
