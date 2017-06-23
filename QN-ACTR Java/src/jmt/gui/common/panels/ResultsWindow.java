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
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowEvent;
import java.text.DecimalFormat;
import java.util.Vector;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.SpringLayout;
import javax.swing.WindowConstants;

import jmt.framework.gui.components.JMTFrame;
import jmt.framework.gui.graph.FastGraph;
import jmt.framework.gui.graph.MeasureValue;
import jmt.framework.gui.layouts.SpringUtilities;
import jmt.gui.common.definitions.AbortMeasure;
import jmt.gui.common.definitions.MeasureDefinition;
import jmt.gui.common.definitions.ResultsConstants;
import jmt.gui.common.definitions.SimulationDefinition;
import jmt.gui.common.resources.JMTImageLoader;
import jmt.gui.common.xml.XMLConstantNames;

/**
 * <p>Title: Results Window</p>
 * <p>Description: Window to show simulation results.</p>
 * 
 * @author Bertoli Marco
 *         Date: 22-set-2005
 *         Time: 15.10.52
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
public class ResultsWindow extends JMTFrame implements ResultsConstants {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private MeasureDefinition results;
	private AbortMeasure abort;
	private JButton start, stop, pause;
	private JProgressBar progressBar;
	// Used to format numbers
	private static DecimalFormat decimalFormatExp = new DecimalFormat("0.000E0");
	private static DecimalFormat decimalFormatNorm = new DecimalFormat("#0.0000");

	/**
	 * Creates a new ResultsWindow
	 * @param results results data structure
	 */
	public ResultsWindow(MeasureDefinition results) {
		this.results = results;
		initGUI();
	}

	/**
	 * Creates a new ResultsWindow
	 * @param results results data structure
	 * @param abort object used to implement abort button action
	 */
	public ResultsWindow(MeasureDefinition results, AbortMeasure abort) {
		this.results = results;
		this.abort = abort;
		initGUI();
	}

	/**
	 * Initialize allgui related stuff
	 */
	private void initGUI() {
		// Sets default title, close operation and dimensions
		this.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
		this.setTitle("Simulation Results...");
		this.setIconImage(JMTImageLoader.loadImage("Results").getImage());
		int width = 800, height = 500;
		this.centerWindow(width, height);

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
		addTabPane(mainPanel, "System Response Time", DESCRIPTION_SYSTEMRESPONSETIMES, results.getSystemResponseTimeMeasures());
		addTabPane(mainPanel, "System Throughput", DESCRIPTION_SYSTEMTHROUGHPUTS, results.getSystemThroughputMeasures());
		addTabPane(mainPanel, "System Drop Rate", DESCRIPTION_DROPRATE, results.getSystemDropRateMeasures());
		addTabPane(mainPanel, "System Number of Customers", DESCRIPTION_CUSTOMERNUMBERS, results.getCustomerNumberMeasures());
		//Added by ASHANKA START
		//Adds the System Power panel in the results window
		addTabPane(mainPanel, "System Power", DESCRIPTION_SYSTEMPOWER, results.getSystemPowerMeasures());
		//Added by ASHANKA STOP
		addTabPane(mainPanel, SimulationDefinition.MEASURE_R_PER_SINK, DESCRIPTION_RESPONSETIME_SINK, results.getResponsetimePerSinkMeasures());
		addTabPane(mainPanel, SimulationDefinition.MEASURE_X_PER_SINK, DESCRIPTION_THROUGHPUT_SINK, results.getThroughputPerSinkMeasures());
		// Creates bottom toolbar
		JToolBar toolbar = new JToolBar();
		toolbar.setFloatable(false);
		toolbar.setRollover(true);
		start = new JButton();
		toolbar.add(start);
		start.setVisible(false);
		pause = new JButton();
		toolbar.add(pause);
		pause.setVisible(false);
		stop = new JButton();
		toolbar.add(stop);
		stop.setVisible(false);
		// Adds a progress bar
		progressBar = new JProgressBar();
		progressBar.setStringPainted(true);
		progressBar.setForeground(Color.BLUE);
		setProgressBar(results.getProgressTime());
		toolbar.add(progressBar);
		// Add close window button
		JButton close = new JButton();
		close.setIcon(JMTImageLoader.loadImage("Close"));
		close.setFocusPainted(false);
		close.setContentAreaFilled(false);
		close.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 0));
		close.setRolloverIcon(JMTImageLoader.loadImage("CloseRO"));
		close.setPressedIcon(JMTImageLoader.loadImage("CloseP"));
		close.setVisible(true);
		close.setToolTipText("Closes this window");
		close.addActionListener(new ActionListener() {
			// Fires a window closing event
			public void actionPerformed(ActionEvent e) {
				ResultsWindow.this.dispatchEvent(new WindowEvent(ResultsWindow.this, WindowEvent.WINDOW_CLOSING));
			}
		});

		toolbar.add(close);
		// Adds toolbar
		this.getContentPane().add(toolbar, BorderLayout.SOUTH);

		// Adds listener for progressBar
		results.setProgressTimeListener(new MeasureDefinition.ProgressTimeListener() {
			public void timeChanged(double time) {
				setProgressBar(time);
			}
		});

	}

	/**
	 * Sets progress bar to specified value
	 * @param percent value. Must be between 0 included and 1 included
	 */
	private void setProgressBar(double percent) {
		int progress = (int) Math.round(percent * 100);
		progressBar.setValue(progress);
		if (progress < 100) {
			progressBar.setString(progress + "% of simulation performed...");
		} else {
			progressBar.setString("Simulation Complete");
		}
	}

	/**
	 * Sets action for toolbar buttons and displays them
	 * @param startAction action associated with start simulation button
	 * @param pauseAction action associated with pause simulation button
	 * @param stopAction action associated with stop simulation button
	 */
	public void addButtonActions(AbstractAction startAction, AbstractAction pauseAction, AbstractAction stopAction) {
		start.setAction(startAction);
		start.setText("");
		start.setIcon(JMTImageLoader.loadImage("Sim"));
		start.setFocusPainted(false);
		start.setContentAreaFilled(false);
		start.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 0));
		start.setRolloverIcon(JMTImageLoader.loadImage("SimRO"));
		start.setPressedIcon(JMTImageLoader.loadImage("SimP"));
		start.setVisible(true);

		pause.setAction(pauseAction);
		pause.setText("");
		pause.setIcon(JMTImageLoader.loadImage("Pause"));
		pause.setFocusPainted(false);
		pause.setContentAreaFilled(false);
		pause.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 0));
		pause.setRolloverIcon(JMTImageLoader.loadImage("PauseRO"));
		pause.setPressedIcon(JMTImageLoader.loadImage("PauseP"));
		pause.setVisible(true);

		stop.setAction(stopAction);
		stop.setText("");
		stop.setIcon(JMTImageLoader.loadImage("Stop"));
		stop.setFocusPainted(false);
		stop.setContentAreaFilled(false);
		stop.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 0));
		stop.setRolloverIcon(JMTImageLoader.loadImage("StopRO"));
		stop.setPressedIcon(JMTImageLoader.loadImage("StopP"));
		stop.setVisible(true);
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
				scroll.add(new MeasurePanel(results, indexe));
			}
			mainpanel.add(new JScrollPane(scroll), BorderLayout.CENTER);

			// Adds tab to parent tabbed pane
			parent.addTab(name, tabPanel);
		}
	}

	/**
	 * Helper method used to convert a double into a String avoiding too many decimals
	 * @param d number to be converted
	 * @return string rappresentation of input number
	 */
	protected static String doubleToString(double d) {
		double abs = (d >= 0) ? d : -d;
		if (abs == 0) {
			return "0.0";
		} else if (abs > 1e4 || abs < 1e-2) {
			return decimalFormatExp.format(d);
		} else {
			return decimalFormatNorm.format(d);
		}
	}

	/**
	 * Inner class to create a panel that holds a specified measure
	 */
	protected class MeasurePanel extends JPanel {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		protected MeasureDefinition md;
		protected int measureIndex;
		protected JLabel icon;
		protected Vector values;
		protected JTextField samples, mean, lower, upper;
		protected JButton abortButton;
		protected FastGraph graph, popupGraph;
		protected JPanel graphPanel;
		protected JFrame popupFrame;
		protected JTextArea textState;

		public MeasurePanel(MeasureDefinition md, int measureIndex) {
			this.md = md;
			this.measureIndex = measureIndex;
			values = md.getValues(measureIndex);
			createPanel();
			addListeners();
		}

		/**
		 * Used to create the panel holding all measure's data
		 */
		protected void createPanel() {
			this.setLayout(new BorderLayout(5, 5));
			this.setBorder(BorderFactory.createRaisedBevelBorder());
			// Sets correct icon for this measure
			icon = new JLabel();
			icon.setIcon(JMTImageLoader.loadImage(IN_PROGRESS_IMAGE));
			add(icon, BorderLayout.WEST);

			//Adds mainPanel with all informations on this measure
			JLabel label;
			JTextField field;
			JPanel mainPanel = new JPanel(new SpringLayout());
			// Station name
			label = new JLabel("Station Name: ");
			if (md.getNodeType(measureIndex).equalsIgnoreCase(XMLConstantNames.NODETYPE_REGION)) {
				label.setText("Region Name: ");
			}
			field = new JTextField();
			field.setEditable(false);
			field.setMaximumSize(new Dimension(field.getMaximumSize().width, field.getMinimumSize().height));
			label.setLabelFor(field);
			// If station name is undefined, disables its fields
			String stationName = md.getStationName(measureIndex);
			if (stationName != null && !stationName.equals("")) {
				field.setText(stationName);
				field.setToolTipText("Name of the station: " + field.getText());
			} else {
				field.setText(ALL_STATIONS);
				field.setToolTipText("This measure is referred to the entire network");
			}
			mainPanel.add(label);
			mainPanel.add(field);
			// Class name
			label = new JLabel("Class Name: ");
			field = new JTextField();
			field.setEditable(false);
			field.setMaximumSize(new Dimension(field.getMaximumSize().width, field.getMinimumSize().height));
			label.setLabelFor(field);
			// If class name is undefined, shows ALL_CLASSES constant
			String className = md.getClassName(measureIndex);
			if (className != null && !className.equals("")) {
				field.setText(className);
				field.setToolTipText("Name of the class: " + field.getText());
			} else {
				field.setText(ResultsConstants.ALL_CLASSES);
				field.setToolTipText("This measure is an aggregate of every class");
			}
			mainPanel.add(label);
			mainPanel.add(field);
			// Alpha/Precision
			label = new JLabel("Conf.Int/Max Rel.Err: ");
			field = new JTextField();
			field.setEditable(false);
			field.setMaximumSize(new Dimension(field.getMaximumSize().width, field.getMinimumSize().height));
			label.setLabelFor(field);
			field.setText(md.getAlpha(measureIndex) + " / " + md.getPrecision(measureIndex)); // AnalyzedSamples
			field.setToolTipText("Confidence Interval and Maximum Relative Error requested for this measure: " + field.getText());
			mainPanel.add(label);
			mainPanel.add(field);
			// Analyzed samples
			label = new JLabel("Analyzed samples: ");
			samples = new JTextField();
			samples.setEditable(false);
			samples.setMaximumSize(new Dimension(samples.getMaximumSize().width, samples.getMinimumSize().height));
			label.setLabelFor(samples);
			samples.setText("" + md.getAnalizedSamples(measureIndex));
			samples.setToolTipText("Number of samples currently analized: " + samples.getText());
			mainPanel.add(label);
			mainPanel.add(samples);

			MeasureValue lastValue = (MeasureValue) values.lastElement();

			// Lower Bound
			label = new JLabel("Min: ");
			lower = new JTextField("-");
			lower.setEditable(false);
			lower.setMaximumSize(new Dimension(lower.getMaximumSize().width, lower.getMinimumSize().height));
			label.setLabelFor(lower);
			if (lastValue.getLowerBound() > 0 && !Double.isInfinite(lastValue.getLowerBound())) {
				lower.setText(doubleToString(lastValue.getLowerBound()));
			}
			lower.setToolTipText("Minimum value of current confidence interval: " + lower.getText());
			mainPanel.add(label);
			mainPanel.add(lower);

			// Upper Bound
			label = new JLabel("Max: ");
			upper = new JTextField("-");
			upper.setEditable(false);
			upper.setMaximumSize(new Dimension(upper.getMaximumSize().width, upper.getMinimumSize().height));
			label.setLabelFor(upper);
			if (lastValue.getUpperBound() > 0 && !Double.isInfinite(lastValue.getUpperBound())) {
				upper.setText(doubleToString(lastValue.getUpperBound()));
			}
			upper.setToolTipText("Maximum value of current confidence interval: " + upper.getText());
			mainPanel.add(label);
			mainPanel.add(upper);

			SpringUtilities.makeCompactGrid(mainPanel, 3, 4, //rows, cols
					6, 6, //initX, initY
					6, 6);//xPad, yPad
			// Temp mean and abort button are threated in a separate panel
			JPanel bottomPanel = new JPanel(new BorderLayout(6, 6));
			label = new JLabel(TEMP_MEAN);
			mean = new JTextField();
			mean.setEditable(false);
			mean.setToolTipText("Current mean value of this measure: " + mean.getText());
			label.setLabelFor(mean);
			mean.setText(doubleToString(((MeasureValue) values.lastElement()).getMeanValue()));
			bottomPanel.add(label, BorderLayout.WEST);
			bottomPanel.add(mean, BorderLayout.CENTER);
			// AbortButton
			abortButton = new JButton();
			abortButton.setText("Abort Measure");
			bottomPanel.add(abortButton, BorderLayout.EAST);
			bottomPanel.setBorder(BorderFactory.createEmptyBorder(0, 6, 6, 6));

			JPanel pivotPanel = new JPanel(new BorderLayout());
			pivotPanel.add(mainPanel, BorderLayout.CENTER);
			pivotPanel.add(bottomPanel, BorderLayout.SOUTH);
			// Pack text area in the north of the panel
			JPanel pivotPanel2 = new JPanel(new BorderLayout());
			pivotPanel2.add(pivotPanel, BorderLayout.NORTH);
			// Adds a textPanel to show state
			textState = new JTextArea();
			textState.setEditable(false);
			textState.setLineWrap(true);
			textState.setWrapStyleWord(true);
			JPanel textStatePanel = new JPanel(new BorderLayout());
			textStatePanel.add(textState, BorderLayout.CENTER);
			textStatePanel.setBorder(BorderFactory.createEmptyBorder(BORDERSIZE / 2, BORDERSIZE / 2, BORDERSIZE / 2, BORDERSIZE / 2));
			pivotPanel2.add(textStatePanel, BorderLayout.SOUTH);
			// Sets a minimal size for text area panel
			pivotPanel2.setPreferredSize(new Dimension(360, 150));

			// Adds graph
			graphPanel = new JPanel(new BorderLayout());
			graph = new FastGraph(values, md.getPollingInterval());
			graph.setToolTipText("Double click on this graph to open it in a new window");
			graphPanel.add(graph, BorderLayout.CENTER);
			graphPanel.add(pivotPanel2, BorderLayout.WEST);
			add(graphPanel, BorderLayout.CENTER);

			// Sets icon image and abort button state
			setCorrectState();
		}

		/**
		 * Adds listeners to this panel, to refresh measures and abort simulation
		 */
		protected void addListeners() {
			if (md.getMeasureState(measureIndex) == MeasureDefinition.MEASURE_IN_PROGRESS) {
				// If simulation is not finished, adds a measure listener
				md.addMeasureListener(measureIndex, new MeasureDefinition.MeasureListener() {
					public void measureChanged(Vector measureValues, boolean finished) {
						// Update graphics
						graph.repaint();
						if (popupGraph != null) {
							popupGraph.repaint();
						}

						MeasureValue lastValue = (MeasureValue) measureValues.lastElement();

						// Updates mean, lower, upper and samples
						if (lastValue.getLowerBound() > 0 && !Double.isInfinite(lastValue.getUpperBound())) {
							lower.setText(doubleToString(lastValue.getLowerBound()));
							lower.setToolTipText("Minimum value of current confidence interval: " + lower.getText());
							upper.setText(doubleToString(lastValue.getUpperBound()));
							upper.setToolTipText("Maximum value of current confidence interval: " + upper.getText());
						} else {
							lower.setText("-");
							upper.setText("-");
						}

						mean.setText(doubleToString(lastValue.getMeanValue()));
						mean.setToolTipText("Current mean value of this measure: " + mean.getText());
						samples.setText("" + md.getAnalizedSamples(measureIndex));
						samples.setToolTipText("Number of samples currently analized: " + samples.getText());
						// If finished is true, state was changed
						if (finished) {
							setCorrectState();
						}
						repaint();
					}
				});
				// Sets AbortButton action, only if abort is available
				if (abort != null) {
					abortButton.addActionListener(new ActionListener() {
						public void actionPerformed(ActionEvent e) {
							abortButton.setEnabled(false);
							abort.abortMeasure(measureIndex);
						}
					});
				}
			}
			// Popups graph if graph window is double clicked
			graph.addMouseListener(new MouseAdapter() {
				@Override
				public void mouseClicked(MouseEvent e) {
					if (e.getClickCount() == 2) {
						if (popupFrame == null) {
							popupFrame = new JFrame();
							popupGraph = new FastGraph(values, md.getPollingInterval());
							popupFrame.getContentPane().add(popupGraph);
							popupFrame.setTitle(md.getName(measureIndex));
							popupFrame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
							int width = 640, height = 480;
							Dimension scrDim = Toolkit.getDefaultToolkit().getScreenSize();
							popupFrame.setBounds((scrDim.width - width) / 2, (scrDim.height - height) / 2, width, height);
						}
						popupFrame.show();
					}
				}
			});

		}

		/**
		 * Sets correct state to icon and abort button
		 */
		protected void setCorrectState() {
			switch (md.getMeasureState(measureIndex)) {
				case MeasureDefinition.MEASURE_IN_PROGRESS:
					icon.setIcon(JMTImageLoader.loadImage(IN_PROGRESS_IMAGE));
					icon.setToolTipText(IN_PROGRESS_TEXT);
					textState.setText(IN_PROGRESS_TEXT);
					abortButton.setEnabled(true);
					break;
				case MeasureDefinition.MEASURE_SUCCESS:
					icon.setIcon(JMTImageLoader.loadImage(SUCCESS_IMAGE));
					icon.setToolTipText(SUCCESS_TEXT);
					textState.setText(SUCCESS_TEXT);
					abortButton.setEnabled(false);
					break;
				case MeasureDefinition.MEASURE_FAILED:
					icon.setIcon(JMTImageLoader.loadImage(FAILED_IMAGE));
					icon.setToolTipText(FAILED_TEXT);
					textState.setText(FAILED_TEXT);
					abortButton.setEnabled(false);
					break;
				case MeasureDefinition.MEASURE_NO_SAMPLES:
					icon.setIcon(JMTImageLoader.loadImage(NO_SAMPLES_IMAGE));
					icon.setToolTipText(NO_SAMPLES_TEXT);
					textState.setText(NO_SAMPLES_TEXT);
					abortButton.setEnabled(false);
					graph.setVisible(false); // Hides graph if no samples were received
					break;
			}
		}
	}
}
