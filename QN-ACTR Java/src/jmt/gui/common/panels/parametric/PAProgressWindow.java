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
import java.awt.Frame;
import java.awt.GridBagLayout;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JToolBar;
import javax.swing.border.EmptyBorder;

import jmt.framework.gui.components.JMTDialog;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.definitions.parametric.ParametricAnalysisDefinition;
import jmt.gui.common.resources.JMTImageLoader;
import jmt.gui.common.startScreen.sampleAnimation.SampleQNAnimation;

/**
 * <p>Title: Parametric Analysis Progress Window</p>
 * <p>Description: This window is shown during parametric analysis</p>
 *
 * @author Francesco D'Aquino
 *         Date: 4-gen-2006
 *         Time: 13.53.23
 */
public class PAProgressWindow extends JMTDialog {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	public static final String RUNNING_MESSAGE = "What-if analysis running, please wait...";
	public static final String ENUMERATION_MESSAGE = "Running simulation";
	public static final String PAUSE_MESSAGE = "Parametric analysis paused...";
	public static final String STOP_MESSAGE = "Parametric analysis stopped";
	//private final double SIMULATION_DURATION_INITIALIZATION = 0;
	private static final double HISTOGRAM_INTERVALS = 10;
	private JButton start, stop, pause;
	private JToolBar toolbar;
	private JProgressBar progressBar;
	private SampleQNAnimation animation;
	private JLabel mainLabel;
	private JLabel stepLabel;
	private JLabel elapsedTimeLabel;
	//private JLabel remainingTimeLabel;

	JLabel stoppedAnimation;

	private int numberOfSimulations;
	private int stepNumber;
	private JPanel globalPanel;
	private JPanel pivotPanel;
	private boolean stopped, paused;
	private String elapsedTimeString;
	private String remainingTimeString;

	private Timer timer;
	//private double MA_cumulativeTime;
	private double[] simulationTimes;
	private double globalCumulativeTime;
	private double globalMeanSimulationTime;
	private double lastSimulationFinishTime;
	private double globalStartTime;

	boolean wasPaused;
	double pauseTime;
	double resumeTime;

	private double elapsedTime;

	public PAProgressWindow(Frame owner, AbstractAction startAction, AbstractAction pauseAction, final AbstractAction stopAction,
			ParametricAnalysisDefinition pad) {
		super(owner, false);
		numberOfSimulations = pad.getNumberOfSteps();
		stepNumber = 1;
		this.getContentPane().setLayout(new BorderLayout());
		this.setTitle("What-if analysis progress");

		globalCumulativeTime = 0;
		globalMeanSimulationTime = 0;

		this.centerWindow(500, 390);

		// Creates bottom toolbar
		toolbar = new JToolBar();
		toolbar.setFloatable(false);
		toolbar.setRollover(true);
		start = new JButton();
		toolbar.add(start);
		pause = new JButton();
		toolbar.add(pause);
		stop = new JButton();
		toolbar.add(stop);

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

		// Adds a progress bar
		progressBar = new JProgressBar();
		progressBar.setStringPainted(true);
		progressBar.setForeground(Color.BLUE);
		toolbar.add(progressBar);

		// Adds toolbar
		this.getContentPane().add(toolbar, BorderLayout.SOUTH);

		//Creates and adds the animation panel
		//animation = new SampleQNAnimation();
		//animation.start();
		pivotPanel = new JPanel(new GridBagLayout());
		//animation.setPreferredSize(new Dimension(200,120));
		//animation.setBackground(new Color(151,151,151));
		stoppedAnimation = new JLabel(JMTImageLoader.loadImage("emptyAnimation"));
		stoppedAnimation.setPreferredSize(new Dimension(200, 120));
		pivotPanel.add(stoppedAnimation);

		//Creates and adds the description label
		JPanel mainLabelPanel = new JPanel();
		mainLabel = new JLabel(CommonConstants.HTML_START + CommonConstants.HTML_FONT_TITLE + RUNNING_MESSAGE + CommonConstants.HTML_FONT_TIT_END
				+ CommonConstants.HTML_END);
		mainLabelPanel.add(mainLabel);

		elapsedTimeString = "00m : 00s";
		remainingTimeString = "-- : --";
		elapsedTimeLabel = new JLabel("Elapsed time: " + elapsedTimeString);
		JPanel elapsedTimeLabelPanel = new JPanel();
		elapsedTimeLabelPanel.add(elapsedTimeLabel);
		//remainingTimeLabel = new JLabel("Residual time: " + remainingTimeString);
		JPanel remainingTimeLabelPanel = new JPanel();
		//remainingTimeLabelPanel.add(remainingTimeLabel);
		remainingTimeLabelPanel.setBorder(new EmptyBorder(10, 0, 10, 0));
		JPanel tempPanel = new JPanel(new BorderLayout());
		tempPanel.add(elapsedTimeLabel, BorderLayout.NORTH);
		//tempPanel.add(remainingTimeLabel,BorderLayout.SOUTH);
		JPanel timeLabelPanel = new JPanel();
		timeLabelPanel.add(tempPanel);
		timeLabelPanel.setBorder(new EmptyBorder(10, 0, 0, 0));
		JPanel stepLabelPanel = new JPanel();
		stepLabel = new JLabel(CommonConstants.HTML_START + CommonConstants.HTML_FONT_NORM + ENUMERATION_MESSAGE + " " + stepNumber + " of "
				+ this.numberOfSimulations + "..." + CommonConstants.HTML_FONT_NOR_END + CommonConstants.HTML_END);
		stepLabelPanel.add(stepLabel);
		JPanel lowerLabelPanel = new JPanel(new BorderLayout());
		lowerLabelPanel.add(timeLabelPanel, BorderLayout.CENTER);
		lowerLabelPanel.add(stepLabelPanel, BorderLayout.SOUTH);

		mainLabelPanel.setBorder(new EmptyBorder(20, 20, 10, 20));
		stepLabelPanel.setBorder(new EmptyBorder(20, 20, 20, 20));
		JPanel upperPanel = new JPanel(new BorderLayout());
		upperPanel.add(mainLabelPanel, BorderLayout.CENTER);
		//upperPanel.add(timeLabelPanel,BorderLayout.SOUTH);
		globalPanel = new JPanel(new BorderLayout());
		globalPanel.add(upperPanel, BorderLayout.NORTH);
		globalPanel.add(pivotPanel, BorderLayout.CENTER);
		globalPanel.add(lowerLabelPanel, BorderLayout.SOUTH);
		this.getContentPane().add(globalPanel, BorderLayout.CENTER);

		elapsedTime = 0;
	}

	/* (non-Javadoc)
	 * @see jmt.framework.gui.components.JMTDialog#canBeClosed()
	 */
	@Override
	public boolean canBeClosed() {
		return stopped;
	}

	/* (non-Javadoc)
	 * @see jmt.framework.gui.components.JMTDialog#doClose()
	 */
	@Override
	protected void doClose() {
		stop.getAction().actionPerformed(null);
	}

	public void initialize(int numberOfStep) {
		simulationTimes = new double[numberOfStep];
		this.numberOfSimulations = numberOfStep;
		progressBar.setMaximum(numberOfSimulations);
	}

	/**
	 * Sets the running number of step
	 * @param step the step to be run
	 */
	public void setStepNumber(int step) {
		stepNumber = step;
		stepLabel.setText(CommonConstants.HTML_START + CommonConstants.HTML_FONT_NORM + ENUMERATION_MESSAGE + " " + stepNumber + " of "
				+ this.numberOfSimulations + "..." + CommonConstants.HTML_FONT_NOR_END + CommonConstants.HTML_END);
		progressBar.setValue(step - 1);
		updateStatistics(step - 1);
	}

	/**
	 * Updates the showed elapsed time
	 */
	public void updateTime() {
		elapsedTimeString = this.getTimeString((int) elapsedTime);
		elapsedTimeLabel.setText("Elapsed time: " + elapsedTimeString);
	}

	/**
	 * This method updates all statistics related stuff. It contains a simple alghorithm to estimate residual execution time
	 * @param lastSimulationCompletedIndex the index of the last finished simulation, beggining from 1.
	 */
	private void updateStatistics(int lastSimulationCompletedIndex) {
		//it happens only at the beginning of simulation
		if (lastSimulationCompletedIndex == 0) {
			return;
		}

		// INITIALIZATIONS //

		//set the precision of residual time estimation
		double histogramIntervals = HISTOGRAM_INTERVALS;
		double currentTime = (((double) System.currentTimeMillis()) / 1000 - //get time elapsed from start in seconds
		this.globalStartTime);

		double simulationDuration;
		if (!wasPaused) {
			simulationDuration = currentTime - lastSimulationFinishTime; // the duration of the last finished simulation
		} else {
			simulationDuration = currentTime - lastSimulationFinishTime - (resumeTime - pauseTime);
			wasPaused = false;
		}
		lastSimulationFinishTime = currentTime; //set the new last simulation finishing time
		globalCumulativeTime += simulationDuration; //increase the cumulative time
		globalMeanSimulationTime = globalCumulativeTime / lastSimulationCompletedIndex; //the mean duration time

		double min = simulationDuration; //initialization useful for the first simulation finished
		double max = simulationDuration;

		// END INITIALIZATIONS//

		simulationTimes[lastSimulationCompletedIndex - 1] = simulationDuration; //insert last simulation duration into the Vector

		//for cycle used to get minimum and maximum simulaion duration
		for (int i = 0; i < lastSimulationCompletedIndex; i++) {
			if (simulationTimes[i] < min) {
				min = simulationTimes[i];
			}
			if (simulationTimes[i] > max) {
				max = simulationTimes[i];
			}
		}

		int estimatedTime;
		int[] hist; // it's a vector containing the number of simulation with a duration in a certain range.
		// It's like a histogram; the algorithm uses the relative frequencies to estimate the most
		// probable residual time.

		double dispersion = max - min;

		if (dispersion != 0) { // always true except for the first case, or if all of the simulations have the same duration

			double gap = dispersion / histogramIntervals; //the width of a single histogram band
			hist = new int[(int) histogramIntervals];

			// for cycle used to populate the histogram
			for (int i = 0; i < lastSimulationCompletedIndex; i++) {
				double thisValue = simulationTimes[i];
				for (int j = 1; j <= histogramIntervals; j++) {
					if (thisValue <= (min + j * gap)) {
						hist[j - 1]++;
						break;
					}
				}
			}

			double sum = 0;
			for (int i = 0; i < histogramIntervals; i++) {
				sum += ((hist[i])) * (min + (i + 1) * gap - gap / 2); //weighted sum of all simulation durations
			}
			estimatedTime = (int) (sum / (lastSimulationCompletedIndex) * (numberOfSimulations - lastSimulationCompletedIndex));
		} else {
			estimatedTime = (int) (max * numberOfSimulations - lastSimulationCompletedIndex); //or min since they are the same number
		}
		this.remainingTimeString = "Residual time: " + getTimeString(estimatedTime);
		//this.remainingTimeLabel.setText(remainingTimeString);
	}

	/**
	 * Used to format a time in seconds to a hh : mm : ss format
	 * @param t time in seconds to be formatted
	 * @return the formatted time
	 */
	private String getTimeString(int t) {
		String timeString;
		int s = (t % 60);
		int m = ((t / 60) % 60);
		int h = ((t / 3600) % 3600);
		if (h == 0) {
			timeString = "";
		} else {
			if (h < 10) {
				timeString = "0" + Integer.toString(h) + "h : ";
			} else {
				timeString = Integer.toString(h) + "h : ";
			}
		}
		if (m < 10) {
			timeString += "0" + Integer.toString(m) + "m : ";
		} else {
			timeString += Integer.toString(m) + "m : ";
		}
		if (s < 10) {
			timeString += "0" + Integer.toString(s) + "s";
		} else {
			timeString += Integer.toString(s) + "s";
		}
		return timeString;
	}

	/**
	 * Sets the window in pause state
	 */
	public void pause() {
		wasPaused = true;
		pauseTime = ((double) System.currentTimeMillis()) / 1000 - globalStartTime;
		mainLabel.setText(CommonConstants.HTML_START + CommonConstants.HTML_FONT_TITLE + PAUSE_MESSAGE + CommonConstants.HTML_FONT_TIT_END
				+ CommonConstants.HTML_END);
		stopAnimation();
		paused = true;
		stopped = false;
		removeAnimation();
	}

	/**
	 * Called to start the progress window
	 */
	public void start() {
		globalStartTime = ((double) System.currentTimeMillis()) / 1000;
		lastSimulationFinishTime = 0;
		elapsedTime = 0;
		paused = false;
		stopped = false;
		timer = new Timer();
		timer.start();
		mainLabel.setText(CommonConstants.HTML_START + CommonConstants.HTML_FONT_TITLE + RUNNING_MESSAGE + CommonConstants.HTML_FONT_TIT_END
				+ CommonConstants.HTML_END);
		restoreAnimation();
	}

	/**
	 * Called to restart the progress window after it was paused
	 */
	public void restart() {
		paused = false;
		stopped = false;
		resumeTime = ((double) System.currentTimeMillis()) / 1000 - globalStartTime;
		timer.setLastTime(resumeTime);
		mainLabel.setText(CommonConstants.HTML_START + CommonConstants.HTML_FONT_TITLE + RUNNING_MESSAGE + CommonConstants.HTML_FONT_TIT_END
				+ CommonConstants.HTML_END);
		restoreAnimation();
	}

	private void removeAnimation() {
		globalPanel.remove(pivotPanel);
		pivotPanel = new JPanel(new GridBagLayout());
		pivotPanel.add(stoppedAnimation);
		globalPanel.add(pivotPanel, BorderLayout.CENTER);
		globalPanel.doLayout();
		globalPanel.validate();
		doLayout();
		validate();
	}

	private void restoreAnimation() {
		globalPanel.remove(pivotPanel);
		animation = new SampleQNAnimation();
		animation.start();
		pivotPanel = new JPanel(new GridBagLayout());
		animation.setPreferredSize(new Dimension(200, 120));
		animation.setBackground(new Color(151, 151, 151));
		pivotPanel.add(animation);
		globalPanel.add(pivotPanel, BorderLayout.CENTER);
		globalPanel.doLayout();
		globalPanel.validate();
		doLayout();
		validate();
	}

	/**
	 * Called when simulation is stopped.
	 */
	public void stop() {
		paused = false;
		stopped = true;
		mainLabel.setText(CommonConstants.HTML_START + CommonConstants.HTML_FONT_TITLE + STOP_MESSAGE + CommonConstants.HTML_FONT_TIT_END
				+ CommonConstants.HTML_END);
		if (stepNumber > 1) {
			stepLabel.setText(CommonConstants.HTML_START + CommonConstants.HTML_FONT_NORM + "Only " + (stepNumber - 1) + " simulations of "
					+ numberOfSimulations + " were completed " + CommonConstants.HTML_FONT_NOR_END + CommonConstants.HTML_END);
		} else {
			stepLabel.setText(CommonConstants.HTML_START + CommonConstants.HTML_FONT_NORM + "No simulation was completed "
					+ CommonConstants.HTML_FONT_NOR_END + CommonConstants.HTML_END);
		}

		stopAnimation();
		removeAnimation();
		progressBar.setValue(0);
		remainingTimeString = "Residual time: -- : --";
		//remainingTimeLabel.setText(remainingTimeString);
	}

	/**
	 * Stops only the animation thread
	 */
	public void stopAnimation() {
		animation.stop();
	}

	/**
	 * Called when the parametric analyis is finished, not when it is stopped
	 */
	public void finished() {
		stopped = true;
		paused = false;
		mainLabel.setText(CommonConstants.HTML_START + CommonConstants.HTML_FONT_TITLE + "Simulation complete" + CommonConstants.HTML_FONT_TIT_END
				+ CommonConstants.HTML_END);
		stepLabel.setText(CommonConstants.HTML_START + CommonConstants.HTML_FONT_NORM + "All of the " + this.numberOfSimulations
				+ " simulations were completed" + CommonConstants.HTML_FONT_NOR_END + CommonConstants.HTML_END);

		progressBar.setValue(numberOfSimulations);
		remainingTimeString = "Residual time: -- : --";
		//remainingTimeLabel.setText(remainingTimeString);
		stopAnimation();
		removeAnimation();
	}

	protected class Timer extends Thread {
		double lastTime;

		public Timer() {
			super("Progress timer");
			this.setPriority(Thread.MAX_PRIORITY);
		}

		public void setLastTime(double timeInSec) {
			lastTime = timeInSec;
		}

		@Override
		public void run() {
			lastTime = ((double) System.currentTimeMillis()) / 1000 - globalStartTime;
			while (!stopped) {
				try {
					sleep(1000);
				} catch (InterruptedException ie) {
					JOptionPane.showMessageDialog(null, "Unexpected progress timer termination");
				}
				if (!paused) {
					double time = ((double) System.currentTimeMillis()) / 1000 - globalStartTime;
					elapsedTime += (time - lastTime);
					lastTime = time;
					updateTime();
				}
			}
		}
	}
}
