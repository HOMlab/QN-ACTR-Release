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
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagLayout;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.DecimalFormat;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JToolBar;
import javax.swing.border.EmptyBorder;

import jmt.gui.common.CommonConstants;
import jmt.gui.common.panels.parametric.PAProgressWindow;
import jmt.gui.common.resources.JMTImageLoader;
import jmt.gui.common.startScreen.sampleAnimation.SampleQNAnimation;
import jmt.gui.exact.ExactModel;
import jmt.gui.exact.link.SolverClient;

/**
 * <p>Title: Progress Window</p>
 * <p>Description: This window is displayed during analytical solution process.</p>
 *
 * @author Bertoli Marco
 *         Date: 23-mag-2006
 *         Time: 14.35.14
 */
public class ProgressWindow extends JDialog {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private SolverClient solver;
	private JProgressBar progressBar;
	private JLabel mainLabel, elapsedTimeLabel, stepLabel;
	private SampleQNAnimation animation;
	private int iterations;
	private Timer timer;

	/**
	 * Creates a new ProgressWindow given a solver client and an exactmodel
	 * @param solver reference to solverclient
	 * @param model reference to current model
	 * @param owner of this dialog
	 */
	public ProgressWindow(SolverClient solver, ExactModel model, Frame owner) {
		super(owner, true);
		this.solver = solver;
		if (model.isWhatIf()) {
			iterations = model.getWhatIfValues().length;
		} else {
			iterations = 1;
		}
		initGUI();
		timer = new Timer();
		timer.start();
	}

	/**
	 * Initialize all graphical components
	 */
	private void initGUI() {
		if (iterations > 1) {
			this.setTitle("What-if analysis progress");
		} else {
			this.setTitle("Solving current model");
		}

		this.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
		int width = 500, height = 390;

		// Centers this dialog on the screen
		Dimension scrDim = Toolkit.getDefaultToolkit().getScreenSize();
		this.setBounds((scrDim.width - width) / 2, (scrDim.height - height) / 2, width, height);

		// Creates bottom toolbar
		JToolBar toolbar = new JToolBar();
		toolbar.setFloatable(false);
		toolbar.setRollover(true);
		final JButton stop = new JButton();
		toolbar.add(stop);

		stop.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				kill();
			}
		});
		stop.setText("");
		stop.setIcon(JMTImageLoader.loadImage("Stop"));
		stop.setFocusPainted(false);
		stop.setContentAreaFilled(false);
		stop.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 0));
		stop.setRolloverIcon(JMTImageLoader.loadImage("StopRO"));
		stop.setPressedIcon(JMTImageLoader.loadImage("StopP"));
		stop.setVisible(true);
		if (iterations == 1) {
			stop.setEnabled(false);
		}

		// Adds a progress bar
		progressBar = new JProgressBar();
		progressBar.setStringPainted(true);
		progressBar.setForeground(Color.BLUE);
		toolbar.add(progressBar);

		// Adds toolbar
		this.getContentPane().add(toolbar, BorderLayout.SOUTH);

		//Creates and adds the animation panel
		animation = new SampleQNAnimation();
		animation.start();
		JPanel pivotPanel = new JPanel(new GridBagLayout());
		animation.setPreferredSize(new Dimension(200, 120));
		animation.setBackground(new Color(151, 151, 151));
		pivotPanel.add(animation);

		//Creates and adds the description label
		JPanel mainLabelPanel = new JPanel();
		if (iterations > 1) {
			mainLabel = new JLabel(CommonConstants.HTML_START + CommonConstants.HTML_FONT_TITLE + PAProgressWindow.RUNNING_MESSAGE
					+ CommonConstants.HTML_FONT_TIT_END + CommonConstants.HTML_END);
		} else {
			mainLabel = new JLabel(CommonConstants.HTML_START + CommonConstants.HTML_FONT_TITLE + "JMVA is solving current model, please wait..."
					+ CommonConstants.HTML_FONT_TIT_END + CommonConstants.HTML_END);
		}
		mainLabelPanel.add(mainLabel);

		elapsedTimeLabel = new JLabel("Elapsed time: " + "00m : 00s");
		JPanel elapsedTimeLabelPanel = new JPanel();
		elapsedTimeLabelPanel.add(elapsedTimeLabel);
		JPanel tempPanel = new JPanel(new BorderLayout());
		tempPanel.add(elapsedTimeLabel, BorderLayout.NORTH);
		JPanel timeLabelPanel = new JPanel();
		timeLabelPanel.add(tempPanel);
		timeLabelPanel.setBorder(new EmptyBorder(10, 0, 0, 0));
		JPanel stepLabelPanel = new JPanel();
		if (iterations > 1) {
			stepLabel = new JLabel("Solving model " + "1 of " + iterations);
		} else {
			stepLabel = new JLabel("");
		}

		stepLabelPanel.add(stepLabel);
		JPanel lowerLabelPanel = new JPanel(new BorderLayout());
		lowerLabelPanel.add(timeLabelPanel, BorderLayout.CENTER);
		lowerLabelPanel.add(stepLabelPanel, BorderLayout.SOUTH);
		mainLabelPanel.setBorder(new EmptyBorder(20, 20, 10, 20));
		stepLabelPanel.setBorder(new EmptyBorder(20, 20, 20, 20));
		JPanel upperPanel = new JPanel(new BorderLayout());
		upperPanel.add(mainLabelPanel, BorderLayout.CENTER);
		JPanel globalPanel = new JPanel(new BorderLayout());
		globalPanel.add(upperPanel, BorderLayout.NORTH);
		globalPanel.add(pivotPanel, BorderLayout.CENTER);
		globalPanel.add(lowerLabelPanel, BorderLayout.SOUTH);
		this.getContentPane().add(globalPanel, BorderLayout.CENTER);
	}

	/**
	 * This method must be called each time an analysis terminates
	 * @param num number of terminated analysis
	 */
	public void terminateAnalysis(int num) {
		if (iterations > 1) {
			stepLabel.setText("Solving model " + (num + 1) + " of " + iterations);
		}
		progressBar.setValue(num * 100 / iterations);
		if (num >= iterations - 1) {
			progressBar.setValue(100);
			timer.kill();
			animation.stop();
			dispose();
		}
	}

	/**
	 * Kills this window and aborts computations
	 */
	public void kill() {
		solver.stop();
		timer.kill();
		animation.stop();
		dispose();
	}

	/**
	 * This class will simply adjust elapsed time until killed
	 */
	private class Timer extends Thread {
		private boolean killed = false;
		private DecimalFormat formatter = new DecimalFormat("#00");

		@Override
		public void run() {
			long initialTime = System.currentTimeMillis();
			synchronized (this) {
				while (!killed) {
					try {
						long elapsedTime = System.currentTimeMillis() - initialTime;
						elapsedTimeLabel.setText("Elapsed time: " + formatter.format(elapsedTime / 60000) + "m : "
								+ formatter.format(((elapsedTime / 1000) % 60)) + "s");
						this.wait(1000);
					} catch (InterruptedException e) {
						e.printStackTrace(); //To change body of catch statement use File | Settings | File Templates.
					}
				}
			}
		}

		public synchronized void kill() {
			killed = true;
			this.notifyAll();
		}
	}
}
