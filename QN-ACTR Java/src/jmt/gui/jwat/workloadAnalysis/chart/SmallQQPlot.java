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
package jmt.gui.jwat.workloadAnalysis.chart;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JFrame;
import javax.swing.JPanel;

import jmt.engine.jwat.VariableNumber;
import jmt.engine.jwat.workloadAnalysis.utils.JWatWorkloadManager;
import jmt.engine.jwat.workloadAnalysis.utils.ModelWorkloadAnalysis;

public class SmallQQPlot extends JPanel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private int x = -1;
	private ModelWorkloadAnalysis model = null;
	public static int NORMAL = 0;
	public static int EXPO = 1;
	private int distribution = NORMAL;
	private double[] normale = { -3.10000, -2.32635, -2.05375, -1.88079, -1.75069, -1.64485, -1.55477, -1.47579, -1.40507, -1.34076, -1.28155,
			-1.22653, -1.17499, -1.12639, -1.08032, -1.03643, -0.99446, -0.95417, -0.91537, -0.87790, -0.84162, -0.80642, -0.77219, -0.73885,
			-0.70630, -0.67449, -0.64335, -0.61281, -0.58284, -0.55338, -0.52440, -0.49585, -0.46770, -0.43991, -0.41246, -0.38532, -0.35846,
			-0.33185, -0.30548, -0.27932, -0.25335, -0.22754, -0.20189, -0.17637, -0.15097, -0.12566, -0.10043, -0.07527, -0.05015, -0.02507,
			0.00000, 0.02507, 0.05015, 0.07527, 0.10043, 0.12566, 0.15097, 0.17637, 0.20189, 0.22754, 0.25335, 0.27932, 0.30548, 0.33185, 0.35846,
			0.38532, 0.41246, 0.43991, 0.46770, 0.49585, 0.52440, 0.55338, 0.58284, 0.61281, 0.64335, 0.67449, 0.70630, 0.73885, 0.77219, 0.80642,
			0.84162, 0.87790, 0.91537, 0.95417, 0.99446, 1.03643, 1.08032, 1.12639, 1.17499, 1.22653, 1.28155, 1.34076, 1.40507, 1.47579, 1.55477,
			1.64485, 1.75069, 1.88079, 2.05375, 2.32635, 3.10000 };

	public SmallQQPlot(ModelWorkloadAnalysis model) {
		setCursor(new Cursor(Cursor.HAND_CURSOR));
		setToolTipText("Double click to enlarge this graph");
		this.model = model;
		setSize(102, 122);
		//Su panel grafico
		addMouseListener(new MouseAdapter() {
			// Creates new windows with enlarged graph
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2) {
					// Creates and sets up new window
					final JFrame plotFrame = new JFrame();

					JWatWorkloadManager.addJMTWindow(plotFrame);
					plotFrame.addWindowListener(new WindowAdapter() {
						@Override
						public void windowClosing(WindowEvent e) {
							JWatWorkloadManager.exit(plotFrame);
						}

						@Override
						public void windowClosed(WindowEvent e) {
							JWatWorkloadManager.exit(plotFrame);
						}
					});
					//plotFrame.setResizable(false);
					plotFrame.setSize(400, 400);
					plotFrame.setContentPane(new EnlargeQQPlot(SmallQQPlot.this.model, x, distribution));
					if (distribution == NORMAL) {
						plotFrame.setTitle("QQ-Plot of " + SmallQQPlot.this.model.getMatrix().getVariables()[x].getName()
								+ " and Normal distribution");
					}
					if (distribution == EXPO) {
						plotFrame.setTitle("QQ-Plot of " + SmallQQPlot.this.model.getMatrix().getVariables()[x].getName()
								+ " and Exponential distribution");
					}
					plotFrame.setVisible(true);
				}
			}
		});
	}

	@Override
	public void paintComponent(Graphics g) {
		super.paintComponent(g);
		g.setColor(Color.WHITE);
		g.fillRect(0, 0, 202, 122);
		//Draw Border
		g.setColor(Color.BLACK);
		g.drawRect(0, 0, 202, 122);
		//Draw gray lines\
		Graphics2D g1 = (Graphics2D) g;
		g1.setColor(Color.LIGHT_GRAY);
		for (int i = 1; i < 9; i++) {
			g1.drawLine(1, 1 + (i * (122 / 9)), 201, 1 + (i * (122 / 9)));
			g1.drawLine(1 + (i * (202 / 9)), 1, 1 + (i * (202 / 9)), 122);
		}
		g1.setColor(Color.BLACK);
		for (int i = 1; i < 9; i++) {
			g1.drawLine(1, 1 + (i * (122 / 9)), 6, 1 + (i * (122 / 9)));
			g1.drawLine(1 + (i * (202 / 9)), 1, 1 + (i * (202 / 9)), 6);
			g1.drawLine(197, 1 + (i * (122 / 9)), 202, 1 + (i * (122 / 9)));
			g1.drawLine(1 + (i * (202 / 9)), 117, 1 + (i * (202 / 9)), 122);
		}
		//Draw graph
		if (x != -1) {
			double mean = model.getMatrix().getVariables()[x].getUniStats().getMean();
			double var = model.getMatrix().getVariables()[x].getUniStats().getVariance();
			//Draw reference line
			g.setColor(Color.RED);
			g.drawLine(1, 121, 201, 1);
			if (distribution == NORMAL) {
				VariableNumber xvar = model.getMatrix().getVariables()[x];
				double xrange = xvar.getUniStats().getRangeValue();
				double yrange = ((normale[normale.length - 1] * var) + mean) - ((normale[0] * var) + mean);
				int[] qx = xvar.getUniStats().getQuantili();
				double xmin = xvar.getUniStats().getMinValue();
				double ymax = ((normale[normale.length - 1] * var) + mean);
				g.setColor(Color.BLUE);
				for (int i = 0; i < qx.length; i++) {
					g.fillOval((int) (1 + ((xvar.getValue(qx[i]) - xmin) / xrange) * 200),
							(int) (1 + ((ymax - ((normale[i] * var) + mean)) / yrange) * 120), 1, 1);
				}
			}
			if (distribution == EXPO) {
				VariableNumber xvar = model.getMatrix().getVariables()[x];
				double xrange = xvar.getUniStats().getRangeValue();
				double yrange = (-Math.log(0.01) * mean) - (-Math.log(1) * mean);
				int[] qx = xvar.getUniStats().getQuantili();
				double xmin = xvar.getUniStats().getMinValue();
				double ymax = (-Math.log(0.01) * mean);
				g.setColor(Color.BLUE);
				for (int i = 0; i < qx.length; i++) {
					g.fillOval((int) (1 + ((xvar.getValue(qx[i]) - xmin) / xrange) * 200),
							(int) (1 + ((ymax - (-Math.log(1 - (0.01 * i)) * mean)) / yrange) * 120), 1, 1);
				}
			}
		}
	}

	public void setVariables(int x) {
		this.x = x;
		repaint();
	}

	public void setDistribution(int dis) {
		distribution = dis;
		repaint();
	}
}
