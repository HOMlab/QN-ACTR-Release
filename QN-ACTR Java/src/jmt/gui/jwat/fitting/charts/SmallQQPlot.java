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
package jmt.gui.jwat.fitting.charts;

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
import jmt.engine.jwat.fitting.FittingAlgorithm;
import jmt.engine.jwat.fitting.ParetoFitting;
import jmt.engine.jwat.fitting.utils.ModelFitting;
import jmt.engine.jwat.workloadAnalysis.utils.JWatWorkloadManager;
import jmt.engine.jwat.workloadAnalysis.utils.ModelWorkloadAnalysis;

public class SmallQQPlot extends JPanel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private ModelFitting model = null;
	private FittingAlgorithm engfitting;
	public static int PARETO = 0;
	public static int EXPO = 1;
	private int distribution = PARETO;

	public SmallQQPlot(ModelFitting model, FittingAlgorithm engfitting) {
		setCursor(new Cursor(Cursor.HAND_CURSOR));
		setToolTipText("Double click to enlarge this graph");
		this.model = model;
		this.engfitting = engfitting;
		setSize(202, 202);
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
					plotFrame.setContentPane(new EnlargeQQPlot(SmallQQPlot.this.model, SmallQQPlot.this.engfitting, distribution));

					if (distribution == EXPO) {
						plotFrame.setTitle("QQ-Plot of the selected variable against the exponential distribution");
					}
					
					if (distribution == PARETO) {
						plotFrame.setTitle("QQ-Plot of the selected variable against the Pareto distribution");
					}
					
					plotFrame.setVisible(true);
				}
			}
		});
	}

	@Override
	public void paintComponent(Graphics g) {
		double[] x = model.getListObservations();
		double[] y = engfitting.generateQQPlot();
		
		super.paintComponent(g);
		g.setColor(Color.WHITE);
		g.fillRect(0, 0, 202, 202);
		//Draw Border
		g.setColor(Color.BLACK);
		g.drawRect(0, 0, 202, 202);
		//Draw gray lines\
		Graphics2D g1 = (Graphics2D) g;
		g1.setColor(Color.LIGHT_GRAY);
		for (int i = 1; i < 9; i++) {
			g1.drawLine(1, 1 + (i * (202 / 9)), 201, 1 + (i * (202 / 9)));
			g1.drawLine(1 + (i * (202 / 9)), 1, 1 + (i * (202 / 9)), 202);
		}
		g1.setColor(Color.BLACK);
		for (int i = 1; i < 9; i++) {
			g1.drawLine(1, 1 + (i * (202 / 9)), 6, 1 + (i * (202 / 9)));
			g1.drawLine(1 + (i * (202 / 9)), 1, 1 + (i * (202 / 9)), 6);
			g1.drawLine(197, 1 + (i * (202 / 9)), 202, 1 + (i * (202 / 9)));
			g1.drawLine(1 + (i * (202 / 9)), 198, 1 + (i * (202 / 9)), 202);
		}
	
		//Draw graph
		//Draw reference line
		g.setColor(Color.RED);
		g.drawLine(1, 201, 201, 1);

		g.setColor(Color.BLUE);
		
		//System.out.println("x.length: "+x.length+" y.length: "+y.length);
		for (int i = 0; i < x.length; i++) {
			//System.out.println(x[i] + " ***** "+y[i]);
			//System.out.println((int) Math.round((x[i] / x[x.length-1]) * 200) + " ----- " + (int) Math.round((y[i] / y[y.length-1]) * 120));
			if(y[i] <= x[x.length-1])
				g.fillOval((int) Math.round((x[i] / x[x.length-1]) * 200),
					200 - (int) Math.round((y[i] / x[x.length-1]) * 200), 5, 5);
		}

}


	public void setDistribution(int dis) {
		distribution = dis;
		repaint();
	}
}
