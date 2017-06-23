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

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import jmt.engine.jwat.workloadAnalysis.utils.ModelWorkloadAnalysis;
import jmt.gui.common.CommonConstants;

public class QQPlotPreviewPanel extends JPanel implements CommonConstants {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private int currVarPos = 0;
	private SmallQQPlot plot = null;
	private ModelWorkloadAnalysis model = null;
	public static int WIDTH = 300;
	public static int HEIGHT = 120;

	private JComboBox distChoose = new JComboBox(new String[] { "Normal distribution", "Exponential distribution" });

	public QQPlotPreviewPanel(ModelWorkloadAnalysis model) {
		super(new GridLayout(1, 1));
		this.model = model;
		distChoose.setToolTipText("Select distribution to compare to variable quantile distribution");
		initGUI();
		addListeners();
	}

	private void addListeners() {
		distChoose.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (distChoose.getSelectedIndex() == 0) {
					plot.setDistribution(SmallQQPlot.NORMAL);
				}
				if (distChoose.getSelectedIndex() == 1) {
					plot.setDistribution(SmallQQPlot.EXPO);
				}
			}
		});
	}

	private void initGUI() {
		this.setSize(WIDTH, HEIGHT);
		JPanel mainBox = new JPanel(new BorderLayout());

		plot = new SmallQQPlot(model); //Add distribution against with to plot

		JPanel empty = new JPanel();
		empty.setPreferredSize(new Dimension(5, 123));
		mainBox.add(empty, BorderLayout.WEST);
		mainBox.add(plot, BorderLayout.CENTER);
		empty = new JPanel();
		empty.setPreferredSize(new Dimension(200, 5));
		mainBox.add(empty, BorderLayout.NORTH);

		JPanel northP = new JPanel(new GridLayout(2, 1));
		JPanel a = new JPanel(new FlowLayout(FlowLayout.LEFT));
		JPanel b = new JPanel(new FlowLayout(FlowLayout.LEFT));
		northP.add(a);
		northP.add(b);

		JPanel ok = new JPanel(new BorderLayout());
		//ok.setPreferredSize(new Dimension(120,123));
		ok.add(distChoose, BorderLayout.NORTH);
		//ok.add(northP,BorderLayout.NORTH);
		ok.add(new JLabel(HTML_START + HTML_FONT_TITLE + "QQ-Plot" + HTML_FONT_TIT_END + HTML_FONT_NORM + "Actual quantile distribution<p>"
				+ "versus normal or exponential<p>" + HTML_FONT_NOR_END + HTML_END), BorderLayout.CENTER);
		mainBox.add(ok, BorderLayout.EAST);
		/*mainBox.add(new JLabel(HTML_START + HTML_FONT_TITLE + "QQ-Plot" + HTML_FONT_TIT_END 
				+ HTML_FONT_NORM + "Comparision of real quantile<p>" +
								   "distribution and normal or <p>" +
								   "exponential theoretical one" + HTML_FONT_NOR_END + HTML_END),BorderLayout.EAST);*/
		this.add(mainBox);
	}

	public void setCurrentVar(int currVar) {
		currVarPos = currVar;
		plot.setVariables(currVarPos);
	}
}
