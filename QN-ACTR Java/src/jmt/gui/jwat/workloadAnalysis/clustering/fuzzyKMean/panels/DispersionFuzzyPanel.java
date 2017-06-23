package jmt.gui.jwat.workloadAnalysis.clustering.fuzzyKMean.panels;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;

import jmt.engine.jwat.workloadAnalysis.WorkloadAnalysisSession;
import jmt.engine.jwat.workloadAnalysis.utils.JavaWatColor;
import jmt.gui.jwat.workloadAnalysis.chart.DispFuzzyMatrix;

public class DispersionFuzzyPanel extends JPanel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private int clusters;
	private DispFuzzyMatrix matrix;
	private boolean redraw = true;

	public DispersionFuzzyPanel(WorkloadAnalysisSession session, int clustering, int clusters) {
		this.setLayout(new BorderLayout());
		this.clusters = clusters;
		matrix = new DispFuzzyMatrix(session, -1);
		this.add(matrix, BorderLayout.CENTER);
		matrix.setClustering(clustering, clusters);
		this.add(new JScrollPane(new myPanel(), ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER),
				BorderLayout.EAST);
	}

	public DispFuzzyMatrix getMatrix() {
		return matrix;
	}

	public void setClustering(int cluster) {
		clusters = cluster;
		redraw = true;
	}

	private class myPanel extends JPanel {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		private int START_SQUARE_H = 5;
		private int START_SQUARE_W = 5;
		private int SQUARE_L = 10;
		private int LINE_H = 10;
		private BufferedImage legenda;

		public myPanel() {
			this.setLayout(new FlowLayout());
			setPreferredSize(new Dimension(110, (SQUARE_L + LINE_H) * (clusters + 4)));
			legenda = new BufferedImage(110, (SQUARE_L + LINE_H) * (clusters + 4), BufferedImage.TYPE_INT_RGB);
		}

		@Override
		public void paint(Graphics g1) {
			if (redraw) {
				redraw = false;
				Graphics2D g = (Graphics2D) legenda.getGraphics();
				g.setColor(Color.WHITE);
				g.fillRect(0, 0, 110, (SQUARE_L + LINE_H) * (clusters + 4));

				for (int i = 0; i <= (clusters + 3); i++) {
					g.setColor(JavaWatColor.getColor((i)));
					if (i == 0) {
						g.fillRect(START_SQUARE_W, (START_SQUARE_H + (SQUARE_L + LINE_H) * (i)), SQUARE_L, SQUARE_L);
						g.setColor(Color.BLACK);
						g.drawString("Not assigned", START_SQUARE_W + SQUARE_L + SQUARE_L, (START_SQUARE_H + (SQUARE_L + LINE_H) * (i) + LINE_H));
						continue;
					}
					if (i == 1) {
						g.fillRect(START_SQUARE_W, (START_SQUARE_H + (SQUARE_L + LINE_H) * (i)), SQUARE_L, SQUARE_L);
						g.setColor(Color.BLACK);
						g.drawString("Multiple", START_SQUARE_W + SQUARE_L + SQUARE_L, (START_SQUARE_H + (SQUARE_L + LINE_H) * (i) + LINE_H));
						continue;
					}
					g.fillRect(START_SQUARE_W, (START_SQUARE_H + (SQUARE_L + LINE_H) * (i)), SQUARE_L, SQUARE_L);
					g.setColor(Color.BLACK);
					g.drawString("Cluster " + (i - 1), START_SQUARE_W + SQUARE_L + SQUARE_L, (START_SQUARE_H + (SQUARE_L + LINE_H) * (i) + LINE_H));
				}
			}
			g1.drawImage(legenda, 0, 0, null);
		}
	}

}
