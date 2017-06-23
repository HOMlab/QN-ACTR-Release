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

import jmt.engine.jwat.fitting.FittingAlgorithm;
import jmt.engine.jwat.fitting.ParetoFitting;
import jmt.engine.jwat.fitting.utils.ModelFitting;
import jmt.engine.jwat.workloadAnalysis.utils.JWatWorkloadManager;

public class SmallPDF extends JPanel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private ModelFitting model = null;
	private FittingAlgorithm engfitting;
	public static int PARETO = 0;
	public static int EXPO = 1;
	private int distribution = PARETO;

	public SmallPDF(ModelFitting model, FittingAlgorithm engfitting) {
		setCursor(new Cursor(Cursor.HAND_CURSOR));
		setToolTipText("Double click to enlarge this graph");
		this.model = model;
		this.engfitting = engfitting;
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
					plotFrame.setContentPane(new EnlargePDF(SmallPDF.this.model));

					plotFrame.setTitle("PDF function of the selected variable");
					
					plotFrame.setVisible(true);
				}
			}
		});
	}

	@Override
	public void paintComponent(Graphics g) {
		double[] x = model.getListObservations();
		double[] freq_y = new double[Math.round(x.length/10)];
		
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
		
		//build the count of frequencies
		int j=0;
		double bucket_dim = x[x.length-1] / Math.round(x.length/10);
		for(int i=0;i<freq_y.length;i++) {
			
			while((x[j] < (i+1)*bucket_dim) && (x[j] >= i*bucket_dim)) {
				freq_y[i]++;
				j++;
			}
			
			if(i<(freq_y.length-1))
				freq_y[i] /= x.length;
			
			//System.out.println("Bucket from "+ i*bucket_dim + " to " + (i+1)*bucket_dim + " Percentage: " + freq_y[i]);
		}
		freq_y[freq_y.length-1]++;
		freq_y[freq_y.length-1] /= x.length;
		//******************************
	
		//Draw graph
		//Draw reference line
		g.setColor(Color.BLUE);
		for (int i = 0; i < freq_y.length; i++) {
			//System.out.println("x: " + Math.round((((i+1)*bucket_dim + i*bucket_dim)/2)/x[x.length-1] * 200) + " y: " + (int) Math.round(freq_y[i] * 200));
			g.fillOval((int) Math.round(((((i+1)*bucket_dim + i*bucket_dim)/2)/x[x.length-1])* 200),
					200 - (int) Math.round(freq_y[i] * 200), 5, 5);
		}

}


	public void setDistribution(int dis) {
		distribution = dis;
		repaint();
	}
}
