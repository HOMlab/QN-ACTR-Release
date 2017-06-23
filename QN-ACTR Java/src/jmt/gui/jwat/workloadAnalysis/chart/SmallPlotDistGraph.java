package jmt.gui.jwat.workloadAnalysis.chart;

import java.awt.Cursor;
import java.awt.Rectangle;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.swing.JFrame;

import jmt.engine.jwat.VariableNumber;
import jmt.engine.jwat.workloadAnalysis.utils.JWatWorkloadManager;
import jmt.engine.jwat.workloadAnalysis.utils.ModelWorkloadAnalysis;
import jmt.engine.jwat.workloadAnalysis.utils.SetMatrixListener;
import jmt.gui.jwat.JWATConstants;
import ptolemy.plot.Plot;

public class SmallPlotDistGraph extends Plot {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	// Variable to plot
	private ModelWorkloadAnalysis model = null;
	// Current variable plotting
	private int current = 0;
	// Graph dimensions
	public static int WIDTH = 300;
	public static int HEIGHT = 120;

	/** 
	 * Constructor, creates white small plot
	 */
	public SmallPlotDistGraph(ModelWorkloadAnalysis model) {
		super();
		setCursor(new Cursor(Cursor.HAND_CURSOR));
		setToolTipText("Double click to enlarge this graph");
		// Sets up plot properties
		this.setSize(WIDTH, HEIGHT);
		// Saves workload analysis model
		this.model = model;
		// Adds listener to model
		model.addOnSetMatrixObservationListener(new SetMatrixListener() {
			public void onSetMatrixObservation() {
				if (SmallPlotDistGraph.this.model.getMatrix() != null) {
					draw(SmallPlotDistGraph.this.model.getMatrix().getVariables()[current], current);
				}
			}

			public void onResetMatrixObservation() {
			}
		});
		// Adds mouselistener
		this.addMouseListener(new MouseAdapter() {
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
					plotFrame.setContentPane(new EnlargePlotDistGraph(SmallPlotDistGraph.this.model, current));
					plotFrame.setTitle(SmallPlotDistGraph.this.model.getMatrix().getVariables()[current].getName() + " frequencies graph");
					plotFrame.setVisible(true);
				}
			}
		});
	}

	/**
	 * Sets up new variable information to draw
	 * @param x variable to draw
	 * @throws IllegalArgumentException
	 */
	public void draw(VariableNumber x, int num) throws IllegalArgumentException {
		current = num;
		this.clear(true);
		int[] values = x.getInterval1000();
		double range = x.getUniStats().getRangeValue();
		double min = x.getUniStats().getMinValue();
		setGrid(true);
		setXRange(x.getValue(0), x.getValue(x.Size() - 1));
		if (x.getType() == JWATConstants.DATE) {
			SimpleDateFormat f = new SimpleDateFormat("dd.MM.yy HH:mm:ss");
			addXTick(f.format(new Date((long) (min + range / 10))), min + range / 10);
			addXTick(f.format(new Date((long) (min + range / 2))), min + range / 2);
			addXTick(f.format(new Date((long) x.getValue(x.Size() - 1))), x.getValue(x.Size() - 1));
			//setXLabel("Time fom [" + f.format(new Date((long)x.getValue(0))) +"] to [" + f.format(new Date((long)x.getValue(x.Size()-1)))+"]");
		}
		setPlotRectangle(new Rectangle(20, 20, WIDTH - 30, HEIGHT - 40));
		for (int i = 1; i < values.length; i++) {
			addPoint(1, min + (range * i / 1000), values[i] - values[i - 1], true);
		}
		repaint();
	}
}
