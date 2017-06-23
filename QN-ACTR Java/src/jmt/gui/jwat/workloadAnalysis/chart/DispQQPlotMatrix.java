package jmt.gui.jwat.workloadAnalysis.chart;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.font.FontRenderContext;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;

import jmt.engine.jwat.TimeConsumingWorker;
import jmt.engine.jwat.VariableNumber;
import jmt.engine.jwat.input.ProgressMonitorShow;
import jmt.engine.jwat.workloadAnalysis.utils.ChangeVariableListener;
import jmt.engine.jwat.workloadAnalysis.utils.JWatWorkloadManager;
import jmt.engine.jwat.workloadAnalysis.utils.ModelWorkloadAnalysis;

import org.freehep.graphics2d.VectorGraphics;
import org.freehep.util.export.ExportDialog;

public class DispQQPlotMatrix extends JScrollPane {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private ModelWorkloadAnalysis model;
	private DispersionPanel panel;
	private ChangeVariableListener listener = new ChangeVariableListener() {
		public void onChangeVariableValues() {
			panel.Redraw = true;
		}
	};

	public DispQQPlotMatrix() {
		super(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		this.setPreferredSize(new Dimension(420, 420));
		panel = new DispersionPanel();
		this.setViewportView(panel);
	}

	public DispQQPlotMatrix(ModelWorkloadAnalysis m, int clustering) {
		super(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		this.model = m;
		this.setPreferredSize(new Dimension(420, 420));
		panel = new DispersionPanel(model);
		panel.setPreferredSize(new Dimension(DispersionPanel.WIDTH_TOT * model.getMatrix().getNumVariables(), DispersionPanel.HEIGHT_TOT
				* model.getMatrix().getNumVariables()));
		this.setViewportView(panel);
		addListener();
	}

	@Override
	protected void paintComponent(Graphics g) {
		if (g == null) {
			return;
		}
		VectorGraphics vg = VectorGraphics.create(g);
		super.paintComponent(vg);
	}

	public void setModel(ModelWorkloadAnalysis m) {
		model = m;
		if (model.getMatrix() != null) {
			panel.setModel(m);
			panel.setPreferredSize(new Dimension(DispersionPanel.WIDTH_TOT * model.getMatrix().getNumVariables(), DispersionPanel.HEIGHT_TOT
					* model.getMatrix().getNumVariables()));
			this.setViewportView(panel);
			addListener();
		}
	}

	private void addListener() {
		model.addOnChangeVariableValue(listener);
	}

	class DispersionPanel extends JPanel {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public DispersionPanel() {
			setCursor(new Cursor(Cursor.HAND_CURSOR));
		}

		public DispersionPanel(ModelWorkloadAnalysis m) {
			setCursor(new Cursor(Cursor.HAND_CURSOR));
			model = m;
			initComponent();
		}

		public void setModel(ModelWorkloadAnalysis m) {
			model = m;
			initComponent();
		}

		private void initComponent() {
			if (model.getMatrix() != null) {
				/* Creazione del buffer per l'immagine */
				chart = new BufferedImage(WIDTH_TOT * model.getMatrix().getNumVariables() + 1, HEIGHT_TOT * model.getMatrix().getNumVariables() + 1,
						BufferedImage.TYPE_USHORT_555_RGB);
				Redraw = true;
				/* Aggiunta del listener del mouse per visualizzazione dello scatter plot corrispondente in caso di pressione del tasto sinistro sul pannello
				 oppure visualizzare la possibilita' di salvare l'immagine in formato .png con tasto destro */
				if (this.getMouseListeners().length > 0) {
					this.removeMouseListener(this.getMouseListeners()[0]);
				}
				this.addMouseListener(new MouseAdapter() {
					@Override
					public void mouseClicked(MouseEvent e) {
						/* Visualizzazione dello scatter plot dettagliato */
						if (e.getButton() == MouseEvent.BUTTON1 && e.getClickCount() == 2) {
							int x = model.getMatrix().getNumVariables() * WIDTH_TOT;
							int y = model.getMatrix().getNumVariables() * HEIGHT_TOT;
							if (e.getX() < x && e.getY() < y && e.getX() / WIDTH_TOT != e.getY() / HEIGHT_TOT) {
								final JFrame f = new JFrame();

								JWatWorkloadManager.addJMTWindow(f);
								f.addWindowListener(new WindowAdapter() {
									@Override
									public void windowClosing(WindowEvent e) {
										JWatWorkloadManager.exit(f);
									}

									@Override
									public void windowClosed(WindowEvent e) {
										JWatWorkloadManager.exit(f);
									}
								});
								f.setSize(400, 400);
								f.setTitle("QQ-Plot of " + DispQQPlotMatrix.this.model.getMatrix().getVariables()[e.getX() / WIDTH_TOT].getName()
										+ " and " + DispQQPlotMatrix.this.model.getMatrix().getVariables()[e.getY() / HEIGHT_TOT].getName());
								f.setContentPane(new EnlargeQQPlotVarVar(model, e.getX() / WIDTH_TOT, e.getY() / HEIGHT_TOT));

								f.setVisible(true);
							}
						}
						/* Opzione di salvataggio immagine */
						if (e.getButton() == MouseEvent.BUTTON3) {
							showScreenShotDialog();
						}
					}
				});
			}
		}

		@Override
		public void paintComponent(Graphics g) {
			super.paintComponent(g);
			if (Redraw && model.getMatrix() != null) {
				Redraw = false;
				Graphics grap = chart.getGraphics();
				grap.setColor(Color.GRAY);
				grap.fillRect(0, 0, WIDTH_TOT * model.getMatrix().getNumVariables() + 1, HEIGHT_TOT * model.getMatrix().getNumVariables() + 1);
				grap.drawImage(chart, 0, 0, null);
				TimeConsumingWorker worker = new TimeConsumingWorker(new ProgressMonitorShow(this, "Constructin Dispersion Matrix...", 1)) {
					@Override
					public Object construct() {
						if (model.getMatrix() != null) {
							Graphics g = chart.getGraphics();
							VariableNumber Elenco[] = model.getMatrix().getVariables();
							try {
								// Pulizia dell'immagine
								g.setColor(Color.WHITE);
								g.fillRect(0, 0, WIDTH_TOT * Elenco.length + 1, HEIGHT_TOT * Elenco.length + 1);

								initShow((Elenco.length * Elenco.length));

								int l = 0;
								// Disegno dei grafici
								for (int row = 0; row < Elenco.length; row++) {
									for (int col = 0; col < Elenco.length; col++) {
										g.setColor(Color.BLACK);
										g.drawRect(col * WIDTH_TOT, row * HEIGHT_TOT, WIDTH_TOT, HEIGHT_TOT);
										if (col == row) {
											// Scrittura della variabile
											Graphics2D gr = (Graphics2D) g;
											gr.setFont(new Font("Arial", Font.BOLD, 13));
											FontRenderContext context = gr.getFontRenderContext();
											Font f = new Font("Arial", Font.BOLD, 12);
											Rectangle2D bounds = f.getStringBounds(Elenco[row].getName(), context);

											g.setFont(new Font("Arial", Font.BOLD, 12));
											g.drawString(Elenco[row].getName(), col * WIDTH_TOT + (WIDTH_TOT - (int) bounds.getWidth()) / 2, row
													* HEIGHT_TOT - (int) bounds.getY() + (HEIGHT_TOT - (int) bounds.getHeight()) / 2);
											g.setFont(new Font("Arial", Font.PLAIN, 12));
										} else {
											int[] quantiliX = Elenco[col].getUniStats().getQuantili();
											int[] quantiliY = Elenco[row].getUniStats().getQuantili();

											int startx = col * WIDTH_TOT + 5;
											int starty = row * HEIGHT_TOT + 5;

											double xRange = Elenco[col].getUniStats().getRangeValue();
											double yRange = Elenco[row].getUniStats().getRangeValue();
											double xmin = Elenco[col].getUniStats().getMinValue();
											double ymax = Elenco[row].getUniStats().getMaxValue();
											//Draw Border line 
											g.setColor(Color.BLACK);
											g.drawRect(startx, starty, WIDTH_GRAPH, HEIGHT_GRAPH);
											//Draw gray line
											g.setColor(Color.LIGHT_GRAY);
											for (int i = 1; i < 9; i++) {
												g.drawLine(startx, starty + (i * (HEIGHT_GRAPH / 9)), startx + WIDTH_GRAPH, starty
														+ (i * (HEIGHT_GRAPH / 9)));
												g.drawLine(startx + (i * (WIDTH_GRAPH / 9)), starty, startx + (i * (WIDTH_GRAPH / 9)), starty
														+ HEIGHT_GRAPH);
											}
											//Draw reference line red	
											g.setColor(Color.RED);
											g.drawLine(startx, starty + HEIGHT_GRAPH, startx + WIDTH_GRAPH, starty);
											//Draw QQ-plot
											g.setColor(Color.BLUE);
											for (int i = 1; i < quantiliX.length; i++) {
												g.fillOval(startx + (int) (((Elenco[col].getValue(quantiliX[i]) - xmin) / xRange) * WIDTH_GRAPH),
														(starty + (int) (((ymax - (Elenco[row].getValue(quantiliY[i]))) / yRange) * HEIGHT_GRAPH)),
														1, 1);
											}
										}
									}
								}
							} catch (Exception e) {
								g.setColor(Color.WHITE);
								g.fillRect(0, 0, 300, 100);
								g.setColor(Color.BLACK);
								g.drawString("With this data the graphic cannot be create", 50, 50);
							}
							updateInfos((Elenco.length * Elenco.length) + 1, "End", false);
						}
						return null;
					}

					@Override
					public void finished() {
						DispersionPanel.this.repaint();
					}
				};
				worker.start();
			} else {
				g.drawImage(chart, 0, 0, null);
			}
		}

		/* Lista delle variabili da visualizzare */
		private ModelWorkloadAnalysis model;
		/* Bufferr d'immagine */
		private BufferedImage chart;
		/* Indica se l'immagine deve essere o meno ridisegnato */
		private boolean Redraw;
		/* COSTANTI */
		private static final int HEIGHT_GRAPH = 100;
		private static final int WIDTH_GRAPH = 100;
		public static final int HEIGHT_TOT = 110;
		public static final int WIDTH_TOT = 110;

		/**
		 * Shows a screenshot dialog used to select screenshot format
		 */
		public void showScreenShotDialog() {
			ExportDialog export = new ExportDialog("Workload analysis tool");
			export.showExportDialog(this, "Export as image...", this, "graph");
		}

	}
}
