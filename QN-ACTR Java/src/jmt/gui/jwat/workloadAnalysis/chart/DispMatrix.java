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
import javax.swing.WindowConstants;

import jmt.engine.jwat.TimeConsumingWorker;
import jmt.engine.jwat.VariableNumber;
import jmt.engine.jwat.input.ProgressMonitorShow;
import jmt.engine.jwat.workloadAnalysis.utils.ChangeVariableListener;
import jmt.engine.jwat.workloadAnalysis.utils.JWatWorkloadManager;
import jmt.engine.jwat.workloadAnalysis.utils.ModelWorkloadAnalysis;

import org.freehep.graphics2d.VectorGraphics;
import org.freehep.util.export.ExportDialog;

//TODO: Aggiungere listener a model per cambiamento della matrice delle osservazioni
public class DispMatrix extends JScrollPane {

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

	public DispMatrix() {
		super(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		this.setPreferredSize(new Dimension(420, 420));
		panel = new DispersionPanel();
		this.setViewportView(panel);
	}

	public DispMatrix(ModelWorkloadAnalysis m) {
		/* Richiamo il costruttore della classe JScorllPanel impostando le barre di scorrimento solo se necessarie */
		super(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		this.model = m;
		/* Settaggio delle proprieta' del pannello di scroll */
		this.setPreferredSize(new Dimension(420, 420));
		/* Creazione e aggiunta del Pannello di visualizzazione della matrice di dispersione */
		panel = new DispersionPanel(model);
		panel.setPreferredSize(new Dimension(DispersionPanel.WIDTH_TOT * model.getMatrix().getNumVariables(), DispersionPanel.HEIGHT_TOT
				* model.getMatrix().getNumVariables()));
		this.setViewportView(panel);
		addListener();
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

	@Override
	protected void paintComponent(Graphics g) {
		if (g == null) {
			return;
		}
		VectorGraphics vg = VectorGraphics.create(g);
		super.paintComponent(vg);
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

							f.setSize(640, 690);
							f.setTitle("Scatter Plot " + model.getMatrix().getVariables()[e.getX() / WIDTH_TOT].getName() + " - "
									+ model.getMatrix().getVariables()[e.getY() / HEIGHT_TOT].getName());
							f.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
							f.setContentPane(new Scatter(e.getX() / WIDTH_TOT, e.getY() / HEIGHT_TOT, model, f));
							f.setVisible(true);
						}
					}
					/* Opzione di salvataggio immagine */
					if (e.getButton() == MouseEvent.BUTTON3) {
						showScreenShotDialog();
						/* inserimento del nome per il salvataggio nel file */
						/*String inputValue = JOptionPane.showInputDialog("Please insert name: ");
						if(inputValue != null)
						{
							File file = new File(inputValue + ".png");
							try{
								ImageIO.write(chart,"PNG",file);
							}catch(Exception ex)
							{
								System.out.println("Saving error, please try again");
							}
						}*/
					}
				}
			});
		}

		@Override
		public void paintComponent(Graphics g) {
			if (g == null) {
				return;
			}

			VectorGraphics vg = VectorGraphics.create(g);
			super.paintComponent(vg);
			super.paintComponent(g);
			if (Redraw && model.getMatrix() != null) {
				Redraw = false;
				Graphics grap = chart.getGraphics();
				grap.setColor(Color.GRAY);
				grap.fillRect(0, 0, WIDTH_TOT * model.getMatrix().getNumVariables() + 1, HEIGHT_TOT * model.getMatrix().getNumVariables() + 1);
				grap.drawImage(chart, 0, 0, null);
				//SwingWorker worker = new SwingWorker()
				TimeConsumingWorker worker = new TimeConsumingWorker(new ProgressMonitorShow(DispMatrix.this, "Plotting scatters...", 1)) {
					@Override
					public Object construct() {
						//doWork();
						Graphics g = chart.getGraphics();
						VariableNumber Elenco[] = model.getMatrix().getVariables();
						try {
							// Pulizia dell'immagine
							g.setColor(Color.WHITE);
							g.fillRect(0, 0, WIDTH_TOT * Elenco.length + 1, HEIGHT_TOT * Elenco.length + 1);

							initShow(10000 * (Elenco.length * Elenco.length));

							int Prog = 0;

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
										// Calcolo del passo del grafico
										double yFoot = HEIGHT_GRAPH / Elenco[row].getUniStats().getRangeValue();
										double xFoot = WIDTH_GRAPH / Elenco[col].getUniStats().getRangeValue();
										// Plottaggio del grafico
										g.setColor(Color.RED);
										for (int i = 1; i <= WIDTH_GRAPH; i++) {
											boolean[] done = new boolean[101];
											for (int j = Elenco[col].getStartInt(i); j < Elenco[col].getEndInt(i); j++) {
												int k = 1;
												if ((int) ((Elenco[col].getValue(j, row) - Elenco[row].getUniStats().getMinValue()) * yFoot) > 0) {
													if (!done[(int) ((Elenco[col].getValue(j, row) - Elenco[row].getUniStats().getMinValue()) * yFoot)]) {
														updateInfos(((row * Elenco.length) + (col)) * 10000 + i + (k++), "Plotting scatter "
																+ ((row * Elenco.length) + (col + 1)), false);
														g.fillOval(col * WIDTH_TOT + 5 + /* i - 1,*/
														(int) (((Elenco[col].getValue(j) - Elenco[col].getUniStats().getMinValue())) * xFoot) - 1,
																(((row + 1) * HEIGHT_TOT) - 5 - (int) (((Elenco[col].getValue(j, row) - Elenco[row]
																		.getUniStats().getMinValue())) * yFoot)), 1, 1);
														done[(int) ((Elenco[col].getValue(j, row) - Elenco[row].getUniStats().getMinValue()) * yFoot)] = true;
													}
												}
											}

										}
									}
								}
							}
						} catch (Exception e) {
							g.setColor(Color.WHITE);
							g.fillRect(0, 0, 300, 100);
							g.setColor(Color.BLACK);
							g.drawString("With this data the graphic cannot be create", 50, 50);
							DispersionPanel.this.repaint();
							e.printStackTrace();

						}
						updateInfos(10000 * (Elenco.length * Elenco.length) + 1, "End", false);
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

		/**
		 * Shows a screenshot dialog used to select screenshot format
		 */
		public void showScreenShotDialog() {
			ExportDialog export = new ExportDialog("JWAT - version ???");
			export.showExportDialog(this, "Export as image...", this, "graph");
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
		/* Finestra principale */
		private JFrame jframeOwner;
		private JFrame jFrameTot;
	}
}
