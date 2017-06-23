package jmt.gui.jwat.workloadAnalysis.chart;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.swing.AbstractAction;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;

import jmt.engine.jwat.TimeConsumingWorker;
import jmt.engine.jwat.VariableNumber;
import jmt.engine.jwat.input.ProgressMonitorShow;
import jmt.engine.jwat.workloadAnalysis.utils.ChangeVariableListener;
import jmt.engine.jwat.workloadAnalysis.utils.ModelWorkloadAnalysis;
import jmt.gui.jwat.JWATConstants;

import org.freehep.graphics2d.VectorGraphics;
import org.freehep.util.export.ExportDialog;

public class Scatter extends JPanel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private int HEIGHT_TEXT_X = 30;
	private int WIDTH_TEXT_Y = 30;
	private int HEIGHT_VALUES_X = 90;
	private int WIDTH_VALUES_Y = 90;
	private int TOP_SPACE = 50;
	private int RIGHT_SPACE = 20;
	private int HEIGHT = 500;
	private int WIDTH = 500;
	private int pointSize = 1;

	private Point UpperLeftGraph = new Point(WIDTH_TEXT_Y + WIDTH_VALUES_Y, TOP_SPACE);
	private Point UpperRightGraph = new Point(WIDTH_TEXT_Y + WIDTH_VALUES_Y + WIDTH, TOP_SPACE);
	private Point LowerLeftGraph = new Point(WIDTH_TEXT_Y + WIDTH_VALUES_Y, HEIGHT + TOP_SPACE);
	private Point LowerRightGraph = new Point(WIDTH_TEXT_Y + WIDTH_VALUES_Y + WIDTH, HEIGHT + TOP_SPACE);
	private Point YText = new Point(WIDTH_TEXT_Y / 2, (TOP_SPACE + HEIGHT + HEIGHT_TEXT_X + HEIGHT_VALUES_X) / 2);
	private Point XText = new Point((WIDTH_TEXT_Y + WIDTH_VALUES_Y + WIDTH + RIGHT_SPACE) / 2, TOP_SPACE + HEIGHT + HEIGHT_VALUES_X + HEIGHT_TEXT_X
			/ 2);

	private BufferedImage graph; //Immagine grafico
	private boolean first = true; //Indica che devono essere ridisegnati i punti nella buffered image
	private boolean zoomming = false; //Indica che si sta operando una selezione di zoom In
	private int xStart, yStart, xEnd, yEnd; //Posizione di inizio e fine della selezione di zoom
	private ModelWorkloadAnalysis model; //Modello contenente i valori da plottare
	private int xVar; //Variabile da plottare in x
	private int yVar; //Variabile da plottare in y
	private double xMin; //Valori limite correnti del grafico
	private double xMax;
	private double yMin;
	private double yMax;
	private DecimalFormat valueFormat; //Formato visualizzazione dei valori sugli assi
	private GlassPanel p;
	private JFrame parent;
	private ScatterPopupMenu popup = new ScatterPopupMenu();

	public Scatter(int x, int y, ModelWorkloadAnalysis model, JFrame f) {
		super();
		parent = f;
		graph = new BufferedImage(WIDTH + 1, HEIGHT + 1, BufferedImage.TYPE_INT_RGB);
		valueFormat = new DecimalFormat("####.##E0");
		parent.setResizable(false);

		xVar = x;
		yVar = y;
		this.model = model;
		this.model.addOnChangeVariableValue(new ChangeVariableListener() {
			public void onChangeVariableValues() {
				first = true;
				xMin = Scatter.this.model.getMatrix().getVariables()[xVar].getUniStats().getMinValue();
				xMax = Scatter.this.model.getMatrix().getVariables()[xVar].getUniStats().getMaxValue();
				yMin = Scatter.this.model.getMatrix().getVariables()[yVar].getUniStats().getMinValue();
				yMax = Scatter.this.model.getMatrix().getVariables()[yVar].getUniStats().getMaxValue();
				repaint();
			}
		});

		xMin = this.model.getMatrix().getVariables()[xVar].getUniStats().getMinValue();
		xMax = this.model.getMatrix().getVariables()[xVar].getUniStats().getMaxValue();
		yMin = this.model.getMatrix().getVariables()[yVar].getUniStats().getMinValue();
		yMax = this.model.getMatrix().getVariables()[yVar].getUniStats().getMaxValue();

		p = new GlassPanel();
		f.setGlassPane(p);
		p.setVisible(true);
		addMouseListener(p);
		addMouseMotionListener(p);
	}

	//protected void paintComponent(Graphics g){
	//System.err.println("paintComponent");
	//if (g == null) return;
	//VectorGraphics vg = VectorGraphics.create(g);
	//super.paintComponent(vg);
	//}
	@Override
	public void paint(Graphics vg) {
		if (vg == null) {
			return;
		}
		VectorGraphics g = VectorGraphics.create(vg);
		super.paint(g);
		// Pulizia piano lavoro
		g.setColor(Color.WHITE);
		g.fillRect(0, 0, 640, 670);
		// Sfondo bianco del grafico
		g.fillRect(UpperLeftGraph.x, UpperLeftGraph.y, WIDTH + 1, HEIGHT + 1);
		// Sfondo grigio per label informativa
		g.setColor(Color.LIGHT_GRAY);
		g.fillRect(0, 0, UpperRightGraph.x + RIGHT_SPACE, UpperLeftGraph.y - 10);
		g.setColor(Color.BLACK);
		g.drawRect(0, 0, UpperRightGraph.x + RIGHT_SPACE, UpperLeftGraph.y - 10);
		Font fn = new Font("Arial", Font.ITALIC, 16);
		g.setFont(fn);
		g.drawString("Single left click and drag to zoom, single right click to choose scatter options", 50, 20);
		// Bordi neri del grafico
		g.setColor(Color.BLACK);
		g.drawLine(UpperLeftGraph.x - 1, UpperLeftGraph.y - 1, UpperRightGraph.x, UpperRightGraph.y - 1); //UP
		g.drawLine(UpperLeftGraph.x - 1, UpperLeftGraph.y - 1, LowerLeftGraph.x - 1, LowerLeftGraph.y); //LEFT
		g.drawLine(LowerLeftGraph.x, LowerLeftGraph.y, LowerRightGraph.x, LowerRightGraph.y); //BOTTOM
		g.drawLine(LowerRightGraph.x, LowerRightGraph.y, UpperRightGraph.x, UpperRightGraph.y); //RIGHT

		// Testo variabile in x
		fn = new Font("Arial", Font.PLAIN, 15);
		g.setFont(fn);
		g.drawString(model.getMatrix().getVariables()[xVar].getName(), XText.x, XText.y);
		// Elenco etichette valori in Y
		fn = new Font("Arial", Font.PLAIN, 9);
		g.setFont(fn);
		for (int i = 0; i <= 9; i++) {
			if (model.getMatrix().getVariables()[yVar].getType() == JWATConstants.DATE) {
				SimpleDateFormat f = new SimpleDateFormat("dd.MM.yy HH:mm:ss");
				g.drawString(f.format(new Date((long) (yMax - (yMax - yMin) / 9 * i))), WIDTH_TEXT_Y, UpperLeftGraph.y + (i * (HEIGHT / 9)));
			} else {
				g.drawString(valueFormat.format(yMax - (yMax - yMin) / 9 * i), WIDTH_TEXT_Y, UpperLeftGraph.y + (i * (HEIGHT / 9)));
			}
		}
		// Testo variabile in Y
		fn = new Font("Arial", Font.PLAIN, 17);
		fn = fn.deriveFont(AffineTransform.getRotateInstance(3.14 / 2 + 3.14));
		g.setFont(fn);
		g.drawString(model.getMatrix().getVariables()[yVar].getName(), YText.x, YText.y);
		// Testo etichette valori in x
		fn = new Font("Arial", Font.PLAIN, 9);
		fn = fn.deriveFont(AffineTransform.getRotateInstance(3.14 / 2 + 3.14));
		g.setFont(fn);
		for (int i = 0; i <= 9; i++) {
			if (model.getMatrix().getVariables()[xVar].getType() == JWATConstants.DATE) {
				SimpleDateFormat f = new SimpleDateFormat("dd.MM.yy HH:mm:ss");
				g.drawString(f.format(new Date((long) (xMin + (xMax - xMin) / 9 * i))), LowerLeftGraph.x + (i * (WIDTH / 9)), LowerLeftGraph.y
						+ HEIGHT_VALUES_X);
			} else {
				g.drawString(valueFormat.format(xMin + (xMax - xMin) / 9 * i), LowerLeftGraph.x + (i * (WIDTH / 9)), LowerLeftGraph.y
						+ HEIGHT_VALUES_X);
			}
		}
		// Disegno punti
		if (first) {
			first = false;
			new TimeConsumingWorker(new ProgressMonitorShow(this, "Plotting observations ...", 100)) {
				@Override
				public Object construct() {
					VectorGraphics g1 = VectorGraphics.create(graph.getGraphics());

					g1.setColor(Color.BLACK);
					g1.drawLine(0, 500, 500, 500);
					g1.drawLine(500, 500, 500, 0);

					g1.setColor(Color.WHITE);
					g1.fillRect(0, 0, WIDTH, HEIGHT);
					g1.setColor(Color.LIGHT_GRAY);
					g1.setStroke(new BasicStroke(1, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 0, new float[] { 1.5F }, 0));
					for (int i = 1; i < 9; i++) {
						g1.drawLine(1, 1 + (i * (HEIGHT / 9)), 499, 1 + (i * (HEIGHT / 9)));
						g1.drawLine(1 + (i * (WIDTH / 9)), 1, 1 + (i * (WIDTH / 9)), 499);
					}
					g1.setColor(Color.RED);
					VariableNumber x = model.getMatrix().getVariables()[xVar];
					x.setRangeIntervallo(x.getIndexMin(xMin), x.getIndexMax(xMax), yMin, yMax, yVar);
					/* 28 Settembre */
					try {
						initShow(x.Size());
					} catch (InterruptedException e1) {
						e1.printStackTrace();
					} catch (InvocationTargetException e1) {
						e1.printStackTrace();
					}
					int xPos = 0;
					for (int i = 0; i < x.Size(); i++) {
						try {
							xPos = x.getNextInt();
							if (i % getStep() == 0) {
								updateInfos(i, "Plotting obs " + i, false);
							}
							g1.fillOval((int) ((x.getValue(xPos) - xMin) / (xMax - xMin) * (WIDTH - 2)), (HEIGHT - 2)
									- (int) ((x.getValue(xPos, yVar) - yMin) / (yMax - yMin) * (HEIGHT - 1)), pointSize, pointSize);
						} catch (Exception e) {
							break;
						}
					}
					updateInfos(x.Size(), "End", true);
					return null;
				}

				@Override
				public void finished() {
					Scatter.this.repaint();
				}
			}.start();
		}
		g.drawImage(graph, UpperLeftGraph.x, UpperLeftGraph.y, null);
	}

	/**
	 * Shows a screenshot dialog used to select screenshot format
	 */
	public void showScreenShotDialog() {
		ExportDialog export = new ExportDialog("JWAT - version ???");
		export.showExportDialog(this, "Export as image...", this, "graph");
	}

	protected class GlassPanel extends JComponent implements MouseListener, MouseMotionListener {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public GlassPanel() {
		}

		/**
		 * Overrides paintComponent method to add support for screenshots
		 */
		/*	    protected void paintComponent(Graphics g) {
			        if (g == null) return;
			        VectorGraphics vg = VectorGraphics.create(g);
			        super.paintComponent(vg);
			    }*/

		@Override
		public void paint(Graphics g) {
			if (zoomming) {
				g.setColor(Color.BLUE);
				if (xStart < xEnd && yStart < yEnd) {
					g.drawRect(xStart, yStart, xEnd - xStart, yEnd - yStart);
				}
				if (xStart < xEnd && yStart > yEnd) {
					g.drawRect(xStart, yEnd, xEnd - xStart, yStart - yEnd);
				}
				if (xStart > xEnd && yStart < yEnd) {
					g.drawRect(xEnd, yStart, xStart - xEnd, yEnd - yStart);
				}
				if (xStart > xEnd && yStart > yEnd) {
					g.drawRect(xEnd, yEnd, xStart - xEnd, yStart - yEnd);
				}
			}
		}

		public void mouseClicked(MouseEvent e) {
			if (e.getButton() == MouseEvent.BUTTON3) {
				popup.show(p, e.getX(), e.getY());
			}
		}

		public void mousePressed(MouseEvent e) {
			// Controllo che la pressione del mouse risulti all'interno dell'area del grafico
			if ((e.getX() - UpperLeftGraph.x) >= 0 && (e.getY() - UpperLeftGraph.y) >= 0 && (e.getX() - UpperLeftGraph.x) < Scatter.this.WIDTH
					&& (e.getY() - UpperLeftGraph.y) < Scatter.this.HEIGHT) {
				// Start visualizzazione zoom area
				if (e.getButton() == MouseEvent.BUTTON1) {
					xStart = e.getX();
					yStart = e.getY();
					zoomming = true;
				}
			}
		}

		public void mouseReleased(MouseEvent e) {
			if (xStart == e.getX() && yStart == e.getY()) {
				zoomming = false;
				return;
			}
			// Controllo che il mouse venga rilasciato all'interno del grafico
			if ((e.getX() - UpperLeftGraph.x) >= 0 && (e.getY() - UpperLeftGraph.y) >= 0 && (e.getX() - UpperLeftGraph.x) < Scatter.this.WIDTH
					&& (e.getY() - UpperLeftGraph.y) < Scatter.this.HEIGHT && zoomming && e.getButton() == MouseEvent.BUTTON1) {
				xEnd = e.getX();
				yEnd = e.getY();
				zoomming = false;
				double rangeX = xMax - xMin;
				double rangeY = yMax - yMin;
				if (xStart < xEnd && yStart < yEnd) {
					xMin = xMin + rangeX / (Scatter.this.WIDTH - 2) * (xStart - UpperLeftGraph.x);
					yMax = yMax - rangeY / (Scatter.this.WIDTH - 2) * (yStart - UpperLeftGraph.y);
					xMax = xMin + rangeX / (Scatter.this.WIDTH - 2) * (xEnd - xStart);
					yMin = yMax - rangeY / (Scatter.this.WIDTH - 2) * (yEnd - yStart);
				}
				if (xStart < xEnd && yStart > yEnd) {
					xMin = xMin + rangeX / (Scatter.this.WIDTH - 2) * (xStart - UpperLeftGraph.x);
					yMax = yMax - rangeY / (Scatter.this.WIDTH - 2) * (yEnd - UpperLeftGraph.y);
					xMax = xMin + rangeX / (Scatter.this.WIDTH - 2) * (xEnd - xStart);
					yMin = yMax - rangeY / (Scatter.this.WIDTH - 2) * (yStart - yEnd);
				}
				if (xStart > xEnd && yStart < yEnd) {
					xMin = xMin + rangeX / (Scatter.this.WIDTH - 2) * (xEnd - UpperLeftGraph.x);
					yMax = yMax - rangeY / (Scatter.this.WIDTH - 2) * (yStart - UpperLeftGraph.y);
					xMax = xMin + rangeX / (Scatter.this.WIDTH - 2) * (xStart - xEnd);
					yMin = yMax - rangeY / (Scatter.this.WIDTH - 2) * (yEnd - yStart);
				}
				if (xStart > xEnd && yStart > yEnd) {
					xMin = xMin + rangeX / (Scatter.this.WIDTH - 2) * (xEnd - UpperLeftGraph.x);
					yMax = yMax - rangeY / (Scatter.this.WIDTH - 2) * (yEnd - UpperLeftGraph.y);
					xMax = xMin + rangeX / (Scatter.this.WIDTH - 2) * (xStart - xEnd);
					yMin = yMax - rangeY / (Scatter.this.WIDTH - 2) * (yStart - yEnd);
				}

				first = true;
				Scatter.this.repaint();
				this.repaint();
				return;
			} else {
				// Annullo lo zoom
				zoomming = false;
				this.repaint();
			}
		}

		public void mouseEntered(MouseEvent e) {

		}

		public void mouseExited(MouseEvent e) {

		}

		public void mouseDragged(MouseEvent e) {
			if ((e.getX() - UpperLeftGraph.x) >= 0 && (e.getY() - UpperLeftGraph.y) >= 0 && (e.getX() - UpperLeftGraph.x) < Scatter.this.WIDTH
					&& (e.getY() - UpperLeftGraph.y) < Scatter.this.HEIGHT && zoomming) {
				xEnd = e.getX();
				yEnd = e.getY();
				p.repaint();
			}
		}

		public void mouseMoved(MouseEvent e) {

		}
	}

	protected class ScatterPopupMenu extends JPopupMenu {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		public JMenuItem restore;
		public JMenuItem saveAs;
		public JMenu point;

		private AbstractAction commonActionSize = new AbstractAction() {
			/**
			 * 
			 */
			private static final long serialVersionUID = 1L;

			public void actionPerformed(ActionEvent e) {
				if (e.getActionCommand().equals("Size 1")) {
					pointSize = 1;
				}
				if (e.getActionCommand().equals("Size 2")) {
					pointSize = 2;
				}
				if (e.getActionCommand().equals("Size 3")) {
					pointSize = 3;
				}
				if (e.getActionCommand().equals("Size 4")) {
					pointSize = 4;
				}
				if (e.getActionCommand().equals("Size 5")) {
					pointSize = 5;
				}
				first = true;
				Scatter.this.repaint();
			}
		};

		public ScatterPopupMenu() {
			restore = new JMenuItem("Original view");
			saveAs = new JMenuItem("Save as...");
			point = new JMenu("Point size");
			for (int i = 0; i < 5; i++) {
				JMenuItem m = new JMenuItem();
				m.setAction(commonActionSize);
				m.setText("Size " + Integer.toString((i + 1)));
				point.add(m);
			}

			this.add(restore);
			this.addSeparator();
			this.add(point);
			this.addSeparator();
			this.add(saveAs);
			addListeners();
		}

		private void addListeners() {
			restore.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					xMin = Scatter.this.model.getMatrix().getVariables()[xVar].getUniStats().getMinValue();
					xMax = Scatter.this.model.getMatrix().getVariables()[xVar].getUniStats().getMaxValue();
					yMin = Scatter.this.model.getMatrix().getVariables()[yVar].getUniStats().getMinValue();
					yMax = Scatter.this.model.getMatrix().getVariables()[yVar].getUniStats().getMaxValue();
					first = true;
					zoomming = false;
					Scatter.this.p.repaint();
					Scatter.this.repaint();
				}
			});
			saveAs.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					Scatter.this.showScreenShotDialog();
				}
			});
		}
	}

	/**
	 * Custom file chooser class
	 */
	protected class PlotImagesFileChooser extends JFileChooser {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		protected PlotImagesFileFilter defaultFilter;

		/**
		 * Creates a File chooser in the appropriate directory user deafault.
		 * @param defaultFilter default file filter
		 */
		public PlotImagesFileChooser(PlotImagesFileFilter defaultFilter) {
			super(new File(System.getProperty("user.dir")));
			this.defaultFilter = defaultFilter;
		}

		/**
		 * Overrides default method to provide a warning if saving over an existing file
		 */
		@Override
		public void approveSelection() {
			// Gets the choosed file name
			String name = getSelectedFile().getName();
			String parent = getSelectedFile().getParent();
			if (getDialogType() == OPEN_DIALOG) {
				super.approveSelection();
			}
			if (getDialogType() == SAVE_DIALOG) {
				PlotImagesFileFilter used = ((PlotImagesFileFilter) this.getFileFilter());
				if (!name.toLowerCase().endsWith(used.getExtension())) {
					name = name + used.getExtension();
					setSelectedFile(new File(parent, name));
				}
				if (getSelectedFile().exists()) {
					int resultValue = JOptionPane.showConfirmDialog(this, "<html>File <font color=#0000ff>" + name
							+ "</font> already exists in this folder.<br>Do you want to replace it?</html>", "JMT - Warning",
							JOptionPane.OK_CANCEL_OPTION, JOptionPane.WARNING_MESSAGE);
					if (resultValue == JOptionPane.OK_OPTION) {
						getSelectedFile().delete();
						super.approveSelection();
					}
				} else {
					super.approveSelection();
				}
			}
		}
	}

	/**
	 * Inner class used to create simple file filters with only extension check
	 */
	protected class PlotImagesFileFilter extends javax.swing.filechooser.FileFilter {
		private String extension, description;

		/**
		 * Creates a new filefilter with specified extension and description
		 * @param extension extension of this filter (for example ".jmt")
		 * @param description description of this filter
		 */
		public PlotImagesFileFilter(String extension, String description) {
			this.extension = extension;
			this.description = description;
		}

		/**
		 * Whether the given file is accepted by this filter.
		 */
		@Override
		public boolean accept(File f) {
			String name = f.getName().toLowerCase();
			return name.endsWith(extension) || f.isDirectory();
		}

		/**
		 * The description of this filter
		 * @see javax.swing.filechooser.FileView#getName
		 */
		@Override
		public String getDescription() {
			return description + " (*" + extension + ")";
		}

		/**
		 * Gets extension of this filter
		 * @return extension of this filter
		 */
		public String getExtension() {
			return extension;
		}
	}
}
