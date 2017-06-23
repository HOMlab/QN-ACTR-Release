package jmt.gui.jwat.workloadAnalysis.chart;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.imageio.ImageIO;
import javax.swing.AbstractAction;
import javax.swing.JFileChooser;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;

import jmt.engine.jwat.VariableNumber;
import jmt.engine.jwat.workloadAnalysis.utils.ChangeVariableListener;
import jmt.engine.jwat.workloadAnalysis.utils.ModelWorkloadAnalysis;
import jmt.gui.jwat.JWATConstants;
import jmt.gui.jwat.workloadAnalysis.chart.EnlargePlotDistGraph.PlotImagesFileChooser;
import jmt.gui.jwat.workloadAnalysis.chart.EnlargePlotDistGraph.PlotImagesFileFilter;
import ptolemy.plot.Plot;

public class EnlargeQQPlotVarVar extends Plot implements MouseListener {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	// Zoom factor for zoom-in / zoom-out
	public static final double PLOT_ZOOM_FACTOR = 0.5;
	private ModelWorkloadAnalysis model = null;
	private int xvar;
	private int yvar;

	private PlotPopup pop = new PlotPopup();

	public EnlargeQQPlotVarVar(ModelWorkloadAnalysis model, int xVar, int yVar) {
		xvar = xVar;
		yvar = yVar;
		this.model = model;
		this.setConnected(false, 1);
		this.setMarksStyle("various", 1);
		this.setConnected(true, 2);
		this.setMarksStyle("point", 2);
		this.setColors(new Color[] { Color.RED, Color.BLUE });
		this.model.addOnChangeVariableValue(new ChangeVariableListener() {
			public void onChangeVariableValues() {
				drawGraph();
			}
		});
		this.addMouseListener(this);
		drawGraph();
	}

	private void drawGraph() {
		clear(true);
		this.setConnected(false, 1);
		this.setMarksStyle("various", 1);
		this.setConnected(true, 2);
		this.setMarksStyle("point", 2);
		this.setColors(new Color[] { Color.RED, Color.BLUE });

		VariableNumber xvar = model.getMatrix().getVariables()[this.xvar];
		VariableNumber yvar = model.getMatrix().getVariables()[this.yvar];
		int[] qx = xvar.getUniStats().getQuantili();
		int[] qy = yvar.getUniStats().getQuantili();
		double xRange = xvar.getUniStats().getRangeValue() / 100;
		double yRange = yvar.getUniStats().getRangeValue() / 100;
		double xMin = xvar.getUniStats().getMinValue();
		double yMin = yvar.getUniStats().getMinValue();
		setXRange(xvar.getUniStats().getMinValue(), xvar.getUniStats().getMaxValue());
		if (xvar.getType() == JWATConstants.DATE) {
			SimpleDateFormat f = new SimpleDateFormat("dd.MM.yy HH:mm:ss");
			addXTick(f.format(new Date((long) (xMin + xRange / 10))), xMin + xRange / 10);
			addXTick(f.format(new Date((long) (xMin + (xRange * 100) / 2))), xMin + (xRange * 100) / 2);
			addXTick(f.format(new Date((long) (xMin + (xRange * 99)))), xMin + (xRange * 99));
		} else {
			setXLabel("Quantiles of " + xvar.getName());
		}
		setYRange(yvar.getUniStats().getMinValue(), yvar.getUniStats().getMaxValue());
		if (yvar.getType() == JWATConstants.DATE) {
			SimpleDateFormat f = new SimpleDateFormat("dd.MM.yy HH:mm:ss");
			addYTick(f.format(new Date((long) (yMin + yRange / 10))), yMin + yRange / 10);
			addYTick(f.format(new Date((long) (yMin + (yRange * 100) / 2))), yMin + (yRange * 100) / 2);
			addYTick(f.format(new Date((long) (yMin + (yRange * 99)))), yMin + (yRange * 99));
		} else {
			setYLabel("Quantiles of " + yvar.getName());
		}
		for (int i = 0; i < qx.length; i++) {
			addPoint(1, xvar.getValue(qx[i]), yvar.getValue(qy[i]), true);
			addPoint(2, xMin + (xRange * i), yMin + (yRange * i), true);
		}
		fillPlot();
	}

	public void mouseClicked(MouseEvent e) {
		if (e.getButton() == MouseEvent.BUTTON3) {
			pop.show(EnlargeQQPlotVarVar.this, e.getX(), e.getY());
		}
	}

	public void mousePressed(MouseEvent e) {
	}

	public void mouseReleased(MouseEvent e) {
	}

	public void mouseEntered(MouseEvent e) {
	}

	public void mouseExited(MouseEvent e) {
	}

	protected class PlotPopup extends JPopupMenu {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		public JMenuItem restore;
		public JMenuItem zoomIn;
		public JMenuItem zoomOut;
		public JMenuItem saveAs;

		public PlotPopup() {
			restore = new JMenuItem("Original view");
			zoomIn = new JMenuItem("Zoom in");
			zoomOut = new JMenuItem("Zoom out");
			saveAs = new JMenuItem("Save as...");

			this.add(restore);
			this.add(zoomIn);
			this.add(zoomOut);
			this.addSeparator();
			this.add(saveAs);
			addListeners();
		}

		private void addListeners() {
			restore.addActionListener(new AbstractAction() {
				/**
				 * 
				 */
				private static final long serialVersionUID = 1L;

				public void actionPerformed(ActionEvent e) {
					fillPlot();
				}
			});

			zoomIn.addActionListener(new AbstractAction() {
				/**
				 * 
				 */
				private static final long serialVersionUID = 1L;

				public void actionPerformed(ActionEvent e) {
					// Gets maximum and minimum values
					double[] XBounds = getXAutoRange();
					double[] YBounds = getYAutoRange();

					double[] xRange = getXRange();
					double[] yRange = getYRange();
					double width = xRange[1] - xRange[0];
					double height = yRange[1] - yRange[0];
					double newWidth = (xRange[1] - xRange[0]) * PLOT_ZOOM_FACTOR;
					double newHeight = (yRange[1] - yRange[0]) * PLOT_ZOOM_FACTOR;
					double newXMin = xRange[0] + (width - newWidth) / 2;
					//The next 7 lines check that the new range is valid
					if (newXMin < XBounds[0]) {
						newXMin = XBounds[0];
					}
					double newXMax = xRange[0] + (width - newWidth) / 2 + newWidth;
					if (newXMax > XBounds[1]) {
						newXMax = XBounds[1];
					}
					double newYMin = yRange[0] + (height - newHeight) / 2;
					if (newYMin < YBounds[0]) {
						newYMin = YBounds[0];
					}
					double newYMax = yRange[0] + (height - newHeight) / 2 + newHeight;
					if (newYMax > YBounds[1]) {
						newYMax = YBounds[1];
					}
					setXRange(newXMin, newXMax);
					setYRange(newYMin, newYMax);
					EnlargeQQPlotVarVar.this.repaint();
				}
			});

			zoomOut.addActionListener(new AbstractAction() {
				/**
				 * 
				 */
				private static final long serialVersionUID = 1L;

				public void actionPerformed(ActionEvent e) {
					// Gets maximum and minimum values
					double[] XBounds = getXAutoRange();
					double[] YBounds = getYAutoRange();

					double[] xRange = getXRange();
					double[] yRange = getYRange();
					double width = xRange[1] - xRange[0];
					double height = yRange[1] - yRange[0];
					double newWidth = (xRange[1] - xRange[0]) / PLOT_ZOOM_FACTOR;
					double newHeight = (yRange[1] - yRange[0]) / PLOT_ZOOM_FACTOR;
					double newXMin = xRange[0] + (width - newWidth) / 2;
					//The next 7 lines check that the new range is valid
					if (newXMin < XBounds[0]) {
						newXMin = XBounds[0];
					}
					double newXMax = xRange[0] + (width - newWidth) / 2 + newWidth;
					if (newXMax > XBounds[1]) {
						newXMax = XBounds[1];
					}
					double newYMin = yRange[0] + (height - newHeight) / 2;
					if (newYMin < YBounds[0]) {
						newYMin = YBounds[0];
					}
					double newYMax = yRange[0] + (height - newHeight) / 2 + newHeight;
					if (newYMax > YBounds[1]) {
						newYMax = YBounds[1];
					}
					setXRange(newXMin, newXMax);
					setYRange(newYMin, newYMax);
					EnlargeQQPlotVarVar.this.repaint();
				}
			});

			saveAs.addActionListener(new AbstractAction() {
				/**
				 * 
				 */
				private static final long serialVersionUID = 1L;

				public void actionPerformed(ActionEvent e) {
					PlotImagesFileFilter PNGfilter = new PlotImagesFileFilter(".png", "Portable Network Graphics images");
					PlotImagesFileFilter EPSfilter = new PlotImagesFileFilter(".eps", "Encapsulated Post Script images");
					PlotImagesFileChooser fileChooser = new PlotImagesFileChooser(PNGfilter);
					fileChooser.setFileFilter(PNGfilter);
					fileChooser.addChoosableFileFilter(EPSfilter);
					int r = fileChooser.showSaveDialog(EnlargeQQPlotVarVar.this);
					if (r == JFileChooser.APPROVE_OPTION) {
						File file = fileChooser.getSelectedFile();
						if (fileChooser.getFileFilter().equals(EPSfilter)) {
							EnlargeQQPlotVarVar plot = EnlargeQQPlotVarVar.this;
							try {
								FileOutputStream fileStream = new FileOutputStream(file);
								plot.export(fileStream);
								fileStream.close();
							} catch (FileNotFoundException fnf) {
								JOptionPane.showMessageDialog(fileChooser, "File not found", "JMT - Error", JOptionPane.ERROR_MESSAGE);
							} catch (IOException ioe) {
								JOptionPane.showMessageDialog(fileChooser, "I/O exception", "JMT - Error", JOptionPane.ERROR_MESSAGE);
							}
						} else {
							EnlargeQQPlotVarVar plot = EnlargeQQPlotVarVar.this;
							BufferedImage image = plot.exportImage();
							try {
								BufferedImage originalImage = convertType(image, BufferedImage.TYPE_INT_RGB);
								ImageIO.write(originalImage, "png", file);
								ImageIO.createImageOutputStream(file).close();
							} catch (IOException fnf) {
								JOptionPane.showMessageDialog(null, "File not found");
							}
						}
					}
				}

			});
		}

		BufferedImage convertType(BufferedImage src, int targetType) {
			if (src.getType() == targetType) {
				return src;
			}
			BufferedImage tgt = new BufferedImage(src.getWidth(), src.getHeight(), targetType);
			Graphics2D g = tgt.createGraphics();
			g.drawRenderedImage(src, null);
			g.dispose();
			return tgt;
		}
	}
}
