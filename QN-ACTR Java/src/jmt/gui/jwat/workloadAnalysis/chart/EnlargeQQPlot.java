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

public class EnlargeQQPlot extends Plot implements MouseListener {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	// Zoom factor for zoom-in / zoom-out
	public static final double PLOT_ZOOM_FACTOR = 0.5;
	private ModelWorkloadAnalysis model = null;
	private int xvar;
	private int distribution;

	private PlotPopup pop = new PlotPopup();

	private double[] normale = { -3.10000, -2.32635, -2.05375, -1.88079, -1.75069, -1.64485, -1.55477, -1.47579, -1.40507, -1.34076, -1.28155,
			-1.22653, -1.17499, -1.12639, -1.08032, -1.03643, -0.99446, -0.95417, -0.91537, -0.87790, -0.84162, -0.80642, -0.77219, -0.73885,
			-0.70630, -0.67449, -0.64335, -0.61281, -0.58284, -0.55338, -0.52440, -0.49585, -0.46770, -0.43991, -0.41246, -0.38532, -0.35846,
			-0.33185, -0.30548, -0.27932, -0.25335, -0.22754, -0.20189, -0.17637, -0.15097, -0.12566, -0.10043, -0.07527, -0.05015, -0.02507,
			0.00000, 0.02507, 0.05015, 0.07527, 0.10043, 0.12566, 0.15097, 0.17637, 0.20189, 0.22754, 0.25335, 0.27932, 0.30548, 0.33185, 0.35846,
			0.38532, 0.41246, 0.43991, 0.46770, 0.49585, 0.52440, 0.55338, 0.58284, 0.61281, 0.64335, 0.67449, 0.70630, 0.73885, 0.77219, 0.80642,
			0.84162, 0.87790, 0.91537, 0.95417, 0.99446, 1.03643, 1.08032, 1.12639, 1.17499, 1.22653, 1.28155, 1.34076, 1.40507, 1.47579, 1.55477,
			1.64485, 1.75069, 1.88079, 2.05375, 2.32635, 3.10000 };

	public EnlargeQQPlot(ModelWorkloadAnalysis model, int xVar, int distr) {
		xvar = xVar;
		this.model = model;
		this.distribution = distr;
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
		double mean = model.getMatrix().getVariables()[this.xvar].getUniStats().getMean();
		this.clear(true);
		this.setConnected(false, 1);
		this.setMarksStyle("various", 1);
		this.setConnected(true, 2);
		this.setMarksStyle("point", 2);
		this.setColors(new Color[] { Color.RED, Color.BLUE });
		if (distribution == SmallQQPlot.NORMAL) {
			VariableNumber xvar = model.getMatrix().getVariables()[this.xvar];
			double var = model.getMatrix().getVariables()[this.xvar].getUniStats().getVariance();
			int[] qx = xvar.getUniStats().getQuantili();
			double xRange = xvar.getUniStats().getRangeValue() / 100;
			double yRange = (((normale[normale.length - 1] * var) + mean) - ((normale[0] * var) + mean)) / 100;
			double xMin = xvar.getUniStats().getMinValue();
			setXRange(xvar.getUniStats().getMinValue(), xvar.getUniStats().getMaxValue());
			if (xvar.getType() == JWATConstants.DATE) {
				SimpleDateFormat f = new SimpleDateFormat("dd.MM.yy HH:mm:ss");
				addXTick(f.format(new Date((long) (xMin + xRange / 10))), xMin + xRange / 10);
				addXTick(f.format(new Date((long) (xMin + (xRange * 100) / 2))), xMin + (xRange * 100) / 2);
				addXTick(f.format(new Date((long) (xMin + (xRange * 99)))), xMin + (xRange * 99));
			} else {
				setXLabel("Quantiles of " + xvar.getName());
			}
			setYRange((normale[0] * var) + mean, (normale[normale.length - 1] * var) + mean);
			setYLabel("Quantiles of normal distribution");
			for (int i = 0; i < qx.length; i++) {
				addPoint(1, xvar.getValue(qx[i]), (normale[i] * var) + mean, true);
				addPoint(2, xMin + (xRange * i), ((normale[0] * var) + mean) + (yRange * i), true);
			}
		}
		if (distribution == SmallQQPlot.EXPO) {
			VariableNumber xvar = model.getMatrix().getVariables()[this.xvar];
			int[] qx = xvar.getUniStats().getQuantili();
			double xRange = xvar.getUniStats().getRangeValue() / 100;
			double yRange = ((-Math.log(0.01) * mean) - (-Math.log(1) * mean)) / 100;
			double xMin = xvar.getUniStats().getMinValue();
			setXRange(xvar.getUniStats().getMinValue(), xvar.getUniStats().getMaxValue());
			if (xvar.getType() == JWATConstants.DATE) {
				SimpleDateFormat f = new SimpleDateFormat("dd.MM.yy HH:mm:ss");
				addXTick(f.format(new Date((long) (xMin + (xRange * 100) / 10))), xMin + (xRange * 100) / 10);
				addXTick(f.format(new Date((long) (xMin + (xRange * 100) / 2))), xMin + (xRange * 100) / 2);
				addXTick(f.format(new Date((long) (xMin + (xRange * 99)))), xMin + (xRange * 99));
			} else {
				setXLabel("Quantiles of " + xvar.getName());
			}

			setYRange(-Math.log(1) * mean, -Math.log(0.01) * mean);
			setYLabel("Quantiles of exponential distribution");
			for (int i = 0; i < qx.length; i++) {
				addPoint(1, xvar.getValue(qx[i]), -Math.log(1 - (0.01 * i)) * mean, true);
				addPoint(2, xMin + (xRange * i), (yRange * i), true);
			}
		}
		fillPlot();
	}

	public void mouseClicked(MouseEvent e) {
		if (e.getButton() == MouseEvent.BUTTON3) {
			pop.show(EnlargeQQPlot.this, e.getX(), e.getY());
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
					EnlargeQQPlot.this.repaint();
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
					EnlargeQQPlot.this.repaint();
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
					int r = fileChooser.showSaveDialog(EnlargeQQPlot.this);
					if (r == JFileChooser.APPROVE_OPTION) {
						File file = fileChooser.getSelectedFile();
						if (fileChooser.getFileFilter().equals(EPSfilter)) {
							EnlargeQQPlot plot = EnlargeQQPlot.this;
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
							EnlargeQQPlot plot = EnlargeQQPlot.this;
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
