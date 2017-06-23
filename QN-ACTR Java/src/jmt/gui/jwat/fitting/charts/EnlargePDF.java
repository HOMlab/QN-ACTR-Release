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
import java.awt.Graphics2D;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

import javax.imageio.ImageIO;
import javax.swing.AbstractAction;
import javax.swing.JFileChooser;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;

import jmt.engine.jwat.fitting.utils.ModelFitting;
import jmt.engine.jwat.workloadAnalysis.utils.ChangeVariableListener;
import jmt.gui.jwat.workloadAnalysis.chart.EnlargePlotDistGraph.PlotImagesFileChooser;
import jmt.gui.jwat.workloadAnalysis.chart.EnlargePlotDistGraph.PlotImagesFileFilter;
import ptolemy.plot.Plot;

public class EnlargePDF extends Plot implements MouseListener {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	// Zoom factor for zoom-in / zoom-out
	public static final double PLOT_ZOOM_FACTOR = 0.5;

	private PlotPopup pop = new PlotPopup();
	private ModelFitting model;
	private double[] data;

	public EnlargePDF(ModelFitting model) {
		this.setConnected(false, 1);
		this.setMarksStyle("various", 1);
		this.setColors(new Color[] {Color.BLUE});
		this.model = model;
		this.data = model.getListObservations();
		
		this.model.addOnChangeVariableValue(new ChangeVariableListener() {
			public void onChangeVariableValues() {
				drawGraph();
			}
		});
		this.addMouseListener(this);
		drawGraph();
	}

	private void drawGraph() {
		double[] freq_y = new double[Math.round(data.length/10)];

		this.clear(true);
		this.setConnected(false, 1);
		this.setMarksStyle("various", 1);
		this.setColors(new Color[] {Color.BLUE});
		
		setXRange(0, data[data.length-1]+1);
		setYRange(0, 1);
		
		//build the count of frequencies
		int j=0;
		double bucket_dim = data[data.length-1] / Math.round(data.length/10);
		for(int i=0;i<freq_y.length;i++) {
			
			while((data[j] < (i+1)*bucket_dim) && (data[j] >= i*bucket_dim)) {
				freq_y[i]++;
				j++;
			}
			
			if(i<(freq_y.length-1))
				freq_y[i] /= data.length;
			
			//System.out.println("Bucket from "+ i*bucket_dim + " to " + (i+1)*bucket_dim + " Percentage: " + freq_y[i]);
		}
		freq_y[freq_y.length-1]++;
		freq_y[freq_y.length-1] /= data.length;
		//******************************
	
		//Draw graph
		//Draw reference line
		for (int i = 0; i < freq_y.length; i++) {
			addPoint(1, ((i+1)*bucket_dim + i*bucket_dim)/2, freq_y[i], true); 
		}

		fillPlot();
	}

	public void mouseClicked(MouseEvent e) {
		if (e.getButton() == MouseEvent.BUTTON3) {
			pop.show(EnlargePDF.this, e.getX(), e.getY());
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

	public class PlotPopup extends JPopupMenu {
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
					EnlargePDF.this.repaint();
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
					EnlargePDF.this.repaint();
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
					int r = fileChooser.showSaveDialog(EnlargePDF.this);
					if (r == JFileChooser.APPROVE_OPTION) {
						File file = fileChooser.getSelectedFile();
						if (fileChooser.getFileFilter().equals(EPSfilter)) {
							EnlargePDF plot = EnlargePDF.this;
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
							EnlargePDF plot = EnlargePDF.this;
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
