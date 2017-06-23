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
package jmt.framework.gui.graph;

import java.awt.Graphics2D;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Iterator;
import java.util.Vector;

import javax.imageio.ImageIO;
import javax.swing.AbstractAction;
import javax.swing.JFileChooser;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;

import ptolemy.plot.Plot;

/**
 * <p>Title: What-If analysis Plot</p>
 * <p>Description: This class provides a customization of Ptolemy Plot
 * object used for JMVA what-if analysis.</p>
 *
 * @author Bertoli Marco
 *         Date: 1-giu-2006
 *         Time: 15.44.43
 */
public class WhatIfPlot extends Plot {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	// Zoom factor for zoom-in / zoom-out
	public static final double PLOT_ZOOM_FACTOR = 0.5;
	// Values for x-axis
	private double[] xAxis;
	// Popup menu
	private PlotPopupMenu popup = new PlotPopupMenu();
	// Rescale listeners
	private Vector<RescaleListener> listeners = new Vector<RescaleListener>();

	public WhatIfPlot(double[] xAxis) {
		super();
		this.xAxis = xAxis;
		// Adds popup menu
		this.addMouseListener(new MouseAdapter() {
			/**
			 * Invoked when the mouse has been clicked on a component.
			 */
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getButton() == MouseEvent.BUTTON3) {
					popup.show(WhatIfPlot.this, e.getX(), e.getY());
				}
			}
		});
	}

	/**
	 * Draw given values on selected line with selected caption
	 * @param num number of dataset to be drawn
	 * @param values values to be drawn
	 * @throws IllegalArgumentException if dataset size does not match with x-axis values
	 */
	public void draw(int num, double[] values) throws IllegalArgumentException {
		if (values.length != xAxis.length) {
			throw new IllegalArgumentException("Length of provided dataset does not match x-axis points.");
		}
		// Clears target dataset
		clear(num);
		// Adds values
		for (int i = 0; i < xAxis.length; i++) {
			addPoint(num, xAxis[i], values[i], true);
		}
	}

	// --- Callbacks for rescale --------------------------------------------------------------
	/**
	 * Adds a rescale listener that is notified each time graph is rescaled.
	 * @param listener listener to be added
	 */
	public void addRescaleListener(RescaleListener listener) {
		listeners.add(listener);
	}

	/**
	 * Removes a rescale listener
	 * @param listener listener to be removed
	 */
	public void removeRescaleListener(RescaleListener listener) {
		listeners.remove(listener);
	}

	/**
	 * Called each time a rescale event occurrs
	 */
	protected void fireRescaleEvent() {
		Iterator<RescaleListener> i = listeners.iterator();
		while (i.hasNext()) {
			i.next().Rescaled();
		}
	}

	/**
	 * Interface used for rescaling callbacks
	 */
	public interface RescaleListener {
		/**
		 * Called each time graph is rescaled
		 */
		public void Rescaled();
	}

	/**
	 * Overrides default method to add firing of rescale change events
	 */
	@Override
	public synchronized void setXRange(double v, double v1) {
		super.setXRange(v, v1);
		fireRescaleEvent();
	}

	/**
	 * Overrides default method to add firing of rescale change events
	 * Avoid problems with machine precision on constant measures too.
	 */
	@Override
	public synchronized void setYRange(double v, double v1) {
		// Avoid rescaling to a too small scale (for machine precision problems)
		if (Math.abs(v - v1) > 1e-8) {
			super.setYRange(v, v1);
		} else {
			double mean = (v + v1) / 2;
			super.setYRange(mean, mean);
		}
		fireRescaleEvent();
	}

	// ----------------------------------------------------------------------------------------

	// --- Methods for popup menu -------------------------------------------------------------
	/**
	 * A simple JPopupMenu used to manage operations on plot. It gives the
	 * choice to zoom in and out on the plot, restore original view and save plot to
	 * images (in EPS or PNG format)
	 */
	protected class PlotPopupMenu extends JPopupMenu {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		public JMenuItem restore;
		public JMenuItem zoomIn;
		public JMenuItem zoomOut;
		public JMenuItem saveAs;

		public PlotPopupMenu() {
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

		public void addListeners() {
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
					WhatIfPlot.this.repaint();
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
					WhatIfPlot.this.repaint();
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
					int r = fileChooser.showSaveDialog(WhatIfPlot.this);
					if (r == JFileChooser.APPROVE_OPTION) {
						File file = fileChooser.getSelectedFile();
						if (fileChooser.getFileFilter().equals(EPSfilter)) {
							WhatIfPlot plot = WhatIfPlot.this;
							try {
								FileOutputStream fileStream = new FileOutputStream(file);
								plot.export(fileStream);
								fileStream.close();
							} catch (FileNotFoundException fnf) {
								JOptionPane.showMessageDialog(fileChooser, "File not found", "File output - Error", JOptionPane.ERROR_MESSAGE);
							} catch (IOException ioe) {
								JOptionPane.showMessageDialog(fileChooser, "I/O exception", "File output - Error", JOptionPane.ERROR_MESSAGE);
							}
						} else {
							WhatIfPlot plot = WhatIfPlot.this;
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

	/**
	 * Custom file chooser class
	 */
	protected static class PlotImagesFileChooser extends JFileChooser {
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
							+ "</font> already exists in this folder.<br>Do you want to replace it?</html>", "File save - Warning",
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
	protected static class PlotImagesFileFilter extends javax.swing.filechooser.FileFilter {
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
	// ----------------------------------------------------------------------------------------
}
