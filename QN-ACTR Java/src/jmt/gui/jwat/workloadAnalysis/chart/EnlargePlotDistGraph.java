package jmt.gui.jwat.workloadAnalysis.chart;

//TODO: aggiungere popupmenu, salvataggio grafico

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

import jmt.engine.jwat.workloadAnalysis.utils.ChangeVariableListener;
import jmt.engine.jwat.workloadAnalysis.utils.ModelWorkloadAnalysis;
import jmt.gui.jwat.JWATConstants;
import ptolemy.plot.Plot;

public class EnlargePlotDistGraph extends Plot implements MouseListener {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	// Zoom factor for zoom-in / zoom-out
	public static final double PLOT_ZOOM_FACTOR = 0.5;
	private ModelWorkloadAnalysis model = null;
	private int var;
	private PlotPopup pop = new PlotPopup();

	/**
	 * @param var
	 */
	public EnlargePlotDistGraph(ModelWorkloadAnalysis model, int var) {
		this.var = var;
		this.model = model;
		this.model.addOnChangeVariableValue(new ChangeVariableListener() {
			public void onChangeVariableValues() {
				drawGraph();
			}
		});
		this.addMouseListener(this);
		drawGraph();
	}

	private void drawGraph() {
		this.repaint();
		this.clear(true);
		int[] values = this.model.getMatrix().getVariables()[var].getInterval1000();
		double range = this.model.getMatrix().getVariables()[var].getUniStats().getRangeValue();
		double min = this.model.getMatrix().getVariables()[var].getUniStats().getMinValue();
		setXRange(this.model.getMatrix().getVariables()[var].getValue(0), this.model.getMatrix().getVariables()[var].getValue(this.model.getMatrix()
				.getVariables()[var].Size() - 1));
		if (this.model.getMatrix().getVariables()[var].getType() == JWATConstants.DATE) {
			SimpleDateFormat f = new SimpleDateFormat("dd.MM.yy HH:mm:ss");
			addXTick(f.format(new Date((long) (min + range / 10))), min + range / 10);
			addXTick(f.format(new Date((long) (min + range / 2))), min + range / 2);
			addXTick(f.format(new Date((long) (min + range))), min + range);
		} else {
			setXLabel("Variable " + this.model.getMatrix().getVariables()[var].getName());
		}
		setYLabel("Frequencies");
		for (int i = 1; i < values.length; i++) {
			addPoint(1, min + (range * i / 1000), values[i] - values[i - 1], true);
		}
		fillPlot();
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
					EnlargePlotDistGraph.this.repaint();
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
					EnlargePlotDistGraph.this.repaint();
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
					int r = fileChooser.showSaveDialog(EnlargePlotDistGraph.this);
					if (r == JFileChooser.APPROVE_OPTION) {
						File file = fileChooser.getSelectedFile();
						if (fileChooser.getFileFilter().equals(EPSfilter)) {
							EnlargePlotDistGraph plot = EnlargePlotDistGraph.this;
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
							EnlargePlotDistGraph plot = EnlargePlotDistGraph.this;
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

	public void mouseClicked(MouseEvent e) {
		if (e.getButton() == MouseEvent.BUTTON3) {
			pop.show(EnlargePlotDistGraph.this, e.getX(), e.getY());
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

	/**
	 * Custom file chooser class
	 */
	public static class PlotImagesFileChooser extends JFileChooser {
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
	public static class PlotImagesFileFilter extends javax.swing.filechooser.FileFilter {
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
