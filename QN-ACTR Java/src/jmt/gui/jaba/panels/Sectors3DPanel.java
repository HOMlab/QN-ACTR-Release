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
package jmt.gui.jaba.panels;

import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.awt.geom.AffineTransform;
import java.awt.geom.Line2D;
import java.text.DecimalFormat;
import java.util.Iterator;
import java.util.TreeSet;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;

import jmt.engine.jaba.Sector3D;
import jmt.engine.jaba.grahamScan;
import jmt.engine.jaba.newPoint;

/**
 * <p>Title: Sectors 3D Panel</p>
 * <p>Description: This panel is used to show saturation sectors on 3-class models.</p>
 * <p>This was heavily modified by Bertoli Marco to allow smart label positioning and
 * skipping if too many labels must be shown on the image. Now it allows resizing too.</p>
 *
 * @author Bertoli Marco, Zanzottera Andrea
 *         Date: 8-feb-2006
 *         Time: 11.42.37
 */
public class Sectors3DPanel extends JPanel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	// Colors for sectors and lines
	private static final Color SINGLE = new Color(255, 200, 0);
	private static final Color DOUBLE = new Color(255, 150, 75);
	private static final Color MORE = new Color(192, 0, 0);
	private static final Color LINESINGLE = Color.gray;
	private static final Color LINEDOUBLE = Color.CYAN;
	private static final Color LINEMORE = Color.BLUE;
	private static final Color BLACK = new Color(0, 0, 0);
	private static final Color GREY = new Color(100, 100, 100);
	// Background
	private static final Color BGCOLOR = new Color(255, 255, 255);
	// Stroke styles
	private static final BasicStroke LABEL_LINE = new BasicStroke(2);
	private static final BasicStroke SECTORS = new BasicStroke(1);
	private static final BasicStroke LINES = new BasicStroke(1);
	// Constants
	private final double rad3 = Math.sqrt(3);
	private final double rad3d2 = Math.sqrt(3) / 2;
	private final double rad3d3 = Math.sqrt(3) / 3;
	// Used to format numbers
	private static final DecimalFormat formatter = new DecimalFormat("0.000");
	// Resulta data and class names
	private Vector<Object> s3d;
	private String[] classNames;

	// Used to immagazine traslation coordinates
	private double traslationX, traslationY;

	// Tells if graph is shown
	private boolean isShown = false;
	// Stores height of the graph
	private int height;

	// Label to show coordinates
	private JLabel coordLabel;

	/**
	 * Builds a new Sectors3D Panel to show results of 3-class models
	 * @param s3d results vector
	 * @param classNames array with class names
	 */
	public Sectors3DPanel(Vector<Object> s3d, String[] classNames) {
		super(new BorderLayout());
		this.s3d = s3d;
		this.classNames = classNames;
		this.setBackground(BGCOLOR);
		this.setBorder(BorderFactory.createEtchedBorder());

		// Label to show coordinates
		coordLabel = new JLabel();
		coordLabel.setBorder(BorderFactory.createEtchedBorder());
		coordLabel.setVisible(false);
		coordLabel.setOpaque(true);
		// Puts label on south-east corner
		JPanel tmp = new JPanel(new BorderLayout());
		tmp.add(coordLabel, BorderLayout.EAST);
		tmp.setOpaque(false);
		this.add(tmp, BorderLayout.SOUTH);

		// Adds a mouseListener to show graph coordinates
		this.addMouseMotionListener(new MouseMotionAdapter() {
			@Override
			public void mouseMoved(MouseEvent e) {
				if (isShown) {
					String coord = getCoordinates(e.getX(), e.getY());
					if (coord != null) {
						coordLabel.setText(coord);
						coordLabel.setVisible(true);
						Sectors3DPanel.this.setCursor(Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR));
					} else {
						coordLabel.setText("");
						coordLabel.setVisible(false);
						Sectors3DPanel.this.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
					}
				}
			}
		});
	}

	/**
	 * Overrides default paint method to draw graph
	 * @param g graphic object. <b>Must</b> be an instance of Graphics2D
	 */
	@Override
	public void paint(Graphics g) {
		isShown = false;
		// This is the height of the graph

		super.paint(g);
		Graphics2D g2;

		// If g is not instance of Graphic2D, aborts method
		if (g instanceof Graphics2D) {
			g2 = (Graphics2D) g;
		} else {
			return;
		}

		Color sectorcolor;

		int maxStationNameWidth = (int) g2.getFontMetrics().getStringBounds("StationX", g2).getWidth();

		int limitHeight, limitWidth;

		limitHeight = (int) (getHeight() * 7.5 / 10.0);
		limitWidth = (int) (((getWidth() / rad3) - maxStationNameWidth * 3 - 20) * .95);

		height = (limitHeight < limitWidth) ? limitHeight : limitWidth;

		// If height is too small, aborts paint
		if (height < 20) {
			return;
		}

		// Sets traslation coordinates
		traslationX = .1 * height + 15;
		traslationY = getHeight() - height * 0.1 - 10;

		// Now traslates image
		AffineTransform aT = g2.getTransform();
		aT.setToTranslation(.1 * height + 15, getHeight() - height * 0.1 - 10);
		g2.transform(aT);

		// Cambia lo Stroke e il colore per evidenziare la linea
		g2.setStroke(SECTORS);

		// Disegna lo sfondo del triangolo
		int[] xt = { 0, (int) Math.floor(rad3d2 * height), (int) Math.floor(rad3 * height) };
		int[] yt = { 0, -1 * height, 0 };
		g2.setColor(SINGLE);
		g2.fillPolygon(xt, yt, 3);

		if (s3d != null) {
			// Used to sort label ascendingly by position
			TreeSet<Caption> labels = new TreeSet<Caption>();

			// per ogni settore
			for (int i = 0; i < s3d.size(); i++) {
				Sector3D sector = (Sector3D) s3d.get(i);
				//System.out.println("Totale: "+s3d.size()+", Parziale: "+i);
				// Sono le coordinate dei punti che saranno passati al metodo fillPolygon
				int[] xxp = new int[sector.CountPoint()];
				int[] yyp = new int[sector.CountPoint()];

				Vector<newPoint> points = new Vector<newPoint>();

				// per ogni punto di un settore
				for (int j = 0; j < sector.CountPoint(); j++) {

					double pb11 = sector.getx(j);
					double pb12 = sector.gety(j);

					int pb1 = (int) Math.floor(pb11 * height);
					int pb2 = (int) Math.floor(pb12 * height);

					//Aggiungo il punto al vettore da passare al Grahamscan
					newPoint temp = new newPoint(pb1, pb2);
					points.addElement(temp);
				}

				//Coloro il settore in base al numero di stazioni che vi saturano
				int numstat = sector.getType();
				if (numstat > 2) {
					sectorcolor = MORE;
				} else if (numstat == 2) {
					sectorcolor = DOUBLE;
				} else {
					sectorcolor = SINGLE;
				}

				grahamScan gr = new grahamScan();
				Vector<newPoint> ordpoint = gr.doGraham(points);

				for (int j = 0; j < ordpoint.size(); j++) {
					xxp[j] = ordpoint.get(j).x;
					yyp[j] = -ordpoint.get(j).y;
				}

				// Disegna i settori di saturazione
				g2.setColor(sectorcolor);
				g2.fillPolygon(xxp, yyp, sector.CountPoint());
				if (numstat != 1) {
					g2.setColor(Color.BLACK);
					g2.draw(new Polygon(xxp, yyp, sector.CountPoint()));
				}

				// Adds this sector to labels
				double[] centre = sector.getCentre();
				labels.add(new Caption(sector.getName(), centre[0], centre[1], sector.getType()));

			}//per ogni settore

			// Now draws captions and link them to the corresponding sector
			Iterator<Caption> i = labels.iterator();
			// Stores prevoius y value to avoid collision
			float previousy = Float.NEGATIVE_INFINITY;
			// Value where label should be placed
			float yvalue;

			boolean skipSingle = false, skipDouble = false;

			// Skips single and double sector labels if they are too many
			Caption first = labels.first();
			float textHeight = (float) g2.getFontMetrics().getStringBounds(first.getLabel(), g2).getHeight();
			if (first.getY() * height + textHeight * labels.size() > getHeight()) {
				skipSingle = true;
				// now checks if double have to be skipped too
				int count = 0;
				while (i.hasNext()) {
					if (i.next().getStatN() > 1) {
						count++;
					}
				}
				// resets iterator for next cycle
				i = labels.iterator();
				// Decides if doubles have to be skipped too
				if (first.getY() * height + textHeight * count > getHeight()) {
					skipDouble = true;
				}
			}

			while (i.hasNext()) {
				Caption c = i.next();
				// Performs skipping if needed
				if (skipSingle && c.getStatN() == 1) {
					continue;
				} else if (skipDouble && c.getStatN() == 2) {
					continue;
				}

				// Select color basing on number of station in this sector
				if (c.getStatN() == 1) {
					g2.setColor(LINESINGLE);
				} else if (c.getStatN() == 2) {
					g2.setColor(LINEDOUBLE);
				} else {
					g2.setColor(LINEMORE);
				}

				yvalue = (float) c.getY() * height;
				if (yvalue < previousy) {
					// collision detected
					yvalue = previousy;
				}

				// Writes label and draw its line
				g2.setStroke(LABEL_LINE);
				g2.draw(new Line2D.Double(height * (rad3 + .1) + 10 - 4, -yvalue, c.getX() * height, -c.getY() * height));
				g2.setStroke(LINES);
				g2.setColor(BLACK);
				g2.drawString(c.getLabel(), (float) (height * (rad3 + .1) + 10), -yvalue
						+ (float) g2.getFontMetrics().getStringBounds(c.getLabel(), g2).getHeight() / 2 - 4);
				// updates previousy adding height of this label
				previousy = yvalue + (float) g2.getFontMetrics().getStringBounds(c.getLabel(), g2).getHeight();
			}

			// Disegna gli assi
			g2.setStroke(LINES);
			sectorcolor = BLACK;
			g2.setColor(sectorcolor);
			Line2D class3 = new Line2D.Double(rad3d2 * height, -1 * height, rad3d2 * height, -1.1 * height);
			Line2D class2 = new Line2D.Double(rad3 * height, 0, rad3 * height + .1 * height, .05 * height);
			Line2D class1 = new Line2D.Double(0, 0, -.1 * height, .05 * height);
			g2.draw(class3);
			g2.draw(class2);
			g2.draw(class1);

			// Arrow on class 3 axis
			Line2D fclass31 = new Line2D.Double(rad3d2 * height, -1.1 * height, rad3d2 * height - 0.05 * height, -1.1 * height + 0.05 * height);
			Line2D fclass32 = new Line2D.Double(rad3d2 * height, -1.1 * height, rad3d2 * height + 0.05 * height, -1.1 * height + 0.05 * height);
			g2.draw(fclass31);
			g2.draw(fclass32);

			// Arrow on class 2 axis
			Line2D fclass21 = new Line2D.Double(rad3 * height + .1 * height, .05 * height, rad3 * height + .1 * height - .06 * height, .05 * height
					+ .03 * height);
			Line2D fclass22 = new Line2D.Double(rad3 * height + .1 * height, .05 * height, rad3 * height + .1 * height - .03 * height, .05 * height
					- .06 * height);
			g2.draw(fclass21);
			g2.draw(fclass22);

			// Arrow on class 1 axis
			Line2D fclass11 = new Line2D.Double(-.1 * height, .05 * height, -.1 * height + .06 * height, .05 * height + 0.03 * height);
			Line2D fclass12 = new Line2D.Double(-.1 * height, .05 * height, -.1 * height + .03 * height, .05 * height - 0.06 * height);
			g2.draw(fclass11);
			g2.draw(fclass12);

			// Axis labels
			g2.setColor(GREY);
			g2.drawString(classNames[2] + " %", (float) (height * rad3d2 - g2.getFontMetrics().getStringBounds(classNames[2] + " %", g2).getWidth()),
					-height);
			g2.drawString(classNames[1] + " %", (float) (height * rad3 - g2.getFontMetrics().getStringBounds(classNames[1] + " %", g2).getWidth()),
					15);
			g2.drawString(classNames[0] + " %", 0, 15);
			// Graph is shown
			isShown = true;

			// Disegna il perimetro del triangolo
			g2.setColor(Color.BLACK);
			g2.draw(new Polygon(xt, yt, 3));
		}
	}

	/**
	 * This method will calculate mouse position in beta coordinates
	 * @param x mouse position x coordinate
	 * @param y mouse position y coordinate
	 * @return  string with mouse position in beta coordinates or null if mouse is out of
	 * graph
	 */
	protected String getCoordinates(int x, int y) {
		double b1 = rad3d3 * (x - traslationX) / height - 0.5 * (traslationY - y) / height;
		double b2 = (traslationY - y) / height;
		double b0 = 1 - b1 - b2;

		if (b0 >= 0 && b0 <= 1 && b1 >= 0 && b1 <= 1 && b2 >= 0 && b2 <= 1) {
			return classNames[0] + ": " + formatter.format(b0) + "% " + classNames[1] + ": " + formatter.format(b1) + "% " + classNames[2] + ": "
					+ formatter.format(b2) + "%";
		} else {
			return null;
		}
	}

	/**
	 * Inner class used to sort sector captions
	 * <br> Author: Bertoli Marco
	 */
	protected class Caption implements Comparable {
		private double x, y;
		private String label;
		private int statN;

		/**
		 * Builds a new Caption object
		 * @param label label of the caption
		 * @param x value of x coordinate
		 * @param y value of y coordinate
		 * @param statN number of stations
		 */
		public Caption(String label, double x, double y, int statN) {
			this.label = label;
			this.x = x;
			this.y = y;
			this.statN = statN;
		}

		/**
		 * Gets value of x
		 * @return value of x
		 */
		public double getX() {
			return x;
		}

		/**
		 * value of y
		 * @return value of y
		 */
		public double getY() {
			return y;
		}

		/**
		 * Gets label caption
		 * @return label caption
		 */
		public String getLabel() {
			return label;
		}

		/**
		 * Gets station number in this sector
		 * @return station number in this sector
		 */
		public int getStatN() {
			return statN;
		}

		/**
		 * <p>Compares this object with the specified object for order.  Returns a
		 * negative integer, zero, or a positive integer as this object is less
		 * than, equal to, or greater than the specified object.</p>
		 * <p>Compares y values and if they are equals compares x values.</p>
		 *
		 * @param o the Object to be compared.
		 * @return a negative integer, zero, or a positive integer as this object
		 *         is less than, equal to, or greater than the specified object.
		 * @throws ClassCastException if the specified object's type prevents it
		 *                            from being compared to this Object.
		 */
		public int compareTo(Object o) {
			Caption c = (Caption) o;
			// Compares y values and if they are equals, compares x
			if (y < c.y) {
				return -1;
			} else if (y > c.y) {
				return 1;
			} else {
				if (x < c.x) {
					return -1;
				} else if (x > c.x) {
					return 1;
				} else {
					return 0;
				}
			}
		}
	}

}
