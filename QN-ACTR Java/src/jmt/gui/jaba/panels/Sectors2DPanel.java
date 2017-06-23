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
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.text.DecimalFormat;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.JPanel;

import jmt.engine.jaba.FinalSect2D;

/**
 * <p>Title: Sectors 2D Panel</p>
 * <p>Description: This panel is used to show saturation sectors on 2-class models.</p>
 * <p>This was reimlemented by Bertoli Marco as original one had strong redraw
 * problems, made strange things with rotations, didn't autoscale and overlapped labels
 * too frequently.</p>
 *
 * @author Bertoli Marco, Zanzottera Andrea
 *         Date: 8-feb-2006
 *         Time: 11.33.48
 */
public class Sectors2DPanel extends JPanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private Vector<Object> s3d;
	private String[] classNames;

	// Margin used for labels and around graph
	private static final int MARGIN = 10;
	// Defines arrow length
	private static final int AMARGIN = 12;

	// Various colors
	private static final Color SINGLE = new Color(125, 255, 0);
	private static final Color DOUBLE = new Color(0, 125, 255);
	private static final Color MORE = Color.RED;
	private static final Color BLACK = Color.BLACK;
	private static final Color GREY = Color.GRAY;

	private static final Color BGCOLOR = Color.WHITE;

	private static final BasicStroke DOTTED = new BasicStroke(1, 1, 1, 1, new float[] { 2f }, 1);
	private static final BasicStroke SECTORS = new BasicStroke(3);
	private static final BasicStroke LINES = new BasicStroke(1);

	// Initial x and y values and measure unit
	private int x0, y0, unit;

	// Used to format numbers
	private static final DecimalFormat formatter = new DecimalFormat("0.000");

	/**
	 * Builds a new Sectors2DPanel
	 * @param s3d vector with output of Jaba engine
	 * @param classNames name of classes to be shown on axis
	 */
	public Sectors2DPanel(Vector<Object> s3d, String[] classNames) {
		super();
		this.s3d = s3d;
		this.classNames = classNames;
		this.setBorder(BorderFactory.createEtchedBorder());
		this.setBackground(BGCOLOR);
	}

	/**
	 * Overrides default paint method to draw the graph
	 * @param g graphic object. <b>Must</b> be an instance of Graphics2D
	 */
	@Override
	public void paint(Graphics g) {
		super.paint(g);

		Graphics2D g2;

		// If g is not instance of Graphic2D, aborts method
		if (g instanceof Graphics2D) {
			g2 = (Graphics2D) g;
		} else {
			return;
		}

		// Gets font metrics
		Rectangle2D fontBounds = g2.getFontMetrics().getStringBounds("0.999", g2);
		int maxStationNameWidth = (int) g2.getFontMetrics().getStringBounds("StationXXXXXX", g2).getWidth();

		int maxDimension;

		// Find if height or width is limiting size
		int limitHeight, limitWidth;

		limitHeight = getHeight() - 2 * MARGIN - 2 * AMARGIN - (int) (fontBounds.getHeight() * 2) - 5;
		limitWidth = getWidth() - 3 * MARGIN - 2 * AMARGIN - (int) fontBounds.getWidth() - 10 - maxStationNameWidth;

		maxDimension = (limitHeight < limitWidth) ? limitHeight : limitWidth;

		// If space is too small or we have not any result yes, aborts draw.
		if (maxDimension < 20 || s3d == null) {
			return;
		}

		Color sectorcolor;

		// Initializes margins
		x0 = (int) fontBounds.getWidth() + MARGIN + 5;
		y0 = getHeight() - MARGIN - (int) (fontBounds.getHeight() * 2) - 5;
		unit = maxDimension;
		// Position of station names labels
		int xetich = (int) (getX(1) + (AMARGIN + MARGIN) * 2);

		// Used to avoid station label overlapping
		float previousFirstPos = Float.POSITIVE_INFINITY;

		for (int i = 0; i < s3d.size(); i++) {
			// Current sector
			FinalSect2D sect = (FinalSect2D) s3d.get(i);
			double pb11 = sect.getBeta11();
			double pb12 = sect.getBeta1();
			double pb21 = sect.getBeta22();
			double pb22 = sect.getBeta2();

			// Station's number
			int numstat = sect.countStation();

			if (numstat > 2) {
				sectorcolor = MORE;
			} else if (numstat == 2) {
				sectorcolor = DOUBLE;
			} else {
				sectorcolor = SINGLE;
			}

			g2.setStroke(SECTORS);

			g2.setColor(sectorcolor);
			g2.draw(new Line2D.Double(getX(pb11), getY(pb12), getX(pb21), getY(pb22)));

			// Dotted line
			g2.setStroke(DOTTED);
			sectorcolor = GREY;
			g2.setColor(sectorcolor);
			g2.draw(new Line2D.Double(getX(pb21), getY(pb22), getX(0), getY(pb22)));
			g2.draw(new Line2D.Double(getX(pb21), getY(pb22), getX(pb21), getY(0)));

			// Values on X-axis
			g2.setColor(BLACK);
			String coordx = formatter.format(pb21);

			if (i % 2 == 1 || s3d.size() == 1) {
				g2.drawString(coordx, (float) (getX(pb21) - fontBounds.getWidth() / 2), (float) getY(0) + 15);
			} else {
				g2.drawString(coordx, (float) (getX(pb21) - fontBounds.getWidth() / 2), (float) (getY(0) + 17 + fontBounds.getHeight()));
				g2.setStroke(DOTTED);
				g2.setColor(GREY);
				g2.draw(new Line2D.Double(getX(pb21), getY(0), getX(pb21), (float) (getY(0) + fontBounds.getHeight() + 2)));
			}

			// Values on Y-axis
			g2.setColor(BLACK);
			String coordy = formatter.format(pb12);

			g2.drawString(coordy, (float) (getX(0) - fontBounds.getWidth() - 5), (float) (getY(pb12) + fontBounds.getHeight() / 2 - 2));

			// Now draws station name labels. They are centered on the line of the caption (if possible)
			g2.setColor(BLACK);
			String etichetta = (sect.getstation()).get(0).getName();

			// position of first label. Try to center with the middle of the sector but avoids overlapping with previous ones
			boolean overlapping = false;
			float firstpos = (float) (getY((pb12 + pb22) / 2) - (numstat - 1) * (fontBounds.getHeight()) / 2) + 3;
			if (firstpos + (numstat + .5) * (float) fontBounds.getHeight() > previousFirstPos) {
				// Labels are overlapping
				firstpos = (float) (previousFirstPos - (numstat + .5) * fontBounds.getHeight());
				overlapping = true;
			}
			// Updates previousFirstPos value
			previousFirstPos = firstpos;

			g2.drawString(etichetta, xetich, firstpos);
			// Next labels
			for (int j = 1; j < numstat; j++) {
				String etich = (sect.getstation()).get(j).getName();
				g2.drawString(etich, xetich, firstpos + j * (float) fontBounds.getHeight());
			}

			// Draws the line of the caption. Prefers horizontal line if possible
			g2.setColor(BLACK);
			g2.setStroke(LINES);
			if (!overlapping) {
				g2
						.draw(new Line2D.Double(getX((pb11 + pb21) / 2) + MARGIN / 2, getY((pb12 + pb22) / 2), xetich - MARGIN / 2,
								getY((pb12 + pb22) / 2)));
			} else {
				g2.draw(new Line2D.Double(getX((pb11 + pb21) / 2) + MARGIN / 2, getY((pb12 + pb22) / 2), xetich - MARGIN / 2,
						(float) (firstpos + (numstat - 1) * (fontBounds.getHeight()) / 2) - 3));
			}

		}

		// Limit values
		if (s3d.size() > 1) {
			g2.setColor(BLACK);
			String coordx = "1.000";
			g2.drawString(coordx, (float) (getX(1) - fontBounds.getWidth() / 2), (float) getY(0) + 15);
			g2.drawString(coordx, (float) (getX(0) - fontBounds.getWidth() - 5), (float) (getY(1) + fontBounds.getHeight() / 2) - 2);
		}
		// Eliminates graph area that overlaps axis
		g2.setColor(BGCOLOR);
		g2.fillRect((int) getX(0) - 5, (int) getY(1) - 5, 5, 10);
		g2.fillRect((int) getX(1) - 5, (int) getY(0), 10, 5);

		// Paints Axis
		g2.setStroke(LINES);
		sectorcolor = BLACK;
		g2.setColor(sectorcolor);
		Line2D assex = new Line2D.Double(getX(0), getY(0), getX(1) + 2 * AMARGIN, getY(0));
		Line2D assey = new Line2D.Double(getX(0), getY(0), getX(0), getY(1) - 2 * AMARGIN);
		g2.draw(assex);
		g2.draw(assey);

		// Axis Label
		g2.setColor(Color.GRAY);
		g2.drawString(classNames[1] + " %", (float) (getX(1) + 2 * AMARGIN + 5), (float) getY(0) + 15);
		g2.drawString(classNames[0] + " %", (float) getX(0) + AMARGIN, (float) (getY(1) - 2 * AMARGIN + 5));
		g2.setColor(BLACK);

		// Arrows on axis
		g2.draw(new Line2D.Double(getX(0) - AMARGIN / 2, getY(1) - AMARGIN / 3 - AMARGIN, getX(0), getY(1) - 2 * AMARGIN));
		g2.draw(new Line2D.Double(getX(0) + AMARGIN / 2, getY(1) - AMARGIN / 3 - AMARGIN, getX(0), getY(1) - 2 * AMARGIN));
		g2.draw(new Line2D.Double(getX(1) + AMARGIN / 3 + AMARGIN, getY(0) - AMARGIN / 2, getX(1) + 2 * AMARGIN, getY(0)));
		g2.draw(new Line2D.Double(getX(1) + AMARGIN / 3 + AMARGIN, getY(0) + AMARGIN / 2, getX(1) + 2 * AMARGIN, getY(0)));
	}

	/**
	 * Transform double x coordinate into screen one
	 * @param x x coordinate
	 * @return screen x coordinate
	 */
	private double getX(double x) {
		return x0 + x * unit;
	}

	/**
	 * Transform double y coordinate into screen one
	 * @param y y coordinate
	 * @return screen y coordinate
	 */
	private double getY(double y) {
		return y0 - y * unit;
	}
}
