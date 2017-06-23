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

import java.awt.Color;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.geom.Rectangle2D;
import java.text.DecimalFormat;
import java.util.Vector;

import javax.swing.JPanel;

/**
 * <p>Title: FastGraph</p>
 * <p>Description: Displays a graph with autoresizing property. This is designed to be a
 * really lightweight component as is supposed to be updated during simulation.
 * The component will draw data from a vector that can be changed to update the graph.
 * Vector must contain only object implementing </code>MeasureDefinition.Value</code> interface.
 * After updating Vector, user should call <code>repaint()</code> method to force update
 * of this graph. Labels on x axis are based on xunit value specified in the constructor.</p>
 * 
 * @author Bertoli Marco
 *         Date: 27-set-2005
 *         Time: 12.34.37
 */
public class FastGraph extends JPanel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private static final Color axisColor = Color.BLACK;
	private static final Color graphBackgroundColor = Color.WHITE;
	private static final Color boundsColor = Color.red;
	private static final int MARGIN = 8;
	private Vector values;
	private double xunit;
	private Color drawColor = Color.BLUE;
	private int x0, y0; // Position of origin of cartesian axes
	private double xstep, ystep; // Increment for each unit in pixels

	// Used to format numbers. Static as will be recycled among all graphs
	private static DecimalFormat decimalFormat0 = new DecimalFormat("0.0E0");
	private static DecimalFormat decimalFormat1 = new DecimalFormat("#0.000");
	private static DecimalFormat decimalFormat2 = new DecimalFormat("#0.00");
	private static DecimalFormat decimalFormat3 = new DecimalFormat("#0.0");
	private static DecimalFormat decimalFormat4 = new DecimalFormat("#00 ");

	/**
	 * Builds a new FastGraph with specified input vector.
	 * @param values vector with values to be shown on graph (in MeasureValue format)
	 * @param xunit measure of unit of x axis. Each sample is distant from previous one of
	 * xunit.
	 */
	public FastGraph(Vector values, double xunit) {
		this.values = values;
		this.xunit = xunit;
	}

	/**
	 * Sets drawing color for this graph. (Default is BLUE)
	 * @param c color to be set
	 */
	public void setDrawColor(Color c) {
		drawColor = c;
	}

	/**
	 * Overrides default paint method to draw the graph
	 * @param g graphic component
	 */
	@Override
	public void paint(Graphics g) {
		super.paint(g);
		int height = this.getHeight();
		int width = this.getWidth();

		// Draw graph area
		g.setColor(graphBackgroundColor);
		g.fillRect(MARGIN / 2, MARGIN / 2, width - MARGIN, height - MARGIN);

		// Aborts drawing if no elements are present
		if (values.size() < 1) {
			return;
		}

		// Aborts graph drawing if width is too small...
		if (width < 80) {
			return;
		}

		// Find maximum value for x
		double maxx;
		if (values.size() > 1) {
			maxx = (values.size() - 1) * xunit;
		} else {
			maxx = values.size() * xunit;
		}
		// Find maximum value for y
		double maxy = 0;
		for (int i = 0; i < values.size(); i++) {
			double currenty = ((MeasureValue) values.get(i)).getMeanValue();
			if (currenty > maxy && !Double.isInfinite(currenty)) {
				maxy = currenty;
			}
			currenty = ((MeasureValue) values.get(i)).getUpperBound();
			if (currenty > maxy && !Double.isInfinite(currenty)) {
				maxy = currenty;
			}
		}
		// Correct zero maxy value, to avoid division per zero in ystep
		if (maxy == 0) {
			maxy = 1;
		}

		// Rounds up maxy and maxx
		maxx = Math.ceil(maxx);

		//Get text bounds
		FontMetrics metric = g.getFontMetrics();
		Rectangle2D xtextBound = metric.getStringBounds(Long.toString(Math.round(maxx)) + "s", g);
		Rectangle2D ytextBound = metric.getStringBounds(formatNumber(maxy), g);

		// Find initial position
		x0 = (int) Math.ceil(ytextBound.getWidth()) + 2 + MARGIN;
		y0 = height - (int) Math.ceil(xtextBound.getHeight()) - 2 - MARGIN;

		xstep = (width - x0 - MARGIN - xtextBound.getWidth() / 2) / maxx;
		ystep = (y0 - MARGIN) / maxy;

		// Draws axis and captions
		g.setColor(axisColor);
		// Y axis
		g.drawLine(x0, y0, x0, getY(maxy));
		int halfHeight = (int) Math.floor(ytextBound.getHeight() / 2);
		int num = (int) Math.floor((y0 - getY(maxy)) / (ytextBound.getHeight() + 2));
		// Draws caption for y axis
		for (int i = 0; i <= num; i++) {
			g.drawLine(x0, getY(maxy / num * i), x0 - 1, getY(maxy / num * i));
			g.drawString(formatNumber(maxy / num * i), MARGIN, getY(maxy / num * i) + halfHeight);
		}

		// X axis
		g.drawLine(x0, y0, getX(maxx), y0);
		int halfWidth;
		num = (int) Math.floor((getX(maxx) - x0) / (xtextBound.getWidth() + 4));
		if (num > maxx) {
			num = (int) maxx;
		}
		int inc = (int) Math.ceil(maxx / num);
		// Draws caption for x axis - must be integer
		for (int i = 0; i <= maxx; i += inc) {
			String label = Integer.toString(i);
			halfWidth = (int) Math.round(metric.getStringBounds(label, g).getWidth() / 2);
			g.drawLine(getX(i), y0, getX(i), y0 + 1);
			g.drawString(label, getX(i) - halfWidth, height - MARGIN);
		}
		// Draws measure unit on X axis
		g.drawString("s", getX(maxx) + MARGIN, height - MARGIN);

		// Draws upper and lower bounds
		g.setColor(boundsColor);
		double value;
		for (int i = 0; i < values.size() - 1; i++) {
			// upper bound
			value = ((MeasureValue) values.get(i)).getUpperBound();
			if (value > 0 && !Double.isInfinite(value)) {
				g.drawLine(getX(i * xunit), getY(value), getX((i + 1) * xunit), getY(((MeasureValue) values.get(i + 1)).getUpperBound()));
			}
			// lower bound
			value = ((MeasureValue) values.get(i)).getLowerBound();
			if (value > 0 && !Double.isInfinite(value)) {
				g.drawLine(getX(i * xunit), getY(value), getX((i + 1) * xunit), getY(((MeasureValue) values.get(i + 1)).getLowerBound()));
			}
		}
		value = ((MeasureValue) values.get(values.size() - 1)).getLowerBound();
		if (value > 0 && !Double.isInfinite(value)) {
			g.fillOval(getX(((values.size() - 1)) * xunit), getY(value), 2, 1);
		}
		value = ((MeasureValue) values.get(values.size() - 1)).getUpperBound();
		if (value > 0 && !Double.isInfinite(value)) {
			g.fillOval(getX(((values.size() - 1)) * xunit), getY(value), 2, 1);
		}

		// Draws the mean value
		g.setColor(drawColor);
		for (int i = 0; i < values.size() - 1; i++) {
			g.drawLine(getX(i * xunit), getY(((MeasureValue) values.get(i)).getMeanValue()), getX((i + 1) * xunit), getY(((MeasureValue) values
					.get(i + 1)).getMeanValue()));
		}
		g.fillOval(getX(((values.size() - 1)) * xunit), getY(((MeasureValue) values.get(values.size() - 1)).getMeanValue()), 2, 1);
	}

	/**
	 * Returns X coordinate for the screen of a point, given its value
	 * @param value value of point X
	 * @return X coordinate on the screen
	 */
	private int getX(double value) {
		return (int) Math.round(x0 + value * xstep);
	}

	/**
	 * Returns Y coordinate for the screen of a point, given its value
	 * @param value value of point Y
	 * @return Y coordinate on the screen
	 */
	private int getY(double value) {
		return (int) Math.round(y0 - value * ystep);
	}

	/**
	 * Formats a number to string to be shown as label of the graph
	 * @param value number to be converted
	 * @return value converted into string
	 */
	private static String formatNumber(double value) {
		if (value == 0) {
			return "0.000";
		} else if (value < 0.001) {
			return decimalFormat0.format(value);
		} else if (value < 10) {
			return decimalFormat1.format(value);
		} else if (value < 100) {
			return decimalFormat2.format(value);
		} else if (value < 1000) {
			return decimalFormat3.format(value);
		} else {
			return decimalFormat4.format(value);
		}
	}
}
