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

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Composite;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.geom.Point2D;
import java.text.DecimalFormat;
import java.util.Vector;

import jmt.engine.jaba.DPoint;
import jmt.engine.jaba.FinalSect2D;
import jmt.engine.jaba.Station2D;

/**
 * This class draws all the part of the graph
 * 
 * @author Carlo Gimondi
 */
public class PainterConvex2D {
	private int height;
	private int width;
	private int pointSize;
	private int fromBorderX;
	private int fromBorderY;
	private int tran_x;
	private int tran_y;
	private double maxValue;
	private double scale;
	private int maxLabel;

	private static final DecimalFormat format2Dec = new DecimalFormat("0.00");
	private static final DecimalFormat format4Dec = new DecimalFormat("0.0000");

	/**
	 * Initialize the object from the vector  where all the points are sored
	 * @param allDominants The vector with all the points
	 * @param height The height of the window
	 * @param width The width of the window
	 */
	public PainterConvex2D(Vector<Point2D> allDominants, int height, int width) {
		DPoint p;

		for (int i = 0; i < allDominants.size(); i++) {
			p = (DPoint) allDominants.get(i);
			if (p.getX() > maxValue) {
				maxValue = p.getX();
			}

			if (p.getY() > maxValue) {
				maxValue = p.getY();
			}

			if (p.getLabel().toString().length() > maxLabel) {
				maxLabel = p.getLabel().toString().length();
			}

		}

		//Max value in the graphic
		maxValue = (int) (maxValue / 10) + 1;
		maxValue = maxValue * 10;

		maxLabel = maxLabel + 5 + ("" + maxValue).length();

		update(height, width);
	}

	/**
	 * Draw the selected line of the convex hull 
	 * and the information about it
	 * @param g Graphic object
	 * @param p1 The first point of the line
	 * @param p2 The second point of the line
	 * @param s3d Information aboute the classes
	 * @param classNames The name of the classes
	 */
	public void drawSelectLine(Graphics2D g, DPoint p1, DPoint p2, Vector<Object> s3d, String[] classNames) {
		if ((p1 != null) && (p2 != null)) {
			//Draw the selected line
			Stroke oldStro = g.getStroke();
			Stroke stroke = new BasicStroke(2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
			g.setStroke(stroke);
			g.setColor(Color.black);
			g.drawLine((int) (p1.getX() * scale) + tran_x, tran_y - (int) (p1.getY() * scale), (int) (p2.getX() * scale) + tran_x, tran_y
					- (int) (p2.getY() * scale));
			g.setStroke(oldStro);

			//Set the middle point
			int x = (int) p2.getX() + (int) ((p1.getX() - p2.getX()) / 2);
			int y = (int) p2.getY() + (int) ((p1.getY() - p2.getY()) / 2);
			x = (int) (x * scale) + tran_x + (pointSize + 3);
			y = tran_y - (int) (y * scale) - (pointSize + 1);

			Font label = new Font("Arial", Font.PLAIN, 7 + pointSize);
			g.setFont(label);

			//Draw the label
			for (int i = 0; i < s3d.size(); i++) {
				// Current sector
				FinalSect2D sect = (FinalSect2D) s3d.get(i);
				String pb11 = format2Dec.format(sect.getBeta11() * 100);
				String pb12 = format2Dec.format(sect.getBeta1() * 100);
				String pb21 = format2Dec.format(sect.getBeta22() * 100);
				String pb22 = format2Dec.format(sect.getBeta2() * 100);

				if (sect.countStation() < 2) {
					continue;
				}

				Station2D d1 = (sect.getstation()).get(0);
				Station2D d2 = (sect.getstation()).get(1);
				int d1x = (int) (d1.getVert()).getX();
				int d1y = (int) (d1.getVert()).getY();
				int d2x = (int) (d2.getVert()).getX();
				int d2y = (int) (d2.getVert()).getY();
				int p1x = (int) (p1.getX() * 100);
				int p1y = (int) (p1.getY() * 100);
				int p2x = (int) (p2.getX() * 100);
				int p2y = (int) (p2.getY() * 100);
				double t1 = ((p1.getY() - p2.getY()) / ((p2.getX() * p1.getY()) - (p1.getX() * p2.getY())));
				double t2 = ((p2.getX() - p1.getX()) / ((p2.getX() * p1.getY()) - (p1.getX() * p2.getY())));

				if (((d1x == p1x) && (d1y == p1y) && (d2x == p2x) && (d2y == p2y)) || ((d1x == p2x) && (d1y == p2y) && (d2x == p1x) && (d2y == p1y))) {
					g.drawString(classNames[0] + "=" + format4Dec.format(t1) + " job/sec", x, y - (8 + pointSize));
					g.drawString(classNames[1] + "=" + format4Dec.format(t2) + " job/sec", x, y);

					g.drawString(classNames[1] + "% [" + pb22 + "," + pb12 + "]", x, y - 2 * (8 + pointSize));
					g.drawString(classNames[0] + "% [" + pb21 + "," + pb11 + "]", x, y - 3 * (8 + pointSize));
					break;
				}
			}
		}
	}

	/**
	 * If the height or the width are changed the parameters of
	 * the graph are update
	 * @param height The height of the window
	 * @param width The width of the window
	 * @return true if somethig is change
	 */
	public boolean update(int height, int width) {
		if ((this.height != height) || (this.width != width)) {

			this.height = height;
			this.width = width;

			pointSize = Math.min(height, width) / 100;

			fromBorderX = 48 + maxLabel * (pointSize / 2);
			fromBorderY = 30 + pointSize;

			tran_y = height - 6 - (2 * (9 + pointSize));
			tran_x = -2 + (((9 + pointSize) / 2) * (("" + maxValue).length()));

			int max_possible = (width - fromBorderX - tran_x);
			if ((tran_y - fromBorderY) < max_possible) {
				max_possible = (tran_y - fromBorderY);
			}

			scale = (float) ((max_possible - (8 + pointSize)) / maxValue);
			return true;
		}
		return false;
	}

	/**
	 * It draws the axis of the graph
	 * @param g The graphic object
	 * @param className The name of the classes
	 */
	public void axis(Graphics2D g, String[] className) {
		g.setColor(Color.black);
		//Axis
		g.drawLine(tran_x, tran_y, tran_x, fromBorderY);
		g.drawLine(tran_x, tran_y, width - fromBorderX, tran_y);
		//Arrows Y
		g.drawLine(tran_x - 4, fromBorderY + 8, tran_x, fromBorderY);
		g.drawLine(tran_x + 4, fromBorderY + 8, tran_x, fromBorderY);
		//Arrows X
		g.drawLine(width - fromBorderX - 8, tran_y - 4, width - fromBorderX, tran_y);
		g.drawLine(width - fromBorderX - 8, tran_y + 4, width - fromBorderX, tran_y);

		Font label = new Font("Arial", Font.PLAIN, 9 + pointSize);
		g.setFont(label);
		//Label Y
		g.drawString(className[0], tran_x - 15, fromBorderY - 8);
		//Label X 
		g.drawString(className[1], width - Math.max(fromBorderX, (int) (className[1].length() * (9 + pointSize) * 0.58)), height - (pointSize));

		//Insert the line on the axis
		for (int i = 1; i < 11; i++) {
			g.drawLine(tran_x + (int) (((maxValue / 10) * i) * scale), tran_y, tran_x + (int) (((maxValue / 10) * i) * scale), tran_y + 2);
			g.drawLine(tran_x, tran_y - (int) (((maxValue / 10) * i) * scale), tran_x - 2, tran_y - (int) (((maxValue / 10) * i) * scale));
		}

		//Setting the Font
		int fontSize = 8 + pointSize;
		Font f = new Font("Arial", Font.PLAIN, fontSize);
		g.setFont(f);

		//Insert the number on the axis X
		for (int i = 1; i < 11; i++) {
			g.drawString("" + (int) (((maxValue / 10) * i)), tran_x + (int) (((maxValue / 10) * i) * scale) - 7, tran_y + 3 + fontSize);
		}

		for (int i = 1; i < 11; i++) {
			int translationt = ((("" + maxValue).length()) - ((("" + ((int) (((maxValue / 10) * i)))).length()))) - 1;
			g.drawString("" + (int) (((maxValue / 10) * i)), (translationt * (fontSize / 2)) - 3, tran_y + 3 - (int) (((maxValue / 10) * i) * scale));
		}

		//Insert 0
		g.drawString("0", tran_x - pointSize - 5, tran_y + pointSize + 5);
	}

	/**
	 * It draws that explains the simbols on the graphic
	 * @param g The graphic object
	 */
	public void summary(Graphics2D g) {
		int size = pointSize + 1;
		int x = width - 53 - (pointSize * 5);
		int y = 7 + size;
		int fontSize = 6 + Math.min(pointSize + 1, 9);
		Font f = new Font("Arial", Font.PLAIN, fontSize);
		g.setFont(f);

		//Potential Bottleneck
		g.setColor(Color.red);
		g.fillOval(5 + (x) - ((size / 2)), (int) (y + (0.5) * fontSize) - ((Math.min(pointSize + 1, 9) / 2)), Math.min(pointSize + 1, 9), Math.min(
				pointSize + 1, 9));
		g.setColor(Color.black);
		g.drawString("Potential", x + 11 + ((size / 2)), y);
		g.drawString("Bottleneck", x + 11 + ((size / 2)), y + fontSize);
		g.drawString("Stations", x + 11 + ((size / 2)), y + 2 * fontSize);

		//Masked-off
		g.setColor(Color.blue);
		g.fillOval(5 + (x) - ((size / 2)), -3 + y + (4) * fontSize - Math.min(pointSize + 1, 9), Math.min(pointSize + 1, 9), Math.min(pointSize + 1,
				9));
		g.setColor(Color.black);
		g.drawString("Masked-off", x + 11 + ((size / 2)), 3 + y + (3) * fontSize);
		g.drawString("Stations", x + 11 + ((size / 2)), 3 + y + (4) * fontSize);

		//Dominated
		g.setColor(new Color(15, 185, 100));
		g.fillOval(5 + (x) - ((size / 2)), 6 + y + (((6 * fontSize) + (5 * fontSize)) / 2) - Math.min(pointSize + 1, 9), Math.min(pointSize + 1, 9),
				Math.min(pointSize + 1, 9));
		g.setColor(Color.black);
		g.drawString("Dominated", x + 11 + ((size / 2)), 6 + y + (5) * fontSize);
		g.drawString("Stations", x + 11 + ((size / 2)), 6 + y + (6) * fontSize);

		//Masked off-Area
		g.drawString("Masked-off", x + 11 + ((size / 2)), 2 + y + (8) * fontSize);
		g.drawString("Area", x + 11 + ((size / 2)), 2 + y + (9) * fontSize);
		int xP1 = 4 + x - ((size / 2));
		int yP1 = 3 + y + (8) * fontSize + ((size / 2));
		int xP2 = 4 + x + size;
		int yP2 = 3 + (y + (8) * fontSize) - size;

		Polygon p = twoPointRectangle(xP1, yP1, xP2, yP2);

		g.setColor(Color.orange);
		g.fill(p);
		g.setColor(Color.gray);
		g.draw(p);

		//Dominated Area
		g.setColor(Color.black);
		g.drawString("Dominated", x + 11 + ((size / 2)), 3 + y + (10) * fontSize);
		g.drawString("Area", x + 11 + ((size / 2)), 3 + y + (11) * fontSize);
		xP1 = 4 + x - ((size / 2));
		yP1 = 4 + y + (10) * fontSize + ((size / 2));
		xP2 = 4 + x + size;
		yP2 = 4 + (y + (10) * fontSize) - size;

		p = twoPointRectangle(xP1, yP1, xP2, yP2);

		g.setColor(new Color(255, 250, 120));
		g.fill(p);
		g.setColor(Color.gray);
		g.draw(p);
	}

	/**
	 * Create a Polygon that is a rectangle draw between two point 
	 * @param xP1 The x of the first point
	 * @param yP1 The y of the first point
	 * @param xP2 The x of the second point
	 * @param yP2 The y of the second point
	 * @return The rectangle in a polygon object
	 */
	public Polygon twoPointRectangle(int xP1, int yP1, int xP2, int yP2) {
		Polygon p = new Polygon();
		p.addPoint(xP1, yP1);
		p.addPoint(xP1, yP2);
		p.addPoint(xP2, yP2);
		p.addPoint(xP2, yP1);

		return p;
	}

	/**
	 * It draws the points contained in a vector. The coordinates of the point must be insered 
	 * in a Point2D object
	 * @param g The graphic object
	 * @param points The Vector who contains the points
	 * @param c The color of the points
	 */
	public void drawPoint(Graphics g, Vector<Point2D> points, Color c, int size) {

		for (int j = 0; j < points.size(); j++) {
			Point2D p = points.get(j);
			g.setColor(c);
			g.fillOval((int) (p.getX() * scale) - ((size / 2)) + tran_x, tran_y - (int) (p.getY() * scale) - ((size / 2)), size, size);
		}
	}

	/**
	 * It draw a temporary point when a point is moved in another place
	 * @param g The graphic object
	 * @param p The position of the point
	 * @param c The color of the point
	 * @param size The size of the point
	 */
	public void drawShadowPoint(Graphics2D g, Point p, Color c, int size) {
		g.setColor(c);

		int fontSize = 7 + pointSize;
		Font f = new Font("Arial", Font.PLAIN, fontSize);
		g.setFont(f);
		double x = Math.max((p.getX() - tran_x) / scale, 0);
		double y = Math.max((-p.getY() + tran_y) / scale, 0);
		double X = (x * scale) + tran_x;
		double Y = -((y * scale) - tran_y);

		g.drawString("(" + format2Dec.format(x) + ", " + format2Dec.format(y) + ")", (int) (X - (fontSize * 3)), (int) Y - 5 - pointSize);

		g.drawOval((int) X - (((size / 2))), (int) Y - (((size / 2))), size, size);

		g.setColor(Color.gray);
		Composite oldComp = g.getComposite();
		Composite alphaComp = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.3f);
		g.setComposite(alphaComp);
		g.fillOval((int) X - (((size / 2))), (int) Y - (((size / 2))), size, size);
		g.setComposite(oldComp);

	}

	/**
	 * Return the scale factor
	 * @return the scale factor
	 */
	public double getScale() {
		return scale;
	}

	/**
	* This function draw te area among the axis and the point of the convex hull
	 * @param g The graphic object
	 * @param allConvex The vector with the points of the convex hull
	 * @param allDominants The vector with the dominant points
	 * @param area The filter area
	 * @param maskedoff The vector with the masked-off points
	 */
	public void drawArea(Graphics g, Vector<Point2D> allConvex, Vector<Point2D> allDominants, Color area, Color maskedoff) {
		//The first color in the maskedoff color beacause after will be draw
		//the non-maskedoff areas
		g.setColor(maskedoff);
		Polygon poly = new Polygon();

		DPoint p;
		p = (DPoint) allConvex.get(0);

		poly.addPoint((int) (p.getX() * scale) + tran_x, tran_y);

		//Add the point a the polygon for paint the convex area
		for (int i = 0; i < allConvex.size(); i++) {
			p = (DPoint) allConvex.get(i);
			poly.addPoint((int) (p.getX() * scale) + tran_x, tran_y - (int) (p.getY() * scale));
		}

		p = (DPoint) allConvex.get(allConvex.size() - 1);
		poly.addPoint(1 + tran_x, tran_y - (int) (p.getY() * scale));
		poly.addPoint(1 + tran_x, tran_y);
		g.fillPolygon(poly);
		g.setColor(Color.GRAY);
		g.drawPolygon(poly);

		//Draw the non-maskedoff area for each dominant point
		Polygon masked = new Polygon();
		g.setColor(area);

		int k = 0;
		for (k = 0; k < allDominants.size() - 1; k++) {
			p = (DPoint) allDominants.get(k);
			if (!p.intEquals((DPoint) allDominants.get(k + 1))) {
				masked = twoPointRectangle(1 + tran_x, tran_y, (int) (p.getX() * scale) + tran_x, tran_y - (int) (p.getY() * scale));
				g.fillPolygon(masked);
			}
		}
		//Last area is 1 point small
		p = (DPoint) allDominants.get(k);
		masked = twoPointRectangle(1 + tran_x, tran_y, (int) (p.getX() * scale) + tran_x, tran_y - (int) (p.getY() * scale) + 1);
		g.fillPolygon(masked);
	}

	/**
	 * Print a label over every point, if the point is select
	 * the label contain the coordinate too
	 * @param gra The graphic object
	 * @param points The vector with all points
	 */
	public void pointLabel(Graphics2D g, Vector<Point2D> points) {
		g.setColor(Color.black);

		//Setting the Font
		int fontSize = 7 + pointSize;
		Font f = new Font("Arial", Font.PLAIN, fontSize);
		g.setFont(f);

		for (int i = 0; i < points.size(); i++) {
			DPoint p = (DPoint) points.get(i);
			if (!p.isSelect()) {
				g.drawString(p.getLabel(), tran_x + (int) (p.getX() * scale) - 15, tran_y - (int) (p.getY() * scale) - 3 - pointSize);
			} else {
				g.drawString(p.getLabel() + " (" + format2Dec.format(p.getX()) + ", " + format2Dec.format(p.getY()) + ")", tran_x
						+ (int) (p.getX() * scale) - 15, tran_y - (int) (p.getY() * scale) - 3 - pointSize);
			}
		}
	}

	/**
	 * When is select a Dominat point is draw an arrow from the dominant
	 * @param g The graphic object
	 * @param dominant The vector with the dominant points
	 * @param dominates The vector with the dominates points
	 */
	public void drawDominantArrow(Graphics2D g, Vector<Point2D> dominant, Vector<Point2D> dominates) {
		DPoint p;
		DPoint p2;
		boolean first = true;

		for (int i = 0; i < dominates.size(); i++) {
			p = (DPoint) dominates.get(i);
			if (p.isSelect()) {
				for (int k = dominant.size() - 1; k >= 0; k--) {
					p2 = (DPoint) dominant.get(k);
					if (p2.isSelect()) {

						g.setColor(Color.magenta);
						//Draw Arrow

						//The angle is calculete throw the rotator point
						DPoint rotator = new DPoint(p2.getX() - p.getX(), p2.getY() - p.getY());
						double angle = rotator.polarAngle();
						//The thestination point in not into the point but near the point
						int xPoint = (int) (p.getX() * scale) + tran_x + (int) (pointSize * Math.cos(angle));
						int yPoint = tran_y - (int) (p.getY() * scale) - (int) (pointSize * Math.sin(angle));

						g.drawLine(xPoint, yPoint, (int) (p2.getX() * scale) + tran_x, tran_y - (int) (p2.getY() * scale));
						//The rotation turn the arrow in the right position
						g.rotate(-angle, xPoint, yPoint);

						g.drawLine(xPoint, yPoint, xPoint + 4 + pointSize, yPoint - pointSize);
						g.drawLine(xPoint, yPoint, xPoint + 4 + pointSize, yPoint + pointSize);

						g.rotate((2 * Math.PI) + angle, xPoint, yPoint);

						//Draw Label on the first arrow (only)
						if (first) {
							first = false;

							g.setColor(new Color(152, 61, 168));
							int fontSize = 7 + pointSize;
							Font f = new Font("Arial", Font.PLAIN, fontSize);
							g.setFont(f);

							double x = (p.getX() + p2.getX()) / 2;
							double y = (p.getY() + p2.getY()) / 2;

							g.drawString("Dominating", (int) (x * scale) + tran_x + 7, tran_y - 3 - (int) (y * scale));
						}
					}
				}
			}
		}
	}

	/**
	 * This function traw a line near the coordinate of the mouse
	 * @param g The graphic object
	 * @param dragPoint The position of the mouse
	 */
	public void drawMousePosition(Graphics2D g, Point dragPoint) {
		g.setColor(Color.darkGray);
		//Draw the line
		g.drawLine(1 + tran_x, (int) (dragPoint.getY()), pointSize + tran_x - 1, (int) (dragPoint.getY()));
		g.drawLine((int) (dragPoint.getX()), tran_y - pointSize + 1, (int) dragPoint.getX(), tran_y);
	}

	/**
	 * Draw a semi-trasparent area that is the filtered area
	 * @param g The graphic object
	 * @param filteredArea The filtered area
	 */
	public void drawFiltArea(Graphics2D g, Area filtArea) {
		AffineTransform t = new AffineTransform();
		t.scale(scale / 100, scale / 100);
		AffineTransform t2 = new AffineTransform();
		t2.translate(tran_x, tran_y);

		filtArea.transform(t);
		filtArea.transform(t2);

		Stroke oldStro = g.getStroke();
		Stroke stroke = new BasicStroke(2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
		g.setStroke(stroke);

		g.setColor(Color.gray);
		Composite oldComp = g.getComposite();
		Composite alphaComp = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.3f);
		g.setComposite(alphaComp);

		g.fill(filtArea);
		g.setComposite(oldComp);
		g.setStroke(oldStro);
	}

	/**
	 * Draw a semi-trasparet area
	 * @param g The graphic object
	 * @param dragPoint The first point
	 * @param beginPoint The second point
	 * @param c The color of the area
	 */
	public void drawDragArea(Graphics2D g, Point dragPoint, Point beginPoint, Color c) {
		g.setColor(c);

		Polygon poly = new Polygon();

		poly.addPoint((int) beginPoint.getX(), (int) beginPoint.getY());
		poly.addPoint((int) beginPoint.getX(), (int) dragPoint.getY());
		poly.addPoint((int) dragPoint.getX(), (int) dragPoint.getY());
		poly.addPoint((int) dragPoint.getX(), (int) beginPoint.getY());

		//Set the widths of the shape's outline
		Stroke oldStro = g.getStroke();
		Stroke stroke = new BasicStroke(2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
		g.setStroke(stroke);
		g.drawPolygon(poly);
		g.setStroke(oldStro);

		//Set the trasparency of the iside of the rectangle
		Composite oldComp = g.getComposite();
		Composite alphaComp = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.4f);
		g.setComposite(alphaComp);
		g.fillPolygon(poly);
		g.setComposite(oldComp);

	}

	/**
	 * Set the parameter point as the dragPoint. If the point in out of the
	 * graphich area it is resized
	 * @param p The point
	 */
	public Point insidePoint(Point p) {
		int x = (int) p.getX();
		int y = (int) p.getY();

		//Resize x and y if they are too big or too small
		if (x < tran_x) {
			Math.max(x = tran_x, 0);
		}
		if (x > (tran_x + (int) (maxValue * scale))) {
			x = (tran_x + (int) (maxValue * scale));
		}
		if (y > tran_y) {
			Math.max(y = tran_y, 0);
		}
		if (y < (tran_y - (int) (maxValue * scale))) {
			y = (tran_y - (int) (maxValue * scale));
		}
		return new Point(x, y);
	}

	/**
	 * Return the true x on the graph from the mouse position
	 * @param XonScreen The x of the point on screen
	 * @return The true x point
	 */
	public double getTrueX(double XonScreen) {
		double x = (((XonScreen - tran_x)) / scale);

		if (x < 0) {
			return 0;
		} else {
			return x;
		}
	}

	/**
	 * Return the true y on the graph from the mouse position
	 * @param YonScreen The y of the point on screen
	 * @return The true y point
	 */
	public double getTrueY(double YonScreen) {
		double y = (((tran_y - YonScreen)) / scale);

		if (y < 0) {
			return 0;
		} else {
			return y;
		}
	}

	/**
	 * Return the size of the points on screen
	 * @return The size of the points
	 */
	public int getPointSize() {
		return pointSize;
	}

	/**
	 * Return true if the point is on a line between the first and the second point
	 * @param p1 The first point of the line
	 * @param p2 The second point of the line
	 * @param point The point that could be on the line
	 * @return True if the point is on a line between the first and the second point
	 */
	public boolean selectLine(DPoint p1, DPoint p2, Point point) {
		Polygon p = new Polygon();
		p.addPoint((int) ((p1.getX() * scale) + tran_x - ((pointSize + 1))), (int) ((tran_y - (p1.getY() * scale) + ((pointSize + 1)))));
		p.addPoint((int) ((p1.getX() * scale) + tran_x + ((pointSize + 1))), (int) ((tran_y - (p1.getY() * scale) - ((pointSize + 1)))));
		p.addPoint((int) ((p2.getX() * scale) + tran_x + ((pointSize + 1))), (int) ((tran_y - (p2.getY() * scale) - ((pointSize + 1)))));
		p.addPoint((int) ((p2.getX() * scale) + tran_x - ((pointSize + 1))), (int) ((tran_y - (p2.getY() * scale) + ((pointSize + 1)))));

		if (p.contains(point)) {
			return true;
		}
		return false;
	}

	/**
	 * If a generic point and a point on the screen are the same point
	 * @param mousePoint The point on screen
	 * @param ifPoint A generic point
	 * @return If the point on screen and a generic point are the same
	 */
	public boolean theSame(Point mousePoint, DPoint ifPoint, int more) {
		double max = Math.max(ifPoint.getX(), ifPoint.getY());
		double error = 1;
		if (max < 1) {
			error = 0.1;
		}
		if (max < 0.1) {
			error = 0.01;
		}

		error = error / scale;

		double minX = (getTrueX(mousePoint.getX()) - error - (((getPointSize() + more) / getScale()) / 2));
		double maxX = (getTrueX(mousePoint.getX()) + error + (((getPointSize() + more) / getScale()) / 2));
		double minY = (getTrueY(mousePoint.getY()) - error - (((getPointSize() + more) / getScale()) / 2));
		double maxY = (getTrueY(mousePoint.getY()) + error + (((getPointSize() + more) / getScale()) / 2));

		if ((minX <= ifPoint.getX()) && (maxX > ifPoint.getX()) && (minY <= ifPoint.getY()) && (maxY > ifPoint.getY())) {
			return true;
		}
		return false;
	}

}
