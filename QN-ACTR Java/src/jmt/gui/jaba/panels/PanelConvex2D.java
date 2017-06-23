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

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.geom.Point2D;
import java.util.Vector;

import javax.swing.JPanel;

import jmt.engine.jaba.DPoint;
import jmt.engine.jaba.EngineConvex2D;
import jmt.framework.data.ArrayUtils;
import jmt.gui.jaba.JabaModel;
import jmt.gui.jaba.JabaWizard;

/**
 * This class creates a panel with the graph of the convex hull
 * 
 * @author Carlo Gimondi
 */
public class PanelConvex2D extends JPanel implements MouseListener, MouseMotionListener {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private PainterConvex2D painter;
	private EngineConvex2D engine;

	private JabaWizard mainWin;
	private JabaModel data;

	private int mouseButtonPress;

	private Point dragPoint;
	private Point beginPoint;

	private Image figura;
	private boolean editFigura;

	private DPoint lineP1;
	private DPoint lineP2;

	private Vector<Object> s3d;

	private double[][][] serviceDemands;

	private boolean dragging;

	private boolean selPoint;
	private boolean canLine;
	private DPoint selDPoint;

	public PanelConvex2D(JabaModel data, JabaWizard mainWin) {
		this.canLine = true;
		this.mainWin = mainWin;
		this.s3d = data.getResults();
		this.data = data;
		String[] stationNames = data.getStationNames();

		serviceDemands = ArrayUtils.copy3per2(data.getServiceTimes(), data.getVisits());

		Vector<Point2D> v = new Vector<Point2D>();
		for (int k = 0; k < stationNames.length; k++) {
			v.add(new DPoint(serviceDemands[k][0][0], serviceDemands[k][1][0], stationNames[k]));
		}
		engine = new EngineConvex2D(v, s3d);
		painter = new PainterConvex2D(engine.getAllDominants(), getHeight(), getWidth());
		addMouseListener(this);
		addMouseMotionListener(this);
		//setBackground(Color.white);
		mouseButtonPress = 0;
		dragPoint = painter.insidePoint(new Point(0, 0));
		beginPoint = painter.insidePoint(new Point(0, 0));
		editFigura = false;
		setPreferredSize(new Dimension(200, 200));
		lineP1 = null;
		lineP2 = null;
		selDPoint = null;
		selPoint = false;
		dragging = false;
	}

	/**
	 * This function in used every time that the window have to be repiainted
	 */
	@Override
	protected void paintComponent(Graphics g) {
		super.paintComponent(g);

		Graphics2D g3 = (Graphics2D) g;

		g3.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

		//The graph is paint in a Image objet, in this way if it has to
		//be rapaint and the graph is no change it's repaint only the image
		if (painter.update(getHeight(), getWidth()) || editFigura) {

			figura = createImage(getWidth(), getHeight());
			Graphics2D g2 = (Graphics2D) figura.getGraphics();

			g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

			g2.setColor(Color.white);
			g2.fillPolygon(painter.twoPointRectangle(0, 0, getWidth(), getHeight()));

			painter.drawArea(g2, engine.getAllConvex(), engine.getAllDominants(), new Color(255, 250, 120), Color.orange);

			painter.drawSelectLine(g2, lineP1, lineP2, s3d, data.getClassNames());

			painter.drawPoint(g2, engine.getDominants(), Color.blue, painter.getPointSize() + 1);

			painter.drawPoint(g2, engine.getDominates(), new Color(15, 185, 100), painter.getPointSize() + 1);

			painter.drawPoint(g2, engine.getConvex(), Color.red, painter.getPointSize() + 3);

			painter.drawPoint(g2, engine.getFiltDominants(), Color.black, painter.getPointSize() + 1);

			painter.drawPoint(g2, engine.getFiltConvex(), Color.black, painter.getPointSize() + 3);

			painter.drawPoint(g2, engine.getFiltDominates(), Color.gray, painter.getPointSize() + 1);

			painter.drawDominantArrow(g2, engine.getDominants(), engine.getDominates());

			painter.pointLabel(g2, engine.getPoints());

			painter.axis(g2, data.getClassNames());

			painter.drawFiltArea(g2, engine.getFilteredArea());

			painter.summary(g2);

			editFigura = false;
		}

		g3.drawImage(figura, 0, 0, this);

		drawDrag(g3);

		//painter.drawMousePosition(g3,dragPoint); 
	}

	/*
	 * When a button of the mouse is press the position of the
	 * cursor and the number of the button are stored. If the cursor is on a 
	 * point it is selected
	 */
	public void mousePressed(MouseEvent e) {
		dragging = false;
		canLine = true;
		mouseButtonPress = e.getButton();
		beginPoint = painter.insidePoint(e.getPoint());

		//If the cursor is on a point and the left button is press
		//the point is select
		DPoint p;
		if (e.getButton() == 1) {
			//Select the dominant
			Vector<Point2D> dominants = engine.getDominants();
			for (int i = 0; i < dominants.size(); i++) {
				p = (DPoint) dominants.get(i);
				p.setSelect(false);
				if (painter.theSame(dragPoint, p, 1)) {
					p.setSelect(true);
					selPoint = true;
					canLine = false;
					selDPoint = p;
				}
			}

			//If the point is Dominated the Dominants are selected
			Vector<Point2D> dominates = engine.getDominates();
			DPoint p2;
			for (int i = 0; i < dominates.size(); i++) {
				p = (DPoint) dominates.get(i);
				p.setSelect(false);
				if (painter.theSame(dragPoint, p, 0)) {
					p.setSelect(true);
					selPoint = true;
					canLine = false;
					selDPoint = p;
					//Select the dominants
					for (int k = 0; k < dominants.size(); k++) {
						p2 = (DPoint) dominants.get(k);
						if ((p2.getX() > p.getX()) && (p2.getY() > p.getY())) {
							p2.setSelect(true);
						}
					}
				}
			}
			if (selPoint) {
				setCursor(Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR));
			}
			editFigura = true;
		}
	}

	/*
	 * When a button of the mouse is released the area between
	 * the begine point and the actual point is filtered or free
	 * if the button is right or left.
	 * If a point was selected and drag the point is released inn the new position
	 */
	public void mouseReleased(MouseEvent e) {

		//When the mouse is released and no point in select
		mouseButtonPress = e.getButton();
		Point dPoint = painter.insidePoint(dragPoint);
		Point bPoint = painter.insidePoint(beginPoint);
		if (!selPoint && dragging) {
			if (mouseButtonPress == 1) {
				mouseButtonPress = 0;
				engine.addFilterArea(painter.getTrueX(dPoint.getX()), painter.getTrueY(dPoint.getY()), painter.getTrueX(bPoint.getX()), painter
						.getTrueY(bPoint.getY()), ((painter.getPointSize() / painter.getScale()) / 2));
				editFigura = true;
				repaint();
			}

			if (mouseButtonPress == 3) {
				mouseButtonPress = 0;
				engine.addFreeArea(painter.getTrueX(dPoint.getX()), painter.getTrueY(dPoint.getY()), painter.getTrueX(bPoint.getX()), painter
						.getTrueY(bPoint.getY()), ((painter.getPointSize() / painter.getScale()) / 2));
				editFigura = true;
				repaint();
			}
		} else //When the mouse is release and a point i select
		{
			if (mouseButtonPress == 1) {

				Vector<Point2D> allPoint = engine.getAllPoints();
				for (int k = 0; k < allPoint.size(); k++) {
					if (((DPoint) allPoint.get(k)).equals(selDPoint)) {
						setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
						serviceDemands[k][0][0] = painter.getTrueX(e.getX());
						serviceDemands[k][1][0] = painter.getTrueY(e.getY());
						selDPoint = new DPoint(-1, -1);

						//If the point was not dragget a new solution is not necessary
						if (!editFigura) {
							commit();
							mainWin.solve();
						}
					}
				}
			}
		}
		dragging = false;
		selPoint = false;

	}

	/*
	 * If the cursor enter after it exited while that point was dragging
	 * the cursor have to change
	 */
	public void mouseEntered(MouseEvent e) {
		if (selPoint) {
			setCursor(Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR));
		}
	}

	//Not in use
	public void mouseExited(MouseEvent e) {
	}

	/*
	 * When the mouse's left button is clicked if the cursor is on a point
	 * this point is select, if the cursors is not on a point all points lost
	 * the selection
	 */
	public void mouseClicked(MouseEvent e) {
		//Set zoom 10%+
		if ((e.getClickCount() > 1) && (e.getButton() == 1)) {
			int Height;
			int Width;

			if ((getWidth() * 1.1 < getHeight()) || (getHeight() * 1.1 > 1000)) {
				Height = getHeight();
			} else {
				Height = (int) (getHeight() * 1.1);
			}

			if ((getHeight() * 1.1 < getWidth()) || (getWidth() * 1.1 > 1000)) {
				Width = getWidth();
			} else {
				Width = (int) (getWidth() * 1.1);
			}
			setPreferredSize(new Dimension(Width - 15, Height));
			setSize(new Dimension(Width, Height));

		}

		// Set zoom 10%-
		if ((e.getClickCount() > 1) && (e.getButton() == 3)) {

			int Height;
			int Width;

			if (getHeight() * 0.9 < 200) {
				Height = getHeight();
			} else {
				Height = (int) (getHeight() * 0.9);
			}

			if (getWidth() * 0.9 < 200) {
				Width = getWidth();
			} else {
				Width = (int) (getWidth() * 0.9);
			}
			setPreferredSize(new Dimension(Width - 15, Height));
			setSize(new Dimension(Width, Height));

		}

		//If the click is on a line of the convex hull this is selected

		if ((e.getClickCount() == 1) && (canLine)) {
			selectLine(e.getPoint());
		} else {
			lineP1 = null;
			lineP2 = null;
		}
		if (e.getClickCount() == 1) {
			repaint();
		}

	}

	/**
	 * If the point is on a line of the convex hull the first and the second point of
	 * the line are stored
	 * @param point The point may be on the line
	 */
	private void selectLine(Point point) {
		Vector<Point2D> convex = engine.getAllConvex();
		for (int k = 0; k < convex.size() - 1; k++) {
			if (painter.selectLine((DPoint) convex.get(k), (DPoint) convex.get(k + 1), point)) {
				lineP1 = (DPoint) convex.get(k);
				lineP2 = (DPoint) convex.get(k + 1);
				return;
			}
		}
		lineP1 = null;
		lineP2 = null;

	}

	/*
	* If the mouse is moving on the graph the dragPoint is update and the 
	* graph is repaint
	 */
	public void mouseDragged(MouseEvent e) {
		dragging = true;
		editFigura = false;
		dragPoint = e.getPoint();
		repaint();
	}

	/*
	 * If the mouse is moving the position point of the cursor
	 * updated
	 */
	public void mouseMoved(MouseEvent e) {
		selPoint = false;
		dragPoint = e.getPoint();

		Vector<Point2D> point = engine.getDominants();
		DPoint p;

		//If the mouse pass over a point, the mouse change
		for (int i = 0; i < point.size(); i++) {
			p = (DPoint) point.get(i);
			if (painter.theSame(e.getPoint(), p, 2)) {
				setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
				return;
			}
		}

		point = engine.getDominates();

		//If the mouse pass over a point, the mouse change
		for (int i = 0; i < point.size(); i++) {
			p = (DPoint) point.get(i);
			if (painter.theSame(e.getPoint(), p, 0)) {
				setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
				return;
			}
		}

		//If the cursor pass over a saturation sector the mouse change
		Vector<Point2D> convex = engine.getAllConvex();
		for (int k = 0; k < convex.size() - 1; k++) {
			if (painter.selectLine((DPoint) convex.get(k), (DPoint) convex.get(k + 1), e.getPoint())) {
				setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
				return;
			}
		}
		setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
	}

	/*
	 * If the mouse is dragged an area is draw a free area or a filter area.
	 * if the mouse is dragged after a selection the new possible position is 
	 * draw
	 */
	private void drawDrag(Graphics2D g) {
		if (dragging) {
			if (!selPoint) {
				if (mouseButtonPress == 3) {
					painter.drawDragArea(g, painter.insidePoint(dragPoint), painter.insidePoint(beginPoint), Color.pink);
				}

				if (mouseButtonPress == 1) {
					painter.drawDragArea(g, painter.insidePoint(dragPoint), painter.insidePoint(beginPoint), Color.gray);
				}
			} else {
				if ((mouseButtonPress == 1)) {
					painter.drawShadowPoint(g, dragPoint, Color.black, painter.getPointSize() + 4);
				}
			}
		}
	}

	/**
	 * If a point on the graph change position the new data is commit 
	 */
	private void commit() {

		synchronized (data) {
			data.setServiceTimes(serviceDemands);
			data.setVisits(createUnitaryVisits());
		}
	}

	/*
	 * Funtion for the commit function
	 */
	private double[][] createUnitaryVisits() {
		double[][] visits = new double[serviceDemands.length][];
		for (int i = 0; i < serviceDemands.length; i++) {
			visits[i] = new double[serviceDemands[i].length];
			for (int j = 0; j < serviceDemands[i].length; j++) {
				if (serviceDemands[i][j][0] != 0) {
					visits[i][j] = 1;
				} else {
					visits[i][j] = 0;
				}
			}
		}
		return visits;
	}

}
