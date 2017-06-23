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

package jmt.engine.jaba;

import java.util.Vector;

/**
 * Created by IntelliJ IDEA.
 * User: Andrea
 * Date: 25-gen-2006
 * Time: 21.24.06
 * To change this template use File | Settings | File Templates.
 */
public class newGraham {

	Vector<PPoint> point = new Vector<PPoint>(2);
	Vector<PPoint> convexHull = new Vector<PPoint>(2);
	Vector<newPoint> newconvexHull = new Vector<newPoint>();
	Vector<newPoint> newnewconvexHull = new Vector<newPoint>();

	int height, width;

	public void emptyPt() {
		point.removeAllElements();
	}

	void addPt() {
		boolean ptin = false;
		PPoint pt = new PPoint();
		pt.x = (int) (Math.random() * (width - 2) + 1);
		pt.y = (int) (Math.random() * (height - 2) + 1);

		pt.angle = 0;
		point.addElement(pt);
	}

	void newPointToPt(newPoint p) {
		PPoint pt = new PPoint();
		pt.x = p.x;
		pt.y = p.y;
		pt.angle = 0;
		point.addElement(pt);
	}

	void PtTonewPoint(PPoint p) {
		newPoint pt = new newPoint(p.x, p.y);
		newconvexHull.addElement(pt);
	}

	public Vector<newPoint> GrahamScan(Vector q) {

		//todo Conversione newPoint a Ppoint
		emptyPt();
		for (int i = 0; i < q.size(); i++) {
			newPointToPt((newPoint) q.get(i));
		}

		Vector<PPoint> p = new Vector<PPoint>(point.size());
		convexHull.removeAllElements();

		PPoint alpha = new PPoint();
		alpha.x = width;
		alpha.y = height;
		int nalpha = -1;

		for (int i = 0; i < point.size(); i++) {
			PPoint npoint = point.elementAt(i);
			if (npoint.y <= alpha.y) {
				if (npoint.y < alpha.y) {
					alpha = npoint;
					nalpha = i;
				} else {
					if (npoint.x < alpha.x) {
						alpha = npoint;
						nalpha = i;
					}
				}
			}
		}

		alpha.angle = 0;
		convexHull.addElement(alpha);

		for (int i = 0; i < point.size(); i++) {
			PPoint npoint = point.elementAt(i);
			// angle will be in range 0 - pi
			npoint.angle = (float) Math.atan2(npoint.y - alpha.y, npoint.x - alpha.x);

			boolean ptin = false;
			for (int j = 0; j < p.size(); j++) {
				if (i == nalpha) {
					ptin = true;
					break;
				}

				PPoint ppoint = p.elementAt(j);
				if (npoint.angle == ppoint.angle) {
					// abandon nearest
					if (Math.sqrt(ppoint.x * ppoint.x + ppoint.y * ppoint.y) < Math.sqrt(npoint.x * npoint.x + npoint.y * npoint.y)) {
						p.setElementAt(npoint, j);
					}
					ptin = true;
					break;
				}
				if (npoint.angle < ppoint.angle) {
					p.insertElementAt(npoint, j);
					ptin = true;
					break;
				}
			}
			if (!ptin) {
				p.addElement(npoint);
			}
		}

		// added all points to p
		// now go through them!

		nalpha = p.size();
		convexHull.addElement(p.elementAt(0));
		convexHull.addElement(p.elementAt(1));

		for (int i = 2; i < nalpha; i++) {
			PPoint p1, p2, pn;
			pn = p.elementAt(i);
			p1 = convexHull.elementAt(convexHull.size() - 2);
			p2 = convexHull.elementAt(convexHull.size() - 1);
			while (NonLeftTurn(p1, p2, pn, convexHull.firstElement())) {
				convexHull.removeElementAt(convexHull.size() - 1);
				p2 = p1;
				p1 = convexHull.elementAt(convexHull.size() - 2);
			}
			convexHull.addElement(pn);
		}

		// add the first element again to close polygon
		//convexHull.addElement(convexHull.firstElement());

		//todo conversione da PPoint a newPoint
		for (int i = 0; i < convexHull.size(); i++) {
			PtTonewPoint(convexHull.get(i));
		}

		//todo bisogna invertire l'ordine di uscita dei punti
		for (int i = newconvexHull.size(); i > 0; i--) {
			newnewconvexHull.addElement(newconvexHull.get(i));
			newconvexHull.removeElementAt(i);
		}

		return newnewconvexHull;

	}

	boolean NonLeftTurn(PPoint p1, PPoint p2, PPoint p3, PPoint p0) {
		double l1, l2, l4, l5, l6, angle1, angle2, angle;

		l1 = Math.sqrt(Math.pow(p2.y - p1.y, 2) + Math.pow(p2.x - p1.x, 2));
		l2 = Math.sqrt(Math.pow(p3.y - p2.y, 2) + Math.pow(p3.x - p2.x, 2));
		l4 = Math.sqrt(Math.pow(p3.y - p0.y, 2) + Math.pow(p3.x - p0.x, 2));
		l5 = Math.sqrt(Math.pow(p1.y - p0.y, 2) + Math.pow(p1.x - p0.x, 2));
		l6 = Math.sqrt(Math.pow(p2.y - p0.y, 2) + Math.pow(p2.x - p0.x, 2));

		angle1 = Math.acos(((l2 * l2) + (l6 * l6) - (l4 * l4)) / (2 * l2 * l6));
		angle2 = Math.acos(((l6 * l6) + (l1 * l1) - (l5 * l5)) / (2 * l6 * l1));

		angle = (Math.PI - angle1) - angle2;

		if (angle <= 0.0) {
			return (true);
		} else {
			return (false);
		}
	}

}

class PPoint {
	int x, y;
	float angle;
}
