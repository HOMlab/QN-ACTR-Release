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
 * User: PoliMi
 * Date: 26-lug-2005
 * Time: 11.15.55
 * To change this template use File | Settings | File Templates.
 */
public class Sector2D {

	private double beta1;
	private double beta11;
	private double beta2;
	private double beta22;
	private newPoint p1;
	private newPoint p2;
	private Vector<newPoint> vp;

	//private int[] stations;

	public Sector2D() {

	}

	public Sector2D(double b1, double b11, double b2, double b22, newPoint p1) {
		beta1 = b1;
		beta11 = b11;
		beta2 = b2;
		beta22 = b22;
		this.p1 = p1;
		this.p2 = new newPoint(-1, -1);
		vp = new Vector<newPoint>();
		//stations[0]=-1;

	}

	public Sector2D(double b1, double b11, double b2, double b22, newPoint p1, newPoint p2) {
		beta1 = b1;
		beta11 = b11;
		beta2 = b2;
		beta22 = b22;
		this.p1 = p1;
		this.p2 = p2;
		vp = new Vector<newPoint>();
		//stations[0]=-1;

	}

	public Sector2D(double b1, double b11, double b2, double b22, newPoint p1, newPoint p2, Vector<newPoint> vs) {
		beta1 = b1;
		beta11 = b11;
		beta2 = b2;
		beta22 = b22;
		this.p1 = p1;
		this.p2 = p2;
		// vs è un vettore di newPoint
		vp = vs;
		//stations[0]=-1;

	}

	public double getBeta1() {
		return beta1;
	}

	public double getBeta11() {
		return beta11;
	}

	public double getBeta2() {
		return beta2;
	}

	public double getBeta22() {
		return beta22;
	}

	public newPoint getP1() {
		return p1;
	}

	public newPoint getP2() {
		return p2;
	}

	/**
	 * @return      il vettore con i newPoint collineari
	 */
	public Vector<newPoint> getCollinear() {
		return vp;
	}

	/**
	 * Conta il numero di punti collineari ad un settore
	 * @return
	 */
	public int CountCollinear() {
		return vp.size();
	}

	/**
	 * Controlla se il settore ha dei punti collineari
	 * @return      true se li ha
	 */
	public boolean hasCollinear() {
		if (vp.size() > 0) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * Controlla se un punto è collineare con i 2 che caratterizzano il settore
	 * Se il controllo dà esito positivo aggiunge il punto al vettore dei collineari
	 * @param ss
	 * @param p3
	 * @return
	 */
	public Sector2D addCollinear(Sector2D ss, newPoint p3) {
		Util2d uti = new Util2d();
		if (uti.Collinear(ss.getP1(), ss.getP2(), p3) && p3.getX() > ss.getP1().getX() && p3.getX() < ss.getP2().getX()) {
			vp.addElement(p3);
		}
		return ss;
	}

	public Sector2D addCollinearLast(Sector2D ss, newPoint p3) {
		Util2d uti = new Util2d();
		newPoint p2 = new newPoint((int) ss.getP1().getX(), 0);
		//if (uti.Collinear(ss.getP1(),p2,p3) && (double)ss.getP1().getY()!=0 && p3.y!=0)
		// Riga alternativa usata come prova
		if (ss.getP1().getX() == p3.getX() && ss.getP1().getY() != p3.getY()) {
			vp.addElement(p3);
		}
		return ss;
	}

	public Sector2D addCollinearFirst(Sector2D ss, newPoint p3) {
		Util2d uti = new Util2d();
		newPoint p2 = new newPoint(0, (int) ss.getP1().getY());
		//if (uti.Collinear(p2,ss.getP1(),p3) && (double)ss.getP1().getX()!=0)
		// Riga alternativa usata come prova
		if (ss.getP1().getY() == p3.getY() && ss.getP1().getX() != p3.getX()) {
			vp.addElement(p3);
		}
		return ss;
	}

	public int AssoStation2D(Vector points, newPoint p) {
		int stat = -1;

		for (int i = 0; i < points.size(); i++) {

			if (p.getX() == ((newPoint) points.get(i)).getX() && p.getY() == ((newPoint) points.get(i)).getY()) {
				stat = i;
			}
		}
		return stat;
	}

	@Override
	public String toString() {
		String out = "";
		if (p2 != null && vp.size() == 0) {
			out = beta1 + " " + beta11 + " " + beta2 + " " + beta22 + " " + p1 + " " + p2;
		} else if (p2 == null && vp.size() != 0) {
			out = beta1 + " " + beta11 + " " + beta2 + " " + beta22 + " " + p1 + " " + vp.size();
		} else if (p2 != null && vp.size() != 0) {
			out = beta1 + " " + beta11 + " " + beta2 + " " + beta22 + " " + p1 + " " + p2 + " " + vp.size();
		} else if (p2 == null && vp.size() == 0) {
			out = beta1 + " " + beta11 + " " + beta2 + " " + beta22 + " " + p1 + " " + p2 + " " + vp.size();
		}
		return out;
	}
}
