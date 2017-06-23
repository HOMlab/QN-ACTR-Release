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
 * User: Andrea Zanzottera
 * Date: 28-nov-2005
 * Time: 12.23.05
 * To change this template use File | Settings | File Templates.
 *
 * Debugged and optimized by Bertoli Marco
 */
public class Mapping3D {
	private final double rad3 = Math.sqrt(3);
	private final double rad3d2 = Math.sqrt(3) / 2;

	/**
	 * Questo metodo è usato per mappare le 3 coordinate beta in un sistema di 2 coordinate
	 * @param beta1
	 * @param beta2
	 * @return xy
	 */
	public double[] Beta2xy(double beta1, double beta2, double beta3) {
		double[] xy = new double[2];
		//Soluzione esatta ma che rende il disegno specchiato.
		//xy[0]=beta3*0.866026+beta2*0.866026*2;//+beta1*0.866026;
		//Soluzione che rende il disegno corretto
		xy[0] = beta3 * rad3d2 + beta2 * rad3;//+beta1*0.866026;
		xy[1] = beta3;
		return xy;
	}

	/**
	 * Questo metodo rimappa in 2D tutti i punti di un settore di saturazione
	 * @param s3d
	 */
	public Sector3D RemapSector(Sector3D s3d) {
		int numofpoints = (s3d).CountPoint();
		for (int i = 0; i < numofpoints; i++) {
			double[] coord = (s3d).getBetas(i);
			double[] newcoord = Beta2xy(coord[0], coord[1], coord[2]);
			(s3d).addxycoord(newcoord);
		}
		return s3d;
	}

	/**
	 * Questo metodo viene usato per mappare in 2D tutti i settori di un Vector di Sector3D
	 * @param sect
	 * @return sect
	 */
	public Vector<Sector3D> RemapAllSectors(Vector<Object> sect) {
		Vector<Sector3D> newsect = new Vector<Sector3D>();
		int numofsect = sect.size();
		for (int i = 0; i < numofsect; i++) {
			newsect.addElement(RemapSector((Sector3D) sect.get(i)));
		}
		return newsect;
	}

}
