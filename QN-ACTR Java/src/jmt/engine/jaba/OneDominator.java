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

import jmt.engine.jaba.Hull.Vertex;

/**
 * Created by IntelliJ IDEA.
 * User: PoliMi
 * Date: 7-ott-2005
 * Time: 14.28.15
 * To change this template use File | Settings | File Templates.
 */
public class OneDominator {

	private Vector<Vertex> vertices; // = new Vector();
	private Vertex dominator; // = new Vertex(-1,-1,-1);

	public boolean IsOneDominator() {
		boolean out = false;
		for (int i = 0; i < vertices.size(); i++) {
			int k = 0;
			int[] coordi = vertices.get(i).getCoords();
			for (int j = 0; j < vertices.size(); j++) {

				int[] coordj = vertices.get(j).getCoords();
				if (coordi[0] > coordj[0] && coordi[1] > coordj[1] && coordi[2] > coordj[2]) {
					k++;
				}

			}
			if (k == (vertices.size() - 1)) {
				out = true;
				dominator = vertices.get(i);
			}
		}
		return out;
	}

	public void setVertices(Vector<Vertex> verts) {
		vertices = verts;
	}

	public Vertex getDominator() {
		return dominator;
	}

}
