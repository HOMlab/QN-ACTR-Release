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
 * User: Andrea
 * Date: 2-ago-2005
 * Time: 17.53.10
 * To change this template use File | Settings | File Templates.
 */
public class Station3D {

	private String nome;
	private Vertex v3d;
	private int[] coord;

	public Station3D() {

	}

	public Station3D(String nome, Vertex v3d) {

		this.nome = nome;
		this.v3d = v3d;
		coord = v3d.getCoords();

	}

	public String getName() {
		return nome;
	}

	public Vertex getV3D() {
		return v3d;
	}

	public int[] getcoord() {
		return coord;
	}

	public Vector<Station3D> CreateStations(Vector vertices) {
		Vector<Station3D> out = new Vector<Station3D>();
		for (int i = 0; i < vertices.size(); i++) {
			Station3D st3d = new Station3D("Stazione " + i, ((Vertex) vertices.get(i)));
			out.addElement(st3d);
		}
		return out;
	}

}
