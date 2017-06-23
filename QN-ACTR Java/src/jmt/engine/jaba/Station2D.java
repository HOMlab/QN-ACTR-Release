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

/**
 * Created by IntelliJ IDEA.
 * User: Andrea
 * Date: 18-ott-2005
 * Time: 16.56.03
 * To change this template use File | Settings | File Templates.
 */
public class Station2D {

	private newPoint vert;
	private String name;

	public Station2D(newPoint vert, String name) {
		this.vert = vert;
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public newPoint getVert() {
		return vert;
	}

	@Override
	public String toString() {
		return name;
	}
}
