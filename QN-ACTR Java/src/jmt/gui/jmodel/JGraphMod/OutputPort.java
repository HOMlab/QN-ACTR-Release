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

package jmt.gui.jmodel.JGraphMod;

import org.jgraph.graph.DefaultPort;
import org.jgraph.graph.Edge;

/**

 * @author Federico Granata
 * Date: 17-lug-2003
 * Time: 11.12.12

 */
public class OutputPort extends DefaultPort {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Creates a new outputPort
	 * @param userObject: Reference to JmtCell that contains this port
	 */
	public OutputPort(Object userObject) {
		super(userObject);
	}

	/**
	 * Adds <code>edge</code> to the list of ports.
	 */
	@Override
	public boolean addEdge(Object edge) {
		if (((Edge) edge).getSource() == this.getParent()) {
			return false;
		} else {
			return edges.add(edge);
		}
	}

}
