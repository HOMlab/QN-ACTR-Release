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

/** Port to connect vertex through connections. the input port is made to
 * create only incomeing connections.

 * @author Federico Granata
 * Date: 17-lug-2003
 * Time: 11.11.22

 * Modified by Bertoli Marco 17-giu-2005

 */
public class InputPort extends DefaultPort {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/** creates an input port
	 *
	 * @param userObject : Reference to JmtCell that contains this port
	 */
	public InputPort(Object userObject) {
		super(userObject);
	}

	/**
	 * Adds <code>edge</code> to the list of ports.
	 */
	@Override
	public boolean addEdge(Object edge) {
		if (((Edge) edge).getTarget() == this.getParent()) {
			return false;
		} else {
			return edges.add(edge);
		}
	}

}
