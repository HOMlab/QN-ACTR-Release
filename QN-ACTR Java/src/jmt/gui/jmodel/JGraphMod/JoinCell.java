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

import jmt.gui.jmodel.controller.Mediator;

import org.jgraph.graph.Port;

/** Generates new jobs in open systems, it generates jobs of 1 JobClass only

 * @author Federico Granata, Bertoli Marco
 * Date: 5-giu-2003
 * Time: 13.13.51

 */
public class JoinCell extends JmtCell {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Tells if this component can be placed on JGraph or has been disabled. This is
	 * useful to load old models with disabled components (like terminals)
	 */
	public static final boolean canBePlaced = true;

	// Do not change this as it is accessed by reflection to forecast new cell dimensions (Bertoli Marco)
	public static final String ICON = Mediator.advanced ? "bc" : "join";

	/**
	 * Creates a graph cell and initializes it with the specified user object.
	 *
	 * @param userObject an Object provided by the user that constitutes
	 *                   the cell's data
	 */
	public JoinCell(Object userObject) {
		super(ICON, userObject);
		//		this.add(new InputPort(userObject));
		//		this.add(new OutputPort(userObject));
		type = JOIN;
		//		this.
	}

	/**creats the ports for this vertex
	 *
	 * @return array of ports
	 */
	@Override
	public Port[] createPorts() {
		Port[] ports = new Port[2];
		ports[0] = new InputPort(this);
		ports[1] = new OutputPort(this);
		return ports;
	}

	/**
	 * Tells if this station generates or destroys jobs (useful for blocking region
	 * management)
	 * @return true if this station generates or destroy jobs, false otherwise
	 */
	@Override
	public boolean generateOrDestroyJobs() {
		return true;
	}

	/**
	 * Returns the name of the icon of this cell
	 *
	 * @return the name of the icon of this cell
	 */
	@Override
	public String getIcon() {
		return ICON;
	}

}
