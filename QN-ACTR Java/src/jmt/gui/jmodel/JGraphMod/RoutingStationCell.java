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

/**
 * <p>Title: Router Cell</p>
 * <p>Description: Routes jobs without servicing capability. This special class was introduced
 * to simplify conversion</p>
 *
 * @author Bertoli Marco
 *         Date: 20-feb-2006
 *         Time: 17.53.21
 */
public class RoutingStationCell extends JmtCell {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	// Disables this component
	public static final boolean canBePlaced = true;

	// Do not change this as it is accessed by reflection to forecast new cell dimensions (Bertoli Marco)
	public static final String ICON = Mediator.advanced ? "bc" : "Router";

	/**
	 * Creates a graph cell and initializes it with the specified user object.
	 *
	 * @param userObject an Object provided by the user that constitutes
	 *                   the cell's data
	 */
	public RoutingStationCell(Object userObject) {
		super(RoutingStationCell.ICON, userObject);
		type = TERMINAL;
	}

	/**
	 * creats the ports for this vertex
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
	 * Returns the name of the icon of this cell
	 *
	 * @return the name of the icon of this cell
	 */
	@Override
	public String getIcon() {
		return ICON;
	}

}
