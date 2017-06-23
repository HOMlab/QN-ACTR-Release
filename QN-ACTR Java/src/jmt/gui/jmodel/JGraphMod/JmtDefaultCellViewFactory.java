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

import org.jgraph.graph.DefaultCellViewFactory;

/**
 * <p>Title: JmtDefaultCellViewFactory </p>
 * @author Giuseppe De Cicco & Fabio Granara
 * 		Date: 22-genn-2007
 * 
 */

public class JmtDefaultCellViewFactory extends DefaultCellViewFactory {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected Mediator mediator;

	public JmtDefaultCellViewFactory(Mediator mediator) {
		super();
		this.mediator = mediator;

	}
}
