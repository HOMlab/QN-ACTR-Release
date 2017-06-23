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

import java.awt.Shape;
import java.awt.geom.GeneralPath;

import jmt.gui.jmodel.controller.Mediator;

import org.jgraph.graph.CellViewRenderer;
import org.jgraph.graph.EdgeView;

/**
 * <p>Title: JmtEdgeView </p>
 * @author Giuseppe De Cicco & Fabio Granara
 * 		Date: 15-ott-2006
 * 
 */

public class JmtEdgeView extends EdgeView {

	/**
		 * 
		 */
	private static final long serialVersionUID = 1L;
	// Questa variabile e' gia statica dai sorgenti della libreria.
	private static JmtEdgeRenderer renderer = new JmtEdgeRenderer();
	private Mediator mediator;

	public JmtEdgeView() {
		super();
	}

	/**
	 */
	public JmtEdgeView(Object cell) {
		super(cell);

	}

	public JmtEdgeView(Object cell, Mediator factory) {
		// TODO Auto-generated constructor stub
		super(cell);
		this.mediator = factory;
	}

	@Override
	public CellViewRenderer getRenderer() {

		return renderer;
	}

	// Giuseppe De Cicco & Fabio Granara
	@Override
	public Shape getShape() {
		//		System.out.println("chiamato");

		if (sharedPath != null) {
			return sharedPath;
		} else if (mediator.getIsReleased()) {
			//			System.out.println("VAlore in View: "+((JmtEdge)(this.getCell())).latiDiIntersezione.size());
			//			System.out.println("**********RESTITUISCO IL RENDERER***********");
			return sharedPath = (GeneralPath) renderer.createShape();
		} else {

			return sharedPath = (GeneralPath) renderer.createShape2();

		}
	}

}
