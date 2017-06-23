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

package jmt.gui.jmodel.controller;

import java.awt.Cursor;
import java.awt.event.MouseEvent;

/**
 * Defines the behaviour of the mouse listner when the UI is in the insert mode
 *

 * @author dekkar (Federico Granata)
 * Date: Jun 13, 2003
 * Time: 11:50:33 AM

 */
public class InsertState extends UIStateDefault {

	protected String insertClass; // className of cell to be inserted

	/**
	 * creates the insert state
	 *
	 * @param mediator
	 */
	public InsertState(Mediator mediator) {
		super(mediator);
	}

	/**
	 * inserts a new vertex in the graph at the point where the mouse is pressed
	 * the new vertex is selected & then sets the select status.
	 *
	 * @param e mouse press event
	 */
	@Override
	public void handlePress(MouseEvent e) {
		// Does vertex overlap an existing cell?
		if (mediator.overlapCells(e.getPoint(), mediator.getCellFactory().predictCellSize(insertClass))) {
			// Yes, do nothing
			return;
		}
		mediator.InsertCell(e.getPoint(), mediator.getCellFactory().createCell(insertClass));
		mediator.selectAt(e);
	}

	@Override
	public void handleEnter(MouseEvent e) {
		mediator.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
	}

	/**
	 * set vertex, it is used to change the vertex tht will be inserted at next
	 * press mouse event
	 *
	 * @param className of component to be inserted
	 *
	 * Bertoli Marco
	 */
	public void setInsertClass(String className) {
		this.insertClass = className;
	}

}
