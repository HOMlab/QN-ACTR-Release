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

import java.awt.event.MouseEvent;

/** Default implementation of UIState interface all the methods are void to let
 * the user programmer implement only the necessary state methods

 * @author dekkar (Federico Granata)
 * Date: Jun 11, 2003
 * Time: 2:47:19 PM

 */
public class UIStateDefault implements UIState {

	protected Mediator mediator;//mediator that connects

	public UIStateDefault(Mediator mediator) {
		this.mediator = mediator;
	}

	public void handleClick(MouseEvent e) {
	}

	public void handlePress(MouseEvent e) {
	}

	public void handleRelease(MouseEvent e) {
	}

	public void handleMove(MouseEvent e) {
		e.consume();
	}

	public void handleEnter(MouseEvent e) {
	}

	public void handleExit(MouseEvent e) {
	}

	public void handleDrag(MouseEvent e) {
	}
}
