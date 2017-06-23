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

/**
 * This interface defines the states of the user interface in particulare of the
 * mouse listener of the GraphMouseListner. It's used to abstract the behaviour
 * of the mouse listner from the state of the UI. In this way it's not necessary
 * to use a case for every situation, it's needed to change the state of the
 * mouse listner to change it's behaviour.
 *

 * @author Federico Granata
 * Date: 6-giu-2003
 * Time: 17.37.08

 */
interface UIState {

	/** handles the click event of the mouse
	 *
	 * @param e
	 */
	void handleClick(MouseEvent e);

	/** handles the press event of the mouse
	 *
	 * @param e
	 */
	void handlePress(MouseEvent e);

	/** handles the release event of the mouse
	 *
	 * @param e
	 */
	void handleRelease(MouseEvent e);

	/** handles the move event of the mouse
	 *
	 * @param e
	 */
	void handleMove(MouseEvent e);

	/** handles the enter event of the mouse
	 *
	 * @param e
	 */
	void handleEnter(MouseEvent e);

	/** handles the exit event of the mouse
	 *
	 * @param e
	 */
	void handleExit(MouseEvent e);

	/** handles the drag event of the mouse
	 *
	 * @param e
	 */
	void handleDrag(MouseEvent e);

}
