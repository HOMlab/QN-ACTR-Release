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

import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.undo.UndoManager;

import jmt.gui.jmodel.controller.actions.AbstractUndoRedoAction;

/**

 * @author alyf (Andrea Conti)
 * Date: 3-set-2003
 * Time: 10.15.38

 */

/**
 * This class relays messages to an actual <code>UndoManager</code>; on success, it requests undo/redo
 * actions to update their status.<br>
 * Please note that you should <b>not</b> add the <code>UndoManager</code> to the listener queue, but only the <code>UndoManagerProxy</code>.
 *
 */
public class UndoManagerProxy implements UndoableEditListener {

	protected UndoManager actual;

	/**
	 * Creates a new <code>UndoManagerProxy</code>
	 * @param actual the <code>UndoManager</code> messages should be relayed to
	 */
	public UndoManagerProxy(UndoManager actual) {
		this.actual = actual;
	}

	/**
	 * @see javax.swing.event.UndoableEditListener#undoableEditHappened(UndoableEditEvent)
	 */
	public void undoableEditHappened(UndoableEditEvent e) {
		actual.addEdit(e.getEdit());
		AbstractUndoRedoAction.updateAll();
	}

	/**
	 * @see javax.swing.undo.UndoManager#discardAllEdits()
	 */
	public void discardAllEdits() {
		actual.discardAllEdits();
		AbstractUndoRedoAction.updateAll();
	}

}
