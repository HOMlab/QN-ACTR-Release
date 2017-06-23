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

package jmt.gui.jmodel.controller.actions;

import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;

import javax.swing.KeyStroke;
import javax.swing.undo.UndoManager;

import jmt.gui.jmodel.controller.Mediator;

/**

 * @author Conti Andrea
 * Date: 27-ago-2003
 * Time: 23.58.12

 */

/**
 * A class representing a "redo" action.
 * @see jmt.gui.jmodel.controller.actions.AbstractUndoRedoAction
 */

public class ActionRedo extends AbstractUndoRedoAction {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected UndoManager um;

	public ActionRedo(Mediator mediator, UndoManager um) {
		super("Redo", mediator);
		putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_R));
		putValue(ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_Y, InputEvent.CTRL_MASK));
		this.um = um;
		update();
	}

	/**
	 * Update the state (name & enabled) of this instance
	 */
	@Override
	public void update() {
		setEnabled(um.canRedo());
		putValue(SHORT_DESCRIPTION, um.getRedoPresentationName());
	}

	public void actionPerformed(ActionEvent e) {
		mediator.redo();
		updateAll();
	}
}
