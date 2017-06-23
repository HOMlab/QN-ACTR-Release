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

import jmt.gui.jmodel.controller.Mediator;

/**

 * @author Federico Granata
 * Date: 4-giu-2003
 * Time: 10.49.03

 */
public class ActionCopy extends AbstractJmodelAction {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Defines an <code>Action</code> object with a default
	 * description string and default icon.
	 */
	public ActionCopy(Mediator mediator) {
		super("Copy", "Copy", mediator);
		putValue(SHORT_DESCRIPTION, "Copy");
		putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_C));
		//Conti Andrea
		putValue(ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_C, InputEvent.CTRL_MASK));
		//end
		setEnabled(false);
	}

	/**
	 * Invoked when an action occurs.
	 */
	public void actionPerformed(ActionEvent e) {
		// 04/08/03 - Conti Andrea //////////////////////////////////////////
		mediator.copySelection();
		mediator.enablePasteAction(true);
		// 04/08/03 - end /////////////////////////////////////////////////////
	}
}
