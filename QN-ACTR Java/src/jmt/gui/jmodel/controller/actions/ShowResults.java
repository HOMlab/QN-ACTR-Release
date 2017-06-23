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

import jmt.gui.jmodel.controller.Mediator;

/**
 * <p>Title: ShowResults Action</p>
 * <p>Description: Action associated to "show results" menu item.
 * I hate this structure but I have to keep formal consistency with existent code.</p>
 *
 * @author Bertoli Marco
 *         Date: 29-set-2005
 *         Time: 15.44.11
 */
public class ShowResults extends AbstractJmodelAction {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Defines an <code>Action</code> object with a default
	 * description string and default icon.
	 */
	public ShowResults(Mediator mediator) {
		super("Show Results", "Results", mediator);
		this.setTooltipText("show simulation results window");
		this.setMnemonicKey(KeyEvent.VK_R);
		this.setAcceleratorKey(KeyEvent.VK_R, InputEvent.ALT_MASK);
		this.setSelectable(true);
		this.setEnabled(false);
	}

	/**
	 * Invoked when an action occurs.
	 */
	public void actionPerformed(ActionEvent e) {
		mediator.showResultsWindow(isSelected());
	}
}
