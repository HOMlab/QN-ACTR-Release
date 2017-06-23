/**    
  * Copyright (C) 2007, Laboratorio di Valutazione delle Prestazioni - Politecnico di Milano

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
package jmt.framework.gui.listeners;

import java.awt.event.ActionEvent;
import java.util.Collection;

/**
 * <p><b>Name:</b> MenuAction</p> 
 * <p><b>Description:</b> 
 * An implementation of the AbstractJMTAction class used to create a menu
 * without writing the actionPerformed method
 * </p>
 * <p><b>Date:</b> 31/gen/07
 * <b>Time:</b> 21:18:34</p>
 * @author Bertoli Marco
 * @version 1.0
 */
public class MenuAction extends AbstractJMTAction {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Builds a new Menu Action
	 * @param name name of the menu
	 * @param mnemonicKey mnemonic key for the menu
	 * @param actions menu items to be added
	 */
	public MenuAction(String name, int mnemonicKey, AbstractJMTAction[] actions) {
		this.setName(name);
		this.setMnemonicKey(mnemonicKey);
		this.setMenuActions(actions);
	}

	/**
	 * Builds a new Menu Action
	 * @param name name of the menu
	 * @param mnemonicKey mnemonic key for the menu
	 * @param actions a collection of AbstractJMTAction of menu items to be added
	 */
	public MenuAction(String name, int mnemonicKey, Collection<AbstractJMTAction> actions) {
		this.setName(name);
		this.setMnemonicKey(mnemonicKey);
		this.setMenuActions(actions);
	}

	/**
	 * Builds a new Menu Action. Mnemonic key is the first letter of name.
	 * @param name name of the menu
	 * @param actions menu items to be added
	 */
	public MenuAction(String name, AbstractJMTAction[] actions) {
		this.setName(name);
		this.setMnemonicKey(name.charAt(0));
		this.setMenuActions(actions);
	}

	/**
	 * Builds a new Menu Action. Mnemonic key is the first letter of name.
	 * @param name name of the menu
	 * @param actions a collection of AbstractJMTAction of menu items to be added
	 */
	public MenuAction(String name, Collection<AbstractJMTAction> actions) {
		this.setName(name);
		this.setMnemonicKey(name.charAt(0));
		this.setMenuActions(actions);
	}

	/* (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		// Override this to perform an action
	}
}
