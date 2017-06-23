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
package jmt.framework.gui.components;

import java.util.Collection;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;

import jmt.framework.gui.image.ImageLoader;
import jmt.framework.gui.listeners.AbstractJMTAction;
import jmt.framework.gui.listeners.SelectedActionButtonChangeListener;

/**
 * <p><b>Name:</b> JMTMenuBar</p> 
 * <p><b>Description:</b> 
 * A generic menu bar with many enhanchments to be used by JMT.
 * TODO Support for button groups and item add/remove is not implemented yet.
 * </p>
 * <p><b>Date:</b> 31/gen/07
 * <b>Time:</b> 18:31:14</p>
 * @author Bertoli Marco
 * @version 1.0
 */
public class JMTMenuBar extends JMenuBar {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	/** Component used to load images */
	protected ImageLoader imageLoader;

	/**
	 * Builds a new JMTMenuBar
	 * @param loader component used to load images
	 */
	public JMTMenuBar(ImageLoader loader) {
		init(loader);
	}

	/**
	 * Initialize this toolbar
	 * @param loader component used to load images
	 */
	private void init(ImageLoader loader) {
		this.imageLoader = loader;
	}

	/**
	 * Adds a menu (or menu item) to this 
	 * @param action action of the menu to be added
	 * @return created menu item
	 */
	public JMenuItem addMenu(AbstractJMTAction action) {
		JMenuItem menu = generateItem(action);
		add(menu);
		return menu;
	}

	/**
	 * Populates this menu bar
	 * @param actions an array of AbstractJMTAction with all actions to be added
	 */
	public void populateMenu(AbstractJMTAction[] actions) {
		for (AbstractJMTAction action : actions) {
			addMenu(action);
		}
	}

	/**
	 * Populates this menu bar
	 * @param actions a collection of AbstractJMTAction with all actions to be added
	 */
	public void populateMenu(Collection<AbstractJMTAction> actions) {
		for (AbstractJMTAction action : actions) {
			addMenu(action);
		}
	}

	/**
	 * Generates a menu item given an action
	 * @param action action associated with menu item
	 * @return generated item
	 */
	protected JMenuItem generateItem(AbstractJMTAction action) {
		JMenuItem item;
		Collection<AbstractJMTAction> items = action.getMenuActions();
		if (items != null) {
			// If this is a menu
			JMenu menu = new JMenu(action);
			item = menu;
			for (AbstractJMTAction current : items) {
				if (current == null) {
					// Add a separator
					menu.addSeparator();
				} else {
					// Add a menu item (or a submenu)
					menu.add(generateItem(current));
				}
			}
		} else {
			// This is a menuitem
			Boolean selectable = (Boolean) action.getValue(AbstractJMTAction.SELECTABLE);
			if (selectable != null && selectable.booleanValue()) {
				// item is selectable
				item = new JCheckBoxMenuItem(action);
				// Sets selected icon
				String iconName = (String) action.getValue(AbstractJMTAction.IMAGE_NAME);
				if (iconName != null) {
					item.setSelectedIcon(imageLoader.loadIcon(iconName, ImageLoader.MODIFIER_SELECTED));
				}
				// Adds a listener to both item and action to synchronize their selected state
				new SelectedActionButtonChangeListener(action, item);
			} else {
				item = new JMenuItem(action);
			}
		}
		return item;
	}
}
