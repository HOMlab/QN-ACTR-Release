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

import java.awt.event.KeyEvent;
import java.util.Arrays;
import java.util.Collection;

import javax.swing.AbstractAction;
import javax.swing.KeyStroke;

import jmt.framework.gui.image.ImageLoader;

public abstract class AbstractJMTAction extends AbstractAction {
	private static final long serialVersionUID = 1L;
	/** Name of the icon to be shown in the button */
	public static final String IMAGE_NAME = "AbstractJMTAction.image.name";
	/** Tells if this action is selectable */
	public static final String SELECTABLE = "AbstractJMTAction.selectable";
	/** Tells if this action is part of a group. Valid only if selectable is true */
	public static final String GROUP = "AbstractJMTAction.group";
	/** A Collection of submenu items iff this action is a menu */
	public static final String MENUACTIONS = "AbstractJMTAction.menu.actions";
	/** This property change event is fired each time selection is toggled */
	public static final String PROPERTY_SELECTED = "AbstractJMTAction.selected";

	/** To support sharing of toggle status between components */
	private boolean selected = false;

	/**
	 * Builds a new AbstractJMTAction, not selectable by default
	 */
	public AbstractJMTAction() {
		setSelectable(false);
	}

	/**
	 * Builds a new AbstractJMTAction, not selectable by default with given name
	 * @param name name of the action.
	 */
	public AbstractJMTAction(String name) {
		this();
		setName(name);
	}

	/**
	 * Sets the icon associated with this image
	 * @param iconName name of the icon
	 * @param loader loader to load the icon
	 */
	protected void setIcon(String iconName, ImageLoader loader) {
		this.putValue(IMAGE_NAME, iconName);
		this.putValue(SMALL_ICON, loader.loadIcon(iconName));
	}

	/**
	 * Text to be displayed in the menu or button (that is the name of the action)
	 * @param text name of the action
	 */
	protected void setName(String text) {
		this.putValue(NAME, text);
	}

	/**
	 * Text to be displayed as tooltip
	 * @param text tooltip for the action
	 */
	protected void setTooltipText(String text) {
		this.putValue(SHORT_DESCRIPTION, text);
	}

	/**
	 * Text to be displayed in context-sensitive help
	 * @param text help for the action
	 */
	protected void setHelpText(String text) {
		this.putValue(LONG_DESCRIPTION, text);
	}

	/**
	 * Sets an accelerator key to perform this action
	 * @param key key to be pressed
	 * @param modifiers a bitwise-ored combination of any modifiers.
	 * @see KeyEvent for keys and modifiers
	 */
	protected void setAcceleratorKey(int key, int modifiers) {
		putValue(ACCELERATOR_KEY, KeyStroke.getKeyStroke(key, modifiers));
	}

	/**
	 * Sets mnemonic key to call this action
	 * @param key key to be pressed
	 * @see KeyEvent for keys and modifiers
	 */
	protected void setMnemonicKey(int key) {
		putValue(MNEMONIC_KEY, new Integer(key));
	}

	/**
	 * Sets if this action is selectable (a toggle button for example)
	 * @param selectable true if this is selectable
	 */
	protected void setSelectable(boolean selectable) {
		putValue(SELECTABLE, new Boolean(selectable));
	}

	/**
	 * Sets the group associated to this action if it's selectable
	 * @param group identifier of group. Must be greater or equal to zero.
	 */
	protected void setGroup(int group) {
		putValue(GROUP, new Integer(group));
	}

	/**
	 * Sets a collection of AbstractJMTAction iff this action is a menu.
	 * A null value is used as separator. 
	 * @param actions a Collection of AbstractJMTAction
	 */
	protected void setMenuActions(Collection<AbstractJMTAction> actions) {
		putValue(MENUACTIONS, actions);
	}

	/**
	 * Sets a collection of AbstractJMTAction iff this action is a menu.
	 * A null value is used as separator. 
	 * @param actions an array of AbstractJMTAction
	 */
	protected void setMenuActions(AbstractJMTAction[] actions) {
		putValue(MENUACTIONS, Arrays.asList(actions));
	}

	/**
	 * Returns a collection of AbstractJMTAction menu actions if this action is a menu.
	 * @return a Collection of AbstractJMTAction. Null if this action is not a menu.
	 */
	@SuppressWarnings("unchecked")
	public Collection<AbstractJMTAction> getMenuActions() {
		return (Collection<AbstractJMTAction>) getValue(MENUACTIONS);
	}

	/**
	 * Tells if this action is selected. Valid only if this action is selectable
	 * @return truth value
	 * @see AbstractJMTAction#setSelectable(boolean)
	 */
	public boolean isSelected() {
		return selected;
	}

	/**
	 * Sets if this action is selected
	 * @param newValue true to select this action, false otherwise.
	 */
	public void setSelected(boolean newValue) {
		boolean oldValue = selected;
		if (oldValue != newValue) {
			selected = newValue;
			firePropertyChange(PROPERTY_SELECTED, Boolean.valueOf(oldValue), Boolean.valueOf(newValue));
		}
	}

}
