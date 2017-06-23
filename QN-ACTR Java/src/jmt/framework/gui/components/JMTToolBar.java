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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;

import javax.swing.AbstractButton;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;

import jmt.framework.gui.image.ImageLoader;
import jmt.framework.gui.listeners.AbstractJMTAction;
import jmt.framework.gui.listeners.SelectedActionButtonChangeListener;

/**
 * <p><b>Name:</b> JMTToolbar</p> 
 * <p><b>Description:</b> 
 * A generic toolbar with many enhanchments to be used by JMT.
 * Image name can be stored inside Action or explicitly inserted. Pressed, rollover and selected (toggled)
 * buttons requires images with the appropriate suffixes.
 * </p>
 * <p><b>Date:</b> 30/gen/07
 * <b>Time:</b> 10:44:05</p>
 * @author Bertoli Marco
 * @version 1.0
 */
public class JMTToolBar extends JToolBar {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/** Component used to load images */
	protected ImageLoader imageLoader;

	/** A map with all buttons with associated component and groups */
	protected HashMap<Action, ButtonWithGroup> buttons = new HashMap<Action, ButtonWithGroup>();
	/** A map with all button groups with associated group */
	protected HashMap<Integer, JMTButtonGroup> buttonGroups = new HashMap<Integer, JMTButtonGroup>();

	/**
	 * Builds a new toolbar
	 * @param loader component used to load images
	 */
	public JMTToolBar(ImageLoader loader) {
		init(loader);
	}

	/**
	 * Initialize this toolbar
	 * @param loader component used to load images
	 */
	private void init(ImageLoader loader) {
		this.imageLoader = loader;
		setRollover(true);
	}

	/**
	 * Adds a new button to this toolbar.
	 * @param action action associated to the button
	 * @param iconName image to be displayed on the button. If null the image from the action
	 * is used. If this is unset too, the name is displayed.
	 * @return created button
	 */
	public JButton addButton(Action action, String iconName) {
		if (buttons.containsKey(action)) {
			return (JButton) (buttons.get(action)).button;
		}
		JButton button = new JButton();
		button.setAction(action);
		button.setText("");
		// Finds icon name
		if (iconName == null) {
			iconName = (String) action.getValue(AbstractJMTAction.IMAGE_NAME);
		}
		if (iconName == null) {
			// At this point uses name as text
			button.setText((String) action.getValue(Action.NAME));
		} else {
			// Sets images
			button.setIcon(imageLoader.loadIcon(iconName));
			button.setRolloverIcon(imageLoader.loadIcon(iconName, ImageLoader.MODIFIER_ROLLOVER));
			button.setPressedIcon(imageLoader.loadIcon(iconName, ImageLoader.MODIFIER_PRESSED));
		}
		button.setFocusPainted(false);
		button.setContentAreaFilled(false);
		button.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 0));
		add(button);
		ButtonWithGroup data = new ButtonWithGroup(button, -1);
		buttons.put(action, data);
		return button;
	}

	/**
	 * Adds a new button to this toolbar.
	 * @param action action associated to the button
	 * @return created button
	 */
	public JButton addButton(AbstractJMTAction action) {
		return addButton(action, null);
	}

	/**
	 * Adds a new button toggle to this toolbar.
	 * @param action action associated to the button
	 * @param iconName image to be displayed on the button. If null the image from the action
	 * is used. If this is unset too, the name is displayed.
	 * @param group identifier of button group. Each button belonging to the same group cannot be pressed together.
	 * Groups are created here, if group is less than zero means no grouping.
	 * @return created button
	 */
	public JToggleButton addToggleButton(AbstractJMTAction action, String iconName, int group) {
		if (buttons.containsKey(action)) {
			return (JToggleButton) (buttons.get(action)).button;
		}
		JToggleButton button = new JToggleButton();
		button.setAction(action);
		// Adds a listener to both button and action to synchronize their selected state
		SelectedActionButtonChangeListener listener = new SelectedActionButtonChangeListener(action, button);
		button.setText("");
		// Finds icon name
		if (iconName == null) {
			iconName = (String) action.getValue(AbstractJMTAction.IMAGE_NAME);
		}
		if (iconName == null) {
			// At this point uses name as text
			button.setText((String) action.getValue(Action.NAME));
		} else {
			// Sets images
			button.setIcon(imageLoader.loadIcon(iconName));
			button.setRolloverIcon(imageLoader.loadIcon(iconName, ImageLoader.MODIFIER_ROLLOVER));
			button.setPressedIcon(imageLoader.loadIcon(iconName, ImageLoader.MODIFIER_PRESSED));
			button.setSelectedIcon(imageLoader.loadIcon(iconName, ImageLoader.MODIFIER_SELECTED));
		}
		button.setFocusPainted(false);
		button.setContentAreaFilled(false);
		button.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 0));
		add(button);
		if (group >= 0) {
			JMTButtonGroup bGroup = getOrCreateGroup(group);
			bGroup.add(button);
		}
		ButtonWithGroup data = new ButtonWithGroup(button, group);
		data.listener = listener;
		buttons.put(action, data);
		return button;
	}

	/**
	 * Adds a new button toggle to this toolbar. This button has no group.
	 * @param action action associated to the button
	 * @param iconName image to be displayed on the button. If null the image from the action
	 * is used. If this is unset too, the name is displayed.
	 * @return created button
	 */
	public JToggleButton addToggleButton(AbstractJMTAction action, String iconName) {
		return this.addToggleButton(action, iconName, -1);
	}

	/**
	 * Adds a new button toggle to this toolbar.
	 * @param action action associated to the button
	 * @param group identifier of button group. Each button belonging to the same group cannot be pressed together.
	 * Groups are created here, if group is less than zero means no grouping.
	 * @return created button
	 */
	public JToggleButton addToggleButton(AbstractJMTAction action, int group) {
		return this.addToggleButton(action, null, group);
	}

	/**
	 * Adds a new button toggle to this toolbar. This button has no group.
	 * @param action action associated to the button
	 * @return created button
	 */
	public JToggleButton addToggleButton(AbstractJMTAction action) {
		return this.addToggleButton(action, null, -1);
	}

	/**
	 * Adds a button deriving all properties from specified action
	 * @param action action associated to the button
	 * @return created button
	 * @see AbstractJMTAction#setSelectable(boolean)
	 * @see AbstractJMTAction#setGroup(int)
	 */
	public AbstractButton addGenericButton(AbstractJMTAction action) {
		Boolean selectable = (Boolean) action.getValue(AbstractJMTAction.SELECTABLE);
		Integer group = (Integer) action.getValue(AbstractJMTAction.GROUP);
		if (selectable != null && selectable.booleanValue()) {
			// This is a toggle button
			if (group != null) {
				return this.addToggleButton(action, group.intValue());
			} else {
				return this.addToggleButton(action);
			}
		} else {
			// This is a button
			return this.addButton(action);
		}
	}

	/**
	 * Removes any type of button (including toggle ones) from this toolbar
	 * @param action action of the button to be removed
	 * @return removed button
	 */
	public AbstractButton removeButton(AbstractJMTAction action) {
		ButtonWithGroup data = buttons.remove(action);
		if (data != null) {
			remove(data.button);
			// Removes from group
			JMTButtonGroup group = getButtonGroup(data.group);
			if (group != null) {
				group.remove(data.button);
				// Removes group if empty
				if (group.getButtonCount() == 0) {
					buttonGroups.remove(new Integer(data.group));
				}
			}
			// Removes listener (if any)
			if (data.listener != null) {
				data.listener.remove();
			}
			return data.button;
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.swing.JToolBar#addSeparator()
	 */
	@Override
	public void addSeparator() {
		add(new CustomSeparator());
	}

	/**
	 * Returns a button group
	 * @param group identifier of button group
	 * @return the button group or null if it doesn't exists
	 */
	public JMTButtonGroup getButtonGroup(int group) {
		return buttonGroups.get(new Integer(group));
	}

	/**
	 * Returns a button, given its name
	 * @param action action of the button to be returned
	 * @return found button
	 */
	public AbstractButton getButton(AbstractJMTAction action) {
		ButtonWithGroup data = buttons.get(action);
		if (data != null) {
			return data.button;
		} else {
			return null;
		}
	}

	/**
	 * Enables or disables every button of a button group
	 * @param group group to be enabled or disabled
	 * @param value true to enable the group, false to disable it.
	 */
	public void enableButtonGroup(int group, boolean value) {
		JMTButtonGroup bGroup = getButtonGroup(group);
		if (bGroup != null) {
			bGroup.setEnabled(value);
		}
	}

	/**
	 * Clears selection of a button group
	 * @param group group to be deselected
	 */
	public void clearButtonGroupSelection(int group) {
		JMTButtonGroup bGroup = getButtonGroup(group);
		if (bGroup != null) {
			bGroup.clearSelection();
		}
	}

	/**
	 * Clicks a given button
	 * @param action action of the button to be clicked
	 */
	public void clickButton(AbstractJMTAction action) {
		AbstractButton button = getButton(action);
		if (button != null) {
			button.doClick();
		}
	}

	/**
	 * Returns all buttons inside this toolbar
	 * @return a set with all buttons
	 */
	public HashSet<AbstractButton> getAllButtons() {
		HashSet<AbstractButton> set = new HashSet<AbstractButton>();
		for (Object element : buttons.values()) {
			ButtonWithGroup bwg = (ButtonWithGroup) element;
			set.add(bwg.button);
		}
		return set;
	}

	/**
	 * Enables or disables all buttons of this toolbar
	 * @param enable true to enable every button, false to disable them
	 */
	public void enableButtons(boolean enable) {
		for (Object element : buttons.keySet()) {
			AbstractJMTAction action = (AbstractJMTAction) element;
			action.setEnabled(enable);
		}
	}

	/**
	 * Populates this toolbar reading all properties from specified actions. null values
	 * are used to add a separator.
	 * @param abstractJMTactions a collection of AbstractJMTAction with all actions to be added
	 * @return an ArrayList with all added objects (each element is instanceof AbstractButton)
	 * @see AbstractJMTAction
	 */
	public ArrayList<JComponent> populateToolbar(Collection<AbstractJMTAction> abstractJMTactions) {
		ArrayList<JComponent> ret = new ArrayList<JComponent>();
		for (AbstractJMTAction abstractJMTAction : abstractJMTactions) {
			AbstractJMTAction action = abstractJMTAction;
			if (action == null) {
				addSeparator();
			} else {
				ret.add(addGenericButton(action));
			}
		}
		return ret;
	}

	/**
	 * Populates this toolbar reading all properties from specified actions. null values
	 * are used to add a separator.
	 * @param abstractJMTactions an array of AbstractJMTAction with all actions to be added
	 * @return an ArrayList with all added objects (each element is instanceof AbstractButton)
	 * @see AbstractJMTAction
	 */
	public ArrayList<AbstractButton> populateToolbar(AbstractJMTAction[] abstractJMTactions) {
		ArrayList<AbstractButton> ret = new ArrayList<AbstractButton>();
		for (AbstractJMTAction action : abstractJMTactions) {
			if (action == null) {
				addSeparator();
			} else {
				ret.add(addGenericButton(action));
			}
		}
		return ret;
	}

	// --- Inner helper methods and data structures ------------------------------------------------------------------    

	/**
	 * Returns a button group if present, otherwise creates and adds it.
	 * @param group group to be get
	 * @return created group
	 */
	protected JMTButtonGroup getOrCreateGroup(int group) {
		JMTButtonGroup bGroup = getButtonGroup(group);
		if (bGroup == null) {
			bGroup = new JMTButtonGroup();
			buttonGroups.put(new Integer(group), bGroup);
		}
		return bGroup;
	}

	/**
	 * Inner class used to store buttons together with their button group (if any)
	 */
	protected class ButtonWithGroup {
		/** Button. Always set. */
		public AbstractButton button;
		/** Button group. -1 if button has not groups. */
		public int group;
		/** A listener to support action select state. This is stored here to be deregistered on button remove. */
		public SelectedActionButtonChangeListener listener;

		/**
		 * Creates a new ButtonWithGroup element
		 * @param button reference to button
		 * @param group reference to button group or -1 if button has no groups.
		 */
		public ButtonWithGroup(AbstractButton button, int group) {
			this.button = button;
			this.group = group;
		}
	}
}
