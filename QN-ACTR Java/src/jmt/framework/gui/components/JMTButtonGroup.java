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

import java.util.Enumeration;

import javax.swing.AbstractButton;
import javax.swing.ButtonGroup;
import javax.swing.JToggleButton;

/**
 * <p><b>Name:</b> JMTButtonGroup</p> 
 * <p><b>Description:</b> 
 * An extension of the buttongroup class that allows unselected buttons and Enable/Disable of all buttons together.
 * </p>
 * <p><b>Date:</b> 31/gen/07
 * <b>Time:</b> 10:39:39</p>
 * @author Bertoli Marco
 * @version 1.0
 */
public class JMTButtonGroup extends ButtonGroup {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	/** Dummy button used to clear selection */
	private JToggleButton dummy;

	/**
	 * Builds a new JMTButton group
	 */
	public JMTButtonGroup() {
		init();
	}

	/**
	 * Initialize this class
	 */
	private void init() {
		dummy = new JToggleButton();
		dummy.setVisible(false);
		add(dummy);
		clearSelection();
	}

	/**
	 * Unselects all buttons in this group
	 */
	public void clearSelection() {
		setSelected(dummy.getModel(), true);
	}

	/**
	 * Enables/disables all buttons of this group
	 * @param value true to enable all buttons, false to disable them.
	 */
	public void setEnabled(boolean value) {
		if (!value) {
			clearSelection();
		}

		Enumeration<AbstractButton> buttons = getElements();
		while (buttons.hasMoreElements()) {
			AbstractButton button = buttons.nextElement();
			if (button != dummy) {
				button.getAction().setEnabled(value);
			}
		}
	}

	/* (non-Javadoc)
	 * @see javax.swing.ButtonGroup#getButtonCount()
	 */
	@Override
	public int getButtonCount() {
		// Removes dummy button from count.
		return super.getButtonCount() - 1;
	}

}
