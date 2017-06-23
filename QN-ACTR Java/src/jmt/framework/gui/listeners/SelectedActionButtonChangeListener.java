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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.AbstractButton;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/**
 * <p><b>Name:</b> SelectedActionChangeListener</p> 
 * <p><b>Description:</b> 
 * A property change listener that will reflect the status of an action on a button and vice versa.
 * This is used to implement selected state in actions.
 * </p>
 * <p><b>Date:</b> 31/gen/07
 * <b>Time:</b> 16:59:17</p>
 * @author Bertoli Marco
 * @version 1.0
 */
public class SelectedActionButtonChangeListener implements PropertyChangeListener, ChangeListener {
	private AbstractJMTAction action;
	private AbstractButton button;

	/**
	 * Adds a selected change listener between target action and target button
	 * @param action action to be linked
	 * @param button button to be linked
	 */
	public SelectedActionButtonChangeListener(AbstractJMTAction action, AbstractButton button) {
		this.action = action;
		this.button = button;
		action.addPropertyChangeListener(this);
		button.addChangeListener(this);
	}

	/* (non-Javadoc)
	 * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
	 */
	public void propertyChange(PropertyChangeEvent evt) {
		if (evt.getPropertyName().equals(AbstractJMTAction.PROPERTY_SELECTED)) {
			// Action was changed
			if (button.isSelected() != action.isSelected()) {
				button.setSelected(action.isSelected());
			}
		}
	}

	/* (non-Javadoc)
	 * @see javax.swing.event.ChangeListener#stateChanged(javax.swing.event.ChangeEvent)
	 */
	public void stateChanged(ChangeEvent e) {
		// Button was changed
		if (e.getSource() == button) {
			if (action.isSelected() != button.isSelected()) {
				action.setSelected(button.isSelected());
			}
		}
	}

	/**
	 * Removes this listener from action and button
	 */
	public void remove() {
		action.removePropertyChangeListener(this);
		button.removeChangeListener(this);
	}

}
