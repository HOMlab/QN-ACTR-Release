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

package jmt.framework.gui.wizard;

import javax.swing.JPanel;

/**

 * @author alyf (Andrea Conti)
 * Date: 11-set-2003
 * Time: 17.38.24

 */

/**
 * Some things all WizardPanels have in common.
 */
public abstract class WizardPanel extends JPanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * a link to the Wizard this WizardPanel is part of
	 */
	protected Wizard parentWizard;

	/**
	 * @return the panel's name
	 */
	@Override
	public abstract String getName();

	/**
	 * @return true if the panel is in a state that allows the wizard to finish
	 */
	public boolean canFinish() {
		return true;
	}

	/**
	 * called by the Wizard before when switching to another panel
	 */
	public void lostFocus() {
	};

	/**
	 * called by the Wizard when the panel becomes active
	 */
	public void gotFocus() {
	}

	/**
	 * called by the Wizard when the user presses the help button
	 */
	public void help() {
	}

	/**
	 * @return true if it is ok for the wizard to go forward. It is ok for the queried panel to pop up a message box
	 * explaining why it cannot go forward.
	 */
	public boolean canGoForward() {
		return true;
	}

	/**
	 * @return true if it is ok for the wizard to go back. It is ok for the queried panel to pop up a message box
	 * explaining why it cannot go back.
	 */
	public boolean canGoBack() {
		return true;
	}

	public Wizard getParentWizard() {
		return parentWizard;
	}

	public void setParentWizard(Wizard parentWizard) {
		this.parentWizard = parentWizard;
	}

}
