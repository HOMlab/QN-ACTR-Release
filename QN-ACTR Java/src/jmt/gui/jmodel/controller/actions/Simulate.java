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

import javax.swing.JOptionPane;
import javax.swing.KeyStroke;

import jmt.gui.jmodel.controller.Mediator;

/**

 * @author Federico Granata
 * Date: 4-giu-2003
 * Time: 10.53.48

 */
public class Simulate extends AbstractJmodelAction {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Defines an <code>Action</code> object with a default
	 * description string and default icon.
	 */
	public Simulate(Mediator mediator) {
		super("Simulate", "Sim", mediator);
		putValue(SHORT_DESCRIPTION, "solve simulating model");
		putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_S));
		putValue(ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_S, InputEvent.ALT_MASK));
		setEnabled(false);
		
		//JOptionPane.showMessageDialog(null, "Simulate(Mediator mediator) ", "Simulate.java", JOptionPane.INFORMATION_MESSAGE); // CAO
	}

	/**
	 * Invoked when an action occurs.
	 */
	public void actionPerformed(ActionEvent e) {
		// 06/10/03 - Massimo Cattai //////////////////////////////////////////
		mediator.startSimulation();
		// 06/10/03 - end /////////////////////////////////////////////////////
	}
}
