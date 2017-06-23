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
import java.awt.event.KeyEvent;

import jmt.gui.common.CommonConstants;
import jmt.gui.common.resources.JMTImageLoader;
import jmt.gui.jmodel.controller.Mediator;

/**

 * @author Federico Granata
 * Date: 5-giu-2003
 * Time: 11.49.31

 */
public class SetInsertState extends AbstractJmodelAction {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private String className;//name of the class that will be instatiated

	/**
	 * Creates an action that sets MouseListener in Insert mode
	 * @param mediator refernce to the mediator of the GUI
	 * @param className the name of the class to be inserted
	 *
	 * Author: Bertoli Marco
	 */
	public SetInsertState(Mediator mediator, String className) {
		super(className, mediator);
		setSelectable(true);
		setGroup(0);
		// Generate default description
		String type = className.substring(0, className.lastIndexOf("Cell"));
		putValue(SHORT_DESCRIPTION, "Insert a new " + CommonConstants.STATION_NAMES.get(type));
		putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_I));
		setIcon(className, JMTImageLoader.getImageLoader());
		this.className = className;
	}

	public void actionPerformed(ActionEvent e) {
		mediator.setInsertState(className);
	}

}
