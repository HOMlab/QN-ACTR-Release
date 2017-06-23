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

import jmt.framework.gui.listeners.AbstractJMTAction;
import jmt.gui.common.resources.JMTImageLoader;
import jmt.gui.jmodel.controller.Mediator;

/** Defines the Abstract action of this application, it is connected to the
 * mediator which has the responsability to react to it.

 * @author Federico Granata
 * Date: 5-giu-2003
 * Time: 10.48.46

 */
public abstract class AbstractJmodelAction extends AbstractJMTAction {

	/* Conti Andrea
	 Actions and accelerator keys:
	 When you create a new action with an accelerator key you must make sure that the keystroke
	 is not already mapped to some "default" action through the graph's InputMap and ActionMap
	 (take a look at BasicGraphUI.createActionMap). In case it is, you have to remove the
	 mapping (see JmtGraphUI.installKeyboardActions()) or the keystroke will trigger the default
	 action instead of yours (and you'll get a bad headache trying to understand why ;)
	 */

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected Mediator mediator;

	/**
	 * Defines an <code>Action</code> object with a default
	 * description string and default icon.
	 */
	public AbstractJmodelAction(String name, Mediator mediator) {
		this.setName(name);
		this.mediator = mediator;
	}

	/**
	 * Defines an <code>Action</code> object with the specified
	 * description string and a the specified icon.
	 */
	public AbstractJmodelAction(String name, String iconName, Mediator mediator) {
		this(name, mediator);
		this.setIcon(iconName, JMTImageLoader.getImageLoader());
	}

}
