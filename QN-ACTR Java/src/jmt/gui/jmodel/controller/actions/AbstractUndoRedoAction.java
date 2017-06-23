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

import java.util.Vector;

import jmt.gui.jmodel.controller.Mediator;

/**

 * @author alyf (Andrea Conti)
 * Date: 3-set-2003
 * Time: 10.01.01

 */

/**
 * An abstract class grouping functionalities common to Undo and Redo action.
 * These kind of action are a bit tricky because we want their displayed name to contain a
 * description of the edit that will be redone. A way to do this is mantaining a list
 * of all created instances and scanning through it at every update.<br>
 * That said, the main function of this class is to provide a means to update the state of all
 * Undo/redo actions through its static method <code>updateAll()</code>
 *
 */
public abstract class AbstractUndoRedoAction extends AbstractJmodelAction {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected static Vector<AbstractUndoRedoAction> instances = new Vector<AbstractUndoRedoAction>(); //a list holding all the instances of this action
	protected static int instanceCount = 0; //local copy of instances.size();

	public AbstractUndoRedoAction(String name, Mediator mediator) {
		super(name, mediator);
		instances.add(this);
		instanceCount++;
	}

	/**
	 * Update the state (name & enabled) of this instance.
	 * To be overridden in subclasses
	 */
	public abstract void update();

	/**
	 * Update the state (name & enabled) of all instances
	 */
	public static void updateAll() {
		for (int i = 0; i < instanceCount; i++) {
			instances.get(i).update();
		}
	}

	/**
	 * Removes this action from the update list.
	 * Should be called before deleting all references to an <code>ActionRedo</code>
	 */
	public void dispose() {
		instanceCount--;
		instances.remove(this);
	}

}
