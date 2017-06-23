package jmt.gui.jmodel.controller.actions;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import jmt.gui.jmodel.controller.Mediator;

/**
 * A class representing a "redo" action.
 * @author Giuseppe De Cicco & Fabio Granara
 * Date: 22-08-2006
 * 
 */
public class ActionSetRight extends AbstractJmodelAction {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Defines an <code>Action</code> object with a default
	 * description string and default icon.
	 */
	public ActionSetRight(Mediator mediator) {
		super("Adjust the graph", "Order", mediator);
		//Giuseppe De Cicco & Fabio Granara
		putValue(SHORT_DESCRIPTION, "Optimize graph");
		putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_Q));
		setEnabled(false);
	}

	/**
	 * Invoked when an action occurs.
	 */
	boolean repeat = false;

	public void actionPerformed(ActionEvent e) {
		//GDC & FG
		mediator.adjustGraph();
	}
}
