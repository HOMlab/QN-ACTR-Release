package jmt.gui.jmodel.controller.actions;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import jmt.gui.jmodel.controller.Mediator;

/**
 *A class representing the "rotate" action.
 * @author Giuseppe De Cicco & Fabio Granara
 * Date: 22-08-2006
 * 
 */
public class ActionRotate extends AbstractJmodelAction {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Defines an <code>Action</code> object with a default
	 * description string and default icon.
	 */
	public ActionRotate(Mediator mediator) {
		super("Rotate", "Rotate", mediator);
		//Giuseppe De Cicco & Fabio Granara
		putValue(SHORT_DESCRIPTION, "Rotate the component");
		putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_R));
		//putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_R));
		//putValue(ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_X, KeyEvent.CTRL_MASK));
		//end
		setEnabled(false);
	}

	/**
	 * Invoked when an action occurs.
	 */
	public void actionPerformed(ActionEvent e) {
		//GDC & FG
		mediator.rotateComponent(null);

	}
}
