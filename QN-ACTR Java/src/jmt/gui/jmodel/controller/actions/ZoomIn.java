/**
 * 2013 QN-Java project file
 * 
 */

package jmt.gui.jmodel.controller.actions;

import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;

import javax.swing.KeyStroke;

import jmt.gui.jmodel.controller.Mediator;


public class ZoomIn extends AbstractJmodelAction {

  private static final long serialVersionUID = 1L;

  /**
   * Defines an <code>Action</code> object with a default
   * description string and default icon.
   */
  public ZoomIn(Mediator mediator) {
    super("ZoomIn", mediator);
    this.setTooltipText("Zoom In");
    setEnabled(false);
  }

  /**
   * Invoked when an action occurs.
   */
  public void actionPerformed(ActionEvent e) {
    mediator.zoomIn();
  }
}
