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


public class ZoomOut extends AbstractJmodelAction {

  private static final long serialVersionUID = 1L;

  /**
   * Defines an <code>Action</code> object with a default
   * description string and default icon.
   */
  public ZoomOut(Mediator mediator) {
    super("ZoomOut", mediator);
    this.setTooltipText("Zoom Out");
    setEnabled(false);
  }

  /**
   * Invoked when an action occurs.
   */
  public void actionPerformed(ActionEvent e) {
    mediator.zoomOut();
  }
}
