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
package jmt.framework.gui.components;

import java.awt.Dialog;
import java.awt.Frame;
import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.WindowConstants;

/**
 * <p><b>Name:</b> JMTDialog</p> 
 * <p><b>Description:</b> 
 * A generic dialog with some enhanchments to be used by JMT
 * </p>
 * <p><b>Date:</b> 24/gen/07
 * <b>Time:</b> 17:45:51</p>
 * @author Bertoli Marco
 * @version 1.0
 */
public class JMTDialog extends JDialog {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/** 
	 * Auto manage closing operation
	 * @see #canBeClosed() to check if window will be closed
	 * @see #doClose() to implement a custom behaviour before window closing
	 */
	public static final int AUTO_MANAGE_ON_CLOSE = 255;

	// Window adapter used for automatic window management
	private WindowAdapter adapter;

	/**
	 * Builds a non-modal JMTDialog
	 */
	public JMTDialog() {
		super();
		init();
	}

	/**
	 * Builds a new JMTDialog
	 * @param owner owner frame (can be null if modal is false)
	 * @param modal true if dialog is modal (ie blocks owner frame)
	 */
	public JMTDialog(Frame owner, boolean modal) {
		super(owner, modal);
		init();
	}

	/**
	 * Builds a new JMTDialog
	 * @param owner owner dialog (can be null if modal is false)
	 * @param modal true if dialog is modal (ie blocks owner frame)
	 */
	public JMTDialog(Dialog owner, boolean modal) {
		super(owner, modal);
		init();
	}

	/**
	 * Initialize this JMTDialog
	 */
	private void init() {
		adapter = new WindowAdapter() {
			/* (non-Javadoc)
			 * @see java.awt.event.WindowAdapter#windowClosing(java.awt.event.WindowEvent)
			 */
			@Override
			public void windowClosing(WindowEvent e) {
				close();
			}
		};
		// Auto manage as default
		this.setDefaultCloseOperation(AUTO_MANAGE_ON_CLOSE);
	}

	/**
	 * @return true iff this frame can be closed. Please override this method to
	 * create save on exit behaviours
	 */
	public boolean canBeClosed() {
		return true;
	}

	/**
	 * Centers this window on the screen
	 */
	public void centerWindow() {
		Toolkit tk = Toolkit.getDefaultToolkit();
		//gets dimensions of the screen to center window.
		int xOffset = ((int) tk.getScreenSize().getWidth() - getWidth()) / 2, yOffset = ((int) tk.getScreenSize().getHeight() - getHeight()) / 2;

		setBounds(xOffset, yOffset, this.getWidth(), this.getHeight());
	}

	/**
	 * Puts this window in screen lower right corner
	 */
	public void moveToLowerRightCorner() {
		Toolkit tk = Toolkit.getDefaultToolkit();
		//gets dimensions of the screen to center window.
		int xOffset = ((int) tk.getScreenSize().getWidth() - getWidth()), yOffset = ((int) tk.getScreenSize().getHeight() - getHeight());

		setBounds(xOffset, yOffset, this.getWidth(), this.getHeight());
	}

	/**
	 * Sets size of this window and centers it on the page
	 * @param width width of the window
	 * @param height height of the window
	 */
	public void centerWindow(int width, int height) {
		centerWindowWithOffset(width, height, 0, 0);
	}

	/**
	 * Sets size of this window and centers it on the page taking into account
	 * the specified offset from the center
	 * @param width width of the window
	 * @param height height of the window
	 * @param xOffsetFromCenter the offset from the center on the x coordinate
	 * @param yOffsetFromCenter the offset from the center on the y coordinate
	 */
	public void centerWindowWithOffset(int width, int height, int xOffsetFromCenter, int yOffsetFromCenter) {
		Toolkit tk = Toolkit.getDefaultToolkit();
		//gets dimensions of the screen to center window.
		int xOffset = ((int) tk.getScreenSize().getWidth() - width) / 2, yOffset = ((int) tk.getScreenSize().getHeight() - height) / 2;

		//add the offset from the center
		xOffset += xOffsetFromCenter;
		yOffset += yOffsetFromCenter;

		setBounds(xOffset, yOffset, width, height);
	}

	/**
	 * Closes this window. (after checking if window can be closed).
	 * @return true if window was closed, false otherwise
	 * @see #canBeClosed() to check if window will be closed
	 * @see #doClose() to implement a custom behaviour before window closing
	 */
	public final boolean close() {
		if (canBeClosed()) {
			doClose();
			this.dispose();
			return true;
		}
		return false;
	}

	/**
	 * Override this method to implement a custom behaviour before windows dispose
	 * @see #canBeClosed()
	 */
	protected void doClose() {
	}

	/**
	 * Sets default operation to be performed when whif window is closed.
	 * @see WindowConstants#DISPOSE_ON_CLOSE
	 * @see WindowConstants#DO_NOTHING_ON_CLOSE
	 * @see JFrame#EXIT_ON_CLOSE
	 * @see JMTFrame#AUTO_MANAGE_ON_CLOSE
	 */
	@Override
	public void setDefaultCloseOperation(int operation) {
		if (operation == JMTFrame.AUTO_MANAGE_ON_CLOSE) {
			super.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
			this.addWindowListener(adapter);
		} else {
			this.removeWindowListener(adapter);
			super.setDefaultCloseOperation(operation);
		}
	}
}
