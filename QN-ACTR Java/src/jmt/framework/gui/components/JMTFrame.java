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

import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Locale;

import javax.swing.JFrame;
import javax.swing.UIManager;
import javax.swing.WindowConstants;

import jmt.framework.gui.controller.Manager;

/**
 * <p><b>Name:</b> JMTFrame</p> 
 * <p><b>Description:</b> 
 * A generic frame with some enhanchments to be used by JMT.
 * </p>
 * <p><b>Date:</b> 23/gen/07
 * <b>Time:</b> 16:32:34</p>
 * @author Bertoli Marco
 * @version 1.0
 */
public class JMTFrame extends JFrame {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private static final String DEFAULT_LOOK_AND_FEEL = "com.jgoodies.looks.plastic.Plastic3DLookAndFeel";
	private static final Locale DEFAULT_LOCALE = Locale.ENGLISH;
	private static final Locale PLATFORM_DEFAULT_LOCALE = Locale.getDefault();

	/** 
	 * Auto manage closing operation
	 * @see #canBeClosed() to check if window will be closed
	 * @see #doClose() to implement a custom behaviour before window closing
	 */
	public static final int AUTO_MANAGE_ON_CLOSE = 255;

	// Window adapter used for automatic window management
	private WindowAdapter adapter;

	// Sets the default look and feel and locale
	static {
		try {
			UIManager.setLookAndFeel(DEFAULT_LOOK_AND_FEEL);
		} catch (Exception ulafe) {
			ulafe.printStackTrace();
		}

		Locale.setDefault(DEFAULT_LOCALE);
	}
	
	/**
	 * @return the platform default locale, that is different from the application one.
	 */
	public static Locale getPlatformDefaultLocale() {
		return PLATFORM_DEFAULT_LOCALE;
	}

	/**
	 * Builds a JMTFrame and initialize it. Automatic management is disabled.
	 */
	public JMTFrame() {
		this(false);
	}

	/**
	 * @return true iff this frame can be closed. Please override this method to
	 * create save on exit behaviours
	 * @see JMTFrame#AUTO_MANAGE_ON_CLOSE
	 */
	public boolean canBeClosed() {
		return true;
	}

	/**
	 * Builds a new JMTframe
	 * @param autoManage true if this frame must be monitored to tell if JVM has to be terminated
	 */
	public JMTFrame(boolean autoManage) {
		init(autoManage);
	}

	/**
	 * Builds a new JMTframe
	 * @param autoManage true if this frame must be monitored to tell if JVM has to be terminated
	 * @param title title of the window
	 */
	public JMTFrame(boolean autoManage, String title) {
		super(title);
		init(autoManage);
	}

	/**
	 * Initialize JMTFrame
	 */
	private void init(final boolean autoManage) {
		// Supports automatic manage to avoid blocked JVMs without open windows.
		if (autoManage) {
			// Adds support for Manager to avoid blocked JVM without windows.
			Manager.addWindow(this);
		}

		adapter = new WindowAdapter() {
			/* (non-Javadoc)
			 * @see java.awt.event.WindowAdapter#windowClosed(java.awt.event.WindowEvent)
			 */
			@Override
			public void windowClosed(WindowEvent e) {
				if (autoManage) {
					Manager.exit(JMTFrame.this);
				}
			}

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
		Toolkit tk = Toolkit.getDefaultToolkit();
		//gets dimensions of the screen to center window.
		int xOffset = ((int) tk.getScreenSize().getWidth() - width) / 2, yOffset = ((int) tk.getScreenSize().getHeight() - height) / 2;

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
	 * @see JMTFrame#AUTO_MANAGE_ON_CLOSE
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

	public static void main(String[] args) {
		JMTFrame frame = new JMTFrame(true, "test");
		frame.centerWindow(640, 480);
		frame.show();
	}
}
