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
package jmt.framework.gui.controller;

import java.awt.Window;
import java.util.ArrayList;

/**
 * <p>Title: Manager</p>
 * <p>Description: This class is used to kill the entire JVM when all applications are
 * terminated. On normal working conditions this is not needed, but some racing issues
 * exists that can cause untermination problems.</p>
 *
 * @author Bertoli Marco
 *         Date: 19-giu-2006
 *         Time: 14.46.35
 */
public class Manager {
	protected static ArrayList<Window> windows = new ArrayList<Window>();

	/**
	 * Adds a new Window to be checked for termination before terminating JVM
	 * @param application application to be checked
	 */
	public static void addWindow(Window application) {
		if (!windows.contains(application)) {
			synchronized (windows) {
				if (!windows.contains(application)) {
					windows.add(application);
				}
			}
		}
	}

	/**
	 * This method has to be called each time a JMT application is terminated. If all applications
	 * are terminated, JVM is killed.
	 * @param application application to be terminated. If application was not disposed,
	 * this method will do 'dispose()' too.
	 */
	public static void exit(Window application) {
		synchronized (windows) {
			// Disposes application window if user didn't do it.
			if (application.isDisplayable()) {
				application.dispose();
			}
			windows.remove(application);
			if (windows.isEmpty()) {
				System.exit(0); // Closes current JVM with no error code.
			}
		}
	}
}
