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

package jmt.gui.common.definitions;

import javax.swing.JFrame;

/**
 * <p>Title: Gui Interface</p>
 * <p>Description: This interface provides a pool of methods common to <code>Mediator</code>
 * and <code>JSIMMain</code> to provide a layer of compatibility used by PollerThread.</p>
 * 
 * @author Bertoli Marco
 *         Date: 7-ott-2005
 *         Time: 11.12.07
 */
public interface GuiInterface {
	/**
	 * Shows a panel with an error message
	 * @param message specified error message
	 */
	public void showErrorMessage(String message);

	/**
	 * Shows a panel with catched exception
	 * @param e exception to be shown
	 */
	public void handleException(Exception e);

	/**
	 * Changes simulation action status. This method is called by DispatcherThread.
	 * @param start state for start action
	 * @param pause state for pause action
	 * @param stop state for stop action
	 */
	public void changeSimActionsState(boolean start, boolean pause, boolean stop);

	/**
	 * Sets resultWindow to be shown. This method is used by pollerThread.
	 * @param rsw window to be set as current ResultsWindow
	 */
	public void setResultsWindow(JFrame rsw);

	/**
	 * Shows results window. This method is used by pollerThread.
	 */
	public void showResultsWindow();

	/**
	 * Used to discover if the instance can display simulation animation
	 * @return true if the instance can display simulation animation
	 */
	public boolean isAnimationDisplayable();

	/**
	 * Shows the panel to solve a problem
	 */
	public void showRelatedPanel(int problemType, int problemSubType, Object relatedStation, Object relatedCLass);

	/**
	 * Shows the class panel
	 */
	public void showClassPanel();

	/**
	 * Sets the Parametric Analysis Results Window
	 * @param parw the Parametric Analysis Results Window to be set
	 */
	//public void setPAResultsWindow(PAResultsWindow parw);
	/**
	 * Sets the availability of results in case of parametric analysis
	 * @param avaible true if they are avaible
	 */
	//public void setPAResultsAvaible(boolean avaible);
}
