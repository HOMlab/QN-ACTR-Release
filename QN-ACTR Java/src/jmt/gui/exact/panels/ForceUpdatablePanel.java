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

package jmt.gui.exact.panels;

/**
 * Created by IntelliJ IDEA.
 * User: OrsotronIII
 * Date: 8-mar-2005
 * Time: 23.25.50
 * This interface enables forcing of update of all data contained into a panel. This can be useful
 * to implement a quick transition from a panel to another. Implementing classes must assure perfect
 * coherence between application data and related image of them contained into the classes themselves.
 * This must be assured at the and of each of this interface's methods' call.
 */
public interface ForceUpdatablePanel {

	/**This method force a stream of data from application to GUI panel. This grants application
	 * user to be working on perfectly updated data as far as <code>retrieveData()</code> was called.*/
	public void retrieveData();

	/**This method force a stream of data from GUI panel to application. This must grant other GUI
	 * panels to be working on the most recently updated version of the data this GUI panel was working
	 * on, so that when other implementing classes call retrieveData() they will have coherent data with
	 * this class.*/
	public void commitData();

}
