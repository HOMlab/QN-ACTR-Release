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

import java.net.URL;

import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;

/**
 * <p><b>Name:</b> QuickHTMLViewer</p> 
 * <p><b>Description:</b> 
 * A simple HTML viewer. If a link is in the same relative path, it will be opened here, otherwise
 * will be opened in system browser.
 * </p>
 * <p><b>Date:</b> 30/gen/07
 * <b>Time:</b> 09:37:41</p>
 * @author Bertoli Marco
 * @version 1.0
 */
public class QuickHTMLViewer extends JMTFrame {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**creates a new instance of this viewer, given the document's url, and sets up
	 * visualization.
	 * @param url: url of document that must be displayed.
	 */
	public QuickHTMLViewer(URL url) {
		this.centerWindow(640, 480);
		HtmlPanel html = new HtmlPanel(url);
		html.setAntiAliasing(true);
		JScrollPane jsp = new JScrollPane(html, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		this.getContentPane().add(jsp);
	}

	/**creates a new instance of this viewer, given the document's url, and sets up
	 * visualization.
	 * @param url: url of document that must be displayed.
	 * @param title: title of the window.
	 */
	public QuickHTMLViewer(URL url, String title) {
		this(url);
		this.setTitle(title);
	}
}
