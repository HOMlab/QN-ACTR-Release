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

package jmt.framework.gui.help;

import java.awt.Component;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

import javax.swing.JLabel;

/**

 * @author alyf (Andrea Conti)
 * Date: 17-set-2003
 * Time: 3.24.59

 */

/**
 * a very basic help system - you register (Component,String) couples and when the mouse enters the component the String is displayed in a JLabel.
 */
public class HoverHelp extends JLabel implements MouseListener {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected Map<Component, String> map;
	protected Stack<String> s;

	/**
	 * Builds a new instance of HoverHelp
	 */
	public HoverHelp() {
		super("");
		map = new HashMap<Component, String>();
		s = new Stack<String>();
		addHelp(this, "This area displays a short help about the objects you move the cursor on");
	}

	/**
	 * Adds help for the specified component
	 * @param comp component to be added
	 * @param help help message
	 */
	public void addHelp(Component comp, String help) {
		if (help == null) {
			return;
		}
		Object v = map.put(comp, help);
		if (v == null) {
			comp.addMouseListener(this);
		}
	}

	/**
	 * Removes help for a specified component
	 * @param comp component to be removed
	 */
	public void removeHelp(Component comp) {
		Object v = map.remove(comp);
		if (v != null) {
			comp.removeMouseListener(this);
		}
	}

	/* (non-Javadoc)
	 * @see java.awt.event.MouseAdapter#mouseEntered(java.awt.event.MouseEvent)
	 */
	public void mouseEntered(MouseEvent e) {
		s.push(this.getText());
		this.setText(map.get(e.getSource()));
	}

	/* (non-Javadoc)
	 * @see java.awt.event.MouseAdapter#mouseExited(java.awt.event.MouseEvent)
	 */
	public void mouseExited(MouseEvent e) {
		if (!s.empty()) {
			this.setText(s.pop());
		}
	}

	public JLabel getHelpLabel() {
		return this;
	}

	/* (non-Javadoc)
	 * @see java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent)
	 */
	public void mouseClicked(MouseEvent e) {
	}

	/* (non-Javadoc)
	 * @see java.awt.event.MouseListener#mousePressed(java.awt.event.MouseEvent)
	 */
	public void mousePressed(MouseEvent e) {
	}

	/* (non-Javadoc)
	 * @see java.awt.event.MouseListener#mouseReleased(java.awt.event.MouseEvent)
	 */
	public void mouseReleased(MouseEvent e) {
	}
}
