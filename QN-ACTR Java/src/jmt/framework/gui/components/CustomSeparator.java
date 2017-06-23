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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;

import javax.swing.JComponent;
import javax.swing.JToolBar;
import javax.swing.SwingConstants;

/**
 * <p>Title: Custom Toolbar Separator</p>
 * <p>Description: Creates a custom separator on a ToolBar. This class is needed as
 * Default JAVA toolbar separator is a whitespace and do not display a separating line.
 * Supports dynamic changing between horizontal and vertical orientation</p>
 * 
 * @author Bertoli Marco
 *         Date: 4-giu-2005
 *         Time: 21.52.53
 */
public class CustomSeparator extends JComponent {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private static final Dimension Horizontal = new Dimension(9, 50);
	private static final Dimension Vertical = new Dimension(50, 9);
	private int orientation;

	/**
	 * Default constructor
	 */
	public CustomSeparator() {
		super();
		setMaximumSize(Horizontal);
		orientation = SwingConstants.HORIZONTAL;
	}

	/**
	 * Override default paint method to draw separating line
	 * @param g Graphics object to be painted
	 */
	@Override
	public void paint(Graphics g) {
		try {
			JToolBar parent = (JToolBar) this.getParent();
			Dimension d = this.getSize();
			g.setColor(Color.gray);
			if (parent.getOrientation() == SwingConstants.HORIZONTAL) {
				if (orientation != SwingConstants.HORIZONTAL) {
					setMaximumSize(Horizontal);
					orientation = SwingConstants.HORIZONTAL;
					parent.revalidate();
				}
				g.drawLine(d.width / 2, 0, d.width / 2, d.height);
			} else {
				if (orientation != SwingConstants.VERTICAL) {
					setMaximumSize(Vertical);
					orientation = SwingConstants.VERTICAL;
					parent.revalidate();
				}
				g.drawLine(0, d.height / 2, d.width, d.height / 2);
			}
		} catch (ClassCastException ex) {
			super.paint(g);
		}
	}
}
