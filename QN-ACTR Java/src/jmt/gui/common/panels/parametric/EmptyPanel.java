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
package jmt.gui.common.panels.parametric;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;

import javax.swing.JPanel;

/**
 * <p>Title:</p>
 * <p>Description:</p>
 *
 * @author Francesco D'Aquino
 *         Date: 9-mar-2006
 *         Time: 16.14.43
 */
public class EmptyPanel extends ParameterOptionPanel {
	/**
	* 
	*/
	private static final long serialVersionUID = 1L;

	public EmptyPanel() {
		initialize();
	}

	public void initialize() {
		JPanel empty = new JPanel();
		empty.setPreferredSize(new Dimension(800, 600));
		this.setLayout(new BorderLayout());
		this.add(empty, BorderLayout.CENTER);
		title.setTitleColor(Color.LIGHT_GRAY);
		this.setBorder(title);
	}

	public String[] getClassNames() {
		return null;
	}
}
