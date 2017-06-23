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

package jmt.gui.common.panels;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.GridBagLayout;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRootPane;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.SwingConstants;

import jmt.gui.common.resources.JMTImageLoader;

/**
 * <p>Title: Warning ScrollPanel for Tables</p>
 * <p>Description: This class will add scrollbars to a given table and displays a given warning
 * message if the table is empty of if any of specified Vectors is empty.</p>
 * 
 * @author Bertoli Marco
 *         Date: 7-ott-2005
 *         Time: 14.30.24
 */
public class WarningScrollTable extends JRootPane {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected static final int BORDERSIZE = 5;
	protected static final Dimension warningBoxSize = new Dimension(170, 90);
	protected JTable table;
	protected Vector<Vector> vectors;

	/**
	 * Constructs a new WarningScrollTable, given a table and a warning message to display if the
	 * table is empty
	 * @param table table to be shown in the panel
	 * @param warningMsg message to be shown if the table is empty
	 */
	public WarningScrollTable(JTable table, String warningMsg) {
		this.table = table;
		// Uses GlassPane to show overlay warning message
		getContentPane().add(new JScrollPane(table));
		setGlassPane(createWarningPanel(warningMsg));
	}

	/**
	 * Constructs a new WarningScrollTable, given a component and a warning message to display if the
	 * specified vectors are empty. This implementation does not add scrollBars despite the name
	 * of this class.
	 * @param component component to be shown in the panel
	 * @param warningMsg message to be shown if vectors are empty
	 */
	public WarningScrollTable(JComponent component, String warningMsg) {
		// Uses GlassPane to show overlay warning message
		getContentPane().add(component);
		setGlassPane(createWarningPanel(warningMsg));
	}

	/**
	 * Adds a new Vector tobe check. If this methodis used behaviour of WarningScrollTable will
	 * change and warning message will be shown only if any of input vectors is empty
	 * @param v input vector to be checked
	 */
	public void addCheckVector(Vector v) {
		if (vectors == null) {
			vectors = new Vector<Vector>();
		}
		vectors.add(v);
	}

	/**
	 * Cleans all check vectors
	 */
	public void clearCheckVectors() {
		vectors = null;
	}

	/**
	 * Override default paint method
	 * @param g graphic object
	 */
	@Override
	public void paint(Graphics g) {
		if (vectors == null) {
			if (table != null) {
				getGlassPane().setVisible(table.getRowCount() == 0 || table.getColumnCount() == 0);
			}
		} else {
			getGlassPane().setVisible(checkEmptyVectors());
		}
		super.paint(g);
	}

	/**
	 * Tells if any vector added with <code>addCheckVector(Vector v)</code> method is empty
	 * @return true iff any vector is empty
	 * @see this.addCheckVector(Vector)
	 */
	protected boolean checkEmptyVectors() {
		boolean res = false;
		for (int i = 0; i < vectors.size(); i++) {
			if (vectors.get(i).size() == 0) {
				res = true;
				break;
			}
		}

		return res;
	}

	/**
	 * Creates the panel to be shown when the table is empty
	 * @param msg message to be shown on the panel
	 * @return created warning panel
	 */
	protected JPanel createWarningPanel(String msg) {
		JPanel warning = new JPanel(new GridBagLayout());
		JPanel innerPanel = new JPanel(new BorderLayout());
		// Adds image
		JLabel icon = new JLabel("");
		icon.setIcon(JMTImageLoader.loadImage("Triangle"));
		icon.setHorizontalAlignment(SwingConstants.CENTER);
		icon.setBorder(BorderFactory.createEmptyBorder(BORDERSIZE, BORDERSIZE, BORDERSIZE, BORDERSIZE));
		innerPanel.add(icon, BorderLayout.NORTH);
		// Adds Text Area
		JTextArea text = new JTextArea();
		text.setEditable(false);
		text.setWrapStyleWord(true);
		text.setLineWrap(true);
		text.setText(msg);
		text.setBorder(BorderFactory.createEmptyBorder(BORDERSIZE, BORDERSIZE, BORDERSIZE, BORDERSIZE));
		text.setBackground(icon.getBackground());
		innerPanel.add(text, BorderLayout.CENTER);
		innerPanel.setBorder(BorderFactory.createEtchedBorder());
		innerPanel.setPreferredSize(warningBoxSize);
		warning.add(innerPanel);
		return warning;
	}
}
