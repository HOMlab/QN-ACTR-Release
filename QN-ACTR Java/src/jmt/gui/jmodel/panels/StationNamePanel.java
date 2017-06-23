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

package jmt.gui.jmodel.panels;

import java.awt.BorderLayout;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.Box;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;

import jmt.gui.jmodel.definitions.JmodelStationDefinition;

/**
 * <p>Title: Station's Name Panel</p>
 * <p>Description: Panel used to change a station's name upon editing its properties</p>
 * 
 * @author Bertoli Marco
 *         Date: 23-ago-2005
 *         Time: 11.31.20
 */
public class StationNamePanel extends JPanel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private JmodelStationDefinition sd;
	private Object key;
	private JTextField name = new JTextField();

	/**
	 * Creates a new Station Name Panel
	 * @param sd station definition data structure
	 * @param key key of search for station to be modified
	 */
	public StationNamePanel(JmodelStationDefinition sd, Object key) {
		this.sd = sd;
		this.key = key;
		init();
	}

	/**
	 * Inits this panel's components and their action listeners
	 */
	private void init() {
		setLayout(new BorderLayout(5, 5));
		setBorder(new TitledBorder(new EtchedBorder(), "Station Name"));
		add(new JLabel("Station Name: "), BorderLayout.WEST);
		name.setText(sd.getStationName(key));
		add(name, BorderLayout.CENTER);
		add(Box.createVerticalStrut(5), BorderLayout.SOUTH);
		inputListener listener = new inputListener();
		name.addKeyListener(listener);
		name.addFocusListener(listener);
	}

	/**
	 * Listener used to change station name (associated to name JTextFields).
	 * Parameters are set when JTextField loses focus or ENTER key is pressed.
	 */
	protected class inputListener implements KeyListener, FocusListener {
		/**
		 * Update station's name
		 */
		protected void updateValues() {
			sd.setStationName(name.getText(), key);

			name.setText(sd.getStationName(key));
		}

		public void focusLost(FocusEvent e) {
			updateValues();
		}

		public void keyPressed(KeyEvent e) {
			if (e.getKeyCode() == KeyEvent.VK_ENTER) {
				updateValues();
				e.consume();
			}
		}

		public void focusGained(FocusEvent e) {
		}

		public void keyReleased(KeyEvent e) {
		}

		public void keyTyped(KeyEvent e) {
		}
	}
}
