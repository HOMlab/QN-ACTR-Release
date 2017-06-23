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

package jmt.gui.jwat;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;

public class JavaWatColor {
	public static void main(String[] v) {
		JFrame f = new JFrame();
		f.setSize(800, 600);
		JButton b = new JButton("Colora");
		b.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				p.repaint();
			}
		});
		p = new JPanel() {
			/**
			 * 
			 */
			private static final long serialVersionUID = 1L;

			@Override
			public void paintComponent(Graphics g) {
				g.setColor(Color.WHITE);
				g.fillRect(0, 0, 1000, 1000);
				for (int i = 0; i < tot; i++) {
					g.setColor(list[i]);
					g.fillRect(0, 0 + i * 11, 800, 10);
				}
			}
		};
		f.getContentPane().add(p);
		p.add(b);
		f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		f.show();
	}

	public static Color getColor(int v) {
		return list[v];
	}

	public static String getName(int v) {
		return name[v];
	}

	static {
		list = new Color[50];
		name = new String[50];
		makeColor();
	}

	private static void makeColor() {
		/* Primi 9 Colori */
		list[0] = Color.RED;
		name[0] = "ROSSO";
		list[1] = Color.BLUE;
		name[1] = "BLU";
		list[2] = Color.MAGENTA;
		name[2] = "MAGENTA";
		list[3] = Color.GREEN;
		name[3] = "VERDE";
		list[4] = Color.BLACK;
		name[4] = "NERO";
		list[5] = Color.YELLOW;
		name[5] = "GIALLO";
		list[6] = Color.PINK;
		name[6] = "ROSA";
		list[7] = Color.CYAN;
		name[7] = "AZZURRO";
		list[8] = Color.ORANGE;
		name[8] = "ARANCIONE";
		list[9] = Color.GRAY;
		name[9] = "GRIGIO";
		list[10] = new Color(155, 0, 0);
		name[10] = "ROSSO SCURO";
		list[11] = new Color(94, 0, 2);
		name[11] = "MARRONE";
		list[12] = new Color(242, 129, 252);
		name[12] = "VIOLA CHIARO";
		list[13] = new Color(129, 69, 135);
		name[13] = "VIOLA SCURO";
		list[14] = new Color(141, 141, 254);
		name[14] = "BLU CHIARO";
		list[15] = new Color(1, 1, 109);
		name[15] = "BLU SCURO";
		list[16] = new Color(3, 124, 146);
		name[16] = "AZZURRO SCURO";
		list[17] = new Color(83, 172, 105);
		name[17] = "VERDINO";
		list[18] = new Color(33, 116, 35);
		name[18] = "VERDE SCURO";
		list[19] = new Color(128, 206, 49);
		name[19] = "VERDACCIO";
		list[20] = new Color(170, 162, 55);
		name[20] = "ORO";
		list[21] = new Color(191, 156, 34);
		name[21] = "OCRA";
		list[22] = new Color(255, 0, 128);
		name[22] = "ROSACCIO";
		list[23] = new Color(198, 50, 50);
		name[23] = "ROSSO N°2";
		list[24] = new Color(99, 55, 137);
		name[24] = "VIOLA SCURO N°2";
		list[25] = new Color(99, 55, 137).brighter();
		name[25] = "ROSSO N°3";
		list[26] = new Color(129, 69, 135).brighter();
		name[26] = "VIOLA SCURO N°2";
		list[27] = new Color(141, 141, 254).darker();
		name[27] = "BLU CHIARO N°2";
		list[28] = new Color(1, 1, 109).brighter().brighter();
		name[28] = "BLU SCURO N°2";
		list[29] = new Color(3, 124, 146).brighter();
		name[29] = "AZZURRO SCURO N°2";
		list[30] = new Color(83, 172, 105).darker();
		name[30] = "VERDINO N°2";
		list[31] = new Color(33, 116, 35).brighter();
		name[31] = "VERDE SCURO N°2";
		list[32] = new Color(128, 206, 49).brighter();
		name[32] = "VERDACCIO N°2";
		list[33] = new Color(170, 162, 55).brighter();
		name[33] = "ORO N°2";
		list[34] = new Color(191, 156, 34).darker();
		name[34] = "OCRA N°2";
		list[35] = new Color(255, 0, 128).brighter();
		name[35] = "ROSACCIO N°2";
		list[36] = new Color(198, 50, 50).darker().darker();
		name[36] = "ROSSO N°4";
		list[37] = new Color(99, 55, 137).brighter().brighter();
		name[37] = "VIOLA SCURO N°3";

	}

	private static JPanel p;
	private static Color[] list;
	private static String[] name;
	private static int tot = 38;
}