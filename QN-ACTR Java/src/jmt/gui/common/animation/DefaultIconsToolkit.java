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

package jmt.gui.common.animation;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;

/**
 * Created by IntelliJ IDEA.
 * User: OrsotronIII
 * Date: 17-feb-2005
 * Time: 16.25.59
 * This class provides constants for rendering of queue net elements.
 */
public class DefaultIconsToolkit extends IconsToolkit {

	@Override
	public Image getJobIcon(Rectangle bounds) {
		int width = 100, height = 100;
		BufferedImage bi = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR);
		Graphics g = bi.getGraphics();
		g.setColor(new Color(0, 0, 0, 50));
		g.fillOval(0, 0, width, height);
		for (int i = 0, monoChannel = 0; i < 10; i++, monoChannel = (int) ((1 - Math.exp(-i * 0.5)) * 255)) {
			g.setColor(new Color(monoChannel, monoChannel, monoChannel, 255));
			int r = (int) Math.pow(i, 1.5), s = (int) (r * 2.9);
			g.fillOval(r, r, width - s, height - s);
		}
		return bi.getScaledInstance(bounds.width, bounds.height, Image.SCALE_SMOOTH);
	}

	@Override
	public Image getStationIcon(String type, Rectangle bounds) {
		int qLength = 60, height = 40, width = 100;
		BufferedImage bi = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR);
		Graphics g = bi.getGraphics();
		for (int i = 0, monoChannel = 0; i < 10; i++, monoChannel = (int) ((1 - Math.exp(-i)) * 50)) {
			g.setColor(new Color(230 - monoChannel, 230 - monoChannel, 230 - monoChannel));
			g.drawPolyline(new int[] { i, i, qLength - i }, new int[] { height - i, i, i }, 3);
			g.fillArc(width - height + i, i, height - 2 * i, height - 2 * i, 45, 180);
			g.setColor(new Color(130 + monoChannel, 130 + monoChannel, 130 + monoChannel));
			g.drawPolyline(new int[] { i, qLength - i, qLength - i }, new int[] { height - i, height - i, i }, 3);
			g.fillArc(width - height + i, i, height - 2 * i, height - 2 * i, 225, 180);
		}
		g.fillRect(5, 5, qLength - 9, height - 9);
		g.fillOval(width - height + 5, 5, height - 10, height - 10);
		return bi.getScaledInstance(bounds.width, bounds.height, Image.SCALE_SMOOTH);
	}

	@Override
	public Image getEdgeIcon(Rectangle bounds, Point[] anglePoints) {
		/*creates background image.*/
		BufferedImage bgImage = new BufferedImage(bounds.width, bounds.height, BufferedImage.TYPE_4BYTE_ABGR);
		Graphics bgGr = bgImage.getGraphics();
		bgGr.setColor(new Color(0, 0, 0, 0));
		bgGr.fillRect(0, 0, bounds.width, bounds.height);
		for (int i = 0; i < anglePoints.length - 1; i++) {
			//Must convert absolute coords to local coords
			int x0 = anglePoints[i].x - bounds.x, y0 = anglePoints[i].y - bounds.y, x1 = anglePoints[i + 1].x - bounds.x, y1 = anglePoints[i + 1].y
					- bounds.y;
			bgGr.setColor(Color.GRAY);
			bgGr.drawLine(x0, y0, x1, y1);
			bgGr.setColor(new Color(30, 30, 30, 30));
			//bgGr.fillRect(Math.min(x0,x1)-1, Math.min(y0,y1)-1, Math.abs(x1-x0)+3, Math.abs(y1-y0)+3);
			bgGr.drawLine(x0 - 1, y0 - 1, x1 - 1, y1 - 1);
		}
		return bgImage;
	}

	@Override
	public Image getBGTileIcon(Rectangle bounds) {
		BufferedImage bi = new BufferedImage(bounds.width, bounds.height, BufferedImage.TYPE_4BYTE_ABGR);
		Graphics g = bi.getGraphics();
		g.setColor(Color.WHITE);
		g.fillRect(0, 0, bounds.width, bounds.height);
		g.setColor(Color.GRAY);
		g.fillRect(0, 0, 1, 1);
		return bi;
	}
}
