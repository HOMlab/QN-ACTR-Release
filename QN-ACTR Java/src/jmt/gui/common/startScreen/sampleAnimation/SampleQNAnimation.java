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

package jmt.gui.common.startScreen.sampleAnimation;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.awt.image.ImageObserver;

import jmt.gui.common.animation.Animator;
import jmt.gui.common.animation.DefaultIconsToolkit;
import jmt.gui.common.animation.EdgeAnimation;
import jmt.gui.common.animation.JobAnimation;
import jmt.gui.common.animation.JobPath;
import jmt.gui.common.animation.QueueNetAnimation;
import jmt.gui.common.animation.RandomPath;
import jmt.gui.common.animation.StationAnimation;

/**
 * Created by IntelliJ IDEA.
 * User: OrsotronIII
 * Date: 2-mar-2005
 * Time: 19.02.36
 * To change this template use Options | File Templates.
 */
public class SampleQNAnimation extends QueueNetAnimation {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	//Thread controller that repaints animation.
	Animator controller;

	public SampleQNAnimation() {
		super(new SampleIconsToolkit());
		setBounds(new Rectangle(198, 98));
		prepareQueueNet();
	}

	private void prepareQueueNet() {
		int x = 16, y = -3;
		Point[][] linkPts = {
				{ new Point(70 + x, 60 + y), new Point(90 + x, 60 + y), new Point(90 + x, 75 + y), new Point(100 + x, 75 + y) },
				{ new Point(70 + x, 60 + y), new Point(90 + x, 60 + y), new Point(90 + x, 40 + y), new Point(100 + x, 40 + y) },
				{ new Point(140 + x, 40 + y), new Point(150 + x, 40 + y), new Point(150 + x, 95 + y), new Point(15 + x, 95 + y),
						new Point(15 + x, 60 + y), new Point(30 + x, 60 + y) },
				{ new Point(140 + x, 75 + y), new Point(150 + x, 75 + y), new Point(150 + x, 95 + y), new Point(15 + x, 95 + y),
						new Point(15 + x, 60 + y), new Point(30 + x, 60 + y) },
				{ new Point(70 + x, 60 + y), new Point(80 + x, 60 + y), new Point(80 + x, 35 + y), new Point(20 + x, 35 + y),
						new Point(20 + x, 60 + y), new Point(30 + x, 60 + y) } };
		ColoredStationAnimation[] stations = { new ColoredStationAnimation(new Point(120 + x, 40 + y), 500),
				new ColoredStationAnimation(new Point(50 + x, 60 + y), 100), new ColoredStationAnimation(new Point(120 + x, 75 + y), 500) };
		for (ColoredStationAnimation station : stations) {
			addStation(station);
		}
		EdgeAnimation[] links = new EdgeAnimation[linkPts.length];
		for (int i = 0; i < linkPts.length; i++) {
			links[i] = new EdgeAnimation(linkPts[i], 20);
		}
		ColoredStationAnimation[] sourceStations = { stations[1], stations[1], stations[0], stations[2], stations[1] };
		ColoredStationAnimation[] targetStations = { stations[2], stations[0], stations[1], stations[1], stations[1] };
		for (int i = 0; i < links.length; i++) {
			addEdge(links[i], sourceStations[i], targetStations[i]);
		}
		for (int i = 0; i < 10; i++) {
			addJob(new JobAnimation(0.075, new JobPath[] { new RandomPath(this, stations[i % stations.length]) }, new Rectangle(6, 6),
					new SampleIconsToolkit()), stations[i % stations.length]);
		}
		controller = new Animator(30, this);
	}

	public void start() {
		controller.start();
	}

	public void stop() {
		controller.terminate();
	}

}

class ColoredStationAnimation extends StationAnimation {

	private static Color[] colors = { new Color(255, 0, 0, 150), new Color(0, 255, 0, 150), new Color(0, 0, 0, 150), new Color(255, 255, 0, 150),
			new Color(0, 0, 255, 150) };

	private double cpuPhase = Math.random() * 360, queuePhase = Math.random() * 16;
	private int colorPhase = (int) (Math.random() * colors.length), jobLength = 6;

	private Image overlayedShading = null;

	public ColoredStationAnimation(Point location, long residenceTime) {
		super("server", new Rectangle(location.x - 20, location.y - 10, 40, 20), residenceTime);
		createOverlayImage();
	}

	@Override
	public void paint(Graphics g, ImageObserver io) {
		createQueue(g);
		createCPU(g);
		g.drawImage(this.overlayedShading, getBounds().x, getBounds().y, io);
	}

	private void createOverlayImage() {
		int height = getBounds().height * 2, width = getBounds().width * 2, qLength = width - (height * 4) / 5, qHeight = (height * 4) / 5, yOffs = (height - qHeight) / 2;
		overlayedShading = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR);
		Color[] shades = new Color[7], lights = new Color[7];
		for (int i = 0; i < shades.length; i++) {
			shades[i] = new Color(0, 0, 0, 160 - (i * 160) / shades.length);
			lights[i] = new Color(255, 255, 255, 200 - (i * 200) / lights.length);
		}
		Graphics g = overlayedShading.getGraphics();
		for (int i = 0; i < shades.length; i++) {
			g.setColor(lights[i]);
			g.drawPolyline(new int[] { i, i, qLength - i }, new int[] { height - i - yOffs, i + yOffs, i + yOffs }, 3);
			g.drawArc(width - height + i, i, height - 2 * i, height - 2 * i, 45 + 4 * i, 180 - 8 * i);
			g.setColor(shades[i]);
			g.drawPolyline(new int[] { i, qLength - i, qLength - i }, new int[] { height - i - yOffs, height - i - yOffs, i + yOffs }, 2);
			g.drawArc(width - height + i, i, height - 2 * i, height - 2 * i, 225 + 4 * i, 180 - 8 * i);
		}
		overlayedShading = overlayedShading.getScaledInstance(getBounds().width, getBounds().height, Image.SCALE_SMOOTH);
	}

	private void createCPU(Graphics g) {
		cpuPhase = (cpuPhase - 6) % 360;
		int width = getBounds().width, height = getBounds().height, x = getBounds().x, y = getBounds().y, turn = colors.length;
		g.setColor(Color.WHITE);
		g.fillOval(width - height + x + 1, y + 1, height - 2, height - 2);
		for (int i = 0; i < turn; i++) {
			g.setColor(colors[i % colors.length]);
			g.fillArc(width - height + x, y, height, height, (int) cpuPhase + (i * 360) / turn, 360 / turn);
		}
	}

	private void createQueue(Graphics g) {
		queuePhase = queuePhase + 1;
		if (queuePhase > jobLength) {
			queuePhase = queuePhase - jobLength;
			colorPhase = (colorPhase + colors.length - 1) % colors.length;
		}
		int width = getBounds().width, height = getBounds().height, x = getBounds().x, y = getBounds().y, turn = (width - (height * 4) / 5), qHeight = (height * 4) / 5, yOffs = (height - qHeight) / 2;
		for (int i = 0, col = colorPhase, offsX = (int) queuePhase; offsX <= turn + queuePhase; i++, col = (col + 1) % colors.length, offsX += jobLength) {
			g.setColor(colors[col]);
			int offs = offsX - jobLength;
			if (i == 0) {
				offs = offsX - (int) queuePhase;
			}
			g.fillRect(x + offs, y + yOffs, offsX - offs, qHeight);
		}
	}
}

class SampleIconsToolkit extends DefaultIconsToolkit {

	private static Color[] colors = { new Color(255, 0, 0, 150), new Color(255, 255, 0, 150), new Color(0, 0, 255, 150),
			new Color(100, 100, 100, 150), new Color(0, 255, 0, 150) };

	@Override
	public Image getJobIcon(Rectangle bounds) {
		BufferedImage bi = new BufferedImage(100, 100, BufferedImage.TYPE_4BYTE_ABGR);
		Graphics jig = bi.getGraphics();
		Color col = colors[(int) (colors.length * Math.random())];
		int red = (255 - col.getRed()) * 2 / 5 + col.getRed(), green = (255 - col.getGreen()) * 2 / 5 + col.getGreen(), blue = (255 - col.getBlue())
				* 2 / 5 + col.getBlue();
		Color[] colGradient = new Color[10];
		//Build a color gradient from black to selected color
		for (int i = 0; i < colGradient.length; i++) {
			colGradient[i] = new Color(channelGradient(i, colGradient.length, red), channelGradient(i, colGradient.length, green), channelGradient(i,
					colGradient.length, blue));
		}
		for (int i = 0; i < colGradient.length; i++) {
			jig.setColor(colGradient[i]);
			jig.fillOval(i * 3, i * 3, 100 - (i * 9), 100 - (i * 9));
		}
		return bi.getScaledInstance(bounds.width, bounds.height, Image.SCALE_SMOOTH);
	}

	private int channelGradient(int x, int rangeX, int rangeY) {
		return (int) (rangeY * Math.pow((double) (x) / rangeX, 0.44));
	}

	@Override
	public Image getBGTileIcon(Rectangle bounds) {
		BufferedImage bi = new BufferedImage(bounds.width, bounds.height, BufferedImage.TYPE_4BYTE_ABGR);
		Graphics g = bi.getGraphics();
		g.setColor(new Color(150, 150, 150, 200));
		g.fillRect(0, 0, bounds.width, bounds.height);
		return bi;
	}

}
