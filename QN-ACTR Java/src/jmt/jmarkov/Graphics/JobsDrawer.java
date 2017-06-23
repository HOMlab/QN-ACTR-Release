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

/*
 * Created on 3-apr-2004 by Ernesto
 *
 */
package jmt.jmarkov.Graphics;

import java.awt.Color;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.LayoutManager;
import java.awt.RenderingHints;
import java.awt.geom.Area;
import java.awt.geom.GeneralPath;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;

import javax.swing.JPanel;

import jmt.jmarkov.Graphics.constants.DrawConstrains;
import jmt.jmarkov.Graphics.constants.DrawNormal;
import jmt.jmarkov.utils.Formatter;

/**
 * MMQueues
 * --------------------------------------
 * 3-apr-2004 - Graphics/JobsDrawer.java
 * 
 * @author Ernesto
 */
public class JobsDrawer extends JPanel implements Notifier/*, Runnable */{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private int donejobs = 0, //number of jobs arrived to the system
			totjobs = 0, //when limited this is number of limited jobs o/w it is 0 
			panelW = 100, //panel width
			panelH = 100; //panel height
	private DrawConstrains dCst;

	/**
	 * 
	 */
	public JobsDrawer() {
		super();
		dCst = new DrawNormal();
	}

	/**
	 * @param isDoubleBuffered
	 */
	public JobsDrawer(boolean isDoubleBuffered) {
		super(isDoubleBuffered);
		dCst = new DrawNormal();
	}

	/**
	 * @param layout
	 */
	public JobsDrawer(LayoutManager layout) {
		super(layout);
		dCst = new DrawNormal();
	}

	/**
	 * @param layout
	 * @param isDoubleBuffered
	 */
	public JobsDrawer(LayoutManager layout, boolean isDoubleBuffered) {
		super(layout, isDoubleBuffered);
		dCst = new DrawNormal();
	}

	public void setTotalJobs(int totjobs) {
		reset();
		this.totjobs = totjobs;
		this.repaint();
	}

	private void drawJobsRemaining(int donejobs, int totjobs, float x, float y, float w, float h, Graphics2D g2d) {
		Area a;
		if (totjobs != 0) {
			float percent = (float) donejobs / (float) totjobs;
			Color col1 = Color.RED;
			Color col2 = Color.GREEN;
			Line2D jline, tjline;
			Rectangle2D jR = new Rectangle2D.Float((x + percent * w), y, w, h);
			GeneralPath tjpath = new GeneralPath();

			//total jobs 
			tjpath.moveTo(x, y);
			tjpath.lineTo(x + w, y + h);
			tjpath.lineTo(x, y + h);
			tjpath.closePath();
			GradientPaint gp = new GradientPaint(x, y, Color.GRAY, x + w, y, Color.LIGHT_GRAY, false);
			g2d.setPaint(gp);
			g2d.fill(tjpath);

			//drawing remaining job triangle 
			a = new Area(tjpath);
			a.intersect(new Area(jR));
			gp = new GradientPaint(x, y, col1, x + w, y, col2, false);
			g2d.setPaint(gp);
			g2d.fill(a);
			g2d.setPaint(Color.BLACK);
			g2d.draw(tjpath);

			//legend:
			for (int i = 0; i < 3; i++) {
				tjline = new Line2D.Float(x + w * i / 2, y + h, x + w * i / 2, y + h + h / 5);
				drawCenteredText("" + (int) (totjobs * (i / 2.0)), Color.BLACK, x + w * i / 2, y + 1.25 * h, g2d, true);

			}
		} else {
			drawCenteredText("total customer arrived: " + Formatter.formatNumber(donejobs, 0), Color.BLACK, this.getWidth() / 2.0,
					this.getHeight() / 2.0, g2d, true);
		}
	}

	public void changeDrawSettings(DrawConstrains dCst) {
		this.dCst = dCst;
		panelH = this.getHeight();
		panelW = this.getWidth();
	}

	private Rectangle2D drawCenteredText(String s, Color c, double centerX, double centerY, Graphics2D g2d, boolean draw) {
		Rectangle2D txtBounds;
		double x, y;
		double gap = dCst.getElementsGap();
		g2d.setFont(dCst.getFont());
		txtBounds = dCst.getFont().getStringBounds(s, g2d.getFontRenderContext());
		x = centerX - txtBounds.getWidth() / 2.0;
		y = centerY - txtBounds.getY() - txtBounds.getHeight() / 2;
		txtBounds.setRect(x - gap, y - txtBounds.getHeight() / 2.0 - gap, txtBounds.getWidth() + 2 * gap, txtBounds.getHeight() + 2 * gap);
		Color ctmp = g2d.getColor();
		g2d.setColor(c);
		if (draw) {
			g2d.drawString(s, (float) x, (float) y);
		}
		g2d.setColor(ctmp);
		return txtBounds;
	}

	@Override
	public void paint(Graphics g) {
		Graphics2D g2d = (Graphics2D) g;
		g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		g2d.clearRect(0, 0, this.getWidth(), this.getHeight());
		g2d.setStroke(dCst.getDrawStroke());
		changeDrawSettings(dCst);

		//disegna jobs
		drawJobsRemaining(donejobs, totjobs, panelW * 0.15f, 10.0f, panelW * 0.7f, panelH / 2.0f, g2d);
	}

	public void enterProcessor(int jobId, int processorId, double time, double executionTime) {
		//JobsDrawer: use only enterQueue and jobLost
	}

	public void enterQueue(int jobId, double time) {
		donejobs++;
		this.repaint();
	}

	public void exitProcessor(int jobId, int processorId, double time) {
		//JobsDrawer: use only enterQueue and jobLost
	}

	public void exitQueue(int jobId, double time) {
		//JobsDrawer: use only enterQueue and jobLost
	}

	public void exitSystem(int jobId, int processorId, double enterQueueTime, double enterCpuTime, double exitSystemTime) {
		//JobsDrawer: use only enterQueue and jobLost		
	}

	public void jobLost(int jobId, double time) {
		donejobs++;
		this.repaint();
	}

	public void reset() {
		donejobs = 0;
		totjobs = 0;
		this.repaint();
	}

	public void updateProcessor(int jobId, int processorId, double remainingTime, double time) {
		//JobsDrawer: use only enterQueue and jobLost		
	}

	public void updateQueue(int jobId, double time) {
		//JobsDrawer: use only enterQueue and jobLost		
	}

}
