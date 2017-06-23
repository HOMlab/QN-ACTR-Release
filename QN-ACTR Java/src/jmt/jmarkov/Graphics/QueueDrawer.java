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
 * Created on 16-mar-2004 by Ernesto
 *
 */
package jmt.jmarkov.Graphics;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.Stroke;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.geom.Area;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.Line2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RoundRectangle2D;

import javax.swing.JComponent;

import jmt.jmarkov.Graphics.constants.DrawConstrains;
import jmt.jmarkov.Graphics.constants.DrawNormal;
import jmt.jmarkov.Queues.QueueLogic;
import jmt.jmarkov.Queues.Exceptions.NonErgodicException;

public class QueueDrawer extends JComponent implements Notifier {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private QueueLogic ql;

	private long[] remainingTime, totTime;
	private int nCpu = 1;

	// queue data
	private int jobs = 0, donejobs = 0, totjobs = 0, queueMax = 0,
	// queueLen = 0,
			queueMedia = 0, lostJobs = 0, view = 0;

	private static final int VIEWS = 2;

	// panel settings
	private double panelH = 250, panelW = 400, minH = 100, minW = 400;

	// draw settings
	private DrawConstrains dCst;
	private static double START_GAP, END_GAP, ELEM_HEIGHT, ELEM_WIDTH, ELEMS_GAP, PROC_RAD, STAT_RAD, MORE_JOBS_MAX = 100, LOST_JOBS_MAX = 100;
	private Rectangle2D[] queue;
	private RoundRectangle2D viewB;
	private Ellipse2D processor, occupiedEll;
	private Rectangle2D occupiedRect, moreStatiRect, lostJobsR, txtBounds, tb;
	private Area occupiedArea, jobsRemaining;
	private String moreJobsStr, lostJobsStr;
	private Stroke stroke;
	private Font f;
	private String[] legendaS;
	private Color[] legendaC;

	// color settings
	private Color emptyC = Color.WHITE, queueC = Color.BLUE, busyC = queueC.brighter().brighter(), animC = Color.RED;
	private boolean procUseGradient = true, queueUseGradient = true, gradientF, viewButtonClicked = false;

	private boolean[] processorRunning;
	private int executingJobId;

	public QueueDrawer(QueueLogic ql) {
		super();
		this.ql = ql;
		panelW = this.getWidth();
		panelH = this.getHeight();
		setCpuNumber(1);
		init();
	}

	public QueueDrawer(Dimension panelSize, QueueLogic ql) {
		super();
		this.ql = ql;
		panelW = panelSize.width;
		panelH = panelSize.height;
		setCpuNumber(1);
		init();
	}

	public void updateLogic(QueueLogic ql) {
		this.ql = ql;
	}

	void init() {
		changeDrawSettings(new DrawNormal());
		setColors(Color.white, Color.BLUE, Color.RED, true);
		legendaS = new String[3];
		legendaC = new Color[3];
		legendaS[0] = "current queue";
		legendaC[0] = queueC;
		legendaS[1] = "dropped customer";
		legendaC[1] = Color.BLACK;
		legendaS[2] = "avg.utilization";
		legendaC[2] = busyC;
		this.reset();
		viewB = new RoundRectangle2D.Double();
		this.addMouseListener(new MouseListener() {
			public void mouseClicked(MouseEvent e) {
			}

			public void mouseEntered(MouseEvent e) {
			}

			public void mouseExited(MouseEvent e) {
			}

			public void mousePressed(MouseEvent e) {
				if (viewB.contains(e.getX(), e.getY())) {
					viewButtonClicked = true;
					repaint();
				}
			}

			public void mouseReleased(MouseEvent e) {
				if (viewB.contains(e.getX(), e.getY())) {
					viewButtonClicked = false;
					changeView();
					repaint();
				}
			}
		});
	}

	public void changeDrawSettings(DrawConstrains dCst) {
		this.dCst = dCst;
		resize();
		// assigning the constant of panel
		f = dCst.getFont();
		stroke = dCst.getDrawStroke();
		START_GAP = dCst.getStartingGap();
		END_GAP = dCst.getStartingGap();
		ELEM_HEIGHT = dCst.getElementHeight();
		ELEM_WIDTH = dCst.getElementWidth();
		ELEMS_GAP = dCst.getElementsGap();
		PROC_RAD = dCst.getProcessorRadius();

		// initilazing the queue
		queue = new Rectangle2D.Double[queueLenght()];
		processor = new Ellipse2D.Double();
		// repaint();
	}

	/**
	 * setting the number of jobs in the queue
	 * 
	 * @param s
	 *            number of jobs in the queue
	 */
	public void setMediaJobs(double s) {
		double queueMediaDouble = s;
		queueMedia = (int) queueMediaDouble;
		if (queueMediaDouble - queueMedia > .5) {
			queueMedia++;
		}
		this.repaint();
	}

	public void setMaxJobs(int j) {
		queueMax = j - 1;// q max = maxjob -1
		if (queueMax > 0) {
			MORE_JOBS_MAX = j;
		} else {
			reset();
			MORE_JOBS_MAX = 100;
		}

		this.repaint();
	}

	@Override
	public void paint(Graphics g) {
		Graphics2D g2d = (Graphics2D) g;
		g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

		g2d.setStroke(stroke);
		panelW = this.getWidth();
		panelH = this.getHeight();
		changeDrawSettings(dCst);
		viewB = drawViewButton(10.0, 10.0, g2d, false, false);
		viewB = drawViewButton(panelW - viewB.getWidth() - END_GAP, panelH - viewB.getHeight() - 1, g2d, true, viewButtonClicked);
		START_GAP = START_GAP * 2 + drawLegend(legendaC, legendaS, dCst.getFont(), START_GAP, START_GAP, g2d, false);

		for (int i = 0; i < queueLenght(); i++) {
			if (i < jobs) {
				drawQueueStatus(queueLenght() - i - 1, queueC, Color.black, gradientF, g2d);
			} else {
				drawQueueStatus(queueLenght() - i - 1, emptyC, Color.black, gradientF, g2d);
			}
		}

		drawProcessorView(view, g2d);
		if ((jobs > queueLenght()) && ((queueLenght() < queueMax) || (queueMax == -1))) {
			drawMoreStatus(queueC, emptyC, Color.black, gradientF, g2d);
		}
		drawJobs(g2d);
		if (lostJobs > 0) {
			drawLostJobs(Color.BLACK, Color.WHITE, Color.BLACK, true, g2d);
		}
		int jobsInTheSystem;
		jobsInTheSystem = jobs;
		for (long element : remainingTime) {
			if (element > 0) {
				jobsInTheSystem++;
			}
		}
		txtBounds = drawCenteredText("cust. in the system:" + jobsInTheSystem, queueC, Color.white, panelW / 2.0, panelH - ELEM_HEIGHT, g2d, false,
				false);
		drawCenteredText("cust. in the station:" + jobsInTheSystem, queueC, Color.white, panelW / 2.0, panelH - txtBounds.getHeight() / 2.0, g2d,
				false, true);
		drawLegend(legendaC, legendaS, dCst.getFont(), dCst.getStartingGap(), dCst.getStartingGap(), g2d, true);
	}

	/**
	 * @param view
	 */
	private void drawProcessorView(int view, Graphics2D g2d) {

		if (nCpu == 1) {
			drawProcessor(Color.white, Color.black, gradientF, g2d);
		} else if (nCpu == 2) {
			drawProcessor2(Color.white, Color.black, gradientF, g2d, 0);
			drawProcessor2(Color.white, Color.black, gradientF, g2d, 1);
		} else if (nCpu > 2) {
			drawProcessorMulti(Color.white, Color.black, gradientF, g2d, 0);
			drawProcessorMulti(Color.white, Color.black, gradientF, g2d, 1);

			drawPoint(Color.white, Color.black, gradientF, g2d, 0);
			drawPoint(Color.white, Color.black, gradientF, g2d, 1);
		}
		double U = 1.0;
		try {
			U = ql.utilization();
		} catch (NonErgodicException e) {
		}
		legendaS[0] = "current queue";
		legendaC[0] = queueC;
		if (nCpu > 2) {
			view = 0;
		}
		switch (view) {
			case 1:
				if (nCpu == 1) {
					drawOccupiedPercentage(animC, Color.black, gradientF, g2d);
				} else if (nCpu == 2) {
					drawOccupiedPercentage2(animC, Color.black, gradientF, g2d, 0);
					drawOccupiedPercentage2(animC, Color.black, gradientF, g2d, 1);
				}
				legendaS[2] = "residual time";
				legendaC[2] = animC;
				break;

			default:
				if (nCpu == 1) {
					drawUtilization(U, busyC, Color.black, gradientF, g2d);
				} else if (nCpu == 2) {
					drawUtilization2(U, busyC, Color.black, gradientF, g2d, 0);
					drawUtilization2(U, busyC, Color.black, gradientF, g2d, 1);
				} else if (nCpu > 2) {
					drawUtilizationMulti(U, busyC, Color.black, gradientF, g2d, 0);
					drawUtilizationMulti(U, busyC, Color.black, gradientF, g2d, 1);
				}
				legendaS[2] = "avg.utilization";
				legendaC[2] = busyC.brighter();
				break;
		}
	}

	/**
	 * @param i
	 * @param color
	 * @param g2d
	 */
	private void drawQueueStatus(int i, Color startC, Color border, boolean gradientFill, Graphics2D g2d) {
		double x = getElementXY(i).x, y = getElementXY(i).y;

		queue[i] = new Rectangle2D.Double(x, y, ELEM_WIDTH, ELEM_HEIGHT);
		if (gradientFill) {
			GradientPaint gp = new GradientPaint((float) x, (float) y, startC.brighter(), (float) x, (float) (y + ELEM_HEIGHT), startC.darker(),
					false);
			g2d.setPaint(gp);
		} else {
			g2d.setPaint(startC);
		}
		g2d.fill(queue[i]);
		g2d.setPaint(border);
		g2d.draw(queue[i]);

		// Controllo se questo stato corrisponde alla media dei jobs
		if (i == queueLenght() - queueMedia) {
			// disegno un triangolo sopra lo stato che rappresenta la media
			GeneralPath gp = new GeneralPath(GeneralPath.WIND_EVEN_ODD);
			gp.moveTo((float) x, (float) (y - ELEM_HEIGHT / 2));
			gp.lineTo((float) (x + ELEM_WIDTH), (float) (y - ELEM_HEIGHT / 2));
			gp.lineTo((float) (x + ELEM_WIDTH / 2.0), (float) (y - ELEMS_GAP));
			gp.closePath();
			drawCenteredText("Q", queueC, null, x + ELEM_WIDTH / 2.0, y - ELEM_HEIGHT * 0.7, g2d, false, true);
			g2d.setColor(queueC);
			g2d.fill(gp);
		}

		x += ELEM_WIDTH / 2.0;
		y += ELEM_HEIGHT / 2.0;
		drawCenteredText("" + (queueLenght() - i), readColor(startC), null, x, y, g2d, false, true);
	}

	// private void drawLostJobs2(Color txtColor, boolean gradientFill,
	// Graphics2D g2d) {
	// double x = getProcessorXY().x + PROC_RAD, y = getElementXY(0).y +
	// ELEMS_GAP + ELEM_HEIGHT;
	// Color tmp = g2d.getColor();
	// int lj;
	// if (lostJobs > LOST_JOBS_MAX) {
	// lj = (int) LOST_JOBS_MAX;
	// lostJobsStr = ">" + (int) LOST_JOBS_MAX;
	// } else {
	// lj = lostJobs;
	// lostJobsStr = "" + (int) lj;
	//
	// }
	// g2d.setColor(txtColor);
	// g2d.drawString("lost jobs:" + lostJobsStr, (float) (START_GAP), (float)
	// (panelH));
	// g2d.setColor(tmp);
	// }

	private void drawJobs(Graphics2D g2d) {
		double x = getProcessorXY().x + 2 * PROC_RAD, y = getProcessorXY().y + PROC_RAD * 2 + 4 * ELEMS_GAP;
		Color tmp = g2d.getColor();
		//		System.out.println(nCpu);
		if (nCpu == 1) {
			txtBounds = drawCenteredText("executing cust.:" + donejobs + ", residual time:" + remainingTime[0] + "ms", Color.BLACK, Color.WHITE, x,
					y, g2d, true, false);
			drawCenteredText("executing cust.:" + donejobs + ", residual time:" + remainingTime[0] + "ms", Color.BLACK, Color.WHITE, x
					- txtBounds.getWidth() / 2.0, y, g2d, true, true);
		}
		//		else if(nCpu==2){
		//			txtBounds = drawCenteredText("executing cust.:" + 
		//					donejobs + ", residual time(1):" + remainingTime[0] + "ms"+ ", residual time(2):" + remainingTime[1] + "ms", Color.BLACK, Color.WHITE, x, y, g2d, true, false);
		//			drawCenteredText("executing cust.:" + 
		//					donejobs + ", residual time(1):" + remainingTime[0] + "ms"+ ", residual time(2):" + remainingTime[1] + "ms", Color.BLACK, Color.WHITE, x - txtBounds.getWidth() / 2.0, y, g2d, true, true);
		//
		//		}
		//		else
		//		{
		//			txtBounds = drawCenteredText("executing cust.:" + donejobs , Color.BLACK, Color.WHITE, x, y, g2d, true, false);
		//			drawCenteredText("executing cust.:" + donejobs , Color.BLACK, Color.WHITE, x - txtBounds.getWidth() / 2.0, y, g2d, true, true);
		//		}
		// draw box around text
		g2d.setColor(tmp);
	}

	private void drawMoreStatus(Color filledc, Color emptyc, Color border, boolean gradientFill, Graphics2D g2d) {
		double x = START_GAP + ELEM_WIDTH * 2 + ELEMS_GAP * 2, y = getElementXY(0).y;
		moreStatiRect = new Rectangle2D.Double(x, y, ELEM_WIDTH * 2.0, ELEM_HEIGHT);
		Area moreStatiA = new Area(moreStatiRect);
		int occ = jobs;
		if ((queueMax > 0) && (jobs > queueMax)) {
			occ = queueMax;
		}
		int mult = 1;
		while (occ > (MORE_JOBS_MAX)) {
			occ = (occ / 10);
			mult = mult * 10;
		}
		moreStatiA.subtract(new Area(new Rectangle2D.Double(x, y, ELEM_WIDTH * 2 - ELEM_WIDTH * occ * 2 / MORE_JOBS_MAX, ELEM_HEIGHT)));
		if (gradientFill) {
			GradientPaint gp;
			gp = new GradientPaint((float) x, (float) y, emptyc.brighter(), (float) x, (float) (y + ELEM_HEIGHT), emptyc.darker(), false);
			g2d.setPaint(gp);
			g2d.fill(moreStatiRect);
			gp = new GradientPaint((float) x, (float) y, filledc.brighter(), (float) x, (float) (y + ELEM_HEIGHT), filledc.darker(), false);
			g2d.setPaint(gp);
			g2d.fill(moreStatiA);

		} else {
			g2d.setPaint(filledc);
			g2d.fill(moreStatiA);
			g2d.setPaint(emptyc);
			g2d.fill(moreStatiRect);
		}

		g2d.setPaint(border);
		g2d.draw(moreStatiA);
		g2d.draw(moreStatiRect);
		drawCenteredText("" + occ, readColor(filledc), null, x + ELEM_WIDTH, y + ELEM_HEIGHT / 2.0, g2d, false, true);
		drawCenteredText("x" + mult, filledc, null, x + ELEM_WIDTH, y - ELEM_HEIGHT / 2.0, g2d, false, true);
		//System.out.println(mult);
	}

	private void drawLostJobs(Color filledc, Color emptyc, Color border, boolean gradientFill, Graphics2D g2d) {
		double x = START_GAP, y = getElementXY(0).y;
		moreStatiRect = new Rectangle2D.Double(x, y, ELEM_WIDTH * 2.0, ELEM_HEIGHT);
		Area moreStatiA = new Area(moreStatiRect);
		int occ = lostJobs;
		int mult = 1;
		while (occ > LOST_JOBS_MAX) {
			occ = (occ / 10);
			mult = mult * 10;
		}
		moreStatiA.subtract(new Area(new Rectangle2D.Double(x, y, ELEM_WIDTH * 2 - ELEM_WIDTH * occ * 2 / LOST_JOBS_MAX, ELEM_HEIGHT)));
		if (gradientFill) {
			GradientPaint gp;
			gp = new GradientPaint((float) x, (float) y, emptyc.brighter(), (float) x, (float) (y + ELEM_HEIGHT), emptyc.darker(), false);
			g2d.setPaint(gp);
			g2d.fill(moreStatiRect);
			gp = new GradientPaint((float) x, (float) y, filledc.brighter(), (float) x, (float) (y + ELEM_HEIGHT), filledc.darker(), false);
			g2d.setPaint(gp);
			g2d.fill(moreStatiA);

		} else {
			g2d.setPaint(filledc);
			g2d.fill(moreStatiA);
			g2d.setPaint(emptyc);
			g2d.fill(moreStatiRect);
		}

		g2d.setPaint(border);
		g2d.draw(moreStatiA);
		g2d.draw(moreStatiRect);
		drawCenteredText("" + occ, readColor(filledc), null, x + ELEM_WIDTH, y + ELEM_HEIGHT / 2.0, g2d, false, true);
		drawCenteredText("x" + mult, filledc, null, x + ELEM_WIDTH, y - ELEM_HEIGHT / 2.0, g2d, false, true);
		drawCenteredText("Dropped", filledc, null, x + ELEM_WIDTH, y + 2.5 * ELEM_HEIGHT / 2.0, g2d, false, true);

	}

	private void drawProcessor(Color startC, Color border, boolean gradientFill, Graphics2D g2d) {
		double x = getProcessorXY().x, y = getProcessorXY().y;
		processor.setFrame(x, y, 2 * PROC_RAD, 2 * PROC_RAD);
		if (gradientFill) {
			GradientPaint gp = new GradientPaint((float) x, (float) y, startC.brighter(), (float) x, (float) (y + PROC_RAD * 2), startC.darker(),
					false);
			g2d.setPaint(gp);
		} else {
			g2d.setPaint(startC);
		}
		g2d.fill(processor);
		g2d.setPaint(border);
		g2d.draw(processor);
	}

	private void drawProcessor2(Color startC, Color border, boolean gradientFill, Graphics2D g2d, int cpu) {
		double x = getProcessorXY().x, y = getProcessorXY().y;
		processor.setFrame(x + PROC_RAD / 2, y + cpu * PROC_RAD + ELEMS_GAP * cpu - ELEMS_GAP / 2, PROC_RAD, PROC_RAD);
		if (gradientFill) {
			GradientPaint gp = new GradientPaint((float) x, (float) y, startC.brighter(), (float) x, (float) (y + PROC_RAD * 2), startC.darker(),
					false);
			g2d.setPaint(gp);
		} else {
			g2d.setPaint(startC);
		}
		g2d.fill(processor);
		g2d.setPaint(border);
		g2d.draw(processor);
	}

	private void drawProcessorMulti(Color startC, Color border, boolean gradientFill, Graphics2D g2d, int cpu) {
		double x = getProcessorXY().x, y = getProcessorXY().y;
		processor.setFrame(x + PROC_RAD / 2 + ELEMS_GAP / 2, y + cpu * (PROC_RAD - ELEMS_GAP) + ELEMS_GAP * cpu * 3 - ELEMS_GAP / 2, PROC_RAD
				- ELEMS_GAP, PROC_RAD - ELEMS_GAP);
		if (gradientFill) {
			GradientPaint gp = new GradientPaint((float) x, (float) y, startC.brighter(), (float) x, (float) (y + PROC_RAD * 2), startC.darker(),
					false);
			g2d.setPaint(gp);
		} else {
			g2d.setPaint(startC);
		}
		g2d.fill(processor);
		g2d.setPaint(border);
		g2d.draw(processor);
	}

	private void drawPoint(Color startC, Color border, boolean gradientFill, Graphics2D g2d, int point) {
		double x = getProcessorXY().x, y = getProcessorXY().y;
		processor.setFrame(x + PROC_RAD, y + PROC_RAD - ELEMS_GAP / 2 + ELEMS_GAP * point, 0.5, 0.5);
		if (gradientFill) {
			GradientPaint gp = new GradientPaint((float) x, (float) y, startC.brighter(), (float) x, (float) (y + PROC_RAD * 2), startC.darker(),
					false);
			g2d.setPaint(gp);
		} else {
			g2d.setPaint(startC);
		}
		g2d.fill(processor);
		g2d.setPaint(border);
		g2d.draw(processor);
	}

	private void drawUtilization(double U, Color startC, Color border, boolean gradientFill, Graphics2D g2d) {

		double x = getProcessorXY().x, y = getProcessorXY().y;
		try {
			occupiedRect = new Rectangle2D.Double(x, y, 2 * PROC_RAD, 2 * PROC_RAD * (1 - U));
		} catch (Exception e) {
			occupiedRect = new Rectangle2D.Double(x, y, 2 * PROC_RAD, 0.0);
		}
		occupiedEll = new Ellipse2D.Double(x, y, 2 * PROC_RAD, 2 * PROC_RAD);
		if (gradientFill) {
			GradientPaint gp = new GradientPaint((float) x, (float) y, startC.brighter(), (float) x, (float) (y + 2 * PROC_RAD), startC.darker(),
					false);
			g2d.setPaint(gp);
		} else {
			g2d.setPaint(startC);
		}
		occupiedArea = new Area(occupiedEll);
		occupiedArea.subtract(new Area(occupiedRect));
		g2d.fill(occupiedArea);
		g2d.setPaint(Color.BLACK);
		g2d.draw(occupiedArea);

		// //draw informations about processes
		// txtBounds = drawCenteredText("job n.:" + donejobs, Color.BLACK, x +
		// PROC_RAD,y + PROC_RAD * 2 + 4 * ELEMS_GAP,g2d, false);
		// //draw orizontal line parallel to occupation
		//			
		// //draw box around text
		// txtBounds.setFrame(
		// x + PROC_RAD - txtBounds.getWidth() / 2,
		// y + 2 * PROC_RAD + 4 * ELEMS_GAP - txtBounds.getHeight() / 2,
		// txtBounds.getWidth(),
		// txtBounds.getHeight());
		//			
		// g2d.draw(txtBounds);
	}

	private void drawUtilization2(double U, Color startC, Color border, boolean gradientFill, Graphics2D g2d, int cpu) {

		double x = getProcessorXY().x, y = getProcessorXY().y;
		try {
			occupiedRect = new Rectangle2D.Double(x + PROC_RAD / 2, y + cpu * PROC_RAD + ELEMS_GAP * cpu - ELEMS_GAP / 2, 2 * PROC_RAD / 2, PROC_RAD
					* (1 - U / nCpu));
		} catch (Exception e) {
			occupiedRect = new Rectangle2D.Double(x + PROC_RAD / 2, y + cpu * PROC_RAD + ELEMS_GAP * cpu - ELEMS_GAP / 2, 2 * PROC_RAD / 2, 0.0);
		}
		occupiedEll = new Ellipse2D.Double(x + PROC_RAD / 2, y + cpu * PROC_RAD + ELEMS_GAP * cpu - ELEMS_GAP / 2, 2 * PROC_RAD / 2, 2 * PROC_RAD / 2);
		if (gradientFill) {
			GradientPaint gp = new GradientPaint((float) x, (float) y, startC.brighter(), (float) x, (float) (y + 2 * PROC_RAD), startC.darker(),
					false);
			g2d.setPaint(gp);
		} else {
			g2d.setPaint(startC);
		}
		occupiedArea = new Area(occupiedEll);
		occupiedArea.subtract(new Area(occupiedRect));
		g2d.fill(occupiedArea);
		g2d.setPaint(Color.BLACK);
		g2d.draw(occupiedArea);

		// //draw informations about processes
		// txtBounds = drawCenteredText("job n.:" + donejobs, Color.BLACK, x +
		// PROC_RAD,y + PROC_RAD * 2 + 4 * ELEMS_GAP,g2d, false);
		// //draw orizontal line parallel to occupation
		//			
		// //draw box around text
		// txtBounds.setFrame(
		// x + PROC_RAD - txtBounds.getWidth() / 2,
		// y + 2 * PROC_RAD + 4 * ELEMS_GAP - txtBounds.getHeight() / 2,
		// txtBounds.getWidth(),
		// txtBounds.getHeight());
		//			
		// g2d.draw(txtBounds);
	}

	private void drawUtilizationMulti(double U, Color startC, Color border, boolean gradientFill, Graphics2D g2d, int cpu) {

		double x = getProcessorXY().x, y = getProcessorXY().y;
		try {
			occupiedRect = new Rectangle2D.Double(x + PROC_RAD / 2 + ELEMS_GAP / 2, y + cpu * (PROC_RAD - ELEMS_GAP) + ELEMS_GAP * cpu * 3
					- ELEMS_GAP / 2, PROC_RAD - ELEMS_GAP, (PROC_RAD - ELEMS_GAP) * (1 - U / nCpu));
		} catch (Exception e) {
			occupiedRect = new Rectangle2D.Double(x + PROC_RAD / 2 + ELEMS_GAP / 2, y + cpu * (PROC_RAD - ELEMS_GAP) + ELEMS_GAP * cpu * 3
					- ELEMS_GAP / 2, PROC_RAD - ELEMS_GAP, 0);
		}
		occupiedEll = new Ellipse2D.Double(x + PROC_RAD / 2 + ELEMS_GAP / 2, y + cpu * (PROC_RAD - ELEMS_GAP) + ELEMS_GAP * cpu * 3 - ELEMS_GAP / 2,
				PROC_RAD - ELEMS_GAP, PROC_RAD - ELEMS_GAP);
		if (gradientFill) {
			GradientPaint gp = new GradientPaint((float) x, (float) y, startC.brighter(), (float) x, (float) (y + 2 * PROC_RAD), startC.darker(),
					false);
			g2d.setPaint(gp);
		} else {
			g2d.setPaint(startC);
		}
		occupiedArea = new Area(occupiedEll);
		occupiedArea.subtract(new Area(occupiedRect));
		g2d.fill(occupiedArea);
		g2d.setPaint(Color.BLACK);
		g2d.draw(occupiedArea);

	}

	private void drawOccupiedPercentage(Color startC, Color border, boolean gradientFill, Graphics2D g2d) {

		if (remainingTime[0] != 0) {
			double x = getProcessorXY().x, y = getProcessorXY().y;
			occupiedRect = new Rectangle2D.Double(x, y, 2 * PROC_RAD, 2 * PROC_RAD * (1 - (double) remainingTime[0] / (double) totTime[0]));
			occupiedEll = new Ellipse2D.Double(x, y, 2 * PROC_RAD, 2 * PROC_RAD);
			if (gradientFill) {
				GradientPaint gp = new GradientPaint((float) x, (float) y, startC.brighter(), (float) x, (float) (y + 2 * PROC_RAD), startC.darker(),
						false);
				g2d.setPaint(gp);
			} else {
				g2d.setPaint(startC);
			}
			occupiedArea = new Area(occupiedEll);
			occupiedArea.subtract(new Area(occupiedRect));
			g2d.fill(occupiedArea);
			g2d.setPaint(Color.BLACK);
			g2d.draw(occupiedArea);

			// draw orizontal line parallel to occupation
			Line2D.Double l = new Line2D.Double(x + PROC_RAD * 2 + ELEMS_GAP, y + PROC_RAD * 2
					* (1 - (double) remainingTime[0] / (double) totTime[0]), x + PROC_RAD * 2 + 2 * ELEMS_GAP, y + PROC_RAD * 2
					* (1 - (double) remainingTime[0] / (double) totTime[0]));
			g2d.draw(l);

			// draw vertical line
			l = new Line2D.Double(x + PROC_RAD * 2 + 2 * ELEMS_GAP, y + PROC_RAD * 2 * (1 - (double) remainingTime[0] / (double) totTime[0]), x
					+ PROC_RAD * 2 + 2 * ELEMS_GAP, y + 2 * PROC_RAD + 4 * ELEMS_GAP - txtBounds.getHeight() / 2);
			g2d.draw(l);

			// draw horizontal line under text
			txtBounds.setFrame(x + PROC_RAD - txtBounds.getWidth() / 2, y + 2 * PROC_RAD + 4 * ELEMS_GAP - txtBounds.getHeight() / 2, txtBounds
					.getWidth(), txtBounds.getHeight());

			g2d.draw(txtBounds);
		}
	}

	private void drawOccupiedPercentage2(Color startC, Color border, boolean gradientFill, Graphics2D g2d, int cpu) {
		//processor.setFrame(x+PROC_RAD/2 , y + cpu*PROC_RAD, 2 * PROC_RAD /2, 2 * PROC_RAD /2);

		//		if (remainingTime[cpu] != 0) {
		double x = getProcessorXY().x, y = getProcessorXY().y;
		occupiedRect = new Rectangle2D.Double(x + PROC_RAD / 2, y + cpu * PROC_RAD + ELEMS_GAP * cpu - ELEMS_GAP / 2, 2 * PROC_RAD / 2, 2 * PROC_RAD
				* (1 - (double) remainingTime[cpu] / (double) totTime[cpu]) / 2);
		occupiedEll = new Ellipse2D.Double(x + PROC_RAD / 2, y + cpu * PROC_RAD + ELEMS_GAP * cpu - ELEMS_GAP / 2, 2 * PROC_RAD / 2, 2 * PROC_RAD / 2);
		if (gradientFill) {
			GradientPaint gp = new GradientPaint((float) x, (float) y, startC.brighter(), (float) x, (float) (y + 2 * PROC_RAD), startC.darker(),
					false);
			g2d.setPaint(gp);
		} else {
			g2d.setPaint(startC);
		}
		occupiedArea = new Area(occupiedEll);
		occupiedArea.subtract(new Area(occupiedRect));
		g2d.fill(occupiedArea);
		g2d.setPaint(Color.BLACK);
		g2d.draw(occupiedArea);

		// draw orizontal line parallel to occupation
		Line2D.Double l = new Line2D.Double(x + PROC_RAD * 2 + ELEMS_GAP, y + cpu * PROC_RAD + ELEMS_GAP * cpu - ELEMS_GAP / 2 + 2 * PROC_RAD
				* (1 - (double) remainingTime[cpu] / (double) totTime[cpu]) / 2,//y + PROC_RAD * 2 * (1 - (double) remainingTime / (double) totTime) /2 + ELEMS_GAP * cpu -  ELEMS_GAP /2  , 
				x + PROC_RAD * 2 + 2 * ELEMS_GAP, y + cpu * PROC_RAD + ELEMS_GAP * cpu - ELEMS_GAP / 2 + 2 * PROC_RAD
						* (1 - (double) remainingTime[cpu] / (double) totTime[cpu]) / 2);//y + PROC_RAD * 2 * (1 - (double) remainingTime / (double) totTime) /2 + ELEMS_GAP * cpu -  ELEMS_GAP /2 );
		g2d.draw(l);

		// draw vertical line
		l = new Line2D.Double(x + PROC_RAD * 2 + 2 * ELEMS_GAP, y + cpu * PROC_RAD + ELEMS_GAP * cpu - ELEMS_GAP / 2 + 2 * PROC_RAD
				* (1 - (double) remainingTime[cpu] / (double) totTime[cpu]) / 2, x + PROC_RAD * 2 + 2 * ELEMS_GAP, y + PROC_RAD * 2 / 2 + cpu
				* PROC_RAD + ELEMS_GAP * cpu - ELEMS_GAP / 2);
		g2d.draw(l);

		//		}
	}

	/**
	 * 
	 * @param s
	 * @param c
	 * @param centerX
	 * @param centerY
	 */
	private Rectangle2D drawCenteredText(String s, Color fg, Color bg, double centerX, double centerY, Graphics2D g2d, boolean drawBorder,
			boolean draw) {
		double x, y;
		Color ctmp = g2d.getColor();
		Rectangle2D bordersR = new Rectangle2D.Float();
		g2d.setFont(f);
		txtBounds = f.getStringBounds(s, g2d.getFontRenderContext());
		x = centerX - txtBounds.getWidth() / 2.0;
		y = centerY - txtBounds.getY() - txtBounds.getHeight() / 2;
		txtBounds.setRect(x - ELEMS_GAP, y - txtBounds.getHeight() / 2.0 - ELEMS_GAP, txtBounds.getWidth() + 2 * ELEMS_GAP, txtBounds.getHeight() + 2
				* ELEMS_GAP);

		if (draw) {
			if (drawBorder) {
				bordersR.setFrame(centerX - txtBounds.getWidth() / 2.0, centerY - txtBounds.getHeight() / 2.0, txtBounds.getWidth(), txtBounds
						.getHeight());
				g2d.setColor(bg);
				g2d.fill(bordersR);
				g2d.setColor(fg);
				g2d.draw(bordersR);
			}
			g2d.setColor(fg);
			g2d.drawString(s, (float) x, (float) y);

		}
		g2d.setColor(ctmp);
		return txtBounds;
	}

	private Color invertedColor(Color c) {
		int invertColor = 255;
		return new Color(invertColor - c.getRed(), invertColor - c.getGreen(), invertColor - c.getBlue());

	}

	private Color readColor(Color c) {
		Color rc;
		int r = 0, g = 0, b = 0;
		if (c.getBlue() < 128) {
			b = 255;
		}
		if (c.getGreen() < 128) {
			g = 255;
		}
		if (c.getRed() < 128) {
			r = 255;
		}

		rc = new Color(r, g, b);

		if (c == Color.BLUE) {
			rc = Color.cyan;
		}
		return rc;
	}

	private int queueLenght() {
		int a = (int) ((panelW - (PROC_RAD * 2 + ELEMS_GAP) - 4 * ELEM_WIDTH - 4 * ELEMS_GAP - START_GAP - END_GAP) / (ELEM_WIDTH + ELEMS_GAP));
		if ((queueMax > 0) && (queueMax < a)) {
			return queueMax;
		}
		return a;

	}

	private Point2D.Double getElementXY(int elem) {
		return new Point2D.Double(START_GAP + ELEMS_GAP + (ELEMS_GAP + ELEM_WIDTH * 2) * 2 + (ELEMS_GAP + ELEM_WIDTH) * elem, panelH * 0.4
				- ELEM_HEIGHT / 2.0);
		// (panelH - ELEM_HEIGHT) / 2.0);
	}

	private Point2D.Double getProcessorXY() {
		return new Point2D.Double(getElementXY(queueLenght()).x, panelH * 0.4 - PROC_RAD);
	}

	private void resize() {
		int x = this.getWidth(), y = this.getHeight();
		if (y < minH) {
			panelH = minH;
		} else {
			panelH = y;
		}
		if (x < minW) {
			panelW = minW;
		} else {
			panelW = x;
		}
	}

	public void setColors(Color emptyC, Color queueC, Color animC, boolean gradient) {
		this.emptyC = emptyC;
		this.busyC = queueC.darker().darker();
		this.queueC = queueC;
		this.animC = animC;
		this.gradientF = gradient;
		this.repaint();
	}

	public void setCpuNumber(int nCpu) {
		if (nCpu < 1) {
			nCpu = 1;
		}
		this.nCpu = nCpu;
		remainingTime = new long[nCpu];
		totTime = new long[nCpu];

	}

	public void setTotalJobs(int totjobs) {
		this.totjobs = totjobs;
	}

	/**
	 * drawing the legend
	 * 
	 * @param ca
	 *            color of the legend
	 * @param sa
	 *            Comments
	 * @param f
	 *            font
	 * @param x
	 *            initial abscissa
	 * @param y
	 *            initial ordinate
	 * @param g2d
	 * @return panel lenght 
	 */
	private double drawLegend(Color[] ca, String[] sa, Font f, double x, double y, Graphics2D g2d, boolean draw) {
		Rectangle2D[] ra = new Rectangle2D[ca.length];
		Rectangle2D[] tba = new Rectangle2D[ca.length];
		double maxw = 0.0, gap = 5.0;
		String ts = "Legenda";
		// backup
		Color ctmp = g2d.getColor();
		Font ftmp = g2d.getFont();

		g2d.setFont(f);
		Rectangle2D tr = f.getStringBounds(ts, g2d.getFontRenderContext());
		maxw = tr.getWidth();
		for (int i = 0; i < ca.length; i++) {
			tba[i] = f.getStringBounds(sa[i], g2d.getFontRenderContext());
			ra[i] = new Rectangle2D.Double(x + gap, y + gap + (tr.getHeight() + gap) * (i + 1), tr.getHeight(), tr.getHeight());
			if (draw) {
				g2d.setColor(ca[i]);
				g2d.fill(ra[i]);
				g2d.setColor(Color.BLACK);
				g2d.draw(ra[i]);
				g2d.drawString(sa[i], (float) (x + gap * 2 + tr.getHeight()), (float) (y + gap + (tr.getHeight() + gap) * (i + 1) + tr.getHeight()
						/ 2.0 - tr.getY() / 2.0));
			}
			if (maxw < tba[i].getWidth()) {
				maxw = tba[i].getWidth();
			}
		}
		if (draw) {
			g2d.drawRect((int) x, (int) y, (int) (maxw + 3.0 * gap + tr.getHeight()), (int) (y + (tr.getHeight() + gap) * (ca.length + 1) + gap));
			g2d.drawRect((int) x, (int) y, (int) (maxw + 3.0 * gap + tr.getHeight()), (int) (tr.getHeight() + gap));
			g2d.fillRect((int) x, (int) y, (int) (maxw + 3.0 * gap + tr.getHeight()), (int) (tr.getHeight() + gap));
			g2d.setColor(Color.WHITE);
			g2d.drawString(ts, (float) (x + gap + (maxw - tr.getWidth() + tr.getHeight()) / 2.0), (float) (y + tr.getY() / 2.0 + tr.getHeight()));
		}

		// restore
		g2d.setFont(ftmp);
		g2d.setColor(ctmp);
		return (maxw + 3.0 * gap + tr.getHeight());

	}

	private void changeView() {
		view = ++view % VIEWS;
	}

	private RoundRectangle2D drawViewButton(double x, double y, Graphics2D g2d, boolean draw, boolean clicked) {
		Rectangle2D dvR;
		RoundRectangle2D dvRR;
		Color tmp;
		Color fg = Color.BLACK;
		Color bg = Color.LIGHT_GRAY;

		if (clicked) {
			tmp = fg;
			fg = bg;
			bg = tmp;
		}

		if (nCpu > 2) {
			fg = Color.darkGray.brighter().brighter();
			bg = Color.LIGHT_GRAY;
		}

		dvR = drawCenteredText("change view", fg, bg, x, y, g2d, false, false);
		dvRR = new RoundRectangle2D.Double(x, y, dvR.getWidth(), dvR.getHeight(), 5.0, 5.0);
		if (draw) {
			tmp = g2d.getColor();
			g2d.setColor(bg);
			g2d.fill(dvRR);
			g2d.setColor(fg);
			g2d.draw(dvRR);
			drawCenteredText("change view", fg, bg, x + dvR.getWidth() / 2.0, y + dvR.getHeight() / 2.0, g2d, false, true);
			g2d.setColor(tmp);
		}
		return dvRR;
	}

	public void enterProcessor(int jobId, int processorId, double time, double executionTime) {
		executingJobId = jobId;
		totTime[processorId] = (long) executionTime;
		updateProcessor(executingJobId, processorId, executionTime, time);//
	}

	public void enterQueue(int jobId, double time) {
		jobs++;
		this.repaint();

	}

	public void exitProcessor(int jobId, int processorId, double time) {
		// TODO Auto-generated method stub
		executingJobId = 0;
		updateProcessor(executingJobId, processorId, 0, time);//

	}

	public void exitQueue(int jobId, double time) {
		jobs--;
		this.repaint();
	}

	public void exitSystem(int jobId, int processorId, double enterQueueTime, double enterCpuTime, double exitSystemTime) {
		// TODO Auto-generated method stub

	}

	public void jobLost(int jobId, double time) {
		// TODO Auto-generated method stub
		lostJobs++;
		this.repaint();
	}

	public void reset() {
		jobs = 0;
		donejobs = 0;
		totjobs = 0;

		for (int i = 0; i < nCpu; i++) {
			totTime[i] = 1000;
			remainingTime[i] = 0;
		}

		donejobs = 0;
		lostJobs = 0;
		//		Ui = 0.0;
		this.repaint();
	}

	public void updateProcessor(int jobId, int processorId, double remainingTime, double time) {
		this.remainingTime[processorId] = (long) remainingTime;
		donejobs = jobId;
		this.repaint();
	}

	public void updateQueue(int jobId, double time) {
		// TODO Auto-generated method stub

	}
}
