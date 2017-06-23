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
 * Created on 18-mar-2004 by Ernesto
 *
 */
package jmt.jmarkov.Graphics;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.LayoutManager;
import java.awt.RenderingHints;
import java.awt.Stroke;
import java.awt.geom.Arc2D;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.Point2D;
import java.awt.geom.QuadCurve2D;
import java.awt.geom.Rectangle2D;

import javax.swing.JPanel;

import jmt.jmarkov.Graphics.constants.DrawConstrains;
import jmt.jmarkov.Graphics.constants.DrawNormal;
import jmt.jmarkov.Queues.QueueLogic;
import jmt.jmarkov.Queues.Exceptions.NonErgodicException;

public class StatiDrawer extends JPanel implements Notifier/*, Runnable*/{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private boolean animate;
	//queue settings
	private QueueLogic ql;
	private double pb[];//status probability of the queue logic 
	private int queueMax,//limited maximum queue
			currentJob,//number of jobs in queue
			lastJob;//for animate 
	//				jobTmp;

	//panel settings
	private double panelH = 250,//panel height
			panelW = 400,//panel width
			minH = 100,//minimum value for panelH
			minW = 400; //minimum value for panelW

	//draw settings
	private DrawConstrains dCst;
	private Rectangle2D txtBounds;
	private double ELEMS_GAP;
	private double STATUS_RAD;
	private double START_GAP;
	private double END_GAP;
	private Stroke stroke, strokeB;
	private Font f;
	private Arc2D[] statusP;
	private Arc2D lastStatusP;
	private Ellipse2D[] statusE;
	private Ellipse2D lastStatusE, transitionE;
	private QuadCurve2D[][] arc;
	private GeneralPath arrow;
	private float arroww = 5.0f, arrowh = 5.0f;
	private int frame, maxframe;

	//colors
	private Color probC, queueProbC, emptyC, queueC, animQueuesC, animProbC, animBorderC, borderC;

	private int currentJobId;

	/**
	 * 
	 */
	public StatiDrawer(QueueLogic ql) {
		super();
		this.ql = ql;
		init();
	}

	/**
	 * @param isDoubleBuffered
	 */
	public StatiDrawer(boolean isDoubleBuffered) {
		super(isDoubleBuffered);
		init();
	}

	/**
	 * @param layout
	 */
	public StatiDrawer(LayoutManager layout) {
		super(layout);
		init();
	}

	/**
	 * @param layout
	 * @param isDoubleBuffered
	 */
	public StatiDrawer(LayoutManager layout, boolean isDoubleBuffered) {
		super(layout, isDoubleBuffered);
		init();
	}

	/**
	 * initialize the parameters
	 *
	 */
	private void init() {
		probC = Color.green;
		queueProbC = Color.green.brighter();
		queueC = Color.green.darker();
		emptyC = Color.WHITE;
		borderC = Color.BLACK;
		animBorderC = Color.BLACK;
		animQueuesC = Color.RED.darker();
		animProbC = Color.RED.brighter();
		maxframe = 3;
		lastJob = 0;
		currentJob = 0;
		frame = 5;
		changeDrawSettings(new DrawNormal());
		updateQueueSettings();
		animate = false;
		this.reset();
	}

	public void changeDrawSettings(DrawConstrains dCst) {
		this.dCst = dCst;

		resize();

		//assigning the design constants
		f = dCst.getFont();
		stroke = dCst.getDrawStroke();
		strokeB = dCst.getBoldStroke();
		START_GAP = dCst.getStartingGap();
		END_GAP = dCst.getStartingGap();
		STATUS_RAD = dCst.getStatusRadius();
		ELEMS_GAP = dCst.getElementsGap();
	}

	private void updateQueueSettings() {
		//queue is initializing
		statusE = new Ellipse2D.Double[queueLenght() + 1];
		statusP = new Arc2D.Double[queueLenght() + 1];
		arc = new QuadCurve2D.Double[queueLenght() + 3][queueLenght() + 3];
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

	@Override
	public void paint(Graphics g) {
		Graphics2D g2d = (Graphics2D) g;
		g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		g2d.setStroke(stroke);
		g2d.setBackground(Color.white);
		g2d.clearRect(0, 0, this.getWidth(), this.getHeight());
		changeDrawSettings(dCst);
		START_GAP = START_GAP
				* 2
				+ drawLegend(new Color[] { probC, queueC, animQueuesC }, new String[] { "probability", "queue", "current state" }, dCst.getFont(),
						START_GAP, START_GAP, g2d);
		updateQueueSettings();
		pb = new double[queueLenght() + 1];
		for (int i = 0; i < queueLenght() + 1; i++) {
			try {
				pb[i] = ql.getStatusProbability(i);
			} catch (NonErgodicException e) {
				pb[i] = 0.0;
			}
			if ((queueLenght() < queueMax) || (queueMax == 0)) { //queue is infinite or bigger than the visible area 
				//drawing the previous of last status 
				if (i == queueLenght() - 2) {
					drawMoreStatus(g2d);
					drawArc(i, (i + 1), false, "", g2d, borderC);
					drawArc((i + 1), i, false, "", g2d, borderC);
					//drawing last status
					//if the status is busy
					if (i <= currentJob) {
						double p = 0.0;
						//finite queue
						if ((queueMax > 0) && (currentJob > queueMax)) {
							try {
								p = ql.getStatusProbability(queueMax);
							} catch (Exception e) {
								p = 0.0;
							}
							drawLastStatus("" + (queueMax), p, g2d, animQueuesC, animProbC);

						}
						//infinite queue
						else {
							try {
								p = ql.getStatusProbability(currentJob);
							} catch (Exception e) {
								p = 0.0;
							}
							drawLastStatus("" + currentJob, p, g2d, animQueuesC, animProbC);
						}
					}
					//the status is not busy
					if (i > currentJob) {
						drawLastStatus("", 0.0, g2d, emptyC, probC);
					}
					break;
				}

			}
			if (i < queueLenght()) {
				drawArc(i, (i + 1), false, "", g2d, borderC);
				drawArc((i + 1), i, false, "", g2d, borderC);
			}
			//status is busy
			if (i < currentJob) {
				drawStatus(i, pb[i], g2d, queueC, queueProbC, borderC, false);
			}
			//status is in animation
			if (i == currentJob) {
				drawStatus(i, pb[i], g2d, animQueuesC, animProbC, animBorderC, false);
			}
			//status is not busy
			if (i > currentJob) {
				drawStatus(i, pb[i], g2d, emptyC, probC, borderC, false);
			}
		}
		//animation
		if ((lastJob != currentJob) && (currentJob < queueLenght() - 2) && animate) {
			animateTransition3(lastJob, currentJob, false, g2d, animBorderC, animQueuesC);
		}

	}

	public void drawStatus(int status, double probability, Graphics2D g2d, Color sc, Color pc, Color borderC, boolean bold) {
		double x = 2.0 * (2.0 * STATUS_RAD + ELEMS_GAP) * status + START_GAP;
		double y = panelH / 2.0 - STATUS_RAD;
		double pie = probability * 360;
		Color ctmp = g2d.getColor();
		if (bold) {
			g2d.setStroke(strokeB);
		}
		statusE[status] = new Ellipse2D.Double(x, y, STATUS_RAD * 2.0, STATUS_RAD * 2.0);
		statusP[status] = new Arc2D.Double(x, y, STATUS_RAD * 2.0, STATUS_RAD * 2.0, 0.0, pie, Arc2D.PIE);
		g2d.setPaint(sc);
		g2d.fill(statusE[status]);
		g2d.setPaint(pc);
		g2d.fill(statusP[status]);
		g2d.setPaint(borderC);
		g2d.draw(statusE[status]);
		drawCenteredText(probabilityToString(probability, 3), Color.BLACK, x + STATUS_RAD, y - STATUS_RAD, g2d, false);
		drawCenteredText("" + status, borderC, x + STATUS_RAD, panelH / 2.0, g2d, false);
		g2d.setColor(ctmp);
		g2d.setStroke(stroke);
	}

	public void drawLastStatus(String jobStr, double probability, Graphics2D g2d, Color sc, Color pc) {
		double x = 2.0 * (2.0 * STATUS_RAD + ELEMS_GAP) * (queueLenght() - 1) + START_GAP;
		double y = panelH / 2.0 - STATUS_RAD;
		Color ctmp = g2d.getColor();
		lastStatusE = new Ellipse2D.Double(x, y, STATUS_RAD * 2.0, STATUS_RAD * 2.0);
		g2d.setPaint(sc);
		g2d.fill(lastStatusE);
		//if(queueMax != 0){
		double pie = probability * 360;
		lastStatusP = new Arc2D.Double(x, y, STATUS_RAD * 2.0, STATUS_RAD * 2.0, 0.0, pie, Arc2D.PIE);
		g2d.setPaint(pc);
		g2d.fill(lastStatusP);
		g2d.setPaint(Color.BLACK);
		drawCenteredText(probabilityToString(probability, 3), Color.BLACK, x + STATUS_RAD, y - STATUS_RAD, g2d, false);
		//drawCenteredText("" + queueMax, Color.BLACK, x + STATUS_RAD, panelH/2.0, g2d, false);
		drawCenteredText(jobStr, Color.BLACK, x + STATUS_RAD, panelH / 2.0, g2d, false);
		//}
		g2d.setPaint(Color.BLACK);
		g2d.draw(lastStatusE);
		g2d.setColor(ctmp);
	}

	public void drawMoreStatus(Graphics2D g2d) {
		double x = 2.0 * (2.0 * STATUS_RAD + ELEMS_GAP) * (queueLenght() - 2) + START_GAP;
		double y = panelH / 2.0 - STATUS_RAD;
		Color ctmp = g2d.getColor();
		g2d.setPaint(Color.BLACK);
		drawCenteredText(" . . . ", Color.BLACK, x + STATUS_RAD, panelH / 2.0, g2d, false);
		g2d.setColor(ctmp);
	}

	public void drawArc(int from, int to, boolean bold, String txt, Graphics2D g2d, Color c) {
		Color oldc = g2d.getColor();
		g2d.setColor(c);
		arrow = new GeneralPath(GeneralPath.WIND_EVEN_ODD);
		double x1, x2, ctrlx, ctrly, arrowx, arrowy, y;
		if (bold) {
			g2d.setStroke(strokeB);
		}
		if (from > to) {

			// arc settings
			x2 = to * (4 * STATUS_RAD + 2 * ELEMS_GAP) + START_GAP + STATUS_RAD * 2;//+ ELEMS_GAP;
			x1 = from * (4 * STATUS_RAD + 2 * ELEMS_GAP) + START_GAP;// - ELEMS_GAP;				
			y = (panelH) / 2.0 + STATUS_RAD;
			ctrly = y + STATUS_RAD;

			// arrow settings
			arrow.moveTo((float) x2, (float) y);
			arrow.lineTo((float) x2 + arroww, (float) y);
			arrow.lineTo((float) x2, (float) y + arroww);
			arrow.closePath();
		} else {

			// arc settings
			x1 = from * (4 * STATUS_RAD + 2 * ELEMS_GAP) + START_GAP + STATUS_RAD * 2;//+ ELEMS_GAP;
			x2 = to * (4 * STATUS_RAD + 2 * ELEMS_GAP) + START_GAP;// - ELEMS_GAP;
			y = (panelH - STATUS_RAD * 2) / 2.0;
			ctrly = y - STATUS_RAD;

			// arrow settings
			arrow.moveTo((float) x2, (float) y);
			arrow.lineTo((float) x2 - arroww, (float) y);
			arrow.lineTo((float) x2, (float) y - arroww);
			arrow.closePath();
		}
		ctrlx = (x1 + x2) / 2;

		arc[from][to] = new QuadCurve2D.Double(x1, y, ctrlx, ctrly, x2, y);
		g2d.draw(arc[from][to]);
		g2d.draw(arrow);
		g2d.fill(arrow);
		g2d.setColor(oldc);
		g2d.setStroke(stroke);
	}

	//	private void animateTransition2(int from, int to, boolean bold, Graphics2D g2d, Color c){
	//			Color oldc = g2d.getColor();
	//			g2d.setColor(c);
	//			//arrow = new GeneralPath(GeneralPath.WIND_EVEN_ODD);
	//			double gap, x1, x2, y, ctrlx, ctrly;
	//			if (bold)
	//				g2d.setStroke(strokeB);
	//			if(from > to){
	//				x2 = to * (4 * STATUS_RAD + 2* ELEMS_GAP) + START_GAP + STATUS_RAD * 2 ;//+ ELEMS_GAP;
	//				x1 = from * (4 * STATUS_RAD + 2 * ELEMS_GAP) + START_GAP;// - ELEMS_GAP;				
	//			
	//				y = (panelH)/ 2.0 + STATUS_RAD;
	//				ctrly = y + STATUS_RAD;
	//				gap = ELEMS_GAP;
	//				
	//			}
	//			else {
	//				x1 = from * (4 * STATUS_RAD + 2* ELEMS_GAP) + START_GAP + STATUS_RAD * 2 ;//+ ELEMS_GAP;
	//				x2 = to * (4 * STATUS_RAD + 2 * ELEMS_GAP) + START_GAP;// - ELEMS_GAP;
	//		
	//				y = (panelH)/ 2.0 - STATUS_RAD;
	//				ctrly = y - STATUS_RAD/2.0;
	//				gap = 2*ELEMS_GAP;
	//			}
	//			ctrlx = (x1 + x2)/2;
	//			//x = (2.0 * STATUS_RAD + ELEMS_GAP) * (double) (from + to)  + START_GAP + (STATUS_RAD / 2.0);
	//			
	//			QuadCurve2D prova = new QuadCurve2D.Double(x1, y, ctrlx, ctrly, x2, y);
	//			prova.subdivide(prova,null);
	//			Point2D p = prova.getP2();
	//			transitionE = new Ellipse2D.Double(p.getX() - ELEMS_GAP, p.getY() - gap, 2*ELEMS_GAP, 2*ELEMS_GAP);
	//			g2d.fill(transitionE);
	//			g2d.setColor(oldc);
	//			g2d.setStroke(stroke);
	//		}

	private void animateTransition3(int from, int to, boolean bold, Graphics2D g2d, Color b, Color f) {
		Color oldc = g2d.getColor();
		g2d.setColor(f);
		//arrow = new GeneralPath(GeneralPath.WIND_EVEN_ODD);
		double gap, x1, x2, y, ctrlx, ctrly;
		if (bold) {
			g2d.setStroke(strokeB);
		}
		if (from > to) {
			x2 = to * (4 * STATUS_RAD + 2 * ELEMS_GAP) + START_GAP + STATUS_RAD * 2;//+ ELEMS_GAP;
			x1 = from * (4 * STATUS_RAD + 2 * ELEMS_GAP) + START_GAP;// - ELEMS_GAP;				

			y = (panelH) / 2.0 + STATUS_RAD;
			ctrly = y + STATUS_RAD;
			gap = ELEMS_GAP;

		} else {
			x1 = from * (4 * STATUS_RAD + 2 * ELEMS_GAP) + START_GAP + STATUS_RAD * 2;//+ ELEMS_GAP;
			x2 = to * (4 * STATUS_RAD + 2 * ELEMS_GAP) + START_GAP;// - ELEMS_GAP;

			y = (panelH) / 2.0 - STATUS_RAD;
			ctrly = y - STATUS_RAD / 2.0;
			gap = 2 * ELEMS_GAP;
		}
		ctrlx = (x1 + x2) / 2;
		QuadCurve2D prova;
		switch (frame) {
			case 1:
				prova = new QuadCurve2D.Double(x1, y, ctrlx, ctrly, x2, y);
				prova.subdivide(prova, null);
				prova.subdivide(prova, null);
				break;

			case 2:
				prova = new QuadCurve2D.Double(x1, y, ctrlx, ctrly, x2, y);
				prova.subdivide(prova, null);
				break;
			case 3:
				prova = new QuadCurve2D.Double(x1, y, ctrlx, ctrly, x2, y);
				prova.subdivide(null, prova);
				prova.subdivide(null, prova);
				break;

			default:
				prova = new QuadCurve2D.Double(x1, y, ctrlx, ctrly, x2, y);
				prova.subdivide(prova, null);
				break;
		}
		Point2D p = prova.getP2();
		transitionE = new Ellipse2D.Double(p.getX() - ELEMS_GAP, p.getY() - gap, 2 * ELEMS_GAP, 2 * ELEMS_GAP);
		g2d.fill(transitionE);
		g2d.setPaint(b);
		g2d.draw(transitionE);
		g2d.setColor(oldc);
		g2d.setStroke(stroke);
	}

	public void drawLastArc(boolean bold, String txt, Graphics2D g2d, Color c) {
		drawArc(queueLenght() + 2, queueLenght() + 1, false, "", g2d, c);
		drawArc(queueLenght() + 1, queueLenght() + 2, false, "", g2d, c);
	}

	//	private void animateTransition(int from, int to, Color c, int frame, int maxframe, Graphics2D g2d){
	//			Color oldc = g2d.getColor();
	//			g2d.setColor(c);
	//			double x, y, diameter;
	//			x = (2.0 * STATUS_RAD + ELEMS_GAP) * (double) (from + to)  + START_GAP + (STATUS_RAD / 2.0);
	//			y = panelH / 2 - STATUS_RAD / 2.0;
	//			diameter = ELEMS_GAP * 2;
	//			transitionE = new Ellipse2D.Double(x, y, STATUS_RAD, STATUS_RAD);
	//			g2d.setPaint(c);
	//			g2d.draw(transitionE);
	//			g2d.fill(transitionE);
	//			g2d.setColor(oldc);
	//	}

	private Rectangle2D drawCenteredText(String s, Color c, double centerX, double centerY, Graphics2D g2d, boolean drawBorder) {
		double x, y;
		g2d.setFont(f);
		txtBounds = f.getStringBounds(s, g2d.getFontRenderContext());
		x = centerX - txtBounds.getWidth() / 2.0;
		y = centerY - txtBounds.getY() - txtBounds.getHeight() / 2;
		txtBounds.setRect(x - ELEMS_GAP, y - txtBounds.getHeight() / 2.0 - ELEMS_GAP, txtBounds.getWidth() + 2 * ELEMS_GAP, txtBounds.getHeight() + 2
				* ELEMS_GAP);
		if (drawBorder) {
			g2d.setColor(invertedColor(c));
			g2d.fill(txtBounds);
			g2d.setColor(c);
			g2d.draw(txtBounds);
		}
		Color ctmp = g2d.getColor();
		g2d.setColor(c);
		g2d.drawString(s, (float) x, (float) y);
		g2d.setColor(ctmp);
		return txtBounds;
	}

	private Color invertedColor(Color c) {
		int invertColor = 255;
		return new Color(invertColor - c.getRed(), invertColor - c.getGreen(), invertColor - c.getBlue());

	}

	private int queueLenght() {

		//		int a = (int)((panelW - 2 * START_GAP  - 4 * (2 * STATUS_RAD + ELEMS_GAP) 
		//			- ELEMS_GAP )/(2 * (2 * STATUS_RAD + ELEMS_GAP)));
		int tmp = (int) ((panelW - START_GAP - END_GAP + (2 * STATUS_RAD + 2 * ELEMS_GAP)) / (2 * (2 * STATUS_RAD + ELEMS_GAP)));
		if ((queueMax > 0) && (queueMax < tmp)) {
			return queueMax;
		}
		return tmp;
	}

	/**
	 * converting the probability into string 
	 * @param p
	 * @param c
	 * @return
	 */
	public String probabilityToString(double p, int decimals) {
		String tmp;
		if (p > 1.0) {
			return "impossibile";
		}
		if (p == 1.0) {
			tmp = "1.";
		} else {
			tmp = "0.";
		}
		return (tmp + ("" + (int) ((p + 1.0) * Math.pow(10, decimals))).substring(1));
	}

	/**
	 * @param i
	 */
	public void setMaxJobs(int i) {
		if (i > 0) {
			queueMax = i;
		} else {
			queueMax = i;
		}
		repaint();
	}

	public void updateLogic(QueueLogic ql) {
		this.ql = ql;
	}

	public void setColors(Color emptyC, Color queueC, Color probC, Color animC) {
		this.emptyC = emptyC;
		this.queueC = queueC;
		this.probC = probC;
		//this.probC = emptyC.darker().darker();
		this.queueProbC = probC.brighter().brighter();
		this.animBorderC = Color.BLACK;
		this.animQueuesC = animC.darker();
		this.animProbC = animC.brighter();
		this.repaint();
	}

	/**
	 * drawing the legend
	 * @param ca colors of the legend
	 * @param sa comments respectivily
	 * @param f font
	 * @param x initial abscissa
	 * @param y initial ordinate
	 * @param g2d
	 * @return panel size
	 */
	private double drawLegend(Color[] ca, String[] sa, Font f, double x, double y, Graphics2D g2d) {
		Rectangle2D[] ra = new Rectangle2D[ca.length];
		Rectangle2D[] tba = new Rectangle2D[ca.length];
		double maxw = 0.0, gap = 5.0;
		String ts = "Legenda";
		//backup
		Color ctmp = g2d.getColor();
		Font ftmp = g2d.getFont();

		g2d.setFont(f);
		Rectangle2D tr = f.getStringBounds(ts, g2d.getFontRenderContext());
		maxw = tr.getWidth();
		for (int i = 0; i < ca.length; i++) {
			tba[i] = f.getStringBounds(sa[i], g2d.getFontRenderContext());
			ra[i] = new Rectangle2D.Double(x + gap, y + gap + (tr.getHeight() + gap) * (i + 1), tr.getHeight(), tr.getHeight());
			g2d.setColor(ca[i]);
			g2d.fill(ra[i]);
			g2d.setColor(Color.BLACK);
			g2d.draw(ra[i]);
			g2d.drawString(sa[i], (float) (x + gap * 2 + tr.getHeight()),
					(float) (y + gap + (tr.getHeight() + gap) * (i + 1) + tr.getHeight() / 2.0 - tr.getY() / 2.0));
			if (maxw < tba[i].getWidth()) {
				maxw = tba[i].getWidth();
			}
		}
		g2d.drawRect((int) x, (int) y, (int) (maxw + 3.0 * gap + tr.getHeight()), (int) (y + (tr.getHeight() + gap) * (ca.length + 1) + gap));
		g2d.drawRect((int) x, (int) y, (int) (maxw + 3.0 * gap + tr.getHeight()), (int) (tr.getHeight() + gap));
		g2d.fillRect((int) x, (int) y, (int) (maxw + 3.0 * gap + tr.getHeight()), (int) (tr.getHeight() + gap));
		g2d.setColor(Color.WHITE);
		g2d.drawString(ts, (float) (x + gap + (maxw - tr.getWidth() + tr.getHeight()) / 2.0), (float) (y + tr.getY() / 2.0 + tr.getHeight()));

		//restore
		g2d.setFont(ftmp);
		g2d.setColor(ctmp);
		return (maxw + 3.0 * gap + tr.getHeight());

	}

	public void enterProcessor(int jobId, int processorId, double time, double executionTime) {
		// TODO Auto-generated method stub

	}

	public void enterQueue(int jobId, double time) {
		lastJob = currentJob;
		currentJob++;
		frame = 0;
		animate = true;
		currentJobId = jobId;
		updateQueue(jobId, time);
	}

	public void exitProcessor(int jobId, int processorId, double time) {
		frame = 0;
		lastJob = currentJob;
		currentJob--;
		animate = true;
		currentJobId = jobId;
		updateQueue(jobId, time);
	}

	public void exitQueue(int jobId, double time) {
		// TODO Auto-generated method stub

	}

	public void exitSystem(int jobId, int processorId, double enterQueueTime, double enterCpuTime, double exitSystemTime) {
		// TODO Auto-generated method stub

	}

	public void jobLost(int jobId, double time) {
		// TODO Auto-generated method stub

	}

	public void reset() {
		currentJob = 0;
		lastJob = 0;
		animate = false;
		queueMax = 0;
		frame = 0;
		repaint();

	}

	public void updateProcessor(int jobId, int processorId, double remainingTime, double time) {
		// TODO Auto-generated method stub

	}

	public void updateQueue(int jobId, double time) {
		boolean lastAnimate = animate;
		if (currentJobId == jobId && frame < maxframe) {
			frame++;
		} else {
			animate = false;
		}
		if (lastAnimate) {
			this.repaint();
		}
	}

}
