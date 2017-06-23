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

package jmt.gui.jmodel.JGraphMod;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.geom.Arc2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.HashMap;
import java.util.Vector;

import jmt.gui.common.Defaults;
import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.distributions.Distribution;
import jmt.gui.common.resources.JMTImageLoader;
import jmt.gui.jmodel.controller.Mediator;
import jmt.gui.jmodel.controller.ModelSnapshot;

/**
 * <p>Title: GraphicalQueueState</p>
 * <p>Description: this class is used to graphically update thestate of queues
 * and utilizations</p>
 *
 * @author Francesco D'Aquino
 *         Date: 22-feb-2006
 *         Time: 09.09.21

 * Modyfied by Bertoli Marco to support JGraph 5.8 - 21/mar/2006
 * Modyfied by Bertoli Marco to greatly speed up everything - 18/may/2006

 */
public class GraphicalQueueState {
	private Mediator mediator;
	private HashMap<Object, JmtCell> tm; // Used to map station keys to their cells

	public GraphicalQueueState(Mediator m) {
		mediator = m;
		tm = new HashMap<Object, JmtCell>();
		// Initialize mapping between station keys and their cell components
		// All cells (included blocking regions children)
		Object[] cells = m.getGraph().getDescendants(m.getGraph().getRoots());
		for (Object tempCell : cells) {
			if (tempCell instanceof JmtCell) {
				JmtCell cell = (JmtCell) tempCell;
				Object key = ((CellComponent) cell.getUserObject()).getKey();
				tm.put(key, cell);
			}
		}
	}

	/**
	 * Draws the queues and the utilizations over the graph
	 * @param queueState a <code> SimulationSnapshot </code> containing information about the queues
	 * @param utilizationState a <code> SimulationSnapshot </code> containing information about the utilizations
	 */
	public void draw(ModelSnapshot queueState, ModelSnapshot utilizationState) {
		int MAX_NUMBER_OF_CLASSES_QUEUE = Defaults.getAsInteger("representableClasses").intValue();

		double nClasses = mediator.getClassDefinition().getClassKeys().size();
		double maxNJobs = queueState.getMaxValue();

		//used to avoid extreme queue lenght variation when the number of jobs inside the system is very small
		if (maxNJobs < 10) {
			maxNJobs = 10;
		}

		double QUEUE_HEIGHT = (JMTImageLoader.loadImage("queue")).getIconHeight();
		double QUEUE_WIDTH = (JMTImageLoader.loadImage("queue")).getIconWidth() - QUEUE_HEIGHT;

		double SOURCE_SIZE = (JMTImageLoader.loadImage("source")).getIconWidth();
		double SOURCE_MARGIN = 9;
		double source_usable_space = SOURCE_SIZE - SOURCE_MARGIN * 2;

		double H_MARGIN = 2;
		double V_MARGIN = 2;

		double width = QUEUE_WIDTH - 2 * H_MARGIN;
		double height = QUEUE_HEIGHT - 2 * V_MARGIN;

		int H_FREE_SPACE = 2;
		int V_FREE_SPACE = (int) ((QUEUE_HEIGHT - (((int) (height / nClasses)) * nClasses)) / 2);
		//int V_FREE_SPACE = 1;

		Graphics2D g = mediator.getGraphGraphics();

		Vector openClasses = mediator.getClassDefinition().getOpenClassKeys();
		Vector sources = mediator.getStationDefinition().getStationKeysSource();
		for (int i = 0; i < sources.size(); i++) {
			Vector<Object> refClasses = new Vector<Object>(0, 1);
			Object thisSource = sources.get(i);
			JmtCell cell = tm.get(thisSource);
			int classCount = 0;
			for (int k = 0; k < openClasses.size(); k++) {
				Object thisClass = openClasses.get(k);
				if (mediator.getClassDefinition().getClassRefStation(thisClass) == thisSource) {
					refClasses.add(thisClass);
					classCount++;
				}
			}
			Point2D p = mediator.getCellCoordinates(cell);

			//int xStart = (int)(p.x + SOURCE_MARGIN) + 8;
			double xStart = p.getX() + ((mediator.getCellDimension(cell).getWidth() - SOURCE_SIZE + 2 * SOURCE_MARGIN) / 2);
			double yStart = (int) (p.getY() + SOURCE_MARGIN) + 1;
			//if the number of class is less than the maximum representable
			if (classCount < MAX_NUMBER_OF_CLASSES_QUEUE) {
				int classSpace;
				for (int k = 0; k < classCount; k++) {
					classSpace = (int) (source_usable_space / classCount);
					if ((((source_usable_space / classCount) - classSpace) * k) > 1) {
						classSpace++;
					}
					Object thisClass = refClasses.get(k);
					g.setColor(mediator.getClassDefinition().getClassColor(thisClass));
					g.fillRect((int) xStart, (int) yStart, classSpace, (int) source_usable_space);
					xStart += classSpace;
				}
			}
			//else..
			else {
				int classSpace;
				Vector<Object> classesReplication = (Vector<Object>) refClasses.clone();
				Vector<Object> classesToBeDrawn = new Vector<Object>(0, 1);
				double min;
				double mean;
				//... find the first MAX_NUMBER_OF_CLASSES_QUEUE with the smallest mean value
				for (int k = 0; k < MAX_NUMBER_OF_CLASSES_QUEUE; k++) {
					min = Double.MAX_VALUE;
					int index = 0;

					for (int j = 0; j < classesReplication.size(); j++) {
						Object thisClass = classesReplication.get(j);
						Distribution temp = (Distribution) mediator.getClassDefinition().getClassParameter(thisClass,
								ClassDefinition.CLASS_DISTRIBUTION);
						if (temp.hasMean()) {
							mean = temp.getMean();
							if (mean < min) {
								index = j;
								min = mean;
							}
						}
					}
					classesToBeDrawn.add(classesReplication.get(index));
					classesReplication.remove(classesReplication.get(index));
				}
				//if less than MAX_NUMBER_OF_CLASSES_QUEUE classes where found
				//add the ( MAX_NUMBER_OF_CLASSES_QUEUE - classesToBeDrawn.size() )
				//with the highest priority
				if (classesToBeDrawn.size() < MAX_NUMBER_OF_CLASSES_QUEUE) {
					int left = MAX_NUMBER_OF_CLASSES_QUEUE - classesToBeDrawn.size();
					for (int j = 0; j < left; j++) {
						int max = -1;
						int priority;
						int index = 0;
						for (int k = 0; k < classesReplication.size(); k++) {
							Object thisClass = classesReplication.get(k);
							priority = mediator.getClassDefinition().getClassPriority(thisClass);
							if (priority > max) {
								max = priority;
								index = k;
							}
						}
						classesToBeDrawn.add(classesReplication.get(index));
						classesReplication.remove(classesReplication.get(index));
					}
				}
				//draw the classes inside the source
				for (int k = 0; k < MAX_NUMBER_OF_CLASSES_QUEUE; k++) {
					classSpace = (int) (source_usable_space / MAX_NUMBER_OF_CLASSES_QUEUE);
					if ((((source_usable_space / MAX_NUMBER_OF_CLASSES_QUEUE) - classSpace) * k) > 1) {
						classSpace++;
					}
					Object thisClass = classesToBeDrawn.get(k);
					g.setColor(mediator.getClassDefinition().getClassColor(thisClass));
					g.fill(new Rectangle2D.Double(xStart, yStart, classSpace, (int) source_usable_space));
					xStart += classSpace;
				}
			}
		}

		//Now draw station's queue
		Vector servers = queueState.getServers();
		Vector classes = mediator.getClassDefinition().getClassKeys();

		//for each server ...
		for (int i = 0; i < servers.size(); i++) {
			JmtCell cell = tm.get(servers.get(i));
			// Skips blocking region special nodes - Bertoli Marco
			if (cell == null) {
				continue;
			}
			g.setColor(Color.LIGHT_GRAY);
			Point2D p = mediator.getCellCoordinates(cell);
			double offset;
			double xStart;
			double yStart = p.getY() + V_FREE_SPACE;
			offset = (int) ((mediator.getCellDimension(cell).getWidth() - QUEUE_WIDTH - QUEUE_HEIGHT) / 2);
			g.fill(new Rectangle2D.Double(p.getX() + offset, p.getY(), (int) width + 1, (int) height + 2));

			//First draw the queues ....
			//if the number of classes is less then MAX_NUMBER_OF_CLASSES_QUEUE
			if (nClasses <= MAX_NUMBER_OF_CLASSES_QUEUE) {
				for (int j = 0; j < nClasses; j++) {
					double nJobs = queueState.getValue(servers.get(i), classes.get(j));

					int thisWidth = (int) ((nJobs / maxNJobs) * width);
					int thisHeight = (int) (height / nClasses);

					Color color = mediator.getClassDefinition().getClassColor(classes.get(j));
					g.setColor(color);
					xStart = p.getX() + offset + (int) (width - thisWidth) + H_FREE_SPACE;
					g.fill(new Rectangle2D.Double(xStart, yStart, thisWidth, thisHeight));
					yStart += thisHeight;
				}
			}

			// else draw only the first MAX_NUMBER_OF_CLASSES_QUEUE with the greates
			// number of job inside thisStation
			else {
				V_FREE_SPACE = (int) ((QUEUE_HEIGHT - (((int) (height / MAX_NUMBER_OF_CLASSES_QUEUE)) * (double) MAX_NUMBER_OF_CLASSES_QUEUE)) / 2);
				yStart = p.getY() + V_FREE_SPACE;

				Vector classesReplication = (Vector) classes.clone();
				Vector classesToBeDrawn = new Vector(0, 1);
				double max;
				double nJobs;
				for (int k = 0; k < MAX_NUMBER_OF_CLASSES_QUEUE; k++) {
					max = -1;
					int index = 0;

					for (int j = 0; j < classesReplication.size(); j++) {
						Object thisClass = classesReplication.get(j);
						nJobs = queueState.getValue(servers.get(i), thisClass);
						if (nJobs > max) {
							index = j;
							max = nJobs;
						}
					}
					classesToBeDrawn.add(classesReplication.get(index));
					classesReplication.remove(classesReplication.get(index));
				}

				for (int k = 0; k < nClasses; k++) {
					Object thisStation = classes.get(k);
					if (classesToBeDrawn.contains(thisStation)) {
						nJobs = queueState.getValue(servers.get(i), thisStation);

						int thisWidth = (int) ((nJobs / maxNJobs) * width);
						int thisHeight = (int) (height / MAX_NUMBER_OF_CLASSES_QUEUE);

						Color color = mediator.getClassDefinition().getClassColor(thisStation);
						g.setColor(color);
						xStart = p.getX() + offset + (int) (width - thisWidth) + H_FREE_SPACE;
						g.fill(new Rectangle2D.Double(xStart, yStart, thisWidth, thisHeight));
						yStart += thisHeight;
					}
				}
			}

			//then draw the utilization
			//if the number of classes is less then MAX_NUMBER_OF_CLASSES_QUEUE
			//offset =  (int) ( (getCellDimension(cell).getWidth() - QUEUE_WIDTH - QUEUE_HEIGHT)/2 + QUEUE_HEIGHT );
			int startAngle = 0;
			int arcDiameter = (int) (QUEUE_HEIGHT) - 2;
			xStart = p.getX() + offset + (int) QUEUE_WIDTH + 1;
			yStart = p.getY() + 1;
			g.setColor(Color.LIGHT_GRAY);
			g.fill(new Arc2D.Double(xStart, yStart, arcDiameter, arcDiameter, 0, 360, Arc2D.PIE));

			for (int j = 0; j < nClasses; j++) {
				double utilization = utilizationState.getValue(servers.get(i), classes.get(j));
				if (mediator.getStationDefinition().getStationNumberOfServers(servers.get(i)).intValue() > 1) {
					utilization = utilization / (mediator.getStationDefinition().getStationNumberOfServers(servers.get(i))).doubleValue();
				}
				int thisAngle = (int) (utilization * 360);

				Color color = mediator.getClassDefinition().getClassColor(classes.get(j));
				g.setColor(color);

				myFillArc(g, (int) xStart, (int) yStart, arcDiameter, startAngle, thisAngle);
				startAngle += thisAngle;
			}
		}
	}

	/**
	 * It acts like Graphics.fillArc(...), I have written this method
	 * since the predifined Java function presented strange behavior with
	 * very small angles
	 *
	 * @param g  the Graphics reference
	 * @param x  see Graphics.fillArc(...)
	 * @param y  see Graphics.fillArc(...)
	 * @param diameter  the diameter of the arc
	 * @param startAngle  the starting angle in degrees (0° is interpreted as 3 o'clock)
	 * @param angle  the angle width
	 */
	private void myFillArc(Graphics g, int x, int y, int diameter, int startAngle, int angle) {
		double c = 2 * Math.PI / 360;
		int radius = diameter / 2;
		double MIN_ANGLE = 1 * c;
		double nPoints = Math.round(angle * c / (float) MIN_ANGLE);
		double xStart = x + radius;
		double yStart = y + radius;
		double reachedAngle = startAngle * c;
		for (int i = 0; i < nPoints; i++) {
			double x1;
			double y1;
			double x2;
			double y2;
			x1 = radius * Math.cos(reachedAngle);
			y1 = radius * Math.sin(reachedAngle);
			x2 = radius * Math.cos(reachedAngle + MIN_ANGLE);
			y2 = radius * Math.sin(reachedAngle + MIN_ANGLE);
			x1 = xStart + x1;
			y1 = yStart - y1;
			x2 = xStart + x2;
			y2 = yStart - y2;
			x1 = Math.round(x1);
			y1 = Math.round(y1);
			x2 = Math.round(x2);
			y2 = Math.round(y2);
			int[] xValues = { (int) xStart, (int) x1, (int) x2 };
			int[] yValues = { (int) yStart, (int) y1, (int) y2 };
			g.fillPolygon(xValues, yValues, 3);
			reachedAngle += MIN_ANGLE;
		}
	}
}
