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

package jmt.gui.jmodel.controller;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Graphics2D;
import java.awt.event.MouseEvent;
import java.awt.geom.Line2D;
import java.awt.geom.Point2D;

import jmt.gui.jmodel.JGraphMod.JmtCell;

import org.jgraph.graph.PortView;
import org.jgraph.graph.VertexView;

//import org.jgraph.JGraph;

/**

 * @author Federico Granata
 * Date: 14-lug-2003
 * Time: 16.59.19

 */
public class ConnectState extends UIStateDefault {

	protected JmtCell startPoint;

	protected Point2D start, current;
	//	public  boolean isReleased=false;
	protected PortView port, firstPort, lastPort;
	//private JmtCell aa;
	//private JmtCell bb;
	//private boolean flag = false;
	protected GraphMouseListner ml;

	protected boolean pressed = false;//if the button isn't pressed

	public ConnectState(Mediator mediator, GraphMouseListner ml) {
		super(mediator);
		this.ml = ml;
	}

	@Override
	public void handlePress(MouseEvent e) {

		if (!e.isConsumed()) {
			start = mediator.snap(e.getPoint());
			firstPort = port = getOutPortViewAt(e.getX(), e.getY());

			if (firstPort != null) {
				start = mediator.toScreen(firstPort.getLocation(null));
				////				Giuseppe De Cicco & Fabio Granara
				//				aa = (JmtCell)((OutputPort)(firstPort.getCell())).getParent();
				//				aa.okout=true;
			}
			e.consume();
		}
		pressed = true;
	}

	@Override
	public void handleExit(MouseEvent e) {
		//		super.handleExit(e);
		mediator.setCursor(mediator.getOldCursor());
	}

	@Override
	public void handleEnter(MouseEvent e) {

		//		super.handleEnter(e);
		mediator.setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
	}

	@Override
	public void handleDrag(MouseEvent e) {

		if (firstPort != null) {
			if (!e.isConsumed()) {
				Graphics2D g = mediator.getGraphGraphics();
				Color bg = mediator.getGraphBackground();
				Color fg = Color.black;
				g.setColor(fg);
				g.setXORMode(bg);
				overlay(g);

				current = mediator.snap(e.getPoint());

				port = getInPortViewAt(e.getX(), e.getY());
				if (port != null) {
					current = mediator.toScreen(port.getLocation(null));
				}

				g.setColor(bg);
				g.setXORMode(fg);
				overlay(g);
				e.consume();
			}
		}

	}

	//	heavely modified by Giuseppe De Cicco & Fabio Granara
	@Override
	public void handleRelease(MouseEvent e) {

		if (e != null && !e.isConsumed()) {
			PortView end = getInPortViewAt(e.getX(), e.getY());

			if (end != null) {
				mediator.connect(start, current, end, firstPort);

				//				bb = (JmtCell)((InputPort)(end.getCell())).getParent();
				//				bb.okin = true;
				//				flag=true;

				if ((firstPort != null) && ((VertexView) (firstPort.getParentView()) != null)) {
					if ((JmtCell) ((VertexView) (firstPort.getParentView())).getCell() != null) {
						JmtCell cell = (JmtCell) ((VertexView) (firstPort.getParentView())).getCell();

						mediator.avoidOverlappingCell(new JmtCell[] { cell });

					}
				}

				mediator.getGraph().getGraphLayoutCache().reload();
			}

			e.consume();
			mediator.graphRepaint();

		}

		//		if(flag){
		//			if((aa!=null) && (bb!=null)){
		//			if(aa.okout && bb.okin && !mediator.no){
		//				aa.AddOut();
		//				bb.AddIn();
		//			}else
		//				mediator.no = true;
		//		
		//
		//		aa.okout = bb.okin = false;
		//		mediator.no = true;
		//			}
		//		}
		//		
		//		flag = false;
		//		aa = null;
		//		bb = null;
		firstPort = null;
		port = null;
		start = null;
		current = null;

	}

	/** gets the first portView of the input port of the cell at position
	 *
	 * @param x
	 * @param y
	 * @return portView of the input port
	 */
	protected PortView getInPortViewAt(int x, int y) {
		return mediator.getInPortViewAt(x, y);
	}

	/** gets the first portView of the output port of the cell at position
	 *
	 * @param x
	 * @param y
	 * @return portView of the output port
	 */
	protected PortView getOutPortViewAt(int x, int y) {
		return mediator.getOutPortViewAt(x, y);
	}

	public void overlay(Graphics2D g) {
		if (start != null) {
			if (current != null) {
				g.draw(new Line2D.Double(start.getX(), start.getY(), current.getX(), current.getY()));
			}
		}
	}

}
