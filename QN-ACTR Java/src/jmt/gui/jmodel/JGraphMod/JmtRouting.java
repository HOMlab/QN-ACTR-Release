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

import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.List;

import org.jgraph.graph.CellView;
import org.jgraph.graph.DefaultPort;
import org.jgraph.graph.Edge;
import org.jgraph.graph.EdgeView;
import org.jgraph.graph.GraphConstants;
import org.jgraph.graph.PortView;

/**

 * @author Federico Granata
 * Date: 28-nov-2003
 * Time: 12.05.46

 * Heavily modyfied by Bertoli Marco to support JGraph 5.8 - 21/mar/2006
 * Heavily modyfied by De Cicco Giuseppe & Fabio Granara to new JmtRouting - 23/sett/2006
 */
public class JmtRouting implements Edge.Routing {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private int offset = 15;//offset per il source
	private int offsetTo2 = -15; //offset per il target

	public List<Point2D> route(EdgeView edgeView) {
		List<Point2D> list = new ArrayList<Point2D>();
		int n = edgeView.getPointCount();
		Point2D from = edgeView.getPoint(0);

		if (edgeView.getSource() instanceof PortView) {
			from = ((PortView) edgeView.getSource()).getLocation();

		}
		Point2D to = edgeView.getPoint(n - 1);
		CellView trg = edgeView.getTarget();
		if (trg instanceof PortView) {

			to = ((PortView) trg).getLocation();
		}

		if (from != null && to != null) {
			JmtEdge edge = (JmtEdge) edgeView.getCell();//qua era defaultEdge l ho cambiato
			JmtCell source = (JmtCell) ((DefaultPort) edge.getSource()).getParent();

			JmtCell target = (JmtCell) ((DefaultPort) edge.getTarget()).getParent();

			Point2D[] routed;
			int offset = this.offset;
			int changeoff = edge.getOffset();

			int offsetTo2 = this.offsetTo2;
			//            int ringOffset=8;
			Rectangle2D sourceBounds = (Rectangle2D) source.getAttributes().get("bounds");
			Rectangle2D targetBounds = (Rectangle2D) target.getAttributes().get("bounds");

			// System.out.println("Punto del from: "+ from.getX()+", punto del bound: "+sourceBounds.getMaxX());
			int boundFrom = (int) (sourceBounds.getCenterX() + (sourceBounds.getWidth() / 2));
			int boundTo = (int) (targetBounds.getCenterX() - (targetBounds.getWidth() / 2));
			if (changeoff > 0 && source.isLeftInputCell()) {
				//           	System.out.println(changeoff);
				offset = changeoff + 15;
			}
			if (!source.isLeftInputCell()) {
				offset = -offset;
				//            	ringOffset=-ringOffset;
				boundFrom = (int) (sourceBounds.getCenterX() - (sourceBounds.getWidth() / 2));
			}

			if (!target.isLeftInputCell()) {
				boundTo = (int) (targetBounds.getCenterX() + (targetBounds.getWidth() / 2));
				offsetTo2 = -offsetTo2;
			}

			edge.setIsRing(false);
			if (source == target) {
				edge.setIsRing(true);
				//outoRing
				routed = new Point2D[4];
				routed[0] = new Point2D.Double(boundFrom + offset - offset / 3, from.getY());
				routed[1] = new Point2D.Double(routed[0].getX(), targetBounds.getMaxY() + 12);
				routed[2] = new Point2D.Double(boundTo - (offset), routed[1].getY());
				routed[3] = new Point2D.Double(routed[2].getX(), routed[0].getY());

			} else {
				//the source is on the left of the target
				if ((int) from.getY() == (int) to.getY()
						&& ((boundFrom < boundTo && offset > 0 && offsetTo2 < 0) || (boundFrom > boundTo && offset < 0 && offsetTo2 > 0))) {
					list.add(from);
					list.add(to);
					return list;
				}
				if ((((boundTo - boundFrom) > offset * (1.1)) && (offset > 0 && offsetTo2 < 0))
						|| (((boundFrom - boundTo) > (-offset * (1.1))) && (offset < 0 && offsetTo2 > 0))) {
					routed = new Point2D[2];
					routed[0] = new Point2D.Double(boundFrom + offset, from.getY());
					routed[1] = new Point2D.Double(routed[0].getX(), to.getY());

				}
				//                ____________inizio
				else if ((offset < 0 && offsetTo2 < 0)) {
					routed = new Point2D[2];
					if ((sourceBounds.getMaxX() < boundTo + offsetTo2 * 2 && boundFrom < boundTo && to.getY() < from.getY())
							|| (from.getY() < targetBounds.getMaxY() && sourceBounds.getMaxY() > to.getY())
							|| ((from.getY() >= targetBounds.getMinY() && from.getY() <= targetBounds.getMaxY()) && (sourceBounds.getMinY() <= targetBounds
									.getMaxY())) || (targetBounds.getMaxY() >= from.getY() && targetBounds.getMaxY() <= sourceBounds.getMaxY())) {
						routed = new Point2D[4];
						routed[0] = new Point2D.Double(boundFrom + offset, from.getY());
						routed[3] = new Point2D.Double(boundTo + offsetTo2, to.getY());

						routed[1] = new Point2D.Double(routed[0].getX(), sourceBounds.getMaxY() + 8);

						if (boundFrom > boundTo) {

							routed[1] = new Point2D.Double(routed[0].getX(), targetBounds.getMaxY() + 8);
						}
						routed[2] = new Point2D.Double(routed[3].getX(), routed[1].getY());
					} else {
						int minX = Math.min((boundFrom + offset - 2), (boundTo + (offsetTo2) - 6));
						if (minX == (boundTo + (offsetTo2) - 6)) {
							minX = minX + 4;
						}
						minX = minX + 2;

						routed[0] = new Point2D.Double(minX, from.getY());
						routed[1] = new Point2D.Double(routed[0].getX(), to.getY());
					}
				} else if ((offset > 0 && offsetTo2 > 0)) {
					routed = new Point2D[2];

					//                	int minY=0;

					if ((boundFrom > boundTo)
							&& ((targetBounds.getMaxY() > from.getY() && targetBounds.getMinY() < from.getY())
									|| (sourceBounds.getMaxY() <= targetBounds.getMaxY()) && ((sourceBounds.getMaxY() >= to.getY())) || ((sourceBounds
									.getMinY() >= targetBounds.getMinY()) && (sourceBounds.getMinY() <= to.getY())))) {
						routed = new Point2D[4];
						routed[0] = new Point2D.Double(boundFrom + offset, from.getY());
						routed[3] = new Point2D.Double(boundTo + offsetTo2, to.getY());

						routed[1] = new Point2D.Double(routed[0].getX(), (int) sourceBounds.getMaxY() + 2);
						routed[2] = new Point2D.Double(routed[3].getX(), (int) sourceBounds.getMaxY() + 2);

					} else if ((boundFrom <= boundTo)
							&& ((targetBounds.getMaxY() > from.getY() && targetBounds.getMinY() < from.getY()) || (from.getY() < to.getY() && boundFrom
									+ offset * 2 < targetBounds.getMinX()))) {

						routed = new Point2D[4];
						routed[0] = new Point2D.Double(boundFrom + offset, from.getY());
						routed[3] = new Point2D.Double(boundTo + offsetTo2, to.getY());
						//                    	if(from.getY()<targetBounds.getMinY()-1){
						//                    		
						//                    		routed[1]=new Point2D.Double(routed[0].getX(),from.getY());
						//                    		routed[2]=new Point2D.Double(routed[3].getX(),routed[1].getY());
						//                    	}else{
						routed[1] = new Point2D.Double(routed[0].getX(), targetBounds.getMaxY() + 2);
						routed[2] = new Point2D.Double(routed[3].getX(), routed[1].getY());
						//                    	}
					} else {

						int maxX = Math.max((boundFrom + offset + 2), (boundTo + (offsetTo2) + 6));

						if ((boundTo + (offsetTo2) + 6) == maxX) {
							maxX = maxX - 4;
						}
						maxX = maxX - 2;

						routed[0] = new Point2D.Double(maxX, from.getY());
						routed[1] = new Point2D.Double(routed[0].getX(), to.getY());
					}
				}

				else {

					if ((from.getY() + sourceBounds.getHeight() - 4) < (to.getY() - (targetBounds.getHeight() / 2))) {
						routed = new Point2D[4];
						routed[0] = new Point2D.Double(boundFrom + offset, from.getY());
						routed[3] = new Point2D.Double(boundTo + (offsetTo2), to.getY());
						double maxY = sourceBounds.getMaxY() + 20;
						routed[1] = new Point2D.Double(routed[0].getX(), maxY);
						routed[2] = new Point2D.Double(routed[3].getX(), maxY);
					} else {

						routed = new Point2D[4];
						routed[0] = new Point2D.Double(boundFrom + offset, from.getY());
						//                    System.out.println("Differenza "+ (sourceBounds.getMaxY()-(routed[0].getY() + sourceBounds.getBounds().getHeight()+2)));
						routed[3] = new Point2D.Double(boundTo + offsetTo2, to.getY());
						//                    double maxY = Math.max(routed[0].getY() + sourceBounds.getBounds().getHeight()+2,
						//                            routed[3].getY() + targetBounds.getBounds().getHeight()+8);
						double maxY = Math.max(sourceBounds.getMaxY() + 20, targetBounds.getMaxY() + 26);
						routed[1] = new Point2D.Double(routed[0].getX(), maxY);
						routed[2] = new Point2D.Double(routed[3].getX(), maxY);

						if (offset > 0) {
							double x;
							double left = sourceBounds.getX() - offset - 4;
							x = Math.min(routed[2].getX(), left);
							routed[2].setLocation(x, routed[2].getY());
							routed[3].setLocation(x, routed[3].getY());

							if (from.getY() < to.getY()) {
								double x2;
								double right = targetBounds.getX() + targetBounds.getWidth() + 15;
								x2 = Math.max(routed[0].getX(), right);
								routed[0].setLocation(x2, routed[0].getY());
								routed[1].setLocation(x2, routed[1].getY());
							}
						} else {
							double x;
							double left = sourceBounds.getX() + sourceBounds.getWidth() + 15 + 4;
							x = Math.max(routed[2].getX(), left);
							routed[2].setLocation(x, routed[2].getY());
							routed[3].setLocation(x, routed[3].getY());

							if (from.getY() < to.getY()) {
								double x2;
								double left2 = targetBounds.getX() - 15;
								x2 = Math.min(routed[0].getX(), left2);
								routed[0].setLocation(x2, routed[0].getY());
								routed[1].setLocation(x2, routed[1].getY());
							}
						}
					}
				}
			}
			//Sets add points
			list.add(from);
			for (Point2D element : routed) {
				list.add(element);
			}
			list.add(to);
		}
		return list;
	}

	public int getPreferredLineStyle(EdgeView edgeView) {

		return GraphConstants.STYLE_ORTHOGONAL;
	}
}
