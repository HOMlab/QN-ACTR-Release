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

import java.awt.Cursor;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

import jmt.gui.jmodel.JGraphMod.BlockingRegion;
import jmt.gui.jmodel.JGraphMod.JmtCell;
import jmt.gui.jmodel.JGraphMod.JmtJGraph;

import org.jgraph.graph.CellView;
import org.jgraph.graph.GraphConstants;

/**
 * Handles all the events when the user is in the select mode
 *

 * @author dekkar (Federico Granata)
 * Date: Jun 20, 2003
 * Time: 10:42:28 AM


 * Modified by Bertoli Marco 28-giu-2005

 */
public class SelectState extends UIStateDefault {

	private Object[] cells = null;
	//		rappresentano l angolo altro-sinistro di ogni cella selezionata
	private Integer[] Xmin = null;
	private Integer[] Ymin = null;
	//		rappresentano l angolo basso-destro di ogni cella selezionata
	private Integer[] Xmax = null;
	private Integer[] Ymax = null;

	private boolean moved = false;
	private JmtJGraph graphtmp = null;

	protected GraphMouseListner ml;//refernce to mouse listner

	/** Creates the select state
	 *
	 * @param mediator
	 * @param ml
	 */
	public SelectState(Mediator mediator, GraphMouseListner ml) {
		super(mediator);
		this.ml = ml;
	}

	/**
	 * Handles press event, it selects the cell that is under the pointer
	 * if there is no cell deselects. There is also the possibility of
	 * activating the marquee handler
	 *
	 * @param e press mouse event
	 */
	@Override
	public void handlePress(MouseEvent e) {
		ml.setHandler(null);

		if (!e.isConsumed() && mediator.isGraphEnabled()) {
			mediator.graphRequestFocus();
			int s = mediator.getTolerance();
			Rectangle2D r = mediator.fromScreen(new Rectangle(e.getX() - s, e.getY() - s, 2 * s, 2 * s));
			Point2D point = mediator.fromScreen(new Point(e.getPoint()));
			if (!(ml.getFocus() != null && ml.getFocus().intersects(mediator.getGraph(), r))) {
				ml.setFocus(null);
			}
			// Avoid toggling of selection between inner components and blocking region
			CellView next = mediator.getNextViewAt(ml.getFocus(), point.getX(), point.getY());
			if (next != null && next.getCell() != null) {
				if (!(ml.getFocus() != null && next.getCell() instanceof BlockingRegion)) {
					ml.setCell(next);
				}
			}
			if (ml.getFocus() == null) {
				ml.setFocus(ml.getCell());
			}

			if (!mediator.isForceMarqueeEvent(e)) {
				if (e.getClickCount() == mediator.getEditClickCount() && ml.getFocus() != null
				//&& ml.getFocus().isLeaf()
				//&& ml.getFocus().getParentView() == null
				) {
					// Start Editing Only if cell is editable - BERTOLI MARCO
					if (mediator.isCellEditable(ml.getFocus().getCell())) {
						ml.handleEditTrigger(ml.getFocus().getCell());
						e.consume();
						ml.setCell(null);
					} // Otherwise do nothing - BERTOLI MARCO
					else {
						e.consume();
					}
				} else if (!mediator.isToggleSelectionEvent(e)) {
					if (ml.getHandle() != null) {
						ml.setHandler(ml.getHandle());
						ml.getHandle().mousePressed(e);
					}
					// Immediate Selection
					if (!e.isConsumed() && ml.getCell() != null && !mediator.isCellSelected(ml.getCell())) {
						mediator.selectCellForEvent(ml.getCell().getCell(), e);
						ml.setFocus(ml.getCell());
						if (ml.getHandle() != null) {
							ml.getHandle().mousePressed(e);
							ml.setHandler(ml.getHandle());
						}
						e.consume();
						ml.setCell(null);
					}
				}
			}

			//Marquee Selection
			if (!e.isConsumed() && (!mediator.isToggleSelectionEvent(e) || ml.getFocus() == null)) {
				if (ml.getMarquee() != null) {
					ml.getMarquee().mousePressed(e);
					ml.setHandler(ml.getMarquee());
				}
			}
		}

	}

	@Override
	public void handleMove(MouseEvent e) {

		if (ml.getPreviousCursor() == null) {
			ml.setPreviousCursor(mediator.getGraphCursor());
		}
		if (mediator.isGraphEnabled()) {
			if (ml.getMarquee() != null) {
				ml.getMarquee().mouseMoved(e);
			}
			if (ml.getHandle() != null) {
				ml.getHandle().mouseMoved(e);
			}
			if (!e.isConsumed() && ml.getPreviousCursor() != null) {
				mediator.setGraphCursor(ml.getPreviousCursor());
				ml.setPreviousCursor(null);
			}

		}

		e.consume();
	}

	@Override
	public void handleDrag(MouseEvent e) {

		mediator.setIsReleased(false);
		mediator.autoscroll(e.getPoint());
		if (ml.getHandler() != null && ml.getHandler() == ml.getMarquee()) {
			ml.getMarquee().mouseDragged(e);
		} else if (ml.getHandler() == null && !mediator.isGraphEditing() && ml.getFocus() != null) {
			if (!mediator.isCellSelected(ml.getFocus().getCell())) {
				mediator.selectCellForEvent(ml.getFocus().getCell(), e);
				ml.setCell(null);
			}
			if (ml.getHandle() != null) {
				ml.getHandle().mousePressed(e);
			}
			ml.setHandler(ml.getHandle());
		}
		if (ml.getHandle() != null && ml.getHandler() == ml.getHandle()) {
			// BERTOLI MARCO - Added to avoid dragging of unselected elements (caused bugs)
			if (mediator.getGraph().getSelectionCells().length > 0) {
				ml.getHandle().mouseDragged(e);
				Xmin = null;
				Ymin = null;
				Xmax = null;
				Ymax = null;
				graphtmp = (JmtJGraph) mediator.getGraph();
				cells = graphtmp.getSelectionCells();
				Xmin = new Integer[cells.length];
				Ymin = new Integer[cells.length];
				Xmax = new Integer[cells.length];
				Ymax = new Integer[cells.length];
				//System.out.println("Numero di cell  in drag: " + cells.length);
				if (cells.length > 0) {
					for (int i = 0; i < cells.length; i++) {
						if (cells[i] instanceof JmtCell) {
							Rectangle2D rett = GraphConstants.getBounds(((JmtCell) cells[i]).getAttributes());
							//                		rappresentano l angolo altro-sinistro di ogni cella selezionata
							Xmin[i] = new Integer((int) rett.getMinX());
							Ymin[i] = new Integer((int) rett.getMinY());
							Xmax[i] = new Integer((int) rett.getMaxX());
							Ymax[i] = new Integer((int) rett.getMaxY());
							moved = true;
						}
						if (cells[i] instanceof BlockingRegion) {
							CellView groupview = (graphtmp.getGraphLayoutCache()).getMapping(cells[i], false);
							Rectangle2D rett2 = groupview.getBounds();

							Object[] celgru = new Object[1];
							celgru[0] = cells[i];
							//celle presenti nel blocking region incluse port e regione

							//                			Object[] celless=graphtmp.getDescendants(celgru);
							//System.out.println("Numero di celle in gruppo: "+ celless.length);

							if (rett2 != null) {
								//System.out.println("Dentro DRAG mi restituisce un rettangolo se è di tipoBLockingRegion");
								Xmin[i] = new Integer((int) rett2.getMinX());
								Ymin[i] = new Integer((int) rett2.getMinY());
								//                        		Xmax[i]=new Integer((int) rett2.getMaxX());
								//                        		Ymax[i]=new Integer((int) rett2.getMaxY());
								//System.out.println("Valori: "+ X[i]+", "+Y[i]);
								moved = true;
							}

						}
					}
				}
			}
		}

	}

	//	Heavely modified by Giuseppe De Cicco & Fabio Granara
	@Override
	public void handleRelease(MouseEvent e) {
		mediator.setIsReleased(true);

		try {
			if (e != null && !e.isConsumed()) {
				if (ml.getHandler() == ml.getMarquee() && ml.getMarquee() != null) {
					ml.getMarquee().mouseReleased(e);
				} else if (ml.getHandler() == ml.getHandle() && ml.getHandle() != null) {
					ml.getHandle().mouseReleased(e);
				}
				if (ml.isDescendant(ml.getCell(), ml.getFocus()) && e.getModifiers() != 0) {
					// Do not switch to parent if Special Selection
					ml.setCell(ml.getFocus());

				}

				// Puts selected cells in good place to avoid overlapping
				if (moved && Xmin.length > 0 && Ymin.length > 0) {
					//                	System.out.println("chiamata in select state");
					mediator.putSelectedCellsInGoodPlace(cells, Xmin, Ymin);

					mediator.avoidOverlappingCell(cells);

					Xmin = null;
					Ymin = null;
					moved = false;

				}

				if (!e.isConsumed() && ml.getCell() != null) {
					Object tmp = ml.getCell().getCell();
					boolean wasSelected = mediator.isCellSelected(tmp);
					mediator.selectCellForEvent(tmp, e);
					ml.setFocus(ml.getCell());
					ml.postProcessSelection(e, tmp, wasSelected);
				}
				// Notify mediator that object can have been placed inside or
				// ouside a blocking region
				mediator.handlesBlockingRegionDrag();

			}
		} finally {
			ml.setHandler(null);
			ml.setCell(null);

		}

	}

	@Override
	public void handleEnter(MouseEvent e) {
		mediator.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
	}

}
