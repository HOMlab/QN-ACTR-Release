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
import java.awt.event.MouseListener;

import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.SwingUtilities;
import javax.swing.TransferHandler;

import jmt.gui.jmodel.JGraphMod.BlockingRegion;
import jmt.gui.jmodel.JGraphMod.JmtCell;
import jmt.gui.jmodel.JGraphMod.JmtEdge;

import org.jgraph.event.GraphSelectionEvent;
import org.jgraph.event.GraphSelectionListener;
import org.jgraph.graph.GraphLayoutCache;
import org.jgraph.graph.GraphSelectionModel;
import org.jgraph.plaf.basic.BasicGraphUI;

/**

 * @author Federico Granata
 * Date: 15-lug-2003
 * Time: 16.12.00
 *
 * @author Bertoli Marco (29-mar-2006)

 */
public class JmtGraphUI extends BasicGraphUI {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	Mediator mediator;

	// Conti Andrea
	public JmtGraphUI(Mediator mediator) {
		setMediator(mediator);
	}

	//end

	/**
	 * Messages to stop the editing session. If the UI the receiver
	 * is providing the look and feel for returns true from
	 * <code>getInvokesStopCellEditing</code>, stopCellEditing will
	 * invoked on the current editor. Then completeEditing will
	 * be messaged with false, true, false to cancel any lingering
	 * editing.
	 */
	@Override
	public void completeEditing() {
		super.completeEditing();
	}

	public GraphLayoutCache getGraphLayoutCache() {
		return graphLayoutCache;
	}

	/**
	 * Creates the listener responsible for calling the correct handlers
	 * based on mouse events, and to select invidual cells.
	 */
	@Override
	protected MouseListener createMouseListener() {
		return null;
	}

	// Conti Andrea 29-08-2003
	/* We want some actions to be enabled/disabled in response to selection changes:
	    a good way to accomplish this is through a GraphSelectionListener */
	@Override
	protected GraphSelectionListener createGraphSelectionListener() {
		return new JmtGraphSelectionHandler();
	}

	/**
	 * A GraphSelectionHandler that can enable/disable UI actions
	 * @author ac
	 */
	public class JmtGraphSelectionHandler extends GraphSelectionHandler {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		@Override
		public void valueChanged(GraphSelectionEvent e) {
			super.valueChanged(e);
			// Bomb out with a ClassCastException in case the event's source is
			// something unexpected. Not robust, but a good debugging feature :)
			GraphSelectionModel sm = (GraphSelectionModel) e.getSource();

			// New --- Bertoli Marco ---
			// Checks selection cells to enables correct toolbar elements
			boolean foundStations = false;
			boolean cannotAddBlockingRegion = false;
			boolean foundConnection = false;
			Object[] cells = sm.getSelectionCells();
			for (Object cell : cells) {
				if (cell instanceof JmtCell) {
					foundStations = true;
					// If selected station is member of a BlockingRegion, signals it
					if (((JmtCell) cell).getParent() instanceof BlockingRegion) {
						cannotAddBlockingRegion = true;
					}
					// If selected cell cannot be added to a blocking region, signals it
					if (((JmtCell) cell).generateOrDestroyJobs()) {
						cannotAddBlockingRegion = true;
					}
				} else if (cell instanceof BlockingRegion) {
					cannotAddBlockingRegion = true;
				} else if (cell instanceof JmtEdge) {
					foundConnection = true;
				}
				// Shorten cycle to avoid useless computation
				if (cannotAddBlockingRegion && foundStations) {
					break;
				}
			}

			if (foundStations || cannotAddBlockingRegion) {
				mediator.enableCopyAction(true);
				mediator.enableCutAction(true);
				mediator.enableDeleteAction(true);
				mediator.enableAddBlockingRegion(!cannotAddBlockingRegion);

				//            	Giuseppe De Cicco & Fabio Granara
				mediator.enableRotateAction(true);
				mediator.enableSetRight(true);

			} else {
				mediator.enableCopyAction(false);
				mediator.enableCutAction(false);
				mediator.enableDeleteAction(false);
				mediator.enableAddBlockingRegion(false);
			}
			if (foundConnection) {
				mediator.enableDeleteAction(true);
				// End new --- Bertoli Marco ---
			}
		}
	}

	// end

	// Conti Andrea 29-08-2003
	// fix that wrong-selection-color-on-insert bug
	// btw, what is "LockedHandle" supposed to mean??? "Focused" maybe???
	@Override
	protected void installDefaults() {
		super.installDefaults();
		graph.setLockedHandleColor(graph.getHighlightColor());
		graph.setMarqueeColor(Color.GRAY); // make it a little more visible
	}

	//end

	// Conti Andrea 29-08-2003
	/* JGraph comes with a boilerplate(?) InputMap/ActionMap set containing some
	 "standard" keystroke->action mappings (see BasicGraphUI.installKeyboardActions).
	 Handy as it might look, this is quite a pain if you're trying to assign the
	 same keystrokes to your own actions through some other means, such as accelerator keys in
	 menuitems. In this case you have to remove the mappings from the graph's ActionMap or
	 the keystrokes will trigger the "default" actions instead of your ones.
	 */
	@Override
	protected void installKeyboardActions() {
		super.installKeyboardActions();
		ActionMap am = SwingUtilities.getUIActionMap(graph);

		am.remove(TransferHandler.getCutAction().getValue(Action.NAME));
		am.remove(TransferHandler.getCopyAction().getValue(Action.NAME));
		am.remove(TransferHandler.getPasteAction().getValue(Action.NAME));

		SwingUtilities.replaceUIActionMap(graph, am);

	}

	//end

	public void setMediator(Mediator mediator) {
		this.mediator = mediator;
	}

	/**
	 * Update the handle using createHandle.
	 */
	@Override
	public void updateHandle() {
		//    	System.out.println("UPDATEHANDLE IN JMTGRAPHUI");
		if (graphLayoutCache != null) {
			Object[] cells = graph.getSelectionCells();
			if (cells != null && cells.length > 0) {
				mediator.setHandle(createHandle(createContext(graph, cells)));
			} else if (mouseListener != null) {
				mediator.setHandle(null);
			}
		}
	}
}
