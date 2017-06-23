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

import java.awt.Graphics;

import jmt.common.GlobalSettings;
import jmt.gui.common.Defaults;
import jmt.gui.jmodel.controller.Mediator;

import org.freehep.graphics2d.VectorGraphics;
import org.freehep.util.export.ExportDialog;
import org.jgraph.JGraph;
import org.jgraph.graph.BasicMarqueeHandler;
import org.jgraph.graph.CellView;
import org.jgraph.graph.DefaultCellViewFactory;
import org.jgraph.graph.DefaultGraphModel;
import org.jgraph.graph.DefaultGraphSelectionModel;
import org.jgraph.graph.GraphLayoutCache;
import org.jgraph.graph.GraphModel;

/** Jmt version of JGraph.

 * @author Federico Granata
 * Date: 11-lug-2003
 * Time: 10.38.27

 * Modyfied by Bertoli Marco to support JGraph 5.8 - 21/mar/2006
 * Added Blocking Region Support 24-mar-2006

 * @author Bertoli Marco (16-jun-2006) Added support for screenshots in vectorial format.

 */
public class JmtJGraph extends JGraph {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	static DefaultCellViewFactory cellViewFactory = new DefaultCellViewFactory();

	// Conti Andrea 29-08-2003
	// added a Mediator reference with get/set and constructor parameters.

	protected Mediator mediator;

	/**
	 * Returns a <code>JGraph</code> with a sample model.
	 */
	public JmtJGraph(Mediator m) {
		this(null, m);
	}

	/**
	 * Returns an instance of <code>JGraph</code> which displays the
	 * the specified data model.
	 *
	 * @param model  the <code>GraphModel</code> to use as the data model
	 */
	public JmtJGraph(GraphModel model, Mediator m) {
		this(model, (GraphLayoutCache) null, m);
	}

	/**
	 * Returns an instance of <code>JGraph</code> which displays
	 * the specified data model using the specified view.
	 *
	 * @param model  the <code>GraphModel</code> to use as the data model
	 * @param mh  the <code>MarqueeHandler</code>
	 */
	public JmtJGraph(GraphModel model, BasicMarqueeHandler mh, Mediator m) {
		this(model, null, mh, m);
	}

	/**
	 * Returns an instance of <code>JGraph</code> which displays
	 * the specified data model using the specified view.
	 *
	 * @param model  the <code>GraphModel</code> to use as the data model
	 * @param view  the <code>GraphLayoutCache</code> to use as the view
	 */
	public JmtJGraph(GraphModel model, GraphLayoutCache view, Mediator m) {
		this(model, view, new BasicMarqueeHandler(), m);
	}

	/**
	 * Returns an instance of <code>JGraph</code> which displays
	 * the specified data model using the specified view.
	 *
	 * @param model  the <code>GraphModel</code> to use as the data model
	 * @param view  the <code>GraphLayoutCache</code> to use as the view
	 */
	public JmtJGraph(GraphModel model, GraphLayoutCache view, BasicMarqueeHandler mh, Mediator m) {
		setMediator(m);
		selectionModel = new DefaultGraphSelectionModel(this);
		setLayout(null);
		marquee = mh;
		if (view == null) {
			view = createDefaultGraphView(this);
		}
		setGraphLayoutCache(view);
		updateUI();
		if (model == null) {
			model = new DefaultGraphModel();
			setModel(model);
			addSampleData(model);
		} else {
			setModel(model);
		}
		setDoubleBuffered(true);
		// Properties for Blocking Region
		setMoveIntoGroups(true);
		setMoveOutOfGroups(true);
	}

	/**
	 * Sets the <code>Mediator</code> for this object
	 * @param mediator
	 */
	public void setMediator(Mediator mediator) {
		this.mediator = mediator;
	}

	/**
	 * @return the <code>Mediator</code> for this object
	 */
	public Mediator getMediator() {
		return mediator;
	}

	/**
	 * Creates and returns a default <code>GraphLayoutCache</code>.
	 *
	 * @return the default <code>GraphLayoutCache</code>
	 */
	protected static GraphLayoutCache createDefaultGraphView(JGraph graph) {
		return new JmtGraphLayoutCache(graph.getModel(), cellViewFactory);
	}

	/** Gets the inport port for the cell under Point(x,y)
	 *
	 * @param x  horizontal coordinate
	 * @param y  vertical coordinate
	 * @return the inport port if available else null
	 */
	public InputPort getInPortAt(int x, int y) {
		JmtCell cell = getVertexAt(x, y);
		for (int i = 0; i < getModel().getChildCount(cell); i++) {
			Object child = getModel().getChild(cell, i);
			if (child instanceof InputPort) {
				return (InputPort) child;
			}
		}
		return null;
	}

	/** Gets the outport port for the cell under Point(x,y)
	 *
	 * @param x  horizontal coordinate
	 * @param y  vertical coordinate
	 * @return the outport port if available else null
	 */
	public OutputPort getOutPortAt(int x, int y) {
		Object cell = getVertexAt(x, y);
		if (cell instanceof JmtCell) {
			for (int i = 0; i < getModel().getChildCount(cell); i++) {
				Object child = getModel().getChild(cell, i);
				if (child instanceof OutputPort) {
					return (OutputPort) child;
				}
			}
		}
		return null;
	}

	/** Gets the first vertex at this position.
	 *
	 * @param x horizontal coordinate
	 * @param y vertical coordinate
	 * @return first vertex
	 */
	public JmtCell getVertexAt(int x, int y) {
		CellView[] cellViews = getGraphLayoutCache().getAllViews();
		for (CellView cellView : cellViews) {
			if (cellView.getCell() instanceof JmtCell) {
				if (cellView.getBounds().contains(x, y)) {
					return (JmtCell) cellView.getCell();
				}
			}
		}
		return null;
		//		Object cell = null;
		//		Object oldCell = null;
		//		do {
		//			cell = getNextCellForLocation(oldCell, x, y);
		//			if (oldCell == cell)
		//				return null;
		//			else
		//				oldCell = cell;
		//		} while (!((cell instanceof JmtCell) || cell == null));

	}

	// Conti Andrea 29-08-2003
	/**
	 * Notification from the <code>UIManager</code> that the L&F has changed.
	 * Replaces the current UI object with the latest version from the
	 * <code>UIManager</code>. Subclassers can override this to support
	 * different GraphUIs.
	 * @see javax.swing.JComponent#updateUI
	 *
	 */
	@Override
	public void updateUI() {
		setUI(new jmt.gui.jmodel.controller.JmtGraphUI(mediator));
		invalidate();
	}

	// end

	// --- Methods used to catch ScreenShots in vectorial format --------------------------------
	/**
	 * Overrides paintComponent method to add support for screenshots
	 */
	@Override
	protected void paintComponent(Graphics g) {
		if (g == null) {
			return;
		}

		VectorGraphics vg = VectorGraphics.create(g);
		super.paintComponent(vg);
	}

	/**
	 * Shows a screenshot dialog used to select screenshot format
	 */
	public void showScreenShotDialog() {
		ExportDialog export = new ExportDialog("Java Modelling Tools - version " + GlobalSettings.getSetting(GlobalSettings.VERSION));
		export.setUserProperties(Defaults.getProperties());
		export.showExportDialog(this, "Export as image...", this, "graph");
		Defaults.save();
	}

	// ------------------------------------------------------------------------------------------

}
