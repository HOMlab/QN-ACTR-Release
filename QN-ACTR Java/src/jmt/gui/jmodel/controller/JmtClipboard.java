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

import java.awt.geom.Point2D;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

import jmt.gui.common.routingStrategies.ProbabilityRouting;
import jmt.gui.common.routingStrategies.RoutingStrategy;
import jmt.gui.jmodel.JGraphMod.CellComponent;
import jmt.gui.jmodel.JGraphMod.JmtCell;
import jmt.gui.jmodel.JGraphMod.JmtEdge;
import jmt.gui.jmodel.definitions.JmodelStationDefinition;

import org.jgraph.JGraph;
import org.jgraph.graph.DefaultGraphCell;

/**
 * <p>Title: Jmt Clipboard</p>
 * <p>Description: This class provides clipboard facility to copy and paste selected sections
 * of current model. It will operate both on data structure (jmodelStationDefinition) end
 * on graphic rapresentation (Jgraph through mediator calls).</p>
 * 
 * @author Bertoli Marco
 *         Date: 21-giu-2005
 *         Time: 11.49.57
 */
public class JmtClipboard {
	protected Mediator mediator;
	protected HashMap<Object, Object> stations;
	protected HashMap<Object, Point2D> stationpositions;
	protected Vector<Connection> links;
	protected Point2D zero;

	/**
	 * When trying to paste over existing cells, try to move of 'move' down and right
	 */
	protected static final int move = 20;

	/**
	 * Initializes a new JmtClipboard with given mediator
	 * @param m Mediator to be referenced
	 */
	public JmtClipboard(Mediator m) {
		this.mediator = m;
	}

	/**
	 * Flushes this clipboard. This method will be called upon copy and can be called whenever
	 * user wants to flush clipboard
	 */
	public void flush() {
		stations = new HashMap<Object, Object>();
		stationpositions = new HashMap<Object, Point2D>();
		links = new Vector<Connection>();
		zero = null;
	}

	/**
	 * Copy current selected Stations and Edges into clipboard
	 */
	public void copy() {
		flush();
		// Gets selected cells
		JmodelStationDefinition sd = mediator.getStationDefinition();
		JGraph graph = mediator.getGraph();
		Object cells[] = graph.getDescendants(graph.getSelectionCells());
		// Temp variables
		Object key; // Station key
		JmtEdge edgetmp;
		JmtCell celltmp;
		Point2D location; // position of a station
		// Saves into data structure selected stations (including position) and links
		for (Object cell : cells) {
			if (cell instanceof JmtCell) {
				celltmp = (JmtCell) cell;
				key = ((CellComponent) celltmp.getUserObject()).getKey();
				stations.put(key, sd.serializeStation(key));
				location = mediator.getCellCoordinates(celltmp);
				stationpositions.put(key, location);
				// Initialize 'zero' as the upper-leftmost point of selected stations
				// Will be used as a bias while pasting
				if (zero == null) {
					zero = location;
				}
				if (zero.getX() > location.getX()) {
					zero = new Point2D.Double(location.getX(), zero.getY());
				}
				if (zero.getY() > location.getY()) {
					zero = new Point2D.Double(zero.getX(), location.getY());
				}
			} else if (cell instanceof JmtEdge) {
				edgetmp = (JmtEdge) cell;
				links.add(new Connection(edgetmp.getSourceKey(), edgetmp.getTargetKey()));
			}
		}
	}

	/**
	 * Copy current selected Stations and Edges into clipboard and deletes them from current graph.
	 * It simply calls copy() and then mediator.deleteSelected()
	 */
	public void cut() {
		copy();
		mediator.deleteSelected();
	}

	/**
	 * Pastes previously copied elements into graph, beginning from the point indicated by where
	 * and constructing new data structures.
	 * @param where upper-left point of the region where pasted components will be put.
	 */
	public void paste(Point2D where) {
		HashMap<Object, Object> tempkey = new HashMap<Object, Object>(); // Used as a translator from old key to new one to paste correct links
		HashMap<Object, JmtCell> newstations = new HashMap<Object, JmtCell>(); // Used to store newly created stations
		Vector<DefaultGraphCell> select = new Vector<DefaultGraphCell>(); // Elements to be selected after paste
		// Temp variables
		JmtCell newcell; //New created cell
		JmtEdge newEdge; //New created edge (link)
		Object newkey; // New station key
		Object oldkey; // Old station key
		Iterator<Object> keys; // All keys in Hashmap stations
		Point2D oldpos; // Old station position
		Point2D newpos; // New station position
		if (stations == null || stations.size() == 0) {
			return;
		}

		JmodelStationDefinition sd = mediator.getStationDefinition();

		// Recreates all stations and position them
		keys = stations.keySet().iterator();
		while (keys.hasNext()) {
			oldkey = keys.next();
			// Creates a new station with parameters got from previous copy operation
			newkey = sd.deserializeStation(stations.get(oldkey));
			newcell = mediator.getCellFactory().createCell(sd.getStationType(newkey) + "Cell", new CellComponent(newkey, sd));
			tempkey.put(oldkey, newkey);
			// Calculates where this station should be put
			oldpos = stationpositions.get(oldkey);
			newpos = new Point2D.Double(where.getX() + oldpos.getX() - zero.getX(), where.getY() + oldpos.getY() - zero.getY());
			// Insert created station into JGraph. Finds the first empty position going
			// down and right with respect of original position
			while (mediator.overlapCells(newpos, newcell)) {
				newpos.setLocation(newpos.getX() + move, newpos.getY() + move);
			}
			mediator.InsertCell(newpos, newcell);
			newstations.put(newkey, newcell);
			select.add(newcell);
		}

		Object sourceKey;
		Object targetKey;

		// Creates new links and show them on graph
		for (int i = 0; i < links.size(); i++) {
			// Translates old key values in new ones
			sourceKey = tempkey.get(links.get(i).sourceKey);
			targetKey = tempkey.get(links.get(i).targetKey);
			newEdge = mediator.connect(newstations.get(sourceKey), newstations.get(targetKey));
			if (newEdge != null) {
				select.add(newEdge);
			}
		}

		// Now adjusts Empirical Routing (if any) - Now named Probability Routing
		keys = tempkey.keySet().iterator();
		Object[] classes;
		RoutingStrategy rs;
		Map oldRouting, newRouting;
		// Search in every new inserted station
		while (keys.hasNext()) {
			newkey = tempkey.get(keys.next());
			classes = mediator.getClassDefinition().getClassKeys().toArray();
			for (Object classe : classes) {
				rs = (RoutingStrategy) mediator.getStationDefinition().getRoutingStrategy(newkey, classe);
				if (rs instanceof ProbabilityRouting) {
					oldRouting = rs.getValues();
					newRouting = new HashMap();
					// For each old destination, set new one if it was copied with
					Object[] oldDest = oldRouting.keySet().toArray();
					for (Object element : oldDest) {
						// Now checks if target station was copied with source one and a link connecting them exists
						if (tempkey.containsKey(element) && mediator.getStationDefinition().areConnected(newkey, tempkey.get(element))) {
							newRouting.put(tempkey.get(element), oldRouting.get(element));
						}
					}
					rs.getValues().clear();
					rs.getValues().putAll(newRouting);
				}
			}
		}

		// Selects every inserted element
		mediator.getGraph().setSelectionCells(select.toArray());
	}

	/**
	 * Pastes previously copied elements into graph, constructing new data structures.
	 * Elements are copied near original ones.
	 */
	public void paste() {
		paste(zero);
	}

	/**
	 * Inner class used to store pairs of sourceKey/targetKey objects.
	 */
	protected class Connection {
		protected Object sourceKey;
		protected Object targetKey;

		public Connection(Object sourceKey, Object targetKey) {
			this.sourceKey = sourceKey;
			this.targetKey = targetKey;
		}
	}
}
