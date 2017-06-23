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

import javax.swing.BorderFactory;
import javax.swing.border.TitledBorder;

import jmt.gui.jmodel.controller.Mediator;

import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.GraphConstants;

/**
 * <p>Title: Blocking Region Component</p>
 * <p>Description: This component is used to describe a blocking region inside a graph.
 * </p>
 *
 * @author Bertoli Marco
 *         Date: 23-mar-2006
 *         Time: 14.15.00
 */
public class BlockingRegion extends DefaultGraphCell {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	// Reference to Mediator
	private Mediator mediator;
	// Search's key for this blocking region
	private Object key;
	// Name of this region
	private String name;
	// border with name of this region
	private TitledBorder titledBorder;

	/**
	 * Creates a new blocking region cell to be placed inside current graph
	 * @param mediator reference to current mediator
	 * @param key search's key for this region
	 */
	public BlockingRegion(Mediator mediator, Object key) {
		this.mediator = mediator;
		this.key = key;
		name = mediator.getBlockingRegionDefinition().getRegionName(key);
		setLayout();
	}

	/**
	 * Define apparence of this component
	 */
	private void setLayout() {
		GraphConstants.setSizeable(attributes, false);
		GraphConstants.setGroupOpaque(attributes, true);
		GraphConstants.setOpaque(attributes, true);
		GraphConstants.setGradientColor(attributes, new Color(.1F, .1F, .9F, .3F));
		titledBorder = BorderFactory.createTitledBorder(" " + name + " ");
		GraphConstants.setBorder(attributes, titledBorder);
		GraphConstants.setInset(attributes, 15);
		GraphConstants.setEditable(attributes, false);
	}

	/**
	 * Adds a list of station to this blocking region and inserts this into current graph
	 * @param cells cells to be added
	 */
	public void addStations(Object[] cells) {
		mediator.getGraph().getGraphLayoutCache().insertGroup(this, cells);
		// Resets parent for added cells
		for (Object cell : cells) {
			if (cell instanceof JmtCell) {
				((JmtCell) cell).resetParent();
			}
		}
	}

	/**
	 * Return search's key for this component
	 * @return search's key for this blocking station
	 */
	public Object getKey() {
		return key;
	}

	/**
	 * Whenever name of this cell is asked, controls if displayed name is correct.
	 * if it's not, changes it. This is a bit dirty but avoids to add a special listener
	 * to this component that will be even more dirty.
	 * @return an empty String
	 */
	@Override
	public String toString() {
		String newName = mediator.getBlockingRegionDefinition().getRegionName(key);
		if (newName != null && !newName.equals(name)) {
			titledBorder.setTitle(" " + newName + " ");
			name = newName;
		}

		return "";
	}
}
