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
package jmt.gui.common.panels.parametric;

import java.awt.Color;
import java.util.Vector;

import javax.swing.JSplitPane;
import javax.swing.border.TitledBorder;

import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.definitions.SimulationDefinition;
import jmt.gui.common.definitions.StationDefinition;

/**
 * <p>Title: ParameterOptionPanel</p>
 * <p>Description: This is the superclass of all types of parameter option panel</p>
 *
 * @author Francesco D'Aquino
 *         Date: 9-mar-2006
 *         Time: 16.12.06
 */

public abstract class ParameterOptionPanel extends JSplitPane {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	Color DEFAULT_TITLE_COLOR = new TitledBorder("").getTitleColor();
	TitledBorder title = new TitledBorder("Parameter options");
	String DESCRIPTION;
	protected ClassDefinition cd;
	protected StationDefinition sd;
	protected SimulationDefinition simd;

	@Override
	public void setEnabled(boolean enabled) {
		if (enabled) {
			title.setTitleColor(DEFAULT_TITLE_COLOR);
		} else {
			title.setTitleColor(Color.LIGHT_GRAY);
		}
	}

	public Object getClassKey(String name) {
		Object key = null;
		Vector classes = cd.getClassKeys();
		for (int i = 0; i < classes.size(); i++) {
			if (cd.getClassName(classes.get(i)).equals(name)) {
				key = classes.get(i);
			}
		}
		return key;
	}

	public Object getStationKey(String name) {
		Object key = null;
		Vector stations = sd.getStationKeys();
		for (int i = 0; i < stations.size(); i++) {
			if (sd.getStationName(stations.get(i)).equals(name)) {
				key = stations.get(i);
			}
		}
		return key;
	}

}
