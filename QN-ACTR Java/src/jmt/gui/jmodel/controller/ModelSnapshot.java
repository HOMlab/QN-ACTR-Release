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

import java.util.HashMap;
import java.util.Iterator;
import java.util.Vector;

/**
 * <p>Title: Model Snapshot</p>
 * <p>Description: it is a snapshot of the model. It extends HashMap. </p>
 *
 * @author Francesco D'Aquino
 *         Date: 9-nov-2005
 *         Time: 12.03.53
 */
public class ModelSnapshot extends HashMap {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	Vector stations;
	Vector classes;
	double maxValue;

	public ModelSnapshot(Vector stationKeys, Vector classKeys) {
		stations = stationKeys;
		classes = classKeys;
	}

	public void put(Object stationKey, HashMap stationContent) {
		super.put(stationKey, stationContent);
	}

	public double getMaxValue() {
		return maxValue;
	}

	public void setMaxValue(double maxJobNumber) {
		maxValue = maxJobNumber;
	}

	public double getValue(Object stationKey, Object classKey) {
		double value = 0;
		HashMap map = (HashMap) this.get(stationKey);
		Object temp = map.get(classKey);
		if (temp instanceof Integer) {
			value = ((Integer) temp).doubleValue();
		} else if (temp instanceof Double) {
			value = ((Double) temp).doubleValue();
		}
		return value;
	}

	public Vector getServers() {
		return stations;
	}

	public void reset() {
		Iterator temp = this.keySet().iterator();
		while (temp.hasNext()) {
			this.remove(temp.next());
		}
		temp = null;
		this.clear();
	}
}
