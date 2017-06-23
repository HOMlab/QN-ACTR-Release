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

package jmt.gui.common.routingStrategies;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Map;

/**
 * Created by IntelliJ IDEA.
 * User: orsotronIII
 * Date: 6-lug-2005
 * Time: 16.29.02
 * To change this template use Options | File Templates.
 * Modified by Bertoli Marco
 *
 *
 * Modified by Francesco D'Aquino
 */
public abstract class RoutingStrategy {
	protected static RoutingStrategy[] all = null; // Used to store all strategies

	protected String description;

	public abstract String getName();

	public String getDescription() {
		return description;
	}

	public abstract Map<Object, Double> getValues();

	@Override
	public String toString() {
		return getName();
	}

	@Override
	public abstract RoutingStrategy clone();

	@Override
	public boolean equals(Object o) {
		if (o instanceof RoutingStrategy) {
			return this.getName().equals(((RoutingStrategy) o).getName());
		} else {
			return false;
		}
	}

	public abstract String getClassPath();

	/**
	 * Return an array with an istance of every allowed Routing Strategy. Uses Reflection on
	 * <code>JMODELConstants</code> field to find all strategies and uses internal
	 * caching to search for strategiess only the first time that this method is called.
	 * <br>
	 * Author: Bertoli Marco
	 *
	 * @return an array with an istance of every allowed RoutingStrategy
	 */
	public static RoutingStrategy[] findAll() {
		if (all != null) {
			return all;
		}
		ArrayList<RoutingStrategy> strategies = new ArrayList<RoutingStrategy>();
		Field[] fields = jmt.gui.jmodel.JMODELConstants.class.getFields();
		try {
			for (Field field : fields) {
				if (field.getName().startsWith("ROUTING_")) {
					strategies.add((RoutingStrategy) field.get(null));
				}
			}
		} catch (IllegalAccessException ex) {
			System.err.println("A security manager has blocked reflection");
			ex.printStackTrace();
		}
		RoutingStrategy[] ret = new RoutingStrategy[strategies.size()];
		for (int i = 0; i < strategies.size(); i++) {
			ret[i] = strategies.get(i);
		}
		all = ret;
		return ret;
	}

	/**
	 * Returns true if the routing strategy is dependent from the state of
	 * the model
	 * @return  true if the routing strategy is dependent from the state of
	 * the model
	 *
	 * Author: Francesco D'Aquino
	 */
	public abstract boolean isModelStateDependent();
}
