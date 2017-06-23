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

package jmt.gui.common.definitions.parametric;

import java.util.HashMap;
import java.util.Set;
import java.util.Vector;

import jmt.gui.common.definitions.ClassDefinition;

/**
 * <p>Title:</p>
 * <p>Description:</p>
 *
 * @author Francesco D'Aquino
 *         Date: 29-gen-2006
 *         Time: 21.51.35
 */
public interface ParametricAnalysis {
	public final String PA_TYPE_NUMBER_OF_CUSTOMERS = "Number of customers";
	public final String PA_TYPE_POPULATION_MIX = "Population mix";
	public final String PA_TYPE_SERVICE_TIMES = "Service times";
	public final String PA_TYPE_ARRIVAL_RATE = "Arrival rates";
	public final String PA_TYPE_SEED = "Seed";
	public final int MAX_STEP_NUMBER = 1000;

	/**
	* <p>Title: ValuesTable</p>
	* <p>Description: this class represents a table used to contain the values that the varying will assume
	*                during the parametric analysis. It contains a HashMap, used to contain a Vector for each
	*                class, containing the values that the parameter will assume step by step.</p>
	*
	* @author Francesco D'Aquino
	*         Date: 18-dic-2005
	*         Time: 09.09.09
	*/

	public class ValuesTable {
		HashMap<Object, Vector> table;
		ClassDefinition classDef;
		int numberOfSteps;
		Vector<Object> classSet;

		public ValuesTable(ClassDefinition cd, Vector<Object> classSet, int numberOfSteps) {
			this.numberOfSteps = numberOfSteps;
			this.classSet = classSet;
			table = new HashMap<Object, Vector>(numberOfSteps, 1);
			for (int i = 0; i < classSet.size(); i++) {
				table.put(classSet.get(i), new Vector(numberOfSteps));
			}
			classDef = cd;

		}

		/**
		 * Sets the next-step value for the parameter of class <code>classKey</code>.
		 * @param classKey the class of the parameter
		 * @param value the value it will assume
		 */
		public void setValue(Object classKey, double value) {
			Vector<Double> classValues = table.get(classKey);
			classValues.add(new Double(value));
		}

		/**
		 * Gets the value assumed by the parameter of class <code>classKey</code> at step <code>step</code>.
		 * @param classKey the class the parameter belongs to.
		 * @param step the step we are interested in.
		 * @return the value the parameter will assume.
		 */
		public double getValue(Object classKey, int step) {
			Vector classValues = table.get(classKey);
			return ((Double) (classValues.get(step))).doubleValue();
		}

		/**
		 * Gets the total number of steps inserted.
		 * @return the total number of steps inserted.
		 */
		public int getNumberOfInsertedSteps() {
			int steps = 0;
			if (!table.isEmpty()) {
				Vector temp = table.get(classSet.get(0));
				steps = temp.size();
			}
			return steps;
		}

		/**
		 * Gets the number of classes the table contains
		 * @return the number of contained classes
		 */
		public int getNumberOfClasses() {
			return table.keySet().size();
		}

		/**
		 * Returns a Set containing the class Keys
		 * @return a Set containing the class Keys
		 */
		public Set<Object> getClassKeySet() {
			return table.keySet();
		}
	}
}
