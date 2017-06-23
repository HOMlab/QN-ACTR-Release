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

import java.util.Map;
import java.util.Vector;

import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.definitions.SimulationDefinition;
import jmt.gui.common.definitions.StationDefinition;

/**
 * <p>Title: ParametricAnalysisDefinition</p>
 * <p>Description: this class represents a description of the parametric analysis
 * to be performed. It includes the initial and final value, and the number of
 * steps. Each type of parametric analysis is described by a subclass of this class.</p>
 *
 * @author Francesco D'Aquino
 *         Date: 14-dic-2005
 *         Time: 10.57.24
 */
public abstract class ParametricAnalysisDefinition implements ParametricAnalysis {
	public static final String TYPE_PROPERTY = "Type";
	public static final String FROM_PROPERTY = "From";
	public static final String TO_PROPERTY = "To";
	public static final String STEPS_PROPERTY = "Steps";
	public static final String IS_SINGLE_CLASS_PROPERTY = "Is single class";
	public static final String REFERENCE_CLASS_PROPERTY = "Class";
	public static final String REFERENCE_STATION_PROPERTY = "Station";

	protected String type; //The type of parametric analysis
	protected double initialValue; //The initial value of the parameter to be varied
	protected double finalValue; //The initial value of the parameter to be varied
	protected int numberOfSteps; //The maximum number of steps to be performed
	protected Vector possibleValues; //The set of values on which the simulation will be iterated on
	protected ClassDefinition classDef; //The class definition of the model
	protected StationDefinition stationDef; //The station definition of the model
	protected SimulationDefinition simDef; //The simulation definition of the model
	protected Object originalValues; //A structure containing the original values of the varying parameter

	/**
	 * Gets the type of parametric analysis
	 *
	 * @return the type of parametric analysis
	 */
	public abstract String getType();

	/**
	 * Gets the initial value of the parameter to be varied
	 *
	 * @return the initial value of the parameter to be varied
	 */
	public double getInitialValue() {
		return initialValue;
	}

	/**
	 * Gets the final value of the parameter to be varied
	 *
	 * @return the final value of the parameter to be varied
	 */
	public double getFinalValue() {
		return finalValue;
	}

	/**
	 * Gets the number of steps to be performed
	 *
	 * @return the maximum number of steps to be performed
	 */
	public int getNumberOfSteps() {
		return numberOfSteps;
	}

	/**
	 * Sets the initial value of the parameter to be varied
	 *
	 * @param initialValue
	 */
	public void setInitialValue(double initialValue) {
		if (this.initialValue != initialValue) {
			simDef.setSaveChanged();
		}
		this.initialValue = initialValue;
	}

	/**
	 * Sets the final value of the parameter to be varied
	 *
	 * @param finalValue
	 */
	public void setFinalValue(double finalValue) {
		if (this.finalValue != finalValue) {
			simDef.setSaveChanged();
		}
		this.finalValue = finalValue;
	}

	/**
	 * Sets the maximum number number of steps to be performed
	 *
	 * @param numberOfSteps
	 */
	public void setNumberOfSteps(int numberOfSteps) {
		if (this.numberOfSteps != numberOfSteps) {
			simDef.setSaveChanged();
		}
		this.numberOfSteps = numberOfSteps;
	}

	/**
	 * Changes the model preparing it for the next step
	 *
	 */
	public abstract void changeModel(int step);

	/**
	 * Gets the maximum number of steps compatible with the model definition and the type of parametric analysis and in some implementation
	 * of this class initializes the <code>validGlobalPopulationSizes</code> Vector, containing the values that the parameter will assume.

	 * @return the maximum number of steps
	 */
	public abstract int searchForAvaibleSteps();

	/**
	 * Finds the set of possible values of the parameter on which the
	 * simulation may be iterated on.
	 *
	 */
	public abstract void createValuesSet();

	/**
	 * Restore the original values of the parameter
	 */
	public abstract void restoreOriginalValues();

	/**
	 * Checks if the PA model is still coherent with simulation model definition. If
	 * the <code>autocorrect</code> variable is set to true, if the PA model is no more
	 * valid but it can be corrected it will be changed.
	 *
	 * @param autocorrect if true the PA model will be autocorrected
	 *
	 * @return 0 - If the PA model is still valid <br>
	 *         1 - If the PA model is no more valid, but it will be corrected <br>
	 *         2 - If the PA model can be no more used
	 */
	public abstract int checkCorrectness(boolean autocorrect);

	/**
	 * Returns the values assumed by the varying parameter
	 * @return a Vector containing the values assumed by the varying parameter
	 */
	public abstract Vector<Number> getParameterValues();

	/**
	 * Gets the reference class.
	 *
	 * @return reference classKey
	 */
	public abstract Object getReferenceClass();

	/**
	 * Get the reference class name
	 * @return the name of the class
	 */
	public abstract String getReferenceClassName();

	///**
	// * Marks the PA model as "modified"
	// * @param modified
	// */
	//public void setModified(boolean modified) {
	//    this.modified = modified;
	//}

	/**
	 * Gets a Map containing for each property its value. The supported properties are
	 * defined as constants inside this class.
	 * @return a Map containing the value for each property
	 */
	public abstract Map<String, String> getProperties();

	/**
	 * Sets the value for the specified property
	 * @param propertyName the name of the property to be set
	 * @param value the value to be set
	 */
	public abstract void setProperty(String propertyName, String value);
}
