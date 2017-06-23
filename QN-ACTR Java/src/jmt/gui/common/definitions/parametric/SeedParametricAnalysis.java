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
import java.util.TreeMap;
import java.util.Vector;

import jmt.engine.random.engine.MersenneTwister;
import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.definitions.SimulationDefinition;
import jmt.gui.common.definitions.StationDefinition;

/**
 * <p>Title: SeedParametricAnalysis</p>
 * <p>Description: This class is used to describe a parametric analysis where
 * the varied parameter is the seed of simulation. It may be used to have several
 * results of the same simulation.</p>
 *
 * @author Francesco D'Aquino
 *         Date: 26-feb-2006
 *         Time: 11.11.49
 */
public class SeedParametricAnalysis extends ParametricAnalysisDefinition {
	public static final int STEPS = 10; //It must be < than ParametricAnalysis.MAX_STEP_NUMBER
	public Vector<Number> values;

	public SeedParametricAnalysis(ClassDefinition cd, StationDefinition sd, SimulationDefinition simd) {
		type = PA_TYPE_SEED;
		numberOfSteps = STEPS;
		classDef = cd;
		stationDef = sd;
		simDef = simd;
		values = new Vector<Number>();
	}

	/**
	 * Gets the type of parametric analysis
	 *
	 * @return the type of parametric analysis
	 */
	@Override
	public String getType() {
		return type;
	}

	/**
	 * Changes the model preparing it for the next step
	 */
	@Override
	public void changeModel(int step) {
		Long nextSeed = (Long) values.get(step);
		simDef.setSimulationSeed(nextSeed);
	}

	/**
	 * Gets the maximum number of steps compatible with the model definition and the type of parametric analysis and in some implementation
	 * of this class initializes the <code>validGlobalPopulationSizes</code> Vector, containing the values that the parameter will assume.
	 *
	 * @return the maximum number of steps
	 */
	@Override
	public int searchForAvaibleSteps() {
		return Integer.MAX_VALUE;
	}

	/**
	 * Finds the set of possible values of the population on which the
	 * simulation may be iterated on.
	 */
	@Override
	public void createValuesSet() {
		MersenneTwister generator = new MersenneTwister();
		generator.setNewSeed(simDef.getSimulationSeed().longValue());
		for (int i = 0; i < numberOfSteps; i++) {
			values.add(new Long(generator.nextLong()));
		}
		originalValues = simDef.getSimulationSeed();
	}

	/**
	 * Restore the original values of the parameter
	 */
	@Override
	public void restoreOriginalValues() {
		simDef.setSimulationSeed((Long) originalValues);
	}

	/**
	 * Checks if the PA model is still coherent with simulation model definition. If
	 * the <code>autocorrect</code> variable is set to true, if the PA model is no more
	 * valid but it can be corrected it will be changed.
	 *
	 * @param autocorrect if true the PA model will be autocorrected
	 * @return 0 - If the PA model is still valid <br>
	 *         1 - If the PA model is no more valid, but it will be corrected <br>
	 *         2 - If the PA model can be no more used
	 */
	@Override
	public int checkCorrectness(boolean autocorrect) {
		if ((classDef.getClassKeys().isEmpty()) && (stationDef.getStationKeys().isEmpty())) {
			return 2;
		} else {
			return 0;
		}
	}

	/**
	 * Returns the values assumed by the varying parameter
	 *
	 * @return a Vector containing the values assumed by the varying parameter
	 */
	@Override
	public Vector<Number> getParameterValues() {
		return values;
	}

	/**
	 * Gets a Map containing for each property its value. The supported properties are
	 * defined as constants inside this class.
	 *
	 * @return a Map containing the value for each property
	 */
	@Override
	public Map<String, String> getProperties() {
		TreeMap<String, String> properties = new TreeMap<String, String>();
		properties.put(TYPE_PROPERTY, getType());
		properties.put(STEPS_PROPERTY, Integer.toString(numberOfSteps));
		return properties;
	}

	/**
	 * Sets the value for the specified property
	 *
	 * @param propertyName the name of the property to be set
	 * @param value        the value to be set
	 */
	@Override
	public void setProperty(String propertyName, String value) {
		if (propertyName.equals(STEPS_PROPERTY)) {
			numberOfSteps = Integer.parseInt(value);
		}
	}

	// ----------------------- USELESS METHODS -----------------------------
	/**
	 * Not implemented
	 *
	 * @return reference classKey
	 */
	@Override
	public Object getReferenceClass() {
		return null; //To change body of implemented methods use File | Settings | File Templates.
	}

	/**
	 * Not implemented
	 *
	 * @return the name of the class
	 */
	@Override
	public String getReferenceClassName() {
		return null; //To change body of implemented methods use File | Settings | File Templates.
	}
	// ---------------------- end USELESS METHODS --------------------------
}
