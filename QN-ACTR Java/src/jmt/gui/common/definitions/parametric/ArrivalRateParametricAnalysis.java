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

import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.definitions.SimulationDefinition;
import jmt.gui.common.definitions.StationDefinition;
import jmt.gui.common.distributions.Distribution;

/**
 * <p>Title: ArrivalRateParametricAnalysis</p>
 * <p>Description: this class is used to describe a parametric analysis where
 * the varied parameter is the mean value of the arrival rate of an/all open
 * classes. It adds the <code >classKey</code> field used to keep the key of
 * the Job-Class whose service time will be varied, and a boolean value
 * <code>singleClass</code> used to choose the type of service time growth
 * (single or all class).</p>
 *
 * @author Francesco D'Aquino
 *         Date: 14-dic-2005
 *         Time: 12.22.28
 */
public class ArrivalRateParametricAnalysis extends ParametricAnalysisDefinition {
	private final double FROM_ALL = 100;
	private final double TO_ALL = 150;
	private final double INCREMENT_SINGLE = 2;
	private final int STEPS = 10;
	private final boolean SINGLE_CLASS = false;

	private boolean singleClass;
	private Object classKey;

	private Vector<Object> avaibleClasses;
	private Object values;

	public ArrivalRateParametricAnalysis(ClassDefinition cd, StationDefinition sd, SimulationDefinition simd) {
		type = PA_TYPE_ARRIVAL_RATE;
		numberOfSteps = STEPS;
		classDef = cd;
		stationDef = sd;
		simDef = simd;
		Vector<Object> avaible = new ParametricAnalysisChecker(cd, sd, simd).checkForArrivalRatesParametricSimulationAvaibleClasses();
		if ((cd.getOpenClassKeys().size() == 1) || (avaible.size() < cd.getOpenClassKeys().size())) {
			singleClass = true;
			classKey = avaible.get(0);
			double arrivalRate = 1 / (((Distribution) cd.getClassDistribution(classKey)).getMean());
			initialValue = arrivalRate;
			finalValue = arrivalRate * INCREMENT_SINGLE;
		} else {
			singleClass = SINGLE_CLASS;
			if (SINGLE_CLASS) {
				double arrivalRate = 1 / (((Distribution) cd.getClassDistribution(classKey)).getMean());
				initialValue = arrivalRate;
				finalValue = arrivalRate * INCREMENT_SINGLE;
			} else {
				initialValue = FROM_ALL;
				finalValue = TO_ALL;
			}
		}

	}

	/**
	 * Returns true if only the arrrival rate of one class will be increased
	 * @return true if only the arrrival rate of one class will be increased
	 */
	public boolean isSingleClass() {
		return singleClass;
	}

	/**
	 * Sets the type of arrrival rate increase. If <code> isSingleClass</code>
	 * param is true only the arrrival rate of one class will be increased
	 * @param isSingleClass
	 */
	public void setSingleClass(boolean isSingleClass) {
		if (singleClass != isSingleClass) {
			simDef.setSaveChanged();
		}
		singleClass = isSingleClass;
	}

	/**
	 * Sets the default initial Value
	 */
	public void setDefaultInitialValue() {
		if (singleClass) {
			double rate = 1 / ((Distribution) (classDef.getClassDistribution(classKey))).getMean();
			initialValue = rate;
		} else {
			initialValue = FROM_ALL;
		}
	}

	/**
	 * Sets default final value
	 */
	public void setDefaultFinalValue() {
		if (singleClass) {
			double rate = (1 / ((Distribution) (classDef.getClassDistribution(classKey))).getMean()) * INCREMENT_SINGLE;
			finalValue = rate;
		} else {
			finalValue = TO_ALL;
		}
	}

	/**
	 * Gets the class key of the class whose arrrival rate will be
	 * increased. If the simulation is not single class, the <code> null </code>
	 * value will be returned
	 * @return the key of the class whose arrrival rate will be increased if the
	 *         parametric analysis is single class, <code> null </code> otherwise.
	 */
	@Override
	public Object getReferenceClass() {
		if (singleClass) {
			return classKey;
		} else {
			return null;
		}
	}

	/**
	 * Sets the class whose arrrival rate will be increased. If <code> singleClass </code>
	 * value is not true nothing will be done
	 * @param classKey the key of the class whose number of job will be
	 *        increased
	 */
	public void setReferenceClass(Object classKey) {
		if (singleClass) {
			if (this.classKey != classKey) {
				simDef.setSaveChanged();
			}
			this.classKey = classKey;
		}
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
	 *
	 */
	@Override
	public void changeModel(int step) {
		if (step >= numberOfSteps) {
			return;
		}
		if (values != null) {
			if (singleClass) {
				Double refAR = (Double) ((Vector) values).get(step);
				Distribution distr = (Distribution) classDef.getClassDistribution(classKey);
				distr.setMean(1 / (refAR.doubleValue()));
			} else {
				for (int i = 0; i < avaibleClasses.size(); i++) {
					Object thisClass = avaibleClasses.get(i);
					double refAR = ((ValuesTable) values).getValue(thisClass, step);
					Distribution distr = (Distribution) classDef.getClassDistribution(thisClass);
					distr.setMean(1 / refAR);
				}
			}
		}
	}

	/**
	 * Gets the maximum number of steps compatible with the model definition and the type of parametric analysis.
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
	 *
	 */
	@Override
	public void createValuesSet() {
		if (singleClass) {
			double sum = 0;
			double increment = (finalValue - initialValue) / ((numberOfSteps - 1));
			values = new Vector(numberOfSteps);
			for (int i = 0; i < numberOfSteps; i++) {
				double value = initialValue + sum;
				((Vector<Double>) values).add(new Double(value));
				sum += increment; //note that the increment may be < 0
			}
			originalValues = new Double(initialValue);
		} else {
			double sum = 1;
			double increment = (finalValue - initialValue) / (100 * (double) (numberOfSteps - 1));
			//find the set of avaible classes
			Vector allClasses = classDef.getOpenClassKeys();
			avaibleClasses = new Vector<Object>(0, 1);
			for (int i = 0; i < allClasses.size(); i++) {
				Object thisClass = allClasses.get(i);
				Object temp = classDef.getClassDistribution(thisClass);
				if (temp instanceof Distribution) {
					Distribution distr = (Distribution) temp;
					if (distr.hasMean()) {
						avaibleClasses.add(thisClass);
					}
				}
			}
			values = new ValuesTable(classDef, avaibleClasses, numberOfSteps);
			for (int i = 0; i < numberOfSteps; i++) {
				for (int k = 0; k < avaibleClasses.size(); k++) {
					Object thisClass = avaibleClasses.get(k);
					double thisInitialArrivalRate = 1 / (((Distribution) classDef.getClassDistribution(thisClass)).getMean());
					double value = thisInitialArrivalRate * (sum);
					((ValuesTable) values).setValue(thisClass, value);
				}
				sum += increment; //note that the increment may be < 0
			}
			//used to save the initial values of service time
			originalValues = new Vector(avaibleClasses.size());
			for (int i = 0; i < avaibleClasses.size(); i++) {
				Object thisClass = avaibleClasses.get(i);
				double thisRate = 1 / (((Distribution) classDef.getClassDistribution(thisClass)).getMean());
				((Vector<Double>) originalValues).add(new Double(thisRate));
			}
		}
	}

	/**
	 * Restore the original values of the parameter
	 */
	@Override
	public void restoreOriginalValues() {
		if (originalValues != null) {
			if (singleClass) {
				Distribution distr = (Distribution) classDef.getClassDistribution(classKey);
				Double rate = (Double) originalValues;
				distr.setMean(1 / (rate.doubleValue()));
			} else {
				Vector values = (Vector) originalValues;
				Vector avaibleClasses = classDef.getOpenClassKeys();
				for (int i = 0; i < avaibleClasses.size(); i++) {
					Object thisClass = avaibleClasses.get(i);
					Distribution distr = (Distribution) classDef.getClassDistribution(thisClass);
					Double thisRate = (Double) values.get(i);
					distr.setMean(1 / (thisRate.doubleValue()));
				}
			}
		}
	}

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
	@Override
	public int checkCorrectness(boolean autocorrect) {
		int code = 0;
		ParametricAnalysisChecker checker = new ParametricAnalysisChecker(classDef, stationDef, simDef);
		Vector openClasses = classDef.getOpenClassKeys();
		Vector<Object> avaibleClasses = checker.checkForArrivalRatesParametricSimulationAvaibleClasses();
		if (avaibleClasses.isEmpty()) {
			code = 2;
		} else {
			//if is single class...
			if (isSingleClass()) {
				// ... and the selected close class is no more avaible
				if (!avaibleClasses.contains(classKey)) {
					code = 1;
					if (autocorrect) {
						classKey = openClasses.get(0); //change the reference class
						setDefaultInitialValue();
						setDefaultFinalValue();
					}
				} else {
					double refRate = 1 / (((Distribution) classDef.getClassDistribution(classKey)).getMean());
					//If the arrival rate of reference class was changed...
					if (initialValue != refRate) {
						code = 1;
						if (autocorrect) {
							initialValue = refRate;
							finalValue = refRate * INCREMENT_SINGLE;
						}
					}
				}
			} else {
				if ((avaibleClasses.size() < openClasses.size()) || (openClasses.size() == 1)) {
					code = 1;
					if (autocorrect) {
						classKey = avaibleClasses.get(0);
						singleClass = true;
						setDefaultInitialValue();
						setDefaultFinalValue();
					}
				}
			}
		}
		return code;
	}

	/**
	 * Returns the values assumed by the varying parameter
	 *
	 * @return a Vector containing the values assumed by the varying parameter
	 */
	@Override
	public Vector<Number> getParameterValues() {
		Vector<Number> assumedValues = new Vector<Number>(numberOfSteps);
		if (singleClass) {
			return (Vector<Number>) values;
		} else {
			ValuesTable temp = (ValuesTable) values;
			double originalValue = ((Double) ((Vector) originalValues).get(0)).doubleValue();
			for (int i = 0; i < numberOfSteps; i++) {
				double thisValue = temp.getValue(avaibleClasses.get(0), i);
				double ratio = thisValue / originalValue * 100;
				assumedValues.add(new Double(ratio));
			}
		}
		return assumedValues;
	}

	/**
	 * Get the reference class name
	 *
	 * @return the name of the class
	 */
	@Override
	public String getReferenceClassName() {
		return classDef.getClassName(classKey);
	}

	/**
	 * Gets a TreeMap containing for each property its value. The supported properties are
	 * defined as constants inside this class.
	 * @return a TreeMap containing the value for each property
	 */
	@Override
	public Map<String, String> getProperties() {
		TreeMap<String, String> properties = new TreeMap<String, String>();
		properties.put(TYPE_PROPERTY, getType());
		properties.put(TO_PROPERTY, Double.toString(finalValue));
		properties.put(STEPS_PROPERTY, Integer.toString(numberOfSteps));
		properties.put(IS_SINGLE_CLASS_PROPERTY, Boolean.toString(singleClass));
		if (singleClass) {
			properties.put(REFERENCE_CLASS_PROPERTY, classDef.getClassName(classKey));
		}
		return properties;
	}

	/**
	 * Sets the value for the specified property. The supported properties are: <br>
	 * - TO_PROPERTY  <br>
	 * - STEPS_PROPERTY <br>
	 * - IS_SINGLE_CLASS_PROPERTY <br>
	 * - REFERENCE_CLASS_PROPERTY
	 * @param propertyName the name of the property to be set
	 * @param value the value to be set
	 */
	@Override
	public void setProperty(String propertyName, String value) {
		if (propertyName.equals(TO_PROPERTY)) {
			finalValue = Double.parseDouble(value);
		} else if (propertyName.equals(STEPS_PROPERTY)) {
			numberOfSteps = Integer.parseInt(value);
			if (numberOfSteps > MAX_STEP_NUMBER) {
				numberOfSteps = MAX_STEP_NUMBER;
			}
		} else if (propertyName.equals(IS_SINGLE_CLASS_PROPERTY)) {
			singleClass = Boolean.valueOf(value).booleanValue();
			this.setDefaultInitialValue();
			this.setDefaultFinalValue();
		} else if (propertyName.equals(REFERENCE_CLASS_PROPERTY)) {
			classKey = classDef.getClassByName(value);
		}
		simDef.setSaveChanged();
	}
}
