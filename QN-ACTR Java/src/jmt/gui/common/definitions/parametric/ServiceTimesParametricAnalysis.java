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
 * <p>Title: ServiceTimesParametricAnalysis</p>
 * <p>Description: this class is used to describe a parametric analysis where the
 * varied parameter is the mean value of service time inside a station. It
 * adds the <code >classKey</code> and <code >stationKey</code> fields, used
 * to keep the key of the Job-Class and the key of the station whose service
 * time will be varied, and a boolean value <code >singleClass</code> used to
 * choose the type of service time growth (single or all class).</p>
 *
 * @author Francesco D'Aquino
 *         Date: 14-dic-2005
 *         Time: 12.03.48
 */
public class ServiceTimesParametricAnalysis extends ParametricAnalysisDefinition {
	private final double FROM_ALL = 100;
	private final double TO_ALL = 150;
	private final double INCREMENT_SINGLE = 2;
	private final int STEPS = 10; //must be < than ParametricAnalysis.MAX_NUMBER_OF_STEPS
	private final boolean SINGLE_CLASS = false;

	private boolean singleClass;
	private Object classKey;
	private Object stationKey;
	private Vector<Object> avaibleClasses;

	private Object values;

	public ServiceTimesParametricAnalysis(ClassDefinition cd, StationDefinition sd, SimulationDefinition simd) {
		type = PA_TYPE_SERVICE_TIMES;
		classDef = cd;
		stationDef = sd;
		simDef = simd;
		numberOfSteps = STEPS;
		ParametricAnalysisChecker checker = new ParametricAnalysisChecker(cd, sd, simd);
		stationKey = checker.checkForServiceTimesParametricAnalysisAvaibleStations().get(0);
		Vector<Object> avaible = checker.checkForServiceTimesParametricSimulationAvaibleClasses(stationKey);
		if ((cd.getClassKeys().size() == 1) || (avaible.size() < cd.getClassKeys().size())) {
			singleClass = true;
			classKey = avaible.get(0);
			double mean = ((Distribution) (stationDef.getServiceTimeDistribution(stationKey, classKey))).getMean();
			initialValue = mean;
			finalValue = mean * INCREMENT_SINGLE;
		} else {
			singleClass = SINGLE_CLASS;
			if (SINGLE_CLASS) {
				double mean = ((Distribution) (stationDef.getServiceTimeDistribution(stationKey, classKey))).getMean();
				initialValue = mean;
				finalValue = mean * INCREMENT_SINGLE;
			} else {
				initialValue = FROM_ALL;
				finalValue = TO_ALL;
			}
		}
	}

	/**
	 * Returns true if only the number of jobs of one class will be increased
	 * @return true if only the number of jobs of one class will be increased
	 */
	public boolean isSingleClass() {
		return singleClass;
	}

	/**
	 * Sets the type of population increase. If <code> isSingleClass</code>
	 * param is true only the number of jobs of one class will be increased
	 * @param isSingleClass
	 */
	public void setSingleClass(boolean isSingleClass) {
		if (isSingleClass != singleClass) {
			simDef.setSaveChanged();
		}
		singleClass = isSingleClass;
	}

	/**
	 * Sets the default initial Value
	 */
	public void setDefaultInitialValue() {
		if (singleClass) {
			double mean = ((Distribution) (stationDef.getServiceTimeDistribution(stationKey, classKey))).getMean();
			initialValue = mean;
		} else {
			initialValue = FROM_ALL;
		}
	}

	/**
	 * Sets default final value
	 */
	public void setDefaultFinalValue() {
		if (singleClass) {
			double mean = ((Distribution) (stationDef.getServiceTimeDistribution(stationKey, classKey))).getMean();
			finalValue = mean * INCREMENT_SINGLE;
		} else {
			finalValue = TO_ALL;
		}
	}

	/**
	 * Gets the class key of the job class whose number of jobs will be
	 * increased. If the simulation is not single class, the <code> null </code>
	 * value will be returned
	 * @return the key of the class whose number of jobs will be increased if the
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
		properties.put(REFERENCE_STATION_PROPERTY, stationDef.getStationName(stationKey));
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
	 * - REFERENCE_STATION_PROPERTY <br>
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
		} else if (propertyName.equals(REFERENCE_STATION_PROPERTY)) {
			stationKey = stationDef.getStationByName(value);
		} else if (propertyName.equals(REFERENCE_CLASS_PROPERTY)) {
			classKey = classDef.getClassByName(value);
		}
	}

	/**
	 * Sets the class whose number of jobs will be increased. If <code> singleClass </code>
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
	 * Gets the station key whose service times will be varied
	 * @return the key of the station whose service times will be varied
	 */
	public Object getReferenceStation() {
		return stationKey;
	}

	/**
	 * Gets name of the whose service times will be varied
	 * @return the key of the station whose service times will be varied
	 */
	public String getReferenceStationName() {
		return stationDef.getStationName(stationKey);
	}

	/**
	 * Sets the station whose service times will be varied
	 * @param stationKey the station whose service times will be varied
	 */
	public void setReferenceStation(Object stationKey) {
		this.stationKey = stationKey;
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
				Double refST = (Double) ((Vector) values).get(step);
				Distribution distr = (Distribution) stationDef.getServiceTimeDistribution(stationKey, classKey);
				distr.setMean(refST.doubleValue());
			} else {
				//Vector classSet = classDef.getClassKeys();
				for (int i = 0; i < avaibleClasses.size(); i++) {
					Object thisClass = avaibleClasses.get(i);
					double refST = ((ValuesTable) values).getValue(thisClass, step);
					Distribution distr = (Distribution) stationDef.getServiceTimeDistribution(stationKey, thisClass);
					distr.setMean(refST);
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
	 * Finds the set of possible values of the parameter on which the
	 * simulation may be iterated on.
	 *
	 */
	@Override
	public void createValuesSet() {
		double initialServiceTime;
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
			Vector allClasses = classDef.getClassKeys();
			avaibleClasses = new Vector<Object>(0, 1);
			for (int i = 0; i < allClasses.size(); i++) {
				Object thisClass = allClasses.get(i);
				Object temp = stationDef.getServiceTimeDistribution(stationKey, thisClass);
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
					double thisInitialServiceTime = ((Distribution) stationDef.getServiceTimeDistribution(stationKey, thisClass)).getMean();
					double value = thisInitialServiceTime * (sum);
					((ValuesTable) values).setValue(thisClass, value);
				}
				sum += increment; //note that the increment may be < 0
			}
			//used to save the initial values of service time
			originalValues = new Vector(avaibleClasses.size());
			for (int i = 0; i < avaibleClasses.size(); i++) {
				Object thisClass = avaibleClasses.get(i);
				double thisServiceTime = ((Distribution) stationDef.getServiceTimeDistribution(stationKey, thisClass)).getMean();
				((Vector<Double>) originalValues).add(new Double(thisServiceTime));
			}
		}
	}

	/**
	 * Restore the original values of service times
	 */
	@Override
	public void restoreOriginalValues() {
		if (originalValues != null) {
			if (singleClass) {
				Distribution distr = (Distribution) stationDef.getServiceTimeDistribution(stationKey, classKey);
				Double mean = (Double) originalValues;
				distr.setMean(mean.doubleValue());
			} else {
				Vector values = (Vector) originalValues;
				for (int i = 0; i < avaibleClasses.size(); i++) {
					Object thisClass = avaibleClasses.get(i);
					Distribution distr = (Distribution) stationDef.getServiceTimeDistribution(stationKey, thisClass);
					Double thisValue = (Double) values.get(i);
					distr.setMean(thisValue.doubleValue());
				}
			}
		}
		//modified = false;
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
		Vector classes = classDef.getClassKeys();
		ParametricAnalysisChecker checker = new ParametricAnalysisChecker(classDef, stationDef, simDef);
		//Find the avaible stations
		Vector<Object> avaibleStations = checker.checkForServiceTimesParametricAnalysisAvaibleStations();
		if (avaibleStations.isEmpty()) {
			code = 2; // -> This type of PA is not avaible
		} else {
			//if the reference station is no more avaible change reference station
			if (!avaibleStations.contains(stationKey)) {
				code = 1;
				if (autocorrect) {
					stationKey = avaibleStations.get(0);
					setDefaultInitialValue();
					setDefaultFinalValue();
				}
			}
			//Find avaible classes for stationKey
			Vector<Object> avaibleClasses = checker.checkForServiceTimesParametricSimulationAvaibleClasses(stationKey);
			//if is single class...
			if (isSingleClass()) {
				// ... and the selected close class is no more avaible
				if (!avaibleClasses.contains(classKey)) {
					code = 1;
					if (autocorrect) {
						classKey = avaibleClasses.get(0); //change the reference class
						setDefaultInitialValue();
						setDefaultFinalValue();
					}
				} else {
					double mean = ((Distribution) stationDef.getServiceTimeDistribution(stationKey, classKey)).getMean();
					//If the service time of reference class was changed...
					if (initialValue != mean) {
						code = 1;
						if (autocorrect) {
							initialValue = mean;
							finalValue = mean * INCREMENT_SINGLE;
						}
					}
				}
			}
			//all class case...
			else {
				if ((avaibleClasses.size() < classes.size()) || (classes.size() == 1)) { //all class parametric analysis is no more avaible
					code = 1;
					if (autocorrect) {
						singleClass = true;
						classKey = avaibleClasses.get(0);
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

}
