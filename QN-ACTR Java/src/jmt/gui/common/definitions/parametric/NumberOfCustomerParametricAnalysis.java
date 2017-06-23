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

/**
 * <p>Title: NumberOfCustomerParametricAnalysis</p>
 * <p>Description: this class is used to describe a parametric analysis where the
 * varied parameter is the global number of close class jobs. It adds the
 * <code >classKey</code> field used to keep the key of the Job-Class whose
 * number of jobs will be varied, and a boolean value <code >singleClass</code>
 * used to choose the type of service time growth (single or all class).</p>
 *
 * @author Francesco D'Aquino
 *         Date: 14-dic-2005
 *         Time: 11.19.26
 */
public class NumberOfCustomerParametricAnalysis extends ParametricAnalysisDefinition {
	private final boolean SINGLE_CLASS = false;

	private boolean singleClass;
	private Object classKey;
	private Vector<Integer> validParameterValues;

	private Object values;

	public NumberOfCustomerParametricAnalysis(ClassDefinition cd, StationDefinition sd, SimulationDefinition simd) {
		type = PA_TYPE_NUMBER_OF_CUSTOMERS;
		classDef = cd;
		stationDef = sd;
		simDef = simd;
		Vector closedClasses = cd.getClosedClassKeys();
		if (closedClasses.size() == 1) {
			classKey = closedClasses.get(0);
			singleClass = true;
		} else {
			singleClass = SINGLE_CLASS;
		}
		initialValue = cd.getTotalCloseClassPopulation();
		finalValue = cd.getTotalCloseClassPopulation() * 2 + 1;
		numberOfSteps = this.searchForAvaibleSteps();
		if (numberOfSteps > MAX_STEP_NUMBER) {
			numberOfSteps = MAX_STEP_NUMBER;
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
	 * Gets the type of parametric analysis
	 *
	 * @return the type of parametric analysis
	 */
	@Override
	public String getType() {
		return type;
	}

	/**
	 * returns the set of values that the varying parameter will assume
	 * @return a structure containing the set of values that the varying parameter will assume
	 */
	public Object getValuesSet() {
		return values;
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
				Integer refPop = (Integer) ((Vector) values).get(step);
				classDef.setClassPopulation(refPop, classKey);

			} else {
				Vector classSet = classDef.getClosedClassKeys();
				for (int i = 0; i < classSet.size(); i++) {
					Object thisClass = classSet.get(i);
					int thisPop = (int) ((ValuesTable) values).getValue(thisClass, step);
					classDef.setClassPopulation(new Integer(thisPop), thisClass);
				}
			}
			simDef.manageJobs();
		}
	}

	/**
	 * Gets the maximum number of steps compatible with the model definition and the type of parametric analysis and initialize
	 * the <code>validParameterValues</code> Vector, containing the values that the parameter will assume. If the simulation
	 * is single class based, it will contain the values of the number of jobs that the reference class will have, otherwise it will
	 * contain the values of global population.
	 *
	 * @return the maximum number of steps
	 */
	@Override
	public int searchForAvaibleSteps() {
		int max = (int) (finalValue - initialValue) + 1;
		validParameterValues = new Vector<Integer>(max, 1);
		if (singleClass) {
			int pop = classDef.getClassPopulation(classKey).intValue();
			for (int i = 0; i < max; i++) {
				validParameterValues.add(new Integer(pop));
				pop++;
			}
			return max;
		} else {
			int avaibleSteps = 0;
			Vector classSet = classDef.getClosedClassKeys();
			//calculate the proportion between classes populations
			double[] betas = new double[classSet.size()];
			int totalPop = classDef.getTotalCloseClassPopulation();
			for (int i = 0; i < classSet.size(); i++) {
				double thisPop = classDef.getClassPopulation(classSet.get(i)).doubleValue();
				betas[i] = thisPop / totalPop;
			}
			for (int i = 0; i < max; i++) {
				int j;
				for (j = 0; j < classSet.size(); j++) {
					double thisClassNextNumberOfJobs = totalPop * betas[j];
					double intValue = Math.ceil(thisClassNextNumberOfJobs - 0.5); //get the closest integer
					//if the next-step number of jobs for this class is not
					//an integer break the cycle, since this total number of
					//job is not valid
					if ((thisClassNextNumberOfJobs - intValue) > 0) {
						break;
					}
				}
				//check if the cycle was exited before checking all classes
				if (!(j < classSet.size())) {
					validParameterValues.add(new Integer(totalPop));
					avaibleSteps++;
				}
				totalPop++;
			}
			return avaibleSteps;
		}
	}

	/**
	 * Finds the set of possible values of the population on which the
	 * simulation may be iterated on.
	 *
	 */
	@Override
	public void createValuesSet() {
		int maxSteps = validParameterValues.size();
		if (singleClass) {
			values = new Vector(0, 1);
			double p = (double) (maxSteps - 1) / (double) (numberOfSteps - 1);
			//int thisStep = 0;
			int thisStep;
			double sum = 0;
			for (int i = 0; i < numberOfSteps; i++) {
				thisStep = (int) (sum);
				((Vector<Integer>) values).add(validParameterValues.get(thisStep));
				sum += p;
			}
			originalValues = new Integer(classDef.getClassPopulation(classKey).intValue());
		} else {
			double p = (double) (maxSteps - 1) / (double) (numberOfSteps - 1);
			int thisStep;
			double sum = 0;
			Vector<Object> classSet = classDef.getClosedClassKeys();
			values = new ValuesTable(classDef, classSet, numberOfSteps);
			//calculate the proportion between classes populations
			double[] betas = new double[classSet.size()];
			double totalPop = classDef.getTotalCloseClassPopulation();
			for (int i = 0; i < classSet.size(); i++) {
				double thisPop = classDef.getClassPopulation(classSet.get(i)).doubleValue();
				betas[i] = thisPop / totalPop;
			}
			for (int i = 0; i < numberOfSteps; i++) {
				thisStep = (int) sum;
				totalPop = validParameterValues.get(thisStep).intValue();
				for (int j = 0; j < classSet.size(); j++) {
					double thisClassNextNumberOfJobs = totalPop * betas[j];
					int value = (int) (thisClassNextNumberOfJobs);
					((ValuesTable) values).setValue(classSet.get(j), value);
				}
				sum += p;
			}
			originalValues = new Vector(classSet.size());
			for (int i = 0; i < classSet.size(); i++) {
				Object thisClass = classSet.get(i);
				Integer thisValue = new Integer(classDef.getClassPopulation(thisClass).intValue());
				((Vector<Integer>) originalValues).add(thisValue);
			}
		}

	}

	/**
	 * Restore the original values of population
	 */
	@Override
	public void restoreOriginalValues() {
		if (singleClass) {
			classDef.setClassPopulation((Integer) originalValues, classKey);
		} else {
			Vector vals = (Vector) originalValues;
			Vector classSet = classDef.getClosedClassKeys();
			for (int i = 0; i < classSet.size(); i++) {
				Object thisClass = classSet.get(i);
				Integer thisVal = new Integer(((Integer) vals.get(i)).intValue());
				classDef.setClassPopulation(thisVal, thisClass);
			}
		}
		simDef.manageJobs();
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
		Vector closeClasses = classDef.getClosedClassKeys();
		if (closeClasses.isEmpty()) {
			code = 2; //This PA model can be no more used
		} else {
			//if is single class...
			if (isSingleClass()) {
				// ... and the selected close class is no more avaible
				if (!closeClasses.contains(classKey)) {
					code = 1;
					if (autocorrect) {
						classKey = closeClasses.get(0); //change the reference class
						int totalPop = classDef.getTotalCloseClassPopulation();
						double minFinal = totalPop + 1;
						initialValue = totalPop;
						if (finalValue < minFinal) {
							finalValue = minFinal;
						}
						numberOfSteps = searchForAvaibleSteps();
						if (numberOfSteps > MAX_STEP_NUMBER) {
							numberOfSteps = MAX_STEP_NUMBER;
						}
					}
				}
				// else check that the initial value equals the number of jobs belonging to the reference class
				else {
					int totalPop = classDef.getTotalCloseClassPopulation();
					double minFinal = totalPop + 1;
					if (initialValue != totalPop) {
						code = 1;
						if (autocorrect) {
							initialValue = totalPop;
							if (finalValue < minFinal) { //the initial value was changed,
								finalValue = minFinal; //so change the final value and
							}
							numberOfSteps = searchForAvaibleSteps(); //number of steps
							if (numberOfSteps > MAX_STEP_NUMBER) {
								numberOfSteps = MAX_STEP_NUMBER;
							}
						}
					}
				}
			} else {
				int totalPop = classDef.getTotalCloseClassPopulation();
				double minFinal;
				if (classDef.getClosedClassKeys().size() == 1) {
					minFinal = totalPop + 1;
				} else {
					minFinal = totalPop * 2 + 1;
				}
				// if the total number of job has changed
				if (initialValue != totalPop) {
					code = 1;
					if (autocorrect) {
						initialValue = totalPop;
						if (finalValue < minFinal) { //the initial value was changed,
							finalValue = minFinal; //so change the final value and
						}
						numberOfSteps = searchForAvaibleSteps(); //number of steps
						if (numberOfSteps > MAX_STEP_NUMBER) {
							numberOfSteps = MAX_STEP_NUMBER;
						}
					}
				} else {
					Vector<Integer> temp = validParameterValues;
					int temp2 = searchForAvaibleSteps();
					//if the total nmber job equals initialValue, but the
					//validParameterValues is different from the old one
					//it means that something was changed
					if (!temp.equals(validParameterValues)) {
						code = 1;
						if (autocorrect) {
							//set the new number of steps
							numberOfSteps = temp2;
							if (numberOfSteps > MAX_STEP_NUMBER) {
								numberOfSteps = MAX_STEP_NUMBER;
							}
						} else {
							validParameterValues = temp;
						}
					}
				}
				if (closeClasses.size() == 1) {
					code = 1;
					if (autocorrect) {
						singleClass = true;
						classKey = closeClasses.get(0);
					}
				}
			}

		}
		//modified = false;
		//lastCode = code;
		return code;
		//}
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
			Vector temp = (Vector) values;
			double initial = ((Integer) originalValues).doubleValue();
			double sum = classDef.getTotalCloseClassPopulation() - initial;
			for (int i = 0; i < numberOfSteps; i++) {
				double val = ((Integer) (temp.get(i))).doubleValue() + sum;
				assumedValues.add(new Double(val));
			}
		} else {
			ValuesTable temp = (ValuesTable) values;
			int classNumber = temp.getNumberOfClasses();
			for (int i = 0; i < numberOfSteps; i++) {
				double sum = 0;
				for (int j = 0; j < classNumber; j++) {
					Object thisClass = classDef.getClosedClassKeys().get(j);
					sum += temp.getValue(thisClass, i);
				}
				assumedValues.add(new Double(sum));
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
	 *  - TO_PROPERTY  <br>
	 *  - STEPS_PROPERTY <br>
	 *  - IS_SINGLE_CLASS_PROPERTY <br>
	 *  - REFERENCE_CLASS_PROPERTY
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
		} else if (propertyName.equals(REFERENCE_CLASS_PROPERTY)) {
			classKey = classDef.getClassByName(value);
		}
		simDef.setSaveChanged();
	}
}
