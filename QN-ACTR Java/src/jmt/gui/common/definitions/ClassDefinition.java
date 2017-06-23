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

package jmt.gui.common.definitions;

import java.util.Vector;

/**
 * Created by IntelliJ IDEA.
 * User: OrsotronIII
 * Date: 23-mag-2005
 * Time: 9.34.34
 *This interface provides methods for editation of model's classes details. In the implementing
 * class of this interface, model's userclasses must be accessed through a research key that must
 * be represented by a number. This gives possibility of a smarter implementation for search and
 * editing methods than a name-based search modality.
 *
 *
 *
 *
 * Modified by Francesco D'Aquino 21/11/2005
 */
public interface ClassDefinition {

	/**Code for class name search*/
	public int CLASS_NAME = 0;
	/**Code for class type search*/
	public int CLASS_TYPE = 1;
	/**Code for class priority search*/
	public int CLASS_PRIORITY = 2;
	/**Code for class population search*/
	public int CLASS_POPULATION = 3;
	/**Code for class distribution search*/
	public int CLASS_DISTRIBUTION = 4;

	/**
	 * This method returns the entire set of class keys.
	 */
	public Vector<Object> getClassKeys();

	/**
	 * This method returns the set of open class keys.
	 *
	 * Author: Francesco D'Aquino
	 */
	public Vector<Object> getOpenClassKeys();

	/**
	 * This method returns the sum of population of each close class
	 * @return the total close class population
	 * 
	 * Author: Francesco D'Aquino
	 */
	public int getTotalCloseClassPopulation();

	/**
	* This method returns the set of closed class keys.
	*/
	public Vector<Object> getClosedClassKeys();

	/**
	 * Returns name of the class linked to the specific key
	 * */
	public String getClassName(Object key);

	/**
	 * Sets name of the class linked to the specific key
	 * */
	public void setClassName(String name, Object key);

	/**
	 * Returns type of the class linked to the specific key. Type of class is represented by
	 * an int number whose value is contained in <code>JSIMConstants</code>.
	 * */
	public int getClassType(Object key);

	/**
	 * Sets type of the class linked to the specific key. Type of class is represented by
	 * an int number whose value is contained in <code>JSIMConstants</code>.
	 * */
	public void setClassType(int type, Object key);

	/**
	 * Returns name of the class linked to the specific key
	 * */
	public int getClassPriority(Object key);

	/**
	 * Sets name of the class linked to the specific key
	 * */
	public void setClassPriority(int priority, Object key);

	/**
	 * Returns population of the class linked to the specific key. Return value is an integer
	 * if specified class is a closed class, null, otherwise.
	 * */
	public Integer getClassPopulation(Object key);

	/**
	 * Sets population of the class linked to the specific key
	 * */
	public void setClassPopulation(Integer population, Object key);

	/**
	 * Returns inter-arrival time distribution of the class linked to the specific key.
	 * Return value is an object representing distribution if specified class is an open
	 *  class, null otherwise.
	 * */
	public Object getClassDistribution(Object key);

	/**
	 * Sets inter-arrival time distribution of the class linked to the specific key
	 * */
	public void setClassDistribution(Object distribution, Object key);

	/**Returns a class parameter, given the class key of search, and the parameter name
	 * specified by proper code.*/
	public Object getClassParameter(Object key, int parameterName);

	/**Sets a class parameter, given the class key of search, and the parameter name
	 * specified by proper code.*/
	public void setClassParameter(Object key, int parameterName, Object value);

	/**Sets all class parameters at once given the class' search key. If search key does not exist,
	 *  a new class is created*/
	public void setClassParameters(Object key, String name, int type, int priority, Integer population, Object distribution);

	/**Adds a class and sets all class parameters at once, return the new search key
	 * identifying the new class.*/
	public Object addClass(String name, int type, int priority, Integer population, Object distribution);

	/**Deletes class given its search key. */
	public void deleteClass(Object key);

	/**
	 * Sets the reference station for a given class
	 * @param classKey search key for class
	 * @param stationKey search key for referenced station
	 */
	public void setClassRefStation(Object classKey, Object stationKey);

	/**
	 * Returns the reference station for a given class
	 * @param classKey search key for class
	 * @return search key for referenced station or null if it was not specified yet
	 */
	public Object getClassRefStation(Object classKey);

	/**
	 * Returns the class key given its name. It returns <code>null</code>
	 * if no such class is found.
	 * @param className the name of the class
	 * @return  the key of the class whose name is <code>className</code>
	 */
	public Object getClassByName(String className);
	
	public Vector<String> getsinkProbabilityUpdateClasses();
	
	public void resetSinkProbabilityUpdateClasses();
}
