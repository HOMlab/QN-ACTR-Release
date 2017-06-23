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

package jmt.engine.NetStrategies.ServiceStrategies;

import jmt.engine.math.parser.Parser;
import jmt.engine.random.Distribution;
import jmt.engine.random.Parameter;

/**
 * <p>Title: Load Dependent Service Time Strategy Parameter</p>
 * <p>Description: Parameter used to specify a sample for Load Dependent service time strategy.
 * This is supposed to hold a range of values, a distribution and a function that have to
 * be evaluated to set mean value for given distribution.</p>
 * 
 * @author Bertoli Marco
 *         Date: 10-ott-2005
 *         Time: 14.58.33
 */
public class LDParameter implements Comparable<Object> {
	private int from;
	private Distribution distribution;
	private Parameter parameter;
	private String function;
	private Parser parser;

	/**
	 * Construct a new Load Dependent Service Strategy parameter
	 * @param from minimum number of jobs for this strategy to apply
	 * @param distr service time distribution
	 * @param parameter parameter for the service time distribution
	 * @param function function to be evaluated to set service time distribution mean value
	 */
	public LDParameter(Integer from, Distribution distr, Parameter parameter, String function) {
		this.from = from.intValue();
		this.distribution = distr;
		this.parameter = parameter;
		if (function != null) {
			this.function = function.toLowerCase();
			parser = new Parser(function, true);
		}

	}

	/**
	 * Construct a new Load Dependent Service Strategy parameter
	 * @param from minimum number of jobs for this strategy to apply
	 * @param distr service time distribution
	 * @param parameter parameter for the service time distribution
	 */
	public LDParameter(Integer from, Distribution distr, Parameter parameter) {
		this(from, distr, parameter, null);
	}

	/**
	 * Gets minimum number of jobs for this strategy to apply
	 * @return minimum number of jobs for this strategy to apply
	 */
	public int getMinJobs() {
		return from;
	}

	/**
	 * Gets distribution used to evaluate service time
	 * @return distribution used to evaluate service times
	 */
	public Distribution getDistribution() {
		return distribution;
	}

	/**
	 * Gets the parameter of the distribution used to evaluate service time
	 * @return the parameter of the distribution used to evaluate service times
	 */
	public Parameter getDistrParameter() {
		return parameter;
	}

	/**
	 * Gets the function that must be used to evaluate mean value
	 * @return function to be evaluated to find mean value or null if not present
	 */
	public String getFunction() {
		return function;
	}

	/**
	 * Gets the parser that must be used to evaluate mean value
	 * @return parser to be used to evaluate mean value or null if not present
	 */
	public Parser getParser() {
		return parser;
	}

	/**
	 * Compares this object with the specified object for order.  Returns a
	 * negative integer, zero, or a positive integer as this object is less
	 * than, equal to, or greater than the specified object.<p>
	 * @param o the Object to be compared.
	 * @return a negative integer, zero, or a positive integer as this object
	 *         is less than, equal to, or greater than the specified object.
	 * @throws ClassCastException if the specified object's type prevents it
	 *                            from being compared to this Object.
	 */
	public int compareTo(Object o) {
		if (o instanceof LDParameter) {
			LDParameter other = (LDParameter) o;
			return this.from - other.from;
		} else if (o instanceof Integer) {
			Integer other = (Integer) o;
			return this.from - other.intValue();
		} else {
			throw new ClassCastException("Unsupported object type");
		}
	}
}
