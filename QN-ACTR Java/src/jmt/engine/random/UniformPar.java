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

package jmt.engine.random;

import jmt.common.exception.IncorrectDistributionParameterException;

/**
 *
 * This is the parameter that should be passed to the Uniform
 * distribution.
 *
 * <br><br>Copyright (c) 2003
 * <br>Politecnico di Milano - dipartimento di Elettronica e Informazione
 * @author Fabrizio Frontera - ffrontera@yahoo.it
 * @author Bertoli Marco 10-oct-2005
 *
 * Edited by Bertoli Marco 2005: Inverted max and min to have classical position
 */

public class UniformPar extends AbstractParameter implements Parameter {

	private double max;
	private double min;

	/**
	 * It creates a new uniform parameter using the bound provided by the user.
	 *
	 * @param max double containing the higher bound for the distribution.
	 * @param min double containing the lower bound for the distribution.
	 *
	 * @throws IncorrectDistributionParameterException if the bound are incorrect.
	 *
	 */

	public UniformPar(double min, double max) throws IncorrectDistributionParameterException {
		this.max = max;
		this.min = min;
		if (!check()) {
			throw new IncorrectDistributionParameterException("Error: the *max* parameter must be > of the *min* one");
		}
	}

	public UniformPar(Double wmin, Double wmax) throws IncorrectDistributionParameterException {
		this(wmin.doubleValue(), wmax.doubleValue());
	}

	/**
	 * It verify if the parameter is correct. For the uniform distribution the parameter
	 * is right if the bound are right which means that max must be greater or equal to min.
	 *
	 * @return boolean, indicating wether the parameter is correct or not.
	 *
	 */

	@Override
	public boolean check() {
		if (max <= min) {
			return false;
		} else {
			return true;
		}
	}

	/**
	 * It verify if the parameter is correct. For the uniform distribution the parameter
	 * is right if the bound are right which means that max must be greater or equal to min.
	 * It is the same of the method check() but it is a service method used only by the constructor.
	 *
	 * @return boolean, indicating wether the parameter is correct or not.
	 *
	 */

	private boolean check(double max, double min) {
		if (max <= min) {
			return false;
		} else {
			return true;
		}
	}

	/**
	 * It returns the value of the minimum bound of the uniform distribution.
	 *
	 * @return double with the min parameter of the distribution.
	 *
	 */

	public double getMin() {
		return min;
	}

	/**
	 * It returns the value of the maximum bound of the uniform distribution.
	 *
	 * @return double with the max parameter of the distribution.
	 *
	 */

	public double getMax() {
		return max;
	}

	/**
	 * it change the min bound of the distribution.
	 * It allow the user to change the minimum bound of the uniform distribution and
	 * it verifies that the new bound is correct (it is less or equal to the maximum bound).
	 *
	 * @param min double indicating the new value of the minimum bound.
	 * @throws IncorrectDistributionParameterException if the provided value is not
	 * less or equal to the maximum bound value.
	 *
	 */

	public void setMin(double min) throws IncorrectDistributionParameterException {
		if (check(max, min)) {
			this.min = min;
		} else {
			throw new IncorrectDistributionParameterException("Error: the *max* parameter must be > of the *min* one");
		}
	}

	/**
	 * it change the max bound of the distribution.
	 * It allow the user to change the maximum bound of the uniform distribution and
	 * it verifies that the new bound is correct (it is greater or equal to the minimum bound).
	 *
	 * @param max double indicating the new value of the maximum bound.
	 * @throws IncorrectDistributionParameterException if the provided value is not
	 * greater or equal to the minimum bound value.
	 *
	 */

	public void setMax(double max) throws IncorrectDistributionParameterException {
		if (check(max, min)) {
			this.max = max;
		} else {
			throw new IncorrectDistributionParameterException("Error: the *max* parameter must be > of the *min* one");
		}
	}

	/**
	 * Sets mean for a given distribution parameter. This method is required to adjust distributions
	 * mean values into Load Dependent Service Time Strategy
	 * @param meanValue new mean value for this distribution
	 * Author: Bertoli Marco
	 * @throws IncorrectDistributionParameterException if mean value is invalid for this distribution
	 */
	@Override
	public void setMean(double meanValue) throws IncorrectDistributionParameterException {
		if (meanValue < 0 || Double.isInfinite(meanValue)) {
			throw new IncorrectDistributionParameterException("Mean value must be finite and greater than zero");
		}
		// Keeps variation coefficient c constant
		double c = (max - min) / (max + min) * Math.sqrt(3);
		max = meanValue + meanValue * c / Math.sqrt(3);
		min = meanValue - meanValue * c / Math.sqrt(3);
	}
} // end UniformPar
