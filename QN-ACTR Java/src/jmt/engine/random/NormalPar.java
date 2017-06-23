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
 * This is the parameter that should be passed to the Normal
 * distribution.
 *
 * <br><br>Copyright (c) 2003
 * <br>Politecnico di Milano - dipartimento di Elettronica e Informazione
 * @author Fabrizio Frontera - ffrontera@yahoo.it
 * @author Bertoli Marco 10-oct-2005
 *
 * Edited by Bertoli Marco 2005: Inverted mean and standardDeviation to have classical position
 *
 */

public class NormalPar extends AbstractParameter implements Parameter {

	private double standardDeviation;
	private double mean;

	/**
	 * it creates a new normal parameter.
	 * It creates a new normal parameter according to the data provided by the user:
	 * the standard deviation and the mean of the new normal distribution. It
	 * verify that these value are correct (the standard deviation, which is the square root of the variance, must be greater than zero).
	 *
	 * @param standardDeviation double containing the standard deviation of the new normal distribution (must be greater than zero).
	 * @param mean double containing the mean of the new normal distribution.
	 *
	 * @throws IncorrectDistributionParameterException if the value provided for the standard deviation is not greater than zero.
	 *
	 */
	public NormalPar(double mean, double standardDeviation) throws IncorrectDistributionParameterException {
		if (standardDeviation <= 0) {
			throw new IncorrectDistributionParameterException("standardDeviation must be > 0");
		} else {
			this.standardDeviation = standardDeviation;
			this.mean = mean;
		}
	}

	public NormalPar(Double wmean, Double wstandardDeviation) throws IncorrectDistributionParameterException {
		this(wmean.doubleValue(), wstandardDeviation.doubleValue());
	}

	/**
	 * It verify that the parameter is correct. for the normal distribution the
	 * parameter is right if the value of standard deviation is greater than zero.
	 *
	 * @return boolean, indicating wether the parameter is correct or not.
	 *
	 */
	@Override
	public boolean check() {
		if (standardDeviation <= 0) {
			return false;
		} else {
			return true;
		}
	}

	/**
	 * It returns the value of mean of the normal distribution.
	 *
	 * @return double with the mean of the distribution.
	 *
	 */

	public double getMean() {
		return mean;
	}

	/**
	 * It returns the value of the standard deviation of the normal distribution.
	 *
	 * @return double with the standard deviation of the distribution.
	 *
	 */

	public double getStandardDeviation() {
		return standardDeviation;
	}

	/**
	 * it change the std. deviation of the distribution.
	 * It allow the user to change the value of the standard deviation for the
	 * normal distribution. It verify that it is correct that is if it is greater than zero.
	 *
	 * @param standardDeviation double indicating the new value of the parameter standardDeviation
	 * @throws IncorrectDistributionParameterException
	 *
	 */

	public void setStandardDeviation(double standardDeviation) throws IncorrectDistributionParameterException {
		if (standardDeviation <= 0) {
			throw new IncorrectDistributionParameterException("standardDeviation must be gtz");
		} else {
			this.standardDeviation = standardDeviation;
		}
	}

	/**
	 * It allow the user to change the value of the mean of the normal distribution.
	 *
	 * @param mean double indicating the new value of the parameter mean
	 *
	 */

	@Override
	public void setMean(double mean) {
		this.mean = mean;
	}

} // end NormalPar
