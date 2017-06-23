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
 * This is the parameter that should be passed to the Poisson
 * distribution.
 *
 * <br><br>Copyright (c) 2003
 * <br>Politecnico di Milano - dipartimento di Elettronica e Informazione
 * @author Fabrizio Frontera - ffrontera@yahoo.it
 *
 */

public class PoissonPar extends AbstractParameter implements Parameter {

	private double mean;

	/**
	 * It creates a new poisson parameter with the mean required by the user. It also
	 * verify that the required mean is correct (mean must be greater than zero).
	 *
	 * @param mean double containing the mean of the distribution (also called lambda).
	 *
	 * @throws IncorrectDistributionParameterException if mean is not greater than zero.
	 *
	 */
	public PoissonPar(double mean) throws IncorrectDistributionParameterException {
		if (mean <= 0) {
			throw new IncorrectDistributionParameterException("mean must be gtz");
		} else {
			this.mean = mean;
		}
	}

	public PoissonPar(Double wmean) throws IncorrectDistributionParameterException {
		this(wmean.doubleValue());
	}

	/**
	 * It verify that the parameter is correct. For the poisson distribution the
	 * parameter is correct if the mean is greater than zero.
	 *
	 * @return boolean, indicating wether the parameter is correct or not.
	 *
	 */

	@Override
	public boolean check() {
		if (mean <= 0) {
			return false;
		} else {
			return true;
		}
	}

	/**
	 * it returns the mean of the distribution.
	 * It return the value of the mean of the poisson distribution (also calleda
	 * lambda in the definition).
	 *
	 * @return double with the value of the mean of the poisson distribution.
	 *
	 */

	public double getMean() {
		return mean;
	}

	/**
	 * it change the mean of the distribution.
	 * It allow the user to change the mean of the poisson distribution and it
	 * verify that the new value is correct (the mean must be greater than zero).
	 *
	 * @param mean double indicating the new value of the parameter mean (or lambda).
	 * @throws IncorrectDistributionParameterException if the provided value is not greater than zero.
	 *
	 */

	@Override
	public void setMean(double mean) throws IncorrectDistributionParameterException {
		if (mean <= 0) {
			throw new IncorrectDistributionParameterException("mean must be gtz");
		} else {
			this.mean = mean;
		}
	}

} // end PoissonPar
