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
 * This is the parameter that should be passed to the Pareto
 * distribution.
 *
 * <br><br>Copyright (c) 2003
 * <br>Politecnico di Milano - dipartimento di Elettronica e Informazione
 * @author Fabrizio Frontera - ffrontera@yahoo.it
 * @author Bertoli Marco 10-oct-2005
 *
 */

public class ParetoPar extends AbstractParameter implements Parameter {

	private double alpha;
	private double k;

	/**
	 * It creates a new pareto parameter according with the data provided by the user.
	 * The user provides the value of the parameter k and alpha of the new pareto
	 * distribution. These value are verified by the constructor.
	 *
	 * @param alpha double containing the alpha parameter of the distribution (must be greater than zero).
	 * @param k double containing the k parameter of the distribution (must be greater than zero).
	 *
	 * @throws IncorrectDistributionParameterException if k or alpha are not both greater than zero.
	 *
	 */
	public ParetoPar(double alpha, double k) throws IncorrectDistributionParameterException {
		this.alpha = alpha;
		this.k = k;
		if (!check()) {
			if (k <= 0) {
				throw new IncorrectDistributionParameterException("k must be gtz");
			}
			if (alpha <= 0) {
				throw new IncorrectDistributionParameterException("alpha must be gtz");
			}
		}
	}

	public ParetoPar(Double walpha, Double wk) throws IncorrectDistributionParameterException {
		this(walpha.doubleValue(), wk.doubleValue());
	}

	/**
	 * It controls if the parameter is correct. For the pareto distribution, the
	 * parameter is right if both alpha and k are greater than zero.
	 *
	 * @return boolean, indicating wether the parameter is correct or not.
	 *
	 */

	@Override
	public boolean check() {
		if ((alpha <= 0) || (k <= 0)) {
			return false;
		} else {
			return true;
		}
	}

	/**
	 * It returns the value of the parameter alpha of the pareto distribution.
	 *
	 * @return double with the alpha parameter of the pareto distribution.
	 *
	 */

	public double getAlpha() {
		return alpha;
	}

	/**
	 * It returns the value of the parameter k of the pareto distribution.
	 *
	 * @return double with the k parameter of the pareto distribution.
	 *
	 */

	public double getK() {
		return k;
	}

	/**
	 * it change the alpha of the distribution.
	 * It allow the user to change the value of the parameter alpha of the pareto
	 * distribution. It verify that the provided value is correct (alpha must be
	 * greater than zero).
	 *
	 * @param alpha double indicating the new value of the parameter alpha
	 * @throws IncorrectDistributionParameterException if alpha is not greater than zero.
	 *
	 */

	public void setAlpha(double alpha) throws IncorrectDistributionParameterException {
		if (alpha <= 0) {
			throw new IncorrectDistributionParameterException("alpha must be gtz");
		} else {
			this.alpha = alpha;
		}
	}

	/**
	 * it change the k of the distribution.
	 * It allow the user to change the value of the parameter k of the pareto
	 * distribution. It verify that the provided value is correct (k must be
	 * greater than zero).
	 *
	 * @param k double indicating the new value of the parameter k
	 * @throws IncorrectDistributionParameterException
	 *
	 */

	public void setK(double k) throws IncorrectDistributionParameterException {
		if (k <= 0) {
			throw new IncorrectDistributionParameterException("k must be gtz");
		} else {
			this.k = k;
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
		if (meanValue <= 0 || Double.isInfinite(meanValue)) {
			throw new IncorrectDistributionParameterException("Mean value must be finite and greater than zero");
		}
		k = meanValue * (alpha - 1) / alpha;
	}

} // end ParetoPar
