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
 * This is the parameter that should be passed to the Gamma
 * distribution.
 *
 * <br><br>Copyright (c) 2003
 * <br>Politecnico di Milano - dipartimento di Elettronica e Informazione
 * @author Fabrizio Frontera - ffrontera@yahoo.it
 * @author Bertoli Marco 10-oct-2005
 *
 */

public class GammaDistrPar extends AbstractParameter implements Parameter {

	private double alpha;
	private double lambda;

	/**
	 * It creates a new gamma parameter according to the value provided by the user.
	 *
	 * @param alpha double containing the alpha parameter of the gamma distribution (must be greater than zero).
	 * @param lambda double containing the lambda parameter of the gamma distribution (must be greater than zero).
	 *
	 * @throws IncorrectDistributionParameterException if alpha or lambda are not greater than zero.
	 *
	 */
	public GammaDistrPar(double alpha, double lambda) throws IncorrectDistributionParameterException {
		if ((alpha <= 0.0) || (lambda <= 0.0)) {
			throw new IncorrectDistributionParameterException("alpha and lambda must be >0.");
		} else {
			this.alpha = alpha;
			this.lambda = lambda;
		}
	}

	public GammaDistrPar(Double walpha, Double wlambda) throws IncorrectDistributionParameterException {
		this(walpha.doubleValue(), wlambda.doubleValue());
	}

	/**
	 * It controls if the parameter is correct or not.
	 * For the gamma distribution, the parameter are right if they are both greater
	 * than zero.
	 *
	 * @return boolean, indicating wether the parameter is correct or not.
	 *
	 */

	@Override
	public boolean check() {
		if ((alpha <= 0.0) || (lambda <= 0.0)) {
			return false;
		} else {
			return true;
		}
	}

	/**
	 * It returns the value of the parameter alpha of the gamma distribution.
	 *
	 * @return double with alpha.
	 *
	 */
	public double getAlpha() {
		return alpha;
	}

	/**
	 * It returns the value of the lambda parameter of the gamma distribution.
	 *
	 * @return double with lambda.
	 *
	 */

	public double getLambda() {
		return lambda;
	}

	/**
	 * it change the value of alpha.
	 * It allow the user to modify the value of the parameter alpha of the gamma
	 * distribution. It verify if the new value is correct, that is if alpha is greater than zero.
	 *
	 * @param alpha double indicating the new value of the parameter alpha
	 * @throws IncorrectDistributionParameterException if the value provided is not greater than zero.
	 *
	 */

	public void setAlpha(double alpha) throws IncorrectDistributionParameterException {
		if (alpha <= 0.0) {
			throw new IncorrectDistributionParameterException("alpha must be >0.");
		} else {
			this.alpha = alpha;
		}
	}

	/**
	 * it change the value of lambda.
	 * It allow the user to modify the value of the parameter lambda of the gamma
	 * distribution. It verify if the new value is correct, that is if lambda is greater than zero.
	 *
	 * @param lambda double indicating the new value of the parameter lambda
	 * @throws IncorrectDistributionParameterException if the value provided is not greater than zero.
	 *
	 */

	public void setLambda(double lambda) throws IncorrectDistributionParameterException {
		if (lambda <= 0.0) {
			throw new IncorrectDistributionParameterException("lambda must be >0.");
		} else {
			this.lambda = lambda;
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
		lambda = meanValue / alpha;
	}

} // end GammaDistrPar
