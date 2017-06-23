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
* This is the parameter that should be passed to the Hyper Exponential
* distribution.
*
* <br><br>Copyright (c) 2006
* <br>Politecnico di Milano - dipartimento di Elettronica e Informazione
* @author Giuliano Casale
*
*/

public class MMPP2Par extends AbstractParameter implements Parameter {

	private double mean;
	private double var;
	private double sigma0;
	private double sigma1;
	private double lambda1;
	private double lambda0;
	protected ExponentialPar expParam1;
	protected ExponentialPar expParam0;

	public MMPP2Par(Double lambda0, Double lambda1, Double sigma0, Double sigma1) throws IncorrectDistributionParameterException {
		this.sigma0 = sigma0.doubleValue();
		this.sigma1 = sigma1.doubleValue();
		this.lambda1 = lambda1.doubleValue();
		this.lambda0 = lambda0.doubleValue();
		testParameters();
		this.mean = (this.sigma0 + this.sigma1) / (this.sigma1 * this.lambda0 + this.sigma0 * this.lambda1);
		this.var = 0;
		// creates 2 ExponentialPar objects
		expParam1 = new ExponentialPar(this.lambda1 + this.sigma1);
		expParam0 = new ExponentialPar(this.lambda0 + this.sigma0);
	}

	/**
	 * Tests the parameters for the constructor requiring p, lambda1 and lambda0.
	 *
	 * @throws IncorrectDistributionParameterException if p is not betwen zero and one or if lambda1 and labda2 are not both greater than zero.
	 *
	 */
	private void testParameters() throws IncorrectDistributionParameterException {
		if (sigma1 <= 0) {
			throw new IncorrectDistributionParameterException("Error: sigma1 must be >= 0 ");
		}
		if (sigma0 <= 0) {
			throw new IncorrectDistributionParameterException("Error: sigma0 must be >= 0 ");
		}
		if (lambda1 < 0) {
			throw new IncorrectDistributionParameterException("Error: lambda1 must be >= 0");
		}
		if (lambda0 < 0) {
			throw new IncorrectDistributionParameterException("Error: lambda0 must be >= 0");
		}
	}

	/**
	 * It verify if the parameter is correct. For the hyper exponential
	 * distribution, the parameter is right if the mean calculated is gtz,
	 * the variance is gtz, p probability is betwen 0 and 1 and both the lambda value are gtz.
	 *
	 * @return boolean, indicating wether the parameter is correct or not.
	 *
	 */

	@Override
	public boolean check() {
		if ((sigma0 <= 0) || (sigma1 <= 0) || (lambda1 < 0) || (lambda0 < 0)) {
			return false;
		} else {
			return true;
		}
	}

	public double getSigma0() {
		return sigma0;
	}

	public double getSigma1() {
		return sigma1;
	}

	/**
	 * it returns the value of lambda1.
	 * It returns the value of the parameter lambda1, the parameter lamda for the
	 * first exponential distribution created by the hyper exponential parameter.
	 *
	 * @return double with lambda1 the value of lambda for the 1st exponential distribution.
	 *
	 */

	public double getLambda1() {
		return lambda1;
	}

	/**
	 * it returns the value of lambda0.
	 * It returns the value of the parameter lambda0, the parameter lamda for the
	 * second exponential distribution created by the hyper exponential parameter.
	 *
	 * @return double with lambda0 the value of lambda for the 2nd exponential distribution.
	 *
	 */

	public double getLambda0() {
		return lambda0;
	}

	/**
	 * it returns the mean of the distribution.
	 * It returns the value of the mean of the hyper exponential distribution which
	 * is provided by the user or evaluated according to other data.
	 *
	 * @return double with the mean of the hyper exponential distribution.
	 *
	 */
	public double getMean() {
		return mean;
	}

	/**
	 * it returns the variance of the distribution.
	 * It returns the value of the variance of the hyper exponential distribution which
	 * is provided by the user or evaluated according to other data.
	 *
	 * @return double with the variance of the hyper exponential distribution.
	 *
	 */

	public double getVar() {
		return var;
	}

	/**
	 * it returns the parameter of the 1st exponential.
	 * It returns the parameter used to create the first of the exponential
	 * distribution used by the hyper exponential distribution.
	 *
	 * @return exponentialPar with expParam1, the parameter of the 1st exponential distribution.
	 *
	 */

	public ExponentialPar getExpParam1() {
		return expParam1;
	}

	/**
	 * it returns the parameter of the 2nd exponential.
	 * It returns the parameter used to create the second of the exponential
	 * distribution used by the hyper exponential distribution.
	 *
	 * @return exponentialPar with expParam0, the parameter of the 2nd exponential distribution.
	 *
	 */

	public ExponentialPar getExpParam0() {
		return expParam0;
	}

} // end MMPP2Par
