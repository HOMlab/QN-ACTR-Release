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
 * <br><br>Copyright (c) 2003
 * <br>Politecnico di Milano - dipartimento di Elettronica e Informazione
 * @author Fabrizio Frontera - ffrontera@yahoo.it
 * @author Bertoli Marco - Debugged as it didn't worked 8/9/2005
 *
 */

public class HyperExpPar extends AbstractParameter implements Parameter {

	private double mean;
	private double var;
	private double p;
	private double lambda1;
	private double lambda2;
	protected ExponentialPar expParam1;
	protected ExponentialPar expParam2;

	/**
	 * it creates a new hyper exponential parameter.
	 * It creates a new hyper exponential parameter according to the value of mean
	 * and variance provided by the user. It verify if these parameter are correct
	 * that is if both of them are greater than zero.
	 *
	 * @param mean double containing the mean of the new hyper exponential distribution.
	 * @param var double containing the variance of the new hyper exponential distribution.
	 *
	 * @throws IncorrectDistributionParameterException if mean of variance are not greater than zero.
	 *
	 */

	public HyperExpPar(double mean, double var) throws IncorrectDistributionParameterException {
		this.mean = mean;
		this.var = var;
		testParameters();
		this.p = .5 * (1 - Math.sqrt((var - 1) / (var + 1))); //set the probability to select one of the two exponential
		this.lambda1 = 2 * this.p / mean;
		this.lambda2 = 2 * (1 - this.p) / mean;
		// creates 2 ExponentialPar objects
		this.expParam1 = new ExponentialPar(lambda1);
		this.expParam2 = new ExponentialPar(lambda2);
	}

	public HyperExpPar(Double wmean, Double wvar) throws IncorrectDistributionParameterException {
		this(wmean.doubleValue(), wvar.doubleValue());
	}

	/**
	 * it creates a new hyper exponential parameter.
	 * It construct a new hyper exponential parameter based on the value provided by
	 * the user. This constructor takes three parameters and creates two instances of
	 * exponential distribution which are selected with probability p provided.
	 *
	 * @param p double containing the probability to select one of the two exponential.
	 * @param lambda1 double containing the value of lambda for the first exponential.
	 * @param lambda2 double containing the value of lambda for the second exponential.
	 *
	 * @throws IncorrectDistributionParameterException if p is not betwen zero and one and if lambda1 and lambda2 are not both greater than zero.
	 *
	 */
	public HyperExpPar(double p, double lambda1, double lambda2) throws IncorrectDistributionParameterException {
		this.p = p; //set the probability to select one of the two exponential
		this.lambda1 = lambda1;
		this.lambda2 = lambda2;
		testParameters1();
		this.mean = p / lambda1 + (1 - p) / lambda2;
		this.var = 2 * (p / (lambda1 * lambda1) + (1 - p) / (lambda2 * lambda2)) - mean * mean;
		// creates 2 ExponentialPar objects
		expParam1 = new ExponentialPar(lambda1);
		expParam2 = new ExponentialPar(lambda2);
	}

	public HyperExpPar(Double wp, Double wlambda1, Double wlambda2) throws IncorrectDistributionParameterException {
		this(wp.doubleValue(), wlambda1.doubleValue(), wlambda2.doubleValue());
	}

	/**
	 * It verify the parameters for the constructor requiring Mean and Var.
	 *
	 * @throws IncorrectDistributionParameterException if mean or variance are not both greater than zero.
	 */
	private void testParameters() throws IncorrectDistributionParameterException {
		if (mean <= 0) {
			throw new IncorrectDistributionParameterException("Error: mean must be > 0");
		}
		if (var <= 0) {
			throw new IncorrectDistributionParameterException("Error: variance must be > 0");
		}
	}

	/**
	 * Tests the parameters for the constructor requiring p, lambda1 and lambda2.
	 *
	 * @throws IncorrectDistributionParameterException if p is not betwen zero and one or if lambda1 and labda2 are not both greater than zero.
	 *
	 */
	private void testParameters1() throws IncorrectDistributionParameterException {
		if (p <= 0 || p >= 1) {
			throw new IncorrectDistributionParameterException("Error: must be 0 < p < 1");
		}
		if (lambda1 <= 0) {
			throw new IncorrectDistributionParameterException("Error: lambda1 must be >= 0");
		}
		if (lambda2 <= 0) {
			throw new IncorrectDistributionParameterException("Error: lambda2 must be >= 0");
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
		if (((mean <= 0) || (var <= 0)) || ((p <= 0 || p >= 1) || (lambda1 <= 0) || (lambda2 <= 0))) {
			return false;
		} else {
			return true;
		}
	}

	/**
	 * it returns the value of p.
	 * It returns the value of the parameter p, the probability to select one of the
	 * exponential distribution.
	 *
	 * @return double with p the probability to select one of the exponential distribution.
	 *
	 */
	public double getP() {
		return p;
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
	 * it returns the value of lambda2.
	 * It returns the value of the parameter lambda2, the parameter lamda for the
	 * second exponential distribution created by the hyper exponential parameter.
	 *
	 * @return double with lambda2 the value of lambda for the 2nd exponential distribution.
	 *
	 */

	public double getLambda2() {
		return lambda2;
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
	 * @return exponentialPar with expParam2, the parameter of the 2nd exponential distribution.
	 *
	 */

	public ExponentialPar getExpParam2() {
		return expParam2;
	}

	/**
	 * it change the value of mean.
	 * It allow the user to change the value of the mean of the hyper
	 * exponential distribution. It verify that the new value is correct and re-calculate
	 * the value of lambda for both the exponential distribution used.
	 *
	 * @param mean double indicating the new value of the parameter mean
	 * @throws IncorrectDistributionParameterException if the new value of mean is not greater than zero.
	 *
	 */

	@Override
	public void setMean(double mean) throws IncorrectDistributionParameterException {
		this.mean = mean;
		try {
			testParameters();
		} catch (IncorrectDistributionParameterException ie) {
			throw ie;
		}
		this.lambda1 = 2 * this.p / mean;
		this.lambda2 = 2 * (1 - this.p) / mean;
	}

	/**
	 * it change the value of variance.
	 * It allow the user to change the value of the variance of the hyper
	 * exponential distribution. It verify that the new value is correct and re-calculate
	 * the value of p.
	 *
	 * @param var double indicating the new value of the parameter var
	 * @throws IncorrectDistributionParameterException if the new value of var is not greater than zero.
	 *
	 */

	public void setVar(double var) throws IncorrectDistributionParameterException {
		this.var = var;
		try {
			testParameters();
		} catch (IncorrectDistributionParameterException ie) {
			throw ie;
		}
		this.p = .5 * (1 - Math.sqrt((var - 1) / (var + 1)));
	}
} // end HyperExpPar
