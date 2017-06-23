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
 * This is the Exponential distribution (see the pdf definition
 * in the constructor description).
 *
 * <br><br>Copyright (c) 2003
 * <br>Politecnico di Milano - dipartimento di Elettronica e Informazione
 * @author Fabrizio Frontera - ffrontera@yahoo.it
 * @author Modified by Stefano Omini, 7/5/2004
 */

public class Exponential extends AbstractDistribution implements Distribution {

	/**
	 * This is the constructor. It creates a new exponential distribution
	 * which is defined from is pdf:
	 * <pre>                  (-lambda*x)
	 * pdf(x)= lambda * e</pre>
	 * with lambda gtz, if x is gtz.
	 * 0 if x<0.
	 */
	public Exponential() {
	}

	/**
	 * it returns the pdf of the distribution.
	 * This method is used to obtain from the distribution his probability distribution
	 * function evaluated where required by the user.
	 *
	 * @param x double indicating where to evaluate the pdf.
	 * @param p parameter of the exponential distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the probability distribution function evaluated in x.
	 */

	//OLD
	//public double pdf(double x, ExponentialPar p)
	public double pdf(double x, Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			//OLD
			//double lambda = p.getLambda();
			double lambda = ((ExponentialPar) p).getLambda();
			if (x < 0.0) {
				return 0.0;
			}
			return lambda * Math.exp(-x * lambda);
		} else {
			throw new IncorrectDistributionParameterException("Remember: parameter lambda must be gtz");
		}
	}

	/**
	 * it returns the cdf of the distribution.
	 * This method is used to obtain from the distribution his cumulative distribution
	 * function evaluated where required by the user.
	 *
	 * @param x double indicating where to evaluate the cdf.
	 * @param p parameter of the exponential distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the cumulative distribution function evaluated in x.
	 */
	//OLD
	//public double cdf(double x, ExponentialPar p)
	public double cdf(double x, Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			if (x <= 0.0) {
				return 0.0;
			}
			//OLD
			//return 1.0 - Math.exp(-x * p.getLambda());
			double lambda = ((ExponentialPar) p).getLambda();
			return 1.0 - Math.exp(-x * lambda);
		} else {
			throw new IncorrectDistributionParameterException("Remember: parameter lambda must be gtz");
		}
	}

	/**
	 * it returns the mean of the distribution.
	 * This method is used to obtain from the distribution the value of his own
	 * theoretic mean.
	 *
	 * @param p parameter of the exponential distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the theoretic mean of the distribution.
	 *
	 * the mean of the exponential distribution is calculated as 1/lambda.
	 */
	//OLD
	//public double theorMean(ExponentialPar p)
	public double theorMean(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			//OLD
			//return 1 / p.getLambda();
			return 1 / ((ExponentialPar) p).getLambda();
		} else {
			throw new IncorrectDistributionParameterException("Remember: parameter lambda must be gtz");
		}
	}

	/**
	 * it returns the variance of the distribution.
	 * This method is used to obtain from the distribution his own theoretical
	 * variance.
	 *
	 * @param p parameter of the exponential distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the theoretic variance of the distribution.
	 *
	 * the variance of an exponential is calculated as 1/(lambda^2).
	 */
	//OLD
	//public double theorVariance(ExponentialPar p)
	public double theorVariance(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			//OLD
			//double lambda = p.getLambda();
			double lambda = ((ExponentialPar) p).getLambda();
			return 1 / (lambda * lambda);
		} else {
			throw new IncorrectDistributionParameterException("Remember: parameter lambda must be gtz");
		}
	}

	/**
	 * it returns the new random number.
	 * This method is used to obtain from the distribution the next number distributed
	 * according to the distribution parameter.
	 *
	 * @param p parameter of the exponential distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the next random number of this distribution.
	 */

	public double nextRand(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			return -Math.log(engine.raw()) / ((ExponentialPar) p).getLambda();
		} else {
			throw new IncorrectDistributionParameterException("Remember: parameter lambda must be gtz");
		}
	}

} // end Exponential
