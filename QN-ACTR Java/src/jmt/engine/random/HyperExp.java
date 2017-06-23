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
 * This is the Hyper Exponential distribution (see the constructor
 * description for dettails).
 *
 * <br><br>Copyright (c) 2003
 * <br>Politecnico di Milano - dipartimento di Elettronica e Informazione
 * @author Fabrizio Frontera - ffrontera@yahoo.it
 */

public class HyperExp extends AbstractDistribution implements Distribution {

	protected Exponential expDistr;

	/**
	 * This is the constructor. It creates a new hyper exponential distribution which
	 * is constituted by N exponential "servers" chosen with probability alpha_i.
	 *
	 */
	public HyperExp() {
		expDistr = new Exponential();
	}

	//TODO: perchè pdf e cdf sono uguali a zero?? Devono essere ancora implementate
	/**
	 * it returns the pdf of the distribution.
	 * This method is used to obtain from the distribution his probability distribution
	 * function evaluated where required by the user.
	 *
	 * @param x double indicating where to evaluate the pdf.
	 * @param p parameter of the hyper exponential distribution.
	 * @return double with the probability distribution function evaluated in x.
	 */

	public double pdf(double x, Parameter p) { //other implementation may use p.check()
		return 0.0;
	}

	/**
	 * it returns the cdf of the distribution.
	 * This method is used to obtain from the distribution his cumulative distribution
	 * function evaluated where required by the user.
	 *
	 * @param x double indicating where to evaluate the cdf.
	 * @param p parameter of the hyper exponential distribution.
	 * @return double with the cumulative distribution function evaluated in x.
	 */

	public double cdf(double x, Parameter p) { //other implementation may use p.check()
		return 0.0;
	}

	/**
	 * it returns the mean of the distribution.
	 * This method is used to obtain from the distribution the value of his own
	 * theoretic mean.
	 *
	 * @param p parameter of the hyper exponential distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the theoretic mean of the distribution.
	 */

	public double theorMean(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			return ((HyperExpPar) p).getMean();
		} else {
			throw new IncorrectDistributionParameterException(
					"Remember: parameter mean, variance, lambda1 and lambda 2 must be gtz; p must be a number betwen 0 and 1");
		}
	}

	/**
	 * it returns the variance of the distribution.
	 * This method is used to obtain from the distribution his own theoretical
	 * variance.
	 *
	 * @param p parameter of the hyper exponential distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the theoretic variance of the distribution.
	 */
	public double theorVariance(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			return ((HyperExpPar) p).getVar();
		} else {
			throw new IncorrectDistributionParameterException(
					"Remember: parameter mean, variance, lambda1 and lambda 2 must be gtz; p must be a number betwen 0 and 1");
		}
	}

	/**
	 * it returns the new random number.
	 * This method is used to obtain from the distribution the next number distributed
	 * according to the distribution parameter.
	 *
	 * @param p parameter of the hyper exponential distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the next random number of this distribution.
	 */

	public double nextRand(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			if (engine.nextDouble() <= ((HyperExpPar) p).getP()) {
				return expDistr.nextRand(((HyperExpPar) p).getExpParam1());
			} else {
				return expDistr.nextRand(((HyperExpPar) p).getExpParam2());
			}
		} else {
			throw new IncorrectDistributionParameterException(
					"Remember: parameter mean, variance, lambda1 and lambda 2 must be gtz; p must be a number betwen 0 and 1");
		}
	}

}

// end HyperExp
