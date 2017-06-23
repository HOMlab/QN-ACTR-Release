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
 * This is the Pareto distribution (see the constructor description
 * for his pdf definition).
 *
 * <br><br>Copyright (c) 2003
 * <br>Politecnico di Milano - dipartimento di Elettronica e Informazione
 * @author Fabrizio Frontera - ffrontera@yahoo.it
 * @author Modified by Stefano Omini, 7/5/2004
 */

public class Pareto extends AbstractDistribution implements Distribution {

	/**
	 * This is the constructor. It creates a new empty pareto distribution which
	 * is defined from is pdf:
	 * <pre>                  alpha      (-(alpha+1))
	 * pdf(x) = alpha * k       *  x</pre>
	 * with alpha gtz and k gtz and less or equal to x
	 */
	public Pareto() {
	}

	/**
	 * it returns the pdf of the distribution.
	 * This method is used to obtain from the distribution his probability distribution
	 * function evaluated where required by the user.
	 *
	 * @param x double indicating where to evaluate the pdf. Must be > alpha
	 * @param p parameter of the pareto distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the probability distribution function evaluated in x.
	 */
	//OLD
	//public double pdf(double x, ParetoPar p)
	public double pdf(double x, Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			//OLD
			//double alfa = p.getAlpha();
			//double k = p.getK();
			double alfa = ((ParetoPar) p).getAlpha();
			double k = ((ParetoPar) p).getK();
			if (x <= alfa) {
				throw new IncorrectDistributionParameterException("Error: x must be >alpha.");
			}
			return (alfa * Math.pow(k, alfa) / Math.pow(x, (alfa + 1)));
		} else {
			throw new IncorrectDistributionParameterException("Remember: parameter alpha and k must be gtz");
		}
	}

	/**
	 * it returns the cdf of the distribution.
	 * This method is used to obtain from the distribution his cumulative distribution
	 * function evaluated where required by the user.
	 *
	 * @param x double indicating where to evaluate the cdf.
	 * @param p parameter of the pareto distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the cumulative distribution function evaluated in x.
	 */

	//OLD
	//public double cdf(double x, ParetoPar p)
	public double cdf(double x, Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			//OLD
			//double alfa = p.getAlpha();
			//double k = p.getK();
			double alfa = ((ParetoPar) p).getAlpha();
			double k = ((ParetoPar) p).getK();
			if (x <= alfa) {
				return 0.0;
			}
			return 1.0 - Math.pow((k / x), alfa);
		} else {
			throw new IncorrectDistributionParameterException("Remember: parameter alpha and k must be gtz");
		}
	}

	/**
	 * it returns the mean of the distribution.
	 * This method is used to obtain from the distribution the value of his own
	 * theoretic mean.
	 *
	 * @param p parameter of the pareto distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the theoretic mean of the distribution.
	 *
	 * the mean is calculated as: (k*alpha)/(alpha-1)
	 */

	//OLD
	//public double theorMean(ParetoPar p)
	public double theorMean(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			//OLD
			//double alfa = p.getAlpha();
			//double k = p.getK();
			double alfa = ((ParetoPar) p).getAlpha();
			double k = ((ParetoPar) p).getK();
			return (alfa * k / (alfa - 1));
		} else {
			throw new IncorrectDistributionParameterException("Remember: parameter alpha and k must be gtz");
		}
	}

	/**
	 * it returns the variance of the distribution.
	 * This method is used to obtain from the distribution his own theoretical
	 * variance.
	 *
	 * @param p parameter of the pareto distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the theoretic variance of the distribution.
	 *
	 * the variance is calculated as: ((alpha*(k^2))/(((alpha-1)^2)*(alpha-2)))
	 */

	//OLD
	//public double theorVariance(ParetoPar p)
	public double theorVariance(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			//OLD
			//double alfa = p.getAlpha();
			//double k = p.getK();
			double alfa = ((ParetoPar) p).getAlpha();
			double k = ((ParetoPar) p).getK();
			return (alfa * k * k / ((alfa - 1) * (alfa - 1) * (alfa - 2)));
		} else {
			throw new IncorrectDistributionParameterException("Remember: parameter alpha and k must be gtz");
		}
	}

	/**
	 * it returns the new random number.
	 * This method is used to obtain from the distribution the next number distributed
	 * according to the distribution parameter.
	 *
	 * @param p parameter of the pareto distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the next random number of this distribution.
	 */

	public double nextRand(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			double alpha = ((ParetoPar) p).getAlpha();
			double k = ((ParetoPar) p).getK();
			return Math.pow((1 - engine.raw()), (-1 / alpha)) * k;
		} else {
			throw new IncorrectDistributionParameterException("Remember: parameter alpha and k must be gtz");
		}
	}

} // end Pareto
