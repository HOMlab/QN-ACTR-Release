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
import jmt.engine.math.Probability;

/**
 *
 * This is the Normal distribution (see the constructor
 * description for his pdf definition).
 *
 * <br><br>Copyright (c) 2003
 * <br>Politecnico di Milano - dipartimento di Elettronica e Informazione
 * @author Fabrizio Frontera - ffrontera@yahoo.it
 * @author Modified by Stefano Omini, 7/5/2004
 * @author Modified by Bertoli Marco, 8/9/2005
 */

public class Normal extends AbstractDistribution implements Distribution {

	protected double cache; // cache for Box-Mueller algorithm
	protected boolean cacheFilled; // Box-Mueller

	/**
	 * This is the constructor. It creates a new normal distribution which is
	 * defined  from is pdf:
	 * <pre>               1                   (x-m)^2
	 * pdf(x) = -------------- * exp (- ----------- )
	 *           sqrt(2*pi)*v              2v^2</pre>
	 * where v^2 is the variance and m is the mean of the distribution
	 * pi is the pi-greco constant.
	 */

	public Normal() {
	}

	/**
	 * it returns the pdf of the distribution.
	 * This method is used to obtain from the distribution his probability distribution
	 * function evaluated where required by the user.
	 *
	 * @param x double indicating where to evaluate the pdf.
	 * @param p parameter of the normal distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the probability distribution function evaluated in x.
	 */

	//OLD
	//public double pdf(double x, NormalPar p)
	public double pdf(double x, Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			//OLD
			//double variance = p.getStandardDeviation() * p.getStandardDeviation();
			//double SQRT_INV = 1.0 / Math.sqrt(2.0 * Math.PI * variance);
			//double mean = p.getMean();
			double variance = ((NormalPar) p).getStandardDeviation() * ((NormalPar) p).getStandardDeviation();
			double SQRT_INV = 1.0 / Math.sqrt(2.0 * Math.PI * variance);
			double mean = ((NormalPar) p).getMean();
			double diff = x - mean;
			return SQRT_INV * Math.exp(-(diff * diff) / (2.0 * variance));
		} else {
			throw new IncorrectDistributionParameterException("Remember: standardDeviation must be gtz");
		}
	}

	/**
	 * it returns the cdf of the distribution.
	 * This method is used to obtain from the distribution his cumulative distribution
	 * function evaluated where required by the user.
	 *
	 * @param x double indicating where to evaluate the cdf.
	 * @param p parameter of the normal distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the cumulative distribution function evaluated in x.
	 */

	//OLD
	//public double cdf(double x, NormalPar p)
	public double cdf(double x, Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			//OLD
			//double mean = p.getMean();
			//double variance = p.getStandardDeviation() * p.getStandardDeviation();
			double mean = ((NormalPar) p).getMean();
			double variance = ((NormalPar) p).getStandardDeviation() * ((NormalPar) p).getStandardDeviation();
			return Probability.normal(mean, variance, x);
		} else {
			throw new IncorrectDistributionParameterException("Remember: standardDeviation must be gtz");
		}
	}

	/**
	 * it returns the mean of the distribution.
	 * This method is used to obtain from the distribution the value of his own
	 * theoretic mean.
	 *
	 * @param p parameter of the normal distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the theoretic mean of the distribution.
	 */

	//OLD
	//public double theorMean(NormalPar p)
	public double theorMean(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			//OLD
			//double mean = p.getMean();
			double mean = ((NormalPar) p).getMean();
			return mean;
		} else {
			throw new IncorrectDistributionParameterException("Remember: standardDeviation must be gtz");
		}
	}

	/**
	 * it returns the variance of the distribution.
	 * This method is used to obtain from the distribution his own theoretical
	 * variance.
	 *
	 * @param p parameter of the normal distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the theoretic variance of the distribution.
	 */

	//OLD
	//public double theorVariance(NormalPar p)
	public double theorVariance(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			//OLD
			//double variance = (p.getStandardDeviation() * p.getStandardDeviation());
			double variance = ((NormalPar) p).getStandardDeviation() * ((NormalPar) p).getStandardDeviation();
			return variance;
		} else {
			throw new IncorrectDistributionParameterException("Remember: standardDeviation must be gtz");
		}
	}

	/**
	 * in returns the new random number.
	 * This method is used to obtain from the distribution the next number distributed
	 * according to the distribution parameter.
	 *
	 * @param p parameter of the normal distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the next random number of this distribution.
	 */

	public double nextRand(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			double mean = ((NormalPar) p).getMean();
			double standardDeviation = ((NormalPar) p).getStandardDeviation();
			// Uses polar Box-Muller transformation.
			if (cacheFilled) {
				cacheFilled = false;
				// If generated number is in the past, reruns this method
				return (cache > 0) ? cache : nextRand(p);
			};
			double x, y, r, z;
			do {
				x = 2.0 * engine.raw() - 1.0;
				y = 2.0 * engine.raw() - 1.0;
				r = x * x + y * y;
			} while (r >= 1.0);
			z = Math.sqrt(-2.0 * Math.log(r) / r);
			cache = mean + standardDeviation * x * z;
			cacheFilled = true;
			// If generated number is in the past, reruns this method
			double ret = mean + standardDeviation * y * z;
			return (ret > 0) ? ret : nextRand(p);
		} else {
			throw new IncorrectDistributionParameterException("Remember: standardDeviation must be gtz");
		}
	}

} // end Normal
