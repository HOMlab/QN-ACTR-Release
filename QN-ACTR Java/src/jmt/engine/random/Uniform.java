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
 * This is the Uniform distribution.This object offers many
 * methods to obtain random value distribuited like a uniform. This is because
 * the Uniform distribution must be also used by external classes as an
 * interface with the real pseudo-random number generator (which return many
 * type of value distribuited like a uniform too). (see the constructor
 * description for his pdf definition).
 *
 * <br><br>Copyright (c) 2003
 * <br>Politecnico di Milano - dipartimento di Elettronica e Informazione
 * @author Fabrizio Frontera - ffrontera@yahoo.it
 * @author Modified by Stefano Omini, 7/5/2004
 */

public class Uniform extends AbstractDistribution implements Distribution {

	/**
	 * A new uniform distribution is defined from is pdf:
	 * <pre>              1
	 * pdf(x) = -----------
	 *            max-min</pre>
	 * where max and min are two double boundaries for the distribution.
	 * max must be greater than min. If x is not betwen min and max, pdf(x)=0.
	 *
	 */

	/**
	 * it returns the pdf of the distribution.
	 * This method is used to obtain from the distribution his probability distribution
	 * function evaluated where required by the user.
	 *
	 * @param x double indicating where to evaluate the pdf.
	 * @param p parameter of the uniform distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the probability distribution function evaluated in x.
	 */

	//OLD
	//public double pdf(double x, UniformPar p)
	public double pdf(double x, Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			//OLD
			//double max = p.getMax();
			//double min = p.getMin();
			double max = ((UniformPar) p).getMax();
			double min = ((UniformPar) p).getMin();
			if (x <= min || x >= max) {
				return 0.0; //if x is out of bound return 0
			}
			return 1.0 / (max - min);
		} else {
			throw new IncorrectDistributionParameterException(
					"Remember: the *max* parameter must be > of the *min* one because min and max reppresent the boud of the distribution");
		}
	}

	/**
	 * it returns the cdf of the distribution.
	 * This method is used to obtain from the distribution his cumulative distribution
	 * function evaluated where required by the user.
	 *
	 * @param x double indicating where to evaluate the cdf.
	 * @param p parameter of the uniform distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the cumulative distribution function evaluated in x.
	 */

	//OLD
	//public double cdf(double x, UniformPar p)
	public double cdf(double x, Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			//OLD
			//double max = p.getMax();
			//double min = p.getMin();
			double max = ((UniformPar) p).getMax();
			double min = ((UniformPar) p).getMin();
			if (x <= min) {
				return 0.0; //if x is lower than the min bound return 0
			}
			if (x >= max) {
				return 1.0; //if x is greater than the max bound return 1
			}
			return (x - min) / (max - min);
		} else {
			throw new IncorrectDistributionParameterException(
					"Remember: the *max* parameter must be > of the *min* one because min and max reppresent the boud of the distribution");
		}
	}

	/**
	 * it returns the mean of the distribution.
	 * This method is used to obtain from the distribution the value of his own
	 * theoretic mean. For the uniform distribution the theoretic mean is
	 * calculated as (max+min)/2.
	 *
	 * @param p parameter of the uniform distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the theoretic mean of the distribution.
	 *
	 */
	//OLD
	//public double theorMean(UniformPar p)
	public double theorMean(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			//OLD
			//double max = p.getMax();
			//double min = p.getMin();
			double max = ((UniformPar) p).getMax();
			double min = ((UniformPar) p).getMin();
			return (max + min) / 2;
		} else {
			throw new IncorrectDistributionParameterException(
					"Remember: the *max* parameter must be > of the *min* one because min and max reppresent the boud of the distribution");
		}
	}

	/**
	 * it returns the variance of the distribution.
	 * This method is used to obtain from the distribution his own theoretical
	 * variance. For the uniform distribution the theoretic variance is calculated
	 * as ((max-min)^2)/12.
	 *
	 * @param p parameter of the uniform distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the theoretic variance of the distribution.
	 *
	 */

	//OLD
	//public double theorVariance(UniformPar p)
	public double theorVariance(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			//OLD
			//double max = p.getMax();
			//double min = p.getMin();
			double max = ((UniformPar) p).getMax();
			double min = ((UniformPar) p).getMin();
			return Math.pow((max - min), 2) / 12;
		} else {
			throw new IncorrectDistributionParameterException(
					"Remember: the *max* parameter must be > of the *min* one because min and max reppresent the boud of the distribution");
		}
	}

	/**
	 * it returns the new random long integer.
	 * This method is used to obtain from the distribution the next Long integer
	 * number distributed according to the distribution parameter.
	 *
	 * @param p parameter of the uniform distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the next random number of this distribution.
	 */

	public double nextLong(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			/* Doing the thing turns out to be more tricky than expected.
			   Avoids overflows and underflows.
			   Treats cases like from=-1, to=1 and the like right.
			   The following code would NOT solve the problem: return (long) (Doubles.randomFromTo(from,to));
			   Rounding avoids the unsymmetric behaviour of casts from double to long: (long) -0.7 = 0, (long) 0.7 = 0.
			   checking for overflows and underflows is also necessary.
			*/
			double min = ((UniformPar) p).getMin();
			double max = ((UniformPar) p).getMax();
			long random = 0;
			try {
				UniformPar param = new UniformPar(0, 0);
				//double param[] = new double[2];

				// first the most likely and also the fastest case.
				if (min >= 0 && max < Long.MAX_VALUE) {
					//    return (long)min + (long) (nextRand(((UniformPar) p)));
					return 0;
				}
				// would we get a numeric overflow?
				// if not, we can still handle the case rather efficient.
				double diff = max - min + 1.0;
				if (diff <= Long.MAX_VALUE) {
					param.setMin(0.0);
					param.setMax(diff);
					return (long) min + (long) (nextRand((p)));
				}
				// now the pathologic boundary cases.
				// they are handled rather slow.
				if (min == Long.MIN_VALUE) {
					if (max == Long.MAX_VALUE) {
						param.setMin(Integer.MIN_VALUE);
						param.setMax(Integer.MAX_VALUE);
						int i1 = nextInt(param);
						int i2 = nextInt(param);
						return ((i1 & 0xFFFFFFFFL) << 32) | (i2 & 0xFFFFFFFFL);
					}
					param.setMin(min);
					param.setMax(max + 1);
					random = Math.round(nextRand(param));
					if (random > max) {
						random = (long) min;
					}
				} else {
					param.setMin(min - 1);
					param.setMax(max);
					random = Math.round(nextRand(param));
					if (random < min) {
						random = (long) max;
					}
				}
			} catch (Exception ex) {
			}
			return random;
		} else {
			throw new IncorrectDistributionParameterException(
					"Remember: the *max* parameter must be > of the *min* one because min and max reppresent the boud of the distribution");
		}
	}

	/**
	 * it returns the new random boolean.
	 * This method is used to obtain from the distribution the next boolean distributed
	 * according to the distribution parameter.
	 *
	 * @return boolean with the next random boolean of this distribution.
	 */

	public boolean nextBoolean() {
		return engine.raw() > 0.5;
	}

	/**
	 * it returns the new random integer.
	 * This method is used to obtain from the distribution the next integer number
	 * distributed according to the distribution parameter.
	 *
	 * @param p parameter of the uniform distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return int with the next random number of this distribution.
	 */

	public int nextInt(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			double min = ((UniformPar) p).getMin();
			double max = ((UniformPar) p).getMax();
			return (int) ((long) min + (long) ((1L + (long) max - (long) min) * engine.raw()));
		} else {
			throw new IncorrectDistributionParameterException(
					"Remember: the *max* parameter must be > of the *min* one because min and max reppresent the boud of the distribution");
		}
	}

	/**
	 * it returns the new random number.
	 * This method is used to obtain from the distribution the next number distributed
	 * according to the distribution parameter.
	 *
	 * @param p parameter of the uniform distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the next random number of this distribution.
	 */

	public double nextRand(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			double min = ((UniformPar) p).getMin();
			double max = ((UniformPar) p).getMax();
			return min + (max - min) * engine.raw();
		} else {
			throw new IncorrectDistributionParameterException(
					"Remember: the *max* parameter must be > of the *min* one because min and max reppresent the boud of the distribution");
		}
	}

} // end Uniform
