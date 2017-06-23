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
 * This is the Empirical distribution, which is constructed with
 * data provided by the user.
 *
 * <br><br>Copyright (c) 2003
 * <br>Politecnico di Milano - dipartimento di Elettronica e Informazione
 * @author Fabrizio Frontera - ffrontera@yahoo.it
 */

public class Empirical extends AbstractDistribution implements Distribution {

	/**
	 * This is the constructor. It creates a new empirical distribution.
	 */
	public Empirical() {
	}

	/**
	 *
	 * This method is used to evaluate the probability
	 * distribution function (pdf) where required by the user.
	 *
	 * @param x double indicating where to evaluate the pdf.
	 * @param p parameter of the empirical distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the probability distribution function evaluated in x.
	 */

	public double pdf(double x, Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {

			int k = (int) x;
			double[] pdf = ((EmpiricalPar) p).getPDF();

			//OLD
			//return pdf[k];

			//TODO: l'approssimazione del casting è per difetto o per eccesso??
			//TODO: e se vado oltre??? return 0???
			//NEW
			//@author Stefano Omini
			//
			//if the given x exceeds the pdf array limits, there may be problems!!
			if (k < 0 || k > pdf.length - 1) {
				//pdf is defined only for K = 0, 1, 2 ... (length-1)
				return 0.0;
			} else {
				return pdf[k];
			}
			//end NEW
		} else {
			throw new IncorrectDistributionParameterException(
					"Remember: all the probability value given must be gtz and the sum of all the value must be 1.0");
		}
	}

	/**
	 *
	 * This method is used to evaluate the cumulative distribution
	 * function (cdf) where required by the user.
	 *
	 * @param x double indicating where to evaluate the cdf.
	 * @param p parameter of the empirical distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the cumulative distribution function evaluated in x.
	 */

	public double cdf(double x, Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			//TODO: x è la posizione???
			int k = (int) x;
			double[] cdf = ((EmpiricalPar) p).getCDF();
			if (k < 0) {
				return 0.0;
			}
			if (k >= cdf.length - 1) {
				return 1.0;
			}
			return cdf[k];
		} else {
			throw new IncorrectDistributionParameterException("Remember: all the probability"
					+ " value given must be greater than zero and the sum of all the values must be 1.0");
		}
	}

	/**
	 * It returns the mean value of the distribution.
	 * This method is used to obtain from the distribution the value of his own
	 * theoretic mean, which is is calculated from the data provided by the user
	 * (contained in the parameter p) as the sum of all the data value, divided by
	 * the number of data.
	 *
	 * @param p parameter of the empirical distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the theoretic mean of the distribution.
	 *
	 */

	public double theorMean(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			double[] pdf = ((EmpiricalPar) p).getPDF();
			double mean = 0;
			//it's a simple weighted mean: the weight of each value is the pdf of the value
			//itself
			for (int ptn = 0; ptn < pdf.length; ptn++) {
				mean += pdf[ptn] * ptn;
			}
			return mean;
		} else {
			throw new IncorrectDistributionParameterException(
					"Remember: all the probability value given must be gtz and the sum of all the value must be 1.0");
		}
	}

	/**
	 * it returns the variance of the distribution.
	 * This method is used to obtain from the distribution his own theoretical
	 * variance which is calculated from the data provided by the user (contained
	 * in the parameter p) as the sum of all the squares of the differences between each
	 * data value and the theoretic mean, divided by the number of data.
	 *
	 * @param p parameter of the empirical distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the theoretic varance of the distribution.
	 *
	 */

	public double theorVariance(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			double[] pdf = ((EmpiricalPar) p).getPDF();
			double mean = 0;
			double variance = 0;
			mean = theorMean(p);
			for (int ptn = 0; ptn < pdf.length; ptn++) {
				// TODO: qual è la formula giusta??
				//OLD
				//variance += (((pdf[ptn] - mean) * (pdf[ptn] - mean)) * ptn);
				//NEW
				//@author Stefano Omini
				variance += (((ptn - mean) * (ptn - mean)) * pdf[ptn]);
				//end NEW
			}
			return variance;
		} else {
			throw new IncorrectDistributionParameterException("Remember: "
					+ "all the probability value given must be greater than zero and the sum of all the values must be 1.0");
		}
	}

	/**
	 * it returns the new random number.
	 * This method is used to obtain from the distribution the next number distributed
	 * according to the distribution parameter.
	 *
	 * @param p parameter of the empirical distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the next random number of this distribution.
	 */
	public double nextRand(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			double rand = engine.raw();
			double[] cdf = ((EmpiricalPar) p).getCDF();
			// binary search in cumulative distribution function:
			int nBins = cdf.length - 1;
			int nbelow = 0; // biggest k such that I[k] is known to be <= rand
			int nabove = nBins; // biggest k such that I[k] is known to be >  rand
			while (nabove > nbelow + 1) {
				int middle = (nabove + nbelow + 1) >> 1; // division by 2 obtained as a bit shifting
				if (rand >= cdf[middle]) {
					nbelow = middle;
				} else {
					nabove = middle;
				}
			}
			// after this binary search, nabove is always nbelow+1 and they straddle rand:
			return nbelow;
		} else {
			throw new IncorrectDistributionParameterException("Remember: "
					+ "all the probability value given must be greater than zero and the sum of all the values must be 1.0");
		}
	}

	/**
	 * Computes the next random value and returns the corresponding object
	 * @param p the parameter of the empirical distribution
	 * @return the object corresponding to the computed random value
	 * @throws IncorrectDistributionParameterException
	 */
	public Object nextRandObject(Parameter p) throws IncorrectDistributionParameterException {
		int random = (int) nextRand(p);
		if (p instanceof EmpiricalPar) {
			EmpiricalPar empiricalPar = (EmpiricalPar) p;
			return empiricalPar.getValue(random);
		}
		return null;
	}

} // end Empirical
