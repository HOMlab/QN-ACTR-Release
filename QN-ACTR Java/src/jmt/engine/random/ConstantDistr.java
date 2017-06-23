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
 * This is the Constant distribution, which returns always the same value,
 * contained in the parameter.
 *
 * <br><br>Copyright (c) 2003
 * <br>Politecnico di Milano - dipartimento di Elettronica e Informazione
 * @author Fabrizio Frontera - ffrontera@yahoo.it
 * @author Modified by Stefano Omini, 7/5/2004
 */

public class ConstantDistr extends AbstractDistribution implements Distribution {

	/**
	 * Returns the pdf of the distribution.
	 * This method is used to obtain from the distribution his probability distribution
	 * function evaluated where required by the user.
	 *
	 * @param x a double indicating where to evaluate the pdf.
	 * @param p parameter of the constant distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the probability distribution function evaluated in x.
	 */

	//OLD
	//public double pdf(double x, ConstantDistrPar p)
	public double pdf(double x, Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			double constValue = ((ConstantDistrPar) p).getParameterValue();
			if (x != constValue) {
				return 0.0;
			} else {
				return 1.0;
			}
		} else {
			throw new IncorrectDistributionParameterException("parameter t must be >0");
		}
	}

	/**
	 * Returns the cdf of the distribution.
	 * This method is used to obtain from the distribution his cumulative distribution
	 * function evaluated where required by the user.
	 *
	 * @param x a double indicating where to evaluate the cdf.
	 * @param p parameter of the constant distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the cumulative distribution function evaluated in x.
	 */

	//OLD
	//public double cdf(double x, ConstantDistrPar p)
	public double cdf(double x, Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			double constValue = ((ConstantDistrPar) p).getParameterValue();
			if (x < constValue) {
				return 0.0;
			} else {
				return 1.0;
			}
		} else {
			throw new IncorrectDistributionParameterException("parameter t must be >0");
		}
	}

	/**
	 * returns the mean of the distribution.
	 * This method is used to obtain from the distribution the value of his own
	 * theoretic mean.
	 *
	 * @param p parameter of the constant distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the theoretic mean of the distribution.
	 */

	//OLD
	//public double theorMean(ConstantDistrPar p)
	public double theorMean(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			//OLD
			//return p.getT();
			return ((ConstantDistrPar) p).getParameterValue();
		} else {
			throw new IncorrectDistributionParameterException("parameter t must be >0");
		}
	}

	/**
	 * returns the variance of the distribution.
	 * This method is used to obtain from the distribution his own theoretical
	 * variance.
	 *
	 * @param p parameter of the constant distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the theoretic varance of the distribution.
	 */

	//OLD
	//public double theorVariance(ConstantDistrPar p)
	public double theorVariance(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			return 0;
		} else {
			throw new IncorrectDistributionParameterException("parameter t must be >0");
		}
	}

	/**
	 * returns the new random number.
	 * This method is used to obtain from the distribution the next number distributed
	 * according to the distribution parameter.
	 *
	 * @param p parameter of the constant distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the next random number of this distribution.
	 */

	public double nextRand(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			double t = ((ConstantDistrPar) p).getParameterValue();
			return t;
		} else {
			throw new IncorrectDistributionParameterException("parameter t must be >0");
		}
	}

} // end ConstantDistr
