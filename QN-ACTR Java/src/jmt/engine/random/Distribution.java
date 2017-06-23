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

/* Distribution abstract class */
package jmt.engine.random;

import jmt.common.exception.IncorrectDistributionParameterException;

/**
 * This is the Distribution abstract class, which is the interface of all distributions.
 *
 * <br><br>Copyright (c) 2003
 * <br>Politecnico di Milano - dipartimento di Elettronica e Informazione
 * @author Federico Granata (modified by Fabrizio Frontera)
 * @author Modified by Stefano Omini, 7/5/2004
 *
 */

public interface Distribution {

	/**
	 * return the new random number.
	 * This method is used to ask all the distribution to return a new random number
	 * distributed according with the distribution name.
	 *
	 * @param p parameter of the distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the next random number from the sequence.
	 */

	public abstract double nextRand(Parameter p) throws IncorrectDistributionParameterException;

	/////@author STEFANO OMINI

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

	public abstract double pdf(double x, Parameter p) throws IncorrectDistributionParameterException;

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

	public abstract double cdf(double x, Parameter p) throws IncorrectDistributionParameterException;

	/**
	 * returns the mean of the distribution.
	 * This method is used to obtain from the distribution the value of his own
	 * theoretic mean.
	 *
	 * @param p parameter of the constant distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the theoretic mean of the distribution.
	 */

	public abstract double theorMean(Parameter p) throws IncorrectDistributionParameterException;

	/**
	 * returns the variance of the distribution.
	 * This method is used to obtain from the distribution his own theoretical
	 * variance.
	 *
	 * @param p parameter of the constant distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the theoretic varance of the distribution.
	 */

	public abstract double theorVariance(Parameter p) throws IncorrectDistributionParameterException;

	/////END @author STEFANO OMINI

} // end Distribution
