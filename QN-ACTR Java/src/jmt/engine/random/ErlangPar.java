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
 * This is the parameter that should be passed to the Erlang
 * distribution.
 *
 * <br><br>Copyright (c) 2003
 * <br>Politecnico di Milano - dipartimento di Elettronica e Informazione
 * @author Fabrizio Frontera - ffrontera@yahoo.it
 * @author Bertoli Marco 10-oct-2005
 *
 */

public class ErlangPar extends AbstractParameter implements Parameter {

	private double alpha;
	private double r;

	/**
	 * creates a new erlang parameter.
	 * It creates a new erlang parameter with the information provided by the user.
	 * @param alpha double containing the "scale" parameter (must be greater than zero).
	 * @param r double containing the "shape" parameter (must be greater than zero).
	 *
	 * @throws IncorrectDistributionParameterException if any of the double provided is less or equal to zero.
	 *
	 */

	public ErlangPar(double alpha, double r) throws IncorrectDistributionParameterException {
		this.alpha = alpha;
		this.r = r;
		testParameters();
	}

	public ErlangPar(Double walpha, Double wr) throws IncorrectDistributionParameterException {
		this(walpha.doubleValue(), wr.doubleValue());
	}

	/**
	 * test the parameters.
	 * Tests the parametrs of an erlang distribution, if alpha or r are
	 * equal to zero, throw an exception. This is only a service method called by
	 * every method used to modify the value of the parameter.
	 *
	 * @throws IncorrectDistributionParameterException if any of the double provided is less or equal to zero.
	 */
	private void testParameters() throws IncorrectDistributionParameterException {
		if (alpha <= 0) {
			throw new IncorrectDistributionParameterException("alpha <=0");
		}
		if (r <= 0) {
			throw new IncorrectDistributionParameterException("r <=0");
		}
	}

	/**
	 * It controls if the parameter are correct or not.
	 * For the erlang distribution, the parameter are right if they are both greater
	 * than zero.
	 *
	 * @return boolean, indicating wether the parameter is correct or not.
	 *
	 */

	@Override
	public boolean check() {
		if ((alpha <= 0) || (r <= 0)) {
			return false;
		} else {
			return true;
		}
	}

	/**
	 * it returns the value of alpha.
	 * It returns the parameter alpha of the erlang parameter, also called the "scale" parameter.
	 *
	 * @return double with alpha which is the "scale" parameter.
	 *
	 */

	public double getAlpha() {
		return alpha;
	}

	/**
	 * it returns the value of r.
	 * It returns the parameter r of the erlang distribution, also called the "shape" parameter.
	 *
	 * @return double with r which is the "shape" parameter.
	 *
	 */

	public double getR() {
		return r;
	}

	/**
	 * it change the value of alpha.
	 * It allow the user to modify the value of the alpha parameter of the erlang distribution.
	 * It verify that the new value is correct. Alpha must be greater than zero.
	 *
	 * @param alpha double indicating the new value of the parameter alpha.
	 * @throws IncorrectDistributionParameterException if the provided parameter is less or equal to zero.
	 *
	 */

	public void setAlpha(double alpha) throws IncorrectDistributionParameterException {
		this.alpha = alpha;
		try {
			testParameters();
		} catch (IncorrectDistributionParameterException ie) {
			throw ie;
		}
	}

	/**
	 * it change the value of r.
	 * It allow the user to modify the value of the r parameter of the erlang distribution.
	 * It verify that the new value is correct. r must be greater than zero.
	 *
	 * @param r double indicating the new value of the parameter r
	 * @throws IncorrectDistributionParameterException
	 *
	 */

	public void setR(double r) throws IncorrectDistributionParameterException {
		this.r = r;
		try {
			testParameters();
		} catch (IncorrectDistributionParameterException ie) {
			throw ie;
		}
	}

	/**
	 * Sets mean for a given distribution parameter. This method is required to adjust distributions
	 * mean values into Load Dependent Service Time Strategy
	 * @param meanValue new mean value for this distribution
	 * Author: Bertoli Marco
	 * @throws IncorrectDistributionParameterException if mean value is invalid for this distribution
	 */
	@Override
	public void setMean(double meanValue) throws IncorrectDistributionParameterException {
		if (meanValue <= 0 || Double.isInfinite(meanValue)) {
			throw new IncorrectDistributionParameterException("Mean value must be finite and greater than zero");
		}
		alpha = r / meanValue;
	}

} // end ErlangPar
