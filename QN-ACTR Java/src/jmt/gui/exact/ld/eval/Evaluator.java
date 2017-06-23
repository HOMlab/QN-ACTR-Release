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

package jmt.gui.exact.ld.eval;

import java.net.URL;

import jmt.common.exception.ExpressionParseException;

/**

 * @author alyf (Andrea Conti)
 * Date: 14-set-2003
 * Time: 15.24.17

 */

/**
 * A common interface for expression evaluators
 */
public interface Evaluator {

	/**
	 * @return the value of the expression evaluated in x
	 * @throws jmt.common.exception.ExpressionParseException if there were errors while parsing the expression
	 */
	public double evaluate(String expression, double x) throws ExpressionParseException;

	/**
	 * @return the values of the expression evaluated in x[0]..x[n]
	 * @throws jmt.common.exception.ExpressionParseException if there were errors while parsing the expression
	 */
	public double[] evaluate(String expression, double[] x) throws ExpressionParseException;

	/**
	 * evaluates x[0]..x[n] into an user-supplied array y. Useful to minimize object creation.
	 * @throws jmt.common.exception.ExpressionParseException if there were errors while parsing the expression
	 * @throws IllegalArgumentException if the two arrays have different sizes
	 */
	public void evaluate(String expression, double[] x, double[] y) throws ExpressionParseException;

	/**
	 * @return a help URL for this evaluator
	 */
	public URL getHelpURL();

}
