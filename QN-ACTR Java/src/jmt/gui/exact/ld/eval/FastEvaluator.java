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
import jmt.engine.math.parser.Parser;
import jmt.gui.common.editors.LDStrategyEditor;

/**
 * <p><b>Name:</b> FastEvaluator</p> 
 * <p><b>Description:</b> 
 * Evaluates given expression using my java fast expression evaluator.
 * </p>
 * <p><b>Date:</b> 09/dic/06
 * <b>Time:</b> 10:38:30</p>
 * @author Bertoli Marco
 * @version 1.0
 */
public class FastEvaluator implements Evaluator {
	private static final String X = "n";

	private URL helpURL;

	/* (non-Javadoc)
	 * @see jmt.gui.exact.ld.eval.Evaluator#evaluate(java.lang.String, double)
	 */
	public double evaluate(String expression, double x) throws ExpressionParseException {
		try {
			Parser p = new Parser(expression, true);
			p.setVariable(X, x);
			return p.getValue();
		} catch (RuntimeException ex) {
			throw new ExpressionParseException(ex.getMessage());
		}
	}

	/* (non-Javadoc)
	 * @see jmt.gui.exact.ld.eval.Evaluator#evaluate(java.lang.String, double[])
	 */
	public double[] evaluate(String expression, double[] x) throws ExpressionParseException {
		try {
			Parser p = new Parser(expression, true);
			double[] y = new double[x.length];
			for (int i = 0; i < x.length; i++) {
				p.setVariable(X, x[i]);
				y[i] = p.getValue();
			}
			return y;
		} catch (RuntimeException ex) {
			throw new ExpressionParseException(ex.getMessage());
		}
	}

	/* (non-Javadoc)
	 * @see jmt.gui.exact.ld.eval.Evaluator#evaluate(java.lang.String, double[], double[])
	 */
	public void evaluate(String expression, double[] x, double[] y) throws ExpressionParseException {
		if (x.length != y.length) {
			throw new IllegalArgumentException("x and y must be the same length");
		}
		for (int i = 0; i < x.length; i++) {
			y[i] = evaluate(expression, x[i]);
		}
	}

	/* (non-Javadoc)
	 * @see jmt.gui.exact.ld.eval.Evaluator#getHelpText()
	 */
	public URL getHelpURL() {
		loadHelp();
		return helpURL;
	}

	/**
	 * Creates the help string, retrieving it from a html file.
	 */
	private void loadHelp() {
		if (helpURL == null) {
			helpURL = LDStrategyEditor.class.getResource(LDStrategyEditor.HELPFILE);
		}
	}

}
