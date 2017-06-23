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

/*
 * PolyFit.java
 *
 * Created on 2 novembre 2002, 20.53
 */

package jmt.engine.dataAnalysis;

import Jama.Matrix;
import Jama.SingularValueDecomposition;

/**
 *  This class performs polynomial fitting on given data: there are two
 *  alghorithms.
 *
 * @author  Federico Granata
 */
public class PolyFit {

	/**
	 * fit:
	 * returns the coefficients of the polynomial which best approximates f
	 * (in the mean square error sense) for the given data.
	 *  y = f(x).
	 *  y' = a0 + a1 x + ..... + an x^(order)
	 *
	 *  @param  ndata   number of data;
	 *  @param  x       values of the indipendent variable
	 *  @param  y       values of the dipendent variable (data)
	 *  @param  order   the order of the polynomial
	 *
	 *  @return array (of order+1 double) of the coefficients of the polynomial
	 */
	public static double[] fit(int ndata, double[] x, double[] y, int order) {
		double[] a = new double[order + 1];//coefficient of polynomial
		double[][] alfa = new double[order + 1][order + 1];
		double[][] beta = new double[order + 1][1];
		double[] poli = new double[order + 1];//values of y'

		if ((y.length < ndata) || (x.length < ndata)) {
			return a;
		}

		//create the matrix to solve to find the polynomial coefficient
		poli[0] = 1;
		for (int i = 0; i < ndata; i++) {
			for (int j = 1; j <= order; j++) {
				poli[j] = Math.pow(x[i], j);//value of polynomial at x
			}
			for (int k = 0; k <= order; k++) {
				beta[k][0] += y[i] * poli[k];
				for (int j = 0; j <= order; j++) {
					alfa[k][j] += poli[k] * poli[j];
				}
			}
		}

		Matrix mAlfa = new Matrix(alfa);
		Matrix mBeta = new Matrix(beta);
		Matrix mA = new Matrix(order + 1, 1);
		mA = mAlfa.solve(mBeta);
		beta = mA.getArrayCopy();
		for (int i = 0; i <= order; i++) {
			a[i] = beta[i][0];
		}
		return a;
	}

	/** fitSVD:
	 * returns the coefficients of the polynomial which best approximates f
	 * (in the mean square error sense) for the given data.
	 *  y = f(x).
	 *  y' = a0 + a1 x + ..... + an x^(order)
	 * it uses the Singular Value Decomposition tecnique, very stable.
	 * For a complete description see "Numerical recipes in C"
	 *
	 * @return array (of order+1 double) of the coefficients of the polynomial
	 * @param ndata number of data
	 * @param x values of the independent variable
	 * @param y values of the dependent variable (data)
	 * @param sd the standard deviation of dependent point
	 * @param order the order of the polynomial
	 */
	public static double[] fitSVD(int ndata, double[] x, double[] y, double[] sd, int order) {
		if ((y.length < ndata) || (x.length < ndata) || ((sd != null) && (sd.length < ndata))) {
			throw new IllegalArgumentException("data paramenters incorrect too short vector");
		}
		Matrix A = new Matrix(ndata, order + 1);
		double[] b = new double[ndata];
		SingularValueDecomposition SVD;
		double[] a = new double[order + 1];
		double xVal;
		double yVal;
		double small = 0.0000000001 * ndata;
		double[] coeff = new double[order + 1];
		double[][] U;
		double[][] V;
		double[] w;

		if (sd == null) {
			sd = new double[ndata];
			for (int i = 0; i < ndata; i++) {
				sd[i] = 1;
			}
		}

		for (int i = 0; i < ndata; i++) {
			A.set(i, 0, 1 / sd[i]);
		}
		for (int i = 0; i < ndata; i++) {
			xVal = 1;
			yVal = 1;
			for (int j = 1; j <= order; j++) {
				xVal *= x[i];
				yVal += xVal;
				A.set(i, j, xVal / sd[i]);
			}
			b[i] = yVal / sd[i];
		}
		SVD = A.svd();
		U = (SVD.getU()).getArray();
		V = (SVD.getV()).getArray();
		w = SVD.getSingularValues();

		for (int j = 0; j <= order; j++) {
			coeff[j] = 0;
			if (w[j] > small) {
				for (int i = 0; i < ndata; i++) {
					coeff[j] += U[i][j] * b[i];
				}
				coeff[j] /= w[j];
			}
		}

		for (int j = 0; j <= order; j++) {
			a[j] = 0;
			for (int k = 0; k <= order; k++) {
				a[j] += coeff[k] * V[k][j];
			}
		}

		return a;
	}

	/* writes some data in D:/java/ParameterAnalysis/provaPoly.dat
	 *  then print some polynomial fit coefficients, You have to test them
	 *  against matlab
	 *  @param  args the command line arguments
	 */

	/** tests the fitSVD method
	 */
	public static void testSVD() {
		//Log l = new Log("D:/java/ParameterAnalysis/provaPoly.dat");
		double[] y = new double[200];
		double[] x = new double[200];
		int order;

		for (int i = 0; i < 200; i++) {
			x[i] = i + 1;
			y[i] = Math.random();
			//System.out.println(y[i]+" ");
			//l.write(pippo);
		}
		System.out.println();
		for (order = 0; order < 10; order++) {
			double[] a;
			a = fitSVD(200, x, y, null, order);
			for (int i = 0; i <= order; i++) {
				System.out.print(a[i] + "  ");
			}
			System.out.println();
			a = fit(200, x, y, order);
			for (int i = 0; i <= order; i++) {
				System.out.print(a[i] + "  ");
			}
			System.out.println();
		}
		//l.close();
	}

}
