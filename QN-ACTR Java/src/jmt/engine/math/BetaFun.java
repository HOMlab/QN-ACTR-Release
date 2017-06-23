/*
 * BetaFun.java
 *
 * Created on 1 novembre 2002, 0.42
 */

package jmt.engine.math;

/** * @(#)pf.js * * Copyright (c) 2000 by Sundar Dorai-Raj
 * * @author Sundar Dorai-Raj
 * * Email: sdoraira@vt.edu
 * * This program is free software; you can redistribute it and/or
 * * modify it under the terms of the GNU General Public License
 * * as published by the Free Software Foundation; either version 2
 * * of the License, or (at your option) any later version,
 * * provided that any use properly credits the author.
 * * This program is distributed in the hope that it will be useful,
 * * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * * GNU General Public License for more details at http://www.gnu.org * * */
public class BetaFun {

	public static double lnBeta(double a, double b) {
		return (GammaFun.lnGamma(a) + GammaFun.lnGamma(b) - GammaFun.lnGamma(a + b));
	}

	public static double betaInv(double x1, double p, double q) {
		// ALGORITHM AS 63 APPL. STATIST. VOL.32, NO.1
		// Computes P(Beta>x)
		double beta, acu, psq, cx, x2, pp, qq, term, ai, betain, ns, rx, temp;
		boolean indx;
		beta = lnBeta(p, q);
		acu = 1e-14;
		if (p <= 0 || q <= 0) {
			throw new IllegalArgumentException("the degrees of freedom of a Fisher distribution must be > 0");
		}
		if (x1 <= 0 || x1 >= 1) {
			throw new IllegalArgumentException("probability must be > 0 and < 1");
		}
		psq = p + q;
		cx = 1 - x1;
		if (p < psq * x1) {
			x2 = cx;
			cx = x1;
			pp = q;
			qq = p;
			indx = true;
		} else {
			x2 = x1;
			pp = p;
			qq = q;
			indx = false;
		}
		term = 1;
		ai = 1;
		betain = 1;
		ns = qq + cx * psq;
		rx = x2 / cx;
		temp = qq - ai;
		if (ns == 0) {
			rx = x2;
		}
		while (temp > acu && temp > acu * betain) {
			term = term * temp * rx / (pp + ai);
			betain = betain + term;
			temp = Math.abs(term);
			if (temp > acu && temp > acu * betain) {
				ai++;
				ns--;
				if (ns >= 0) {
					temp = qq - ai;
					if (ns == 0) {
						rx = x2;
					}
				} else {
					temp = psq;
					psq += 1;
				}
			}
		}
		betain *= Math.exp(pp * Math.log(x2) + (qq - 1) * Math.log(cx) - beta) / pp;
		if (indx) {
			betain = 1 - betain;
		}
		return betain;
	}

}
