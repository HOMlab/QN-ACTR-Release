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
 *
 *
 * Created on 24 luglio 2002, 10.33
 */

package jmt.engine.math;

/** This Class computes Cumulative Distribution Function and ICDF for the
 *  TStudent distribution
 *
 * @author  Federico Granata
 * @version
 */
public class TStudent {

	//TODO: il parametro quantile passato deve essere il doppio dell'alfa voluto?
	/**
	 * return the t_student Inverse Cumulative Distribution Function
	 *  prob = 1 - quantile/2
	 * @param   quantile
	 * @param   freedomDegrees
	 */
	public static double ICDF(double quantile, int freedomDegrees) {
		double t = 0;
		int j = 0;
		double p = 1 - (quantile / 2);
		if (p == .6) {
			j = 0;
		} else if (p == .75) {
			j = 1;
		} else if (p == .90) {
			j = 2;
		} else if (p == .95) {
			j = 3;
		} else if (p == .975) {
			j = 4;
		} else if (p == .99) {
			j = 5;
		} else if (p == .995) {
			j = 6;
		} else if (p == .9995) {
			j = 7;
		} else {
			return tStudent(quantile, freedomDegrees, false);
		}
		if (freedomDegrees <= 30) {
			t = T_STUDENT_TABLE[freedomDegrees - 1][j];
		} else {
			t = T_STUDENT_TABLE[30][j] - (1 - 30 / ((double) freedomDegrees)) * (T_STUDENT_TABLE[30][j] - T_STUDENT_TABLE[31][j]);
		}
		return t;
	}

	/** tStudent
	 *  return the t_student table for the assigned percentile and degrees of
	 *  freeedom.
	 *  @param   p          quantile
	 *  @param   ndf        degrees of freedom
	 *  @param   lower_tail if it the lower tail part
	 */
	public static double tStudent(double p, double ndf, boolean lower_tail) {
		// Algorithm 396: Student's t-quantiles by
		// G.W. Hill CACM 13(10), 619-620, October 1970
		if (p <= 0 || p >= 1 || ndf < 1) {
			throw new IllegalArgumentException("Invalid p or df in call to qt(double,double,boolean).");
		}
		double eps = 1e-12;
		double M_PI_2 = 1.570796326794896619231321691640; // pi/2
		boolean neg;
		double P, q, prob, a, b, c, d, y, x;
		if ((lower_tail && p > 0.5) || (!lower_tail && p < 0.5)) {
			neg = false;
			P = 2 * (lower_tail ? (1 - p) : p);
		} else {
			neg = true;
			P = 2 * (lower_tail ? p : (1 - p));
		}

		if (Math.abs(ndf - 2) < eps) { /* df ~= 2 */
			q = Math.sqrt(2 / (P * (2 - P)) - 2);
		} else if (ndf < 1 + eps) { /* df ~= 1 */
			prob = P * M_PI_2;
			q = Math.cos(prob) / Math.sin(prob);
		} else { /*-- usual case;  including, e.g.,  df = 1.1 */
			a = 1 / (ndf - 0.5);
			b = 48 / (a * a);
			c = ((20700 * a / b - 98) * a - 16) * a + 96.36;
			d = ((94.5 / (b + c) - 3) / b + 1) * Math.sqrt(a * M_PI_2) * ndf;
			y = Math.pow(d * P, 2 / ndf);
			if (y > 0.05 + a) {
				/* Asymptotic inverse expansion about normal */
				x = qnorm(0.5 * P, false);
				y = x * x;
				if (ndf < 5) {
					c += 0.3 * (ndf - 4.5) * (x + 0.6);
				}
				c = (((0.05 * d * x - 5) * x - 7) * x - 2) * x + b + c;
				y = (((((0.4 * y + 6.3) * y + 36) * y + 94.5) / c - y - 3) / b + 1) * x;
				y = a * y * y;
				if (y > 0.002) {
					y = Math.exp(y) - 1;
				} else { /* Taylor of    e^y -1 : */
					y = (0.5 * y + 1) * y;
				}
			} else {
				y = ((1 / (((ndf + 6) / (ndf * y) - 0.089 * d - 0.822) * (ndf + 2) * 3) + 0.5 / (ndf + 4)) * y - 1) * (ndf + 1) / (ndf + 2) + 1 / y;
			}
			q = Math.sqrt(ndf * y);
		}
		if (neg) {
			q = -q;
		}
		return q;
	}

	public static double qnorm(double p, boolean upper) {
		/* Reference:
		   J. D. Beasley and S. G. Springer
		   Algorithm AS 111: "The Percentage Points of the Normal Distribution"
		   Applied Statistics
		*/
		if (p < 0 || p > 1) {
			throw new IllegalArgumentException("Illegal argument " + p + " for qnorm(p).");
		}
		double split = 0.42, a0 = 2.50662823884, a1 = -18.61500062529, a2 = 41.39119773534, a3 = -25.44106049637, b1 = -8.47351093090, b2 = 23.08336743743, b3 = -21.06224101826, b4 = 3.13082909833, c0 = -2.78718931138, c1 = -2.29796479134, c2 = 4.85014127135, c3 = 2.32121276858, d1 = 3.54388924762, d2 = 1.63706781897, q = p - 0.5;
		double r, ppnd;
		if (Math.abs(q) <= split) {
			r = q * q;
			ppnd = q * (((a3 * r + a2) * r + a1) * r + a0) / ((((b4 * r + b3) * r + b2) * r + b1) * r + 1);
		} else {
			r = p;
			if (q > 0) {
				r = 1 - p;
			}
			if (r > 0) {
				r = Math.sqrt(-Math.log(r));
				ppnd = (((c3 * r + c2) * r + c1) * r + c0) / ((d2 * r + d1) * r + 1);
				if (q < 0) {
					ppnd = -ppnd;
				}
			} else {
				ppnd = 0;
			}
		}
		if (upper) {
			ppnd = 1 - ppnd;
		}
		return (ppnd);
	}

	private static double[][] T_STUDENT_TABLE = { { 0.60, 0.75, 0.90, 0.95, 0.975, 0.99, 0.995, 0.9995 },
			{ 0.324920, 1.000000, 3.077684, 6.313752, 12.70620, 31.82052, 63.65674, 636.6192 },
			{ 0.288675, 0.816497, 1.885618, 2.919986, 4.30265, 6.96456, 9.92484, 31.5991 },
			{ 0.276671, 0.764892, 1.637744, 2.353363, 3.18245, 4.54070, 5.84091, 12.9240 },
			{ 0.270722, 0.740697, 1.533206, 2.131847, 2.77645, 3.74695, 4.60409, 8.6103 },
			{ 0.267181, 0.726687, 1.475884, 2.015048, 2.57058, 3.36493, 4.03214, 6.8688 },
			{ 0.264835, 0.717558, 1.439756, 1.943180, 2.44691, 3.14267, 3.70743, 5.9588 },
			{ 0.263167, 0.711142, 1.414924, 1.894579, 2.36462, 2.99795, 3.49948, 5.4079 },
			{ 0.261921, 0.706387, 1.396815, 1.859548, 2.30600, 2.89646, 3.35539, 5.0413 },
			{ 0.260955, 0.702722, 1.383029, 1.833113, 2.26216, 2.82144, 3.24984, 4.7809 },
			{ 0.260185, 0.699812, 1.372184, 1.812461, 2.22814, 2.76377, 3.16927, 4.5869 },
			{ 0.259556, 0.697445, 1.363430, 1.795885, 2.20099, 2.71808, 3.10581, 4.4370 },
			{ 0.259033, 0.695483, 1.356217, 1.782288, 2.17881, 2.68100, 3.05454, 4.3178 },
			{ 0.258591, 0.693829, 1.350171, 1.770933, 2.16037, 2.65031, 3.01228, 4.2208 },
			{ 0.258213, 0.692417, 1.345030, 1.761310, 2.14479, 2.62449, 2.97684, 4.1405 },
			{ 0.257885, 0.691197, 1.340606, 1.753050, 2.13145, 2.60248, 2.94671, 4.0728 },
			{ 0.257599, 0.690132, 1.336757, 1.745884, 2.11991, 2.58349, 2.92078, 4.0150 },
			{ 0.257347, 0.689195, 1.333379, 1.739607, 2.10982, 2.56693, 2.89823, 3.9651 },
			{ 0.257123, 0.688364, 1.330391, 1.734064, 2.10092, 2.55238, 2.87844, 3.9216 },
			{ 0.256923, 0.687621, 1.327728, 1.729133, 2.09302, 2.53948, 2.86093, 3.8834 },
			{ 0.256743, 0.686954, 1.325341, 1.724718, 2.08596, 2.52798, 2.84534, 3.8495 },
			{ 0.256580, 0.686352, 1.323188, 1.720743, 2.07961, 2.51765, 2.83136, 3.8193 },
			{ 0.256432, 0.685805, 1.321237, 1.717144, 2.07387, 2.50832, 2.81876, 3.7921 },
			{ 0.256297, 0.685306, 1.319460, 1.713872, 2.06866, 2.49987, 2.80734, 3.7676 },
			{ 0.256173, 0.684850, 1.317836, 1.710882, 2.06390, 2.49216, 2.79694, 3.7454 },
			{ 0.256060, 0.684430, 1.316345, 1.708141, 2.05954, 2.48511, 2.78744, 3.7251 },
			{ 0.255955, 0.684043, 1.314972, 1.705618, 2.05553, 2.47863, 2.77871, 3.7066 },
			{ 0.255858, 0.683685, 1.313703, 1.703288, 2.05183, 2.47266, 2.77068, 3.6896 },
			{ 0.255768, 0.683353, 1.312527, 1.701131, 2.04841, 2.46714, 2.76326, 3.6739 },
			{ 0.255684, 0.683044, 1.311434, 1.699127, 2.04523, 2.46202, 2.75639, 3.6594 },
			{ 0.255605, 0.682756, 1.310415, 1.697261, 2.04227, 2.45726, 2.75000, 3.6460 },
			{ 0.253347, 0.674490, 1.281552, 1.644854, 1.95996, 2.32635, 2.57583, 3.2905 } };

}
