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
import jmt.engine.math.Probability;
import jmt.engine.math.Sfun;

/**
 *
 * This is the Gamma distribution (see the pdf definition in the
 * constructor description).
 *
 * <br><br>Copyright (c) 2003
 * <br>Politecnico di Milano - dipartimento di Elettronica e Informazione
 * @author Fabrizio Frontera - ffrontera@yahoo.it
 * @author Modified by Stefano Omini, 7/5/2004
 */

public class GammaDistr extends AbstractDistribution implements Distribution {

	/**
	 * This is the constructor. It creates a new gamma distribution which
	 * is defined from is pdf:
	 * <pre>              (alpha-1)    (-x/beta)
	 * pdf(x) = k * x          * e</pre>
	 * with k = 1/(g(alpha) * b^a where g() is the "eulero" function
	 *
	 */
	public GammaDistr() {
	}

	/**
	 * it returns the pdf of the distribution.
	 * This method is used to obtain from the distribution his probability distribution
	 * function evaluated where required by the user.
	 *
	 * @param x double indicating where to evaluate the pdf.
	 * @param p parameter of the gamma distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the probability distribution function evaluated in x.
	 */

	//OLD
	//public double pdf(double x, GammaDistrPar p)
	public double pdf(double x, Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			//OLD
			//double lambda = p.getLambda();
			//double alpha = p.getAlpha();
			double lambda = ((GammaDistrPar) p).getLambda();
			double alpha = ((GammaDistrPar) p).getAlpha();
			if (x < 0) {
				throw new IncorrectDistributionParameterException("x must be >= 0.0");
			}
			if (x == 0) {
				if (alpha == 1.0) {
					return 1.0 / lambda;
				} else {
					return 0.0;
				}
			}
			if (alpha == 1.0) {
				return Math.exp(-x / lambda) / lambda;
			}

			return Math.exp((alpha - 1.0) * Math.log(x / lambda) - x / lambda - Sfun.logGamma(alpha)) / lambda;
		} else {
			throw new IncorrectDistributionParameterException("Remember: parameter alpha and lambda must be gtz");
		}
	}

	/**
	 * it returns the cdf of the distribution.
	 * This method is used to obtain from the distribution his cumulative distribution
	 * function evaluated where required by the user.
	 *
	 * @param x double indicating where to evaluate the cdf.
	 * @param p parameter of the gamma distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the cumulative distribution function evaluated in x.
	 */
	//OLD
	//public double cdf(double x, GammaDistrPar p)
	public double cdf(double x, Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			//OLD
			//double lambda = p.getLambda();
			//double alpha = p.getAlpha();
			double lambda = ((GammaDistrPar) p).getLambda();
			double alpha = ((GammaDistrPar) p).getAlpha();
			return Probability.gamma(alpha, lambda, x);
		} else {
			throw new IncorrectDistributionParameterException("Remember: parameter alpha and lambda must be gtz");
		}
	}

	/**
	 * it returns the mean of the distribution.
	 * This method is used to obtain from the distribution the value of his own
	 * theoretic mean.
	 *
	 * @param p parameter of the gamma distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the theoretic mean of the distribution.
	 *
	 * the theoretic mean of the gamma distribution is calculated as alpha/lamda.
	 */
	//OLD
	//public double theorMean(GammaDistrPar p)
	public double theorMean(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			//OLD
			//double lambda = p.getLambda();
			//double alpha = p.getAlpha();
			double lambda = ((GammaDistrPar) p).getLambda();
			double alpha = ((GammaDistrPar) p).getAlpha();
			return alpha / lambda;
		} else {
			throw new IncorrectDistributionParameterException("Remember: parameter alpha and lambda must be gtz");
		}
	}

	/**
	 * it returns the variance of the distribution.
	 * This method is used to obtain from the distribution his own theoretical
	 * variance.
	 *
	 * @param p parameter of the gamma distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the theoretic variance of the distribution.
	 *
	 * the variance of the gamma distribution is calculated as alpha/(lambda^2).
	 */

	//OLD
	//public double theorVariance(GammaDistrPar p)
	public double theorVariance(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			//OLD
			//double lambda = p.getLambda();
			//double alpha = p.getAlpha();
			double lambda = ((GammaDistrPar) p).getLambda();
			double alpha = ((GammaDistrPar) p).getAlpha();
			return alpha / (lambda * lambda);
		} else {
			throw new IncorrectDistributionParameterException("Remember: parameter alpha and lambda must be gtz");
		}
	}

	/**
	 * it returns the new random number.
	 * This method is used to obtain from the distribution the next number distributed
	 * according to the distribution parameter.
	 *
	 * @param p parameter of the gamma distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the next random number of this distribution.
	 */

	public double nextRand(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			double alpha = ((GammaDistrPar) p).getAlpha();
			double lambda = ((GammaDistrPar) p).getLambda();
			double a = alpha;
			double aa = -1.0, aaa = -1.0,

			b = 0.0, c = 0.0, d = 0.0, e, r, s = 0.0, si = 0.0, ss = 0.0, q0 = 0.0, q1 = 0.0416666664, q2 = 0.0208333723, q3 = 0.0079849875, q4 = 0.0015746717, q5 = -0.0003349403, q6 = 0.0003340332, q7 = 0.0006053049, q8 = -0.0004701849, q9 = 0.0001710320, a1 = 0.333333333, a2 = -0.249999949, a3 = 0.199999867, a4 = -0.166677482, a5 = 0.142873973, a6 = -0.124385581, a7 = 0.110368310, a8 = -0.112750886, a9 = 0.104089866, e1 = 1.000000000, e2 = 0.499999994, e3 = 0.166666848, e4 = 0.041664508, e5 = 0.008345522, e6 = 0.001353826, e7 = 0.000247453;

			double gds, par, q, t, sign_u, u, v, w, x;
			double v1, v2, v12;

			if (a < 1.0) { // CASE A: Acceptance rejection algorithm gs
				b = 1.0 + 0.36788794412 * a; // Step 1
				for (;;) {
					par = b * engine.raw();
					if (par <= 1.0) { // Step 2. Case gds <= 1
						gds = Math.exp(Math.log(par) / a);
						if (Math.log(engine.raw()) <= -gds) {
							return (gds / lambda);
						}
					} else { // Step 3. Case gds > 1
						gds = -Math.log((b - par) / a);
						if (Math.log(engine.raw()) <= ((a - 1.0) * Math.log(gds))) {
							return (gds / lambda);
						}
					}
				}
			} else { // CASE B: Acceptance complement algorithm gd (gaussian distribution, box muller transformation)
				if (a != aa) { // Step 1. Preparations
					aa = a;
					ss = a - 0.5;
					s = Math.sqrt(ss);
					d = 5.656854249 - 12.0 * s;
				}
				// Step 2. Normal deviate
				do {
					v1 = 2.0 * engine.raw() - 1.0;
					v2 = 2.0 * engine.raw() - 1.0;
					v12 = v1 * v1 + v2 * v2;
				} while (v12 > 1.0);
				t = v1 * Math.sqrt(-2.0 * Math.log(v12) / v12);
				x = s + 0.5 * t;
				gds = x * x;
				if (t >= 0.0) {
					return (gds / lambda); // Immediate acceptance
				}

				u = engine.raw(); // Step 3. Uniform random number
				if (d * u <= t * t * t) {
					return (gds / lambda); // Squeeze acceptance
				}

				if (a != aaa) { // Step 4. Set-up for hat case
					aaa = a;
					r = 1.0 / a;
					q0 = ((((((((q9 * r + q8) * r + q7) * r + q6) * r + q5) * r + q4) * r + q3) * r + q2) * r + q1) * r;
					if (a > 3.686) {
						if (a > 13.022) {
							b = 1.77;
							si = 0.75;
							c = 0.1515 / s;
						} else {
							b = 1.654 + 0.0076 * ss;
							si = 1.68 / s + 0.275;
							c = 0.062 / s + 0.024;
						}
					} else {
						b = 0.463 + s - 0.178 * ss;
						si = 1.235;
						c = 0.195 / s - 0.079 + 0.016 * s;
					}
				}
				if (x > 0.0) { // Step 5. Calculation of q
					v = t / (s + s); // Step 6.
					if (Math.abs(v) > 0.25) {
						q = q0 - s * t + 0.25 * t * t + (ss + ss) * Math.log(1.0 + v);
					} else {
						q = q0 + 0.5 * t * t * ((((((((a9 * v + a8) * v + a7) * v + a6) * v + a5) * v + a4) * v + a3) * v + a2) * v + a1) * v;
					} // Step 7. Quotient acceptance
					if (Math.log(1.0 - u) <= q) {
						return (gds / lambda);
					}
				}

				for (;;) { // Step 8. Double exponential deviate t
					do {
						e = -Math.log(engine.raw());
						u = engine.raw();
						u = u + u - 1.0;
						sign_u = (u > 0) ? 1.0 : -1.0;
						t = b + (e * si) * sign_u;
					} while (t <= -0.71874483771719); // Step 9. Rejection of t
					v = t / (s + s); // Step 10. New q(t)
					if (Math.abs(v) > 0.25) {
						q = q0 - s * t + 0.25 * t * t + (ss + ss) * Math.log(1.0 + v);
					} else {
						q = q0 + 0.5 * t * t * ((((((((a9 * v + a8) * v + a7) * v + a6) * v + a5) * v + a4) * v + a3) * v + a2) * v + a1) * v;
					}
					if (q <= 0.0) {
						continue; // Step 11.
					}
					if (q > 0.5) {
						w = Math.exp(q) - 1.0;
					} else {
						w = ((((((e7 * q + e6) * q + e5) * q + e4) * q + e3) * q + e2) * q + e1) * q;
					} // Step 12. Hat acceptance
					if (c * u * sign_u <= w * Math.exp(e - 0.5 * t * t)) {
						x = s + 0.5 * t;
						return (x * x / lambda);
					}
				}
			}
		} else {
			throw new IncorrectDistributionParameterException("Remember: parameter alpha and lambda must be gtz");
		}
	}

} // end GammaDistr
