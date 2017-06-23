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
import jmt.engine.math.Arithmetic;
import jmt.engine.math.Probability;

/**
 *
 * This is the Poisson distribution (see the constructor description
 * for his pdf definition).
 *
 * <br><br>Copyright (c) 2003 (thanks to CERN - European Organization
 * for Nuclear Research for the Patchwork Rejection/Inversion method).
 * <br>Politecnico di Milano - dipartimento di Elettronica e Informazione
 * @author Fabrizio Frontera - ffrontera@yahoo.it
 * @author Modified by Stefano Omini, 7/5/2004
 */

public class Poisson extends AbstractDistribution implements Distribution {

	// precomputed and cached values (for performance only)
	// cache for < SWITCH_MEAN
	protected double my_old = -1.0;
	protected double par, q, p0, ppp;
	protected double[] pp = new double[36];
	protected int llll;

	// cache for >= SWITCH_MEAN
	protected double my_last = -1.0;
	protected double ll;
	protected int k2, k4, k1, k5;
	protected double dl, dr, r1, r2, r4, r5, lr, l_my, c_pm;
	protected double f1, f2, f4, f5, p1, p2, p3, p4, p5, p6;

	// cache for both;
	protected int m;

	protected static final double MEAN_MAX = Integer.MAX_VALUE; // for all means larger than that, we don't try to compute a poisson deviation, but return the mean.
	protected static final double SWITCH_MEAN = 10.0; // switch from method A to method B

	/**
	 * This is the constructor. It creates a new poisson distribution which
	 * is defined from is pdf:
	 * <pre>         (lambda^x)     (-lambda)
	 * pdf(x) = ----------- * e
	 *              x!</pre>
	 * where lamda is the variance of the distribution and it is also the mean
	 * and x is an integer
	 */

	public Poisson() {
	}

	private static double f(int k, double l_nu, double c_pm) {
		return Math.exp(k * l_nu - Arithmetic.logFactorial(k) - c_pm);
	}

	/**
	 * it returns the pdf of the distribution.
	 * This method is used to obtain from the distribution his probability distribution
	 * function evaluated where required by the user.
	 *
	 * @param x double indicating where to evaluate the pdf. Even if it is defined as double, it is required to be an integer.
	 * @param p parameter of the poisson distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the probability distribution function evaluated in x.
	 */

	//OLD
	//public double pdf(double x, PoissonPar p)
	public double pdf(double x, Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			//OLD
			//double mean = p.getMean();
			double mean = ((PoissonPar) p).getMean();
			if (Math.floor(x) != x) {
				throw new IncorrectDistributionParameterException("Error: x must be integer.");
			}
			return Math.exp(x * Math.log(mean) - Arithmetic.logFactorial((int) x) - mean);
		} else {
			throw new IncorrectDistributionParameterException("Remember: parameter mean must be gtz");
		}
	}

	/**
	 * it returns the cdf of the distribution.
	 * This method is used to obtain from the distribution his cumulative distribution
	 * function evaluated where required by the user.
	 *
	 * @param x double indicating where to evaluate the cdf.
	 * @param p parameter of the poisson distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the cumulative distribution function evaluated in x.
	 */

	//OLD
	//public double cdf(double x, PoissonPar p)
	public double cdf(double x, Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			//OLD
			//double mean = p.getMean();
			double mean = ((PoissonPar) p).getMean();
			return Probability.poisson((int) x, mean);
		} else {
			throw new IncorrectDistributionParameterException("Remember: parameter mean must be gtz");
		}
	}

	/**
	 * it returns the mean of the distribution.
	 * This method is used to obtain from the distribution the value of his own
	 * theoretic mean.
	 *
	 * @param p parameter of the poisson distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the theoretic mean of the distribution.
	 *
	 * the theoretic mean is simply the parameter mean supplied by the user.
	 */

	//OLD
	//public double theorMean(PoissonPar p)
	public double theorMean(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			//OLD
			//return p.getMean();
			return ((PoissonPar) p).getMean();
		} else {
			throw new IncorrectDistributionParameterException("Remember: parameter mean must be gtz");
		}
	}

	/**
	 * it returns the variance of the distribution.
	 * This method is used to obtain from the distribution his own theoretical
	 * variance.
	 *
	 * @param p parameter of the poisson distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the theoretic variance of the distribution.
	 *
	 * the theoretic variance is simply the parameter mean
	 */

	//OLD
	//public double theorVariance(PoissonPar p)
	public double theorVariance(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			//OLD
			//return p.getMean();
			return ((PoissonPar) p).getMean();
		} else {
			throw new IncorrectDistributionParameterException("Remember: parameter mean must be gtz");
		}
	}

	/**
	 * it returns the new random number.
	 * This method is used to obtain from the distribution the next number distributed
	 * according to the distribution parameter.
	 *
	 * @param p parameter of the poisson distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the next random number of this distribution.
	 */

	public double nextRand(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			/******************************************************************
			 *                                                                *
			 * Poisson Distribution - Patchwork Rejection/Inversion           *
			 * with many thanks to CERN - European                            *
			 * Organization for Nuclear Research.                             *
			 *                                                                *
			 ******************************************************************
			 *                                                                *
			 * For parameter  my < 10  Tabulated Inversion is applied.        *
			 * For my >= 10  Patchwork Rejection is employed:                 *
			 * The area below the histogram function f(x) is rearranged in    *
			 * its body by certain point reflections. Within a large center   *
			 * interval variates are sampled efficiently by rejection from    *
			 * uniform hats. Rectangular immediate acceptance regions speed   *
			 * up the generation. The remaining tails are covered by          *
			 * exponential functions.                                         *
			 *                                                                *
			 *****************************************************************/
			double my = ((PoissonPar) p).getMean();

			double u;
			int k, i;

			if (my < SWITCH_MEAN) { // CASE B: Inversion- start new table and calculate p0
				if (my != my_old) {
					my_old = my;
					llll = 0;
					ppp = Math.exp(-my);
					q = ppp;
					p0 = ppp;
				}
				m = (my > 1.0) ? (int) my : 1;
				for (;;) {
					u = engine.raw(); // Step U. Uniform sample
					k = 0;
					if (u <= p0) {
						return (k);
					}
					if (llll != 0) { // Step T. Table comparison
						i = (u > 0.458) ? Math.min(llll, m) : 1;
						for (k = i; k <= llll; k++) {
							if (u <= pp[k]) {
								return (k);
							}
						}
						if (llll == 35) {
							continue;
						}
					}
					for (k = llll + 1; k <= 35; k++) { // Step C. Creation of new prob.
						ppp *= my / k;
						q += ppp;
						pp[k] = q;
						if (u <= q) {
							llll = k;
							return (k);
						}
					}
					llll = 35;
				}
			} // end my < SWITCH_MEAN
			else if (my < MEAN_MAX) { // CASE A: acceptance complement
				int Dk, X, Y;
				double Ds, U, V, W;

				m = (int) my;
				if (my != my_last) { //  set-up
					my_last = my;

					// approximate deviation of reflection points k2, k4 from my - 1/2
					Ds = Math.sqrt(my + 0.25);

					// mode m, reflection points k2 and k4, and points k1 and k5, which
					// delimit the centre region of h(x)
					k2 = (int) Math.ceil(my - 0.5 - Ds);
					k4 = (int) (my - 0.5 + Ds);
					k1 = k2 + k2 - m + 1;
					k5 = k4 + k4 - m;

					// range width of the critical left and right centre region
					dl = (k2 - k1);
					dr = (k5 - k4);

					// recurrence constants r(k) = ppp(k)/ppp(k-1) at k = k1, k2, k4+1, k5+1
					r1 = my / k1;
					r2 = my / k2;
					r4 = my / (k4 + 1);
					r5 = my / (k5 + 1);

					// reciprocal values of the scale parameters of expon. tail envelopes
					ll = Math.log(r1); // expon. tail left
					lr = -Math.log(r5); // expon. tail right

					// Poisson constants, necessary for computing function values f(k)
					l_my = Math.log(my);
					c_pm = m * l_my - Arithmetic.logFactorial(m);

					// function values f(k) = ppp(k)/ppp(m) at k = k2, k4, k1, k5
					f2 = f(k2, l_my, c_pm);
					f4 = f(k4, l_my, c_pm);
					f1 = f(k1, l_my, c_pm);
					f5 = f(k5, l_my, c_pm);

					// area of the two centre and the two exponential tail regions
					// area of the two immediate acceptance regions between k2, k4
					p1 = f2 * (dl + 1.0); // immed. left
					p2 = f2 * dl + p1; // centre left
					p3 = f4 * (dr + 1.0) + p2; // immed. right
					p4 = f4 * dr + p3; // centre right
					p5 = f1 / ll + p4; // expon. tail left
					p6 = f5 / lr + p5; // expon. tail right
				} // end set-up

				for (;;) {
					// generate uniform number U -- U(0, p6)
					// case distinction corresponding to U
					if ((U = engine.raw() * p6) < p2) { // centre left

						// immediate acceptance region R2 = [k2, m) *[0, f2),  X = k2, ... m -1
						if ((V = U - p1) < 0.0) {
							return (k2 + (int) (U / f2));
						}
						// immediate acceptance region R1 = [k1, k2)*[0, f1),  X = k1, ... k2-1
						if ((W = V / dl) < f1) {
							return (k1 + (int) (V / f1));
						}

						// computation of candidate X < k2, and its counterpart Y > k2
						// either squeeze-acceptance of X or acceptance-rejection of Y
						Dk = (int) (dl * engine.raw()) + 1;
						if (W <= f2 - Dk * (f2 - f2 / r2)) { // quick accept of
							return (k2 - Dk); // X = k2 - Dk
						}
						if ((V = f2 + f2 - W) < 1.0) { // quick reject of Y
							Y = k2 + Dk;
							if (V <= f2 + Dk * (1.0 - f2) / (dl + 1.0)) {// quick accept of
								return (Y); // Y = k2 + Dk
							}
							if (V <= f(Y, l_my, c_pm)) {
								return (Y); // final accept of Y
							}
						}
						X = k2 - Dk;
					} else if (U < p4) { // centre right
						// immediate acceptance region R3 = [m, k4+1)*[0, f4), X = m, ... k4
						if ((V = U - p3) < 0.0) {
							return (k4 - (int) ((U - p2) / f4));
						}
						// immediate acceptance region R4 = [k4+1, k5+1)*[0, f5)
						if ((W = V / dr) < f5) {
							return (k5 - (int) (V / f5));
						}

						// computation of candidate X > k4, and its counterpart Y < k4
						// either squeeze-acceptance of X or acceptance-rejection of Y
						Dk = (int) (dr * engine.raw()) + 1;
						if (W <= f4 - Dk * (f4 - f4 * r4)) { // quick accept of
							return (k4 + Dk); // X = k4 + Dk
						}
						if ((V = f4 + f4 - W) < 1.0) { // quick reject of Y
							Y = k4 - Dk;
							if (V <= f4 + Dk * (1.0 - f4) / dr) { // quick accept of
								return (Y); // Y = k4 - Dk
							}
							if (V <= f(Y, l_my, c_pm)) {
								return (Y); // final accept of Y
							}
						}
						X = k4 + Dk;
					} else {
						W = engine.raw();
						if (U < p5) { // expon. tail left
							Dk = (int) (1.0 - Math.log(W) / ll);
							if ((X = k1 - Dk) < 0) {
								continue; // 0 <= X <= k1 - 1
							}
							W *= (U - p4) * ll; // W -- U(0, h(x))
							if (W <= f1 - Dk * (f1 - f1 / r1)) {
								return (X); // quick accept of X
							}
						} else { // expon. tail right
							Dk = (int) (1.0 - Math.log(W) / lr);
							X = k5 + Dk; // X >= k5 + 1
							W *= (U - p5) * lr; // W -- U(0, h(x))
							if (W <= f5 - Dk * (f5 - f5 * r5)) {
								return (X); // quick accept of X
							}
						}
					}

					// acceptance-rejection test of candidate X from the original area
					// test, whether  W <= f(k),    with  W = U*h(x)  and  U -- U(0, 1)
					// getLog f(X) = (X - m)*getLog(my) - getLog X! + getLog m!
					if (Math.log(W) <= X * l_my - Arithmetic.logFactorial(X) - c_pm) {
						return (X);
					}
				}
			} else { // mean is too large
				return (int) my;
			}
		} else {
			throw new IncorrectDistributionParameterException("Remember: parameter mean must be gtz");
		}
	}

} // end Poisson
