/*
 * Created on Oct 31, 2006
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package jmt.engine.random;

import jmt.common.exception.IncorrectDistributionParameterException;

/**
 * @author casale
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class MAP extends AbstractDistribution implements Distribution {

	private int curState;
	protected Exponential expDistr;

	public MAP() {
		curState = 0;
		expDistr = new Exponential();
	}

	public int getCurState() {
		return curState;
	}

	public void setCurState(int newState) {
		curState = newState;
	}

	public double pdf(double x, Parameter p) { //other implementation may use p.check()
		return 0.0;
	}

	public double cdf(double x, Parameter p) { //other implementation may use p.check()
		return 0.0;
	}

	public double theorMean(Parameter p) throws IncorrectDistributionParameterException {
		return 0.0;
	}

	public double theorVariance(Parameter p) throws IncorrectDistributionParameterException {
		return 0.0;
	}

	/**
	 * it returns the new random number.
	 * This method is used to obtain from the distribution the next number distributed
	 * according to the distribution parameter.
	 *
	 * @param p parameter of the hyper exponential distribution.
	 * @throws IncorrectDistributionParameterException
	 * @return double with the next random number of this distribution.
	 */

	public double nextRand(Parameter p) throws IncorrectDistributionParameterException {
		if (p.check()) {
			if (this.getCurState() == 0) {
				if (engine.nextDouble() <= ((MAPPar) p).getR0()) {
					this.setCurState(1);
				}
				return expDistr.nextRand(((MAPPar) p).getExpParam1());
			} else {
				if (engine.nextDouble() <= ((MAPPar) p).getR1()) {
					this.setCurState(0);
				}
				return expDistr.nextRand(((MAPPar) p).getExpParam2());
			}
		} else {
			throw new IncorrectDistributionParameterException(
					"Remember: parameter mean, variance, lambda1 and lambda 2 must be gtz; p must be a number betwen 0 and 1");
		}
	}

}
