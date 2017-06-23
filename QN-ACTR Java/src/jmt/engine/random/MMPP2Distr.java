/*
 * Created on Oct 31, 2006
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package jmt.engine.random;

import jmt.common.exception.IncorrectDistributionParameterException;
import jmt.engine.random.engine.RandomEngine;

/**
 * @author casale
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class MMPP2Distr extends AbstractDistribution implements Distribution {
	private int curState;
	protected Exponential expDistr;

	public MMPP2Distr() {
		curState = 0;
		engine = RandomEngine.makeDefault();
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
		return (((MMPP2Par) p).getSigma0() + ((MMPP2Par) p).getSigma1())
				/ (((MMPP2Par) p).getLambda0() * ((MMPP2Par) p).getSigma1() + ((MMPP2Par) p).getLambda1() * ((MMPP2Par) p).getSigma0());
	}

	public double theorVariance(Parameter p) throws IncorrectDistributionParameterException {
		return 0.0;//(2*q21*mu11^2*q12+mu11*q21^2*mu22-2*q12*mu22*q21*mu11+q12^2*mu22*mu11+q21*mu11*q12^2+mu11*q21^3+2*mu11*q21^2*q12+2*q12*mu22^2*q21+q12*mu22*q21^2+q12^3*mu22+2*q12^2*mu22*q21)/(mu11*q21+q12*mu22)^2/(q12*mu22+mu11*q21+mu11*mu22);
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
		double r = 0.0;
		if (p.check()) {
			if (this.getCurState() == 0) {
				r = r + expDistr.nextRand(((MMPP2Par) p).getExpParam0());
				if (engine.nextDouble() <= ((MMPP2Par) p).getSigma0() / (((MMPP2Par) p).getSigma0() + ((MMPP2Par) p).getLambda0())) {
					this.setCurState(1);
					r = r + nextRand(p);
				}
				return r;
			} else {
				r = r + expDistr.nextRand(((MMPP2Par) p).getExpParam1());
				if (engine.nextDouble() <= ((MMPP2Par) p).getSigma1() / (((MMPP2Par) p).getSigma1() + ((MMPP2Par) p).getLambda1())) {
					this.setCurState(0);
					r = r + nextRand(p);
				}
				return r;
			}
		} else {
			throw new IncorrectDistributionParameterException("Expection in generation of MMPP2 random numbers");
		}
	}

}
