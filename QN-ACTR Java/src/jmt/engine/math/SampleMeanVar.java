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
 * SampleMeanVar.java
 *
 * Created on 19 ottobre 2002, 14.56
 */
package jmt.engine.math;

/**
 *
 * @author  Federico Granata
 */
public class SampleMeanVar {
	protected double sum;// sum of all data
	protected double smean;//sample mean of all data
	protected double svar;//sample variance of data
	protected double n;//number of data
	protected double max;//the greatest sample of all data inserted
	protected double min;//the smallestest sample of all data inserted
	protected double S;// sum of (sample - mean)^2
	protected double confInt;

	/** Creates a new instance of SampleMeanVar */
	public SampleMeanVar() {
		sum = 0;
		smean = 0;
		svar = 0;
		n = 0;
		max = 0;
		min = 0;
		S = 0;
		confInt = 0;
	}

	/** Creates a new instance of SampleMeanVar calculating the parameters with
	 * the initializing data sequence
	 *  @param data the initial data sequence;
	 */
	public SampleMeanVar(double[] data) {
		sum = 0;
		smean = 0;
		svar = 0;
		n = 0;
		max = 0;
		min = 0;
		S = 0;
		confInt = 0;
		for (double element : data) {
			putNewSample(element);
		}
	}

	/** initalize a new SampleMeanVar with parameters all zero
	 */
	public void initialize() {
		sum = 0;
		smean = 0;
		svar = 0;
		n = 0;
		max = 0;
		min = 0;
		S = 0;
		confInt = 0;
	}

	/** update the sample mean & variance with the new value it uses stable
	 *  algorithms
	 *  @param  newValue
	 */
	public void putNewSample(double newValue) {
		calcVar(newValue);
		// coumputes the maximum and the minimum value of the sequence
		max = Math.max(max, newValue);
		min = Math.min(min, newValue);
	}

	/** coumputes the new mean using a more stable algorithm
	 *  @param newValue the new element to be added to the sequence
	 */
	protected void calcMean(double newValue) {
		n++;
		smean += (newValue - smean) / n;
	}

	/** coumputes the new sample variance using a stable algorithm
	 *  it also computes the mean with the stable algorithm.
	 *  @param newValue the new element to be added to the sequence
	 */
	protected void calcVar(double newValue) {
		double y;
		double z;
		n++;
		if (n > 1) {
			y = newValue - smean;
			z = y / n;
			smean += z;
			S += (n - 1) * y * z;
			svar = S / (n - 1);
		} else {
			smean = newValue;
			S = 0;
			svar = 0;
		}
	}

	/** get the sample mean
	 *  @return the actual sample mean
	 */
	public double getMean() {
		return smean;
	}

	/** get the sample variance
	 *  @return the actual sample variance
	 */
	public double getVar() {
		return svar;
	}

	/** get the gratest value
	 *  @return the actual max value
	 */
	public double getMax() {
		return max;
	}

	/** get the smallest value
	 *  @return the actual min value
	 */
	public double getMin() {
		return min;
	}

	private void calcConfInt(double perc) {
		double alfa = (1 - (perc / 100));
		double t = TStudent.ICDF(alfa, (int) n);
		confInt = t * Math.sqrt(svar / n);
	}

	/** get the actual conf int
	 * it is 0 if the variance has not been calculated
	 *  @param  perc    the percentile required
	 *  @return the confidence interval requested
	 */
	public double getConfInt(double perc) {
		calcConfInt(perc);
		return confInt;
	}

	/** creats a string with the calculated parametrs of the sequence monitored
	 *  by SampleMeanVar
	 *  @return a string
	 */
	@Override
	public String toString() {
		StringBuffer buf = new StringBuffer();
		buf.append(getClass().getName());
		calcConfInt(90);
		buf.append("\n--------------------------");
		buf.append("\nNumber of samples : " + n);
		buf.append("\nUpper limit 90%   : " + (smean + confInt));
		buf.append("\nSample mean       : " + smean);
		buf.append("\nLower limit 90%   : " + (smean - confInt));
		buf.append("\nSample variance   : " + svar);
		buf.append("\nLargest Sample    : " + max);
		buf.append("\nSmallest Sample   : " + min);
		return buf.toString();
	}

}
