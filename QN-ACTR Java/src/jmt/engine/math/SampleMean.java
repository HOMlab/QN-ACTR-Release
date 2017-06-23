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
 * SampleMean.java
 *
 * Created on 19 ottobre 2002, 14.56
 */

package jmt.engine.math;

/**
 *
 * @author  Federico Granata
 */
public class SampleMean {
	private double sum;// sum of all data
	private double smean;//sample mean of all data
	private double n;//number of data
	private double max;//the greatest sample of all data inserted
	private double min;//the smallestest sample of all data inserted

	/** Creates a new instance of SampleMean */
	public SampleMean() {
		sum = 0;
		smean = 0;
		n = 0;
		max = 0;
		min = 0;
	}

	/** Creates a new instance of SampleMean calculating the parameters with the
	 * initializing data sequence
	 *  @param  data initial data sequence;
	 */
	public SampleMean(double[] data) {
		sum = 0;
		smean = 0;
		n = 0;
		max = 0;
		min = 0;
		for (double element : data) {
			putNewSample(element);
		}
	}

	/** initalizes
	 *   a new SampleMean with parameters all zero
	 */
	public void initialize() {
		sum = 0;
		smean = 0;
		n = 0;
		max = 0;
		min = 0;
	}

	/** updates the sample mean & variance with the new value.
	 *  it uses stable
	 *  algorithms
	 *  @param  newValue
	 */
	public double putNewSample(double newValue) {
		calcMean(newValue);
		// updates the maximum and the minimum value of the sequence
		max = Math.max(max, newValue);
		min = Math.min(min, newValue);
		return smean;
	}

	/** coumputes the new mean using a more stable algorithm
	 *  @param newValue the new element to be added to the sequence
	 */
	protected void calcMean(double newValue) {
		n++;
		smean += (newValue - smean) / n;
	}

	/** gets the sample mean
	 *  @return the actual sample mean
	 */
	public double getMean() {
		return smean;
	}

	/** gets the greatest value
	 *  @return the actual max value
	 */
	public double getMax() {
		return max;
	}

	/** gets the smallest value
	 *  @return the actual min value
	 */
	public double getMin() {
		return min;
	}

	/** creates a string with the calculated parameters of the sequence monitored
	 *  by SampleMean
	 *  @return a string
	 */
	@Override
	public String toString() {
		StringBuffer buf = new StringBuffer();
		buf.append(getClass().getName());
		buf.append("\n--------------------------");
		buf.append("\nNumber of samples : " + n);
		buf.append("\nLargest Sample    : " + max);
		buf.append("\nSmallest Sample   : " + min);
		return buf.toString();
	}

}
