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

package jmt.engine.math;

/**
 *
 * @author  Stefano Omini
 */
public class WeightedMeanVar {

	protected double mean;//mean of all data ( E[x] )
	protected double var;//variance of data

	protected double max;//the greatest sample of all data inserted
	protected double min;//the smallestest sample of all data inserted

	protected double expectedValueX2; // E[x^2]
	protected double sumWeights;//sum of all weights
	protected double sumSamples;//sum of all samples

	/** Creates a new instance of SampleMeanVar */
	public WeightedMeanVar() {
		mean = 0;
		var = 0;

		expectedValueX2 = 0;

		max = 0;
		min = 0;

		sumWeights = 0;
		sumSamples = 0;
	}

	/** Creates a new instance of SampleMeanVar */
	public void initialize() {
		mean = 0;
		var = 0;

		expectedValueX2 = 0;

		max = 0;
		min = 0;

		sumWeights = 0;
		sumSamples = 0;
	}

	/** Creates a new instance of SampleMeanVar calculating the parameters with
	 * the initializing data sequence
	 *  @param data the initial data sequence;
	 */
	public WeightedMeanVar(double[] data, double[] weight) {

		mean = 0;
		var = 0;
		expectedValueX2 = 0;
		max = 0;
		min = 0;

		sumWeights = 0;
		sumSamples = 0;

		for (int i = 0; i < data.length; i++) {
			putNewSample(data[i], weight[i]);
		}
	}

	/** Creates a new instance of SampleMeanVar calculating the parameters with
	 * the initializing data sequence
	 *  @param data the initial data sequence;
	 */
	public void initialize(double[] data, double[] weight) {

		mean = 0;
		var = 0;
		max = 0;
		min = 0;
		expectedValueX2 = 0;

		sumWeights = 0;
		sumSamples = 0;

		for (int i = 0; i < data.length; i++) {
			putNewSample(data[i], weight[i]);
		}
	}

	/** update the sample mean & variance with the new value it uses stable
	 *  algorithms
	 *  @param  newValue
	 */
	public void putNewSample(double newValue, double newWeight) {

		// coumputes the maximum and the minimum value of the sequence
		if (sumWeights == 0) {
			//it's the first value
			max = newValue;
			min = newValue;
		} else {
			max = Math.max(max, newValue);
			min = Math.min(min, newValue);
		}

		//computes mean and var
		calcMeanAndVar(newValue, newWeight);

	}

	/** coumputes the new sample variance using a stable algorithm
	 *  it also computes the mean with the stable algorithm.
	 *  @param newValue the new element to be added to the sequence
	 */
	protected void calcMeanAndVar(double newValue, double newWeight) {

		//double oldMean = mean;
		//double oldVar = var;
		double oldSumWeights = sumWeights;
		double oldExpectedX2 = expectedValueX2;

		sumSamples += newValue * newWeight;
		sumWeights += newWeight;

		//new E[x^2]
		expectedValueX2 = (oldExpectedX2 * oldSumWeights + newWeight * Math.pow(newValue, 2)) / sumWeights;

		//new E[x]
		mean = sumSamples / sumWeights;

		//new var[x] (computed in this way: var[x] = E[x^2] - E[x]^2

		if (oldSumWeights != 0) {
			var = expectedValueX2 - Math.pow(mean, 2);
		} else {
			//it's the first sample, var = 0
			var = 0;
		}
	}

	/** get the sample mean
	 *  @return the actual sample mean
	 */
	public double getMean() {
		return mean;
	}

	/** get the sample variance
	 *  @return the actual sample variance
	 */
	public double getVar() {
		return var;
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

}
