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
 * <p>Title: Batch Circular List data structure</p>
 * <p>Description: This class provides a data structure used to collect
 * samples with weights in batches. When data structure is full, this is managed
 * as a circular list. Mean value is calculated and updated when a sample is
 * inserted or removed from the list. Structure is optimized to provide
 * O(1) access for insertion of samples, get of batches and mean calculation.</p>
 *
 * @author Bertoli Marco
 *         Date: 17-gen-2006
 *         Time: 13.59.14
 */
public class BatchCircularList {
	// Current batch size
	protected int batchSize;
	// Maximum number of batches
	protected int maxBatches;
	// Maximum number of samples
	protected int maxSamples;
	// Batches for samples and weights
	protected double[] samples, weights;
	// Number of stored samples (not batches)
	protected int count;
	// Starting point (batch)
	protected int start;
	// Number of dicarded samples
	protected int discarded;
	// Used to compute mean value
	protected double samplesSum, weightsSum, mean;

	/**
	 * Construct a new BatchCircularList.
	 * @param batchSize batch size, must be > 0
	 * @param maxBatches maximum number of batches, must be > 0
	 */
	public BatchCircularList(int batchSize, int maxBatches) {
		this.batchSize = batchSize;
		this.maxBatches = maxBatches + 1;
		maxSamples = this.maxBatches * batchSize;
		start = count = discarded = 0;
		samplesSum = weightsSum = 0.0;
		samples = new double[this.maxBatches];
		weights = new double[this.maxBatches];
	}

	public void add(double sample, double weight) {
		double tmpSample = sample * weight;
		// Adds new samples to correct batch
		samples[count / batchSize] += tmpSample;
		weights[count / batchSize] += weight;
		// Manage circular list
		count++;
		if (count >= maxSamples) {
			count = 0;
		}
		// Must delete oldest batch
		if (count / batchSize == start && count % batchSize == 0) {
			// Updates mean values
			samplesSum -= samples[start];
			weightsSum -= weights[start];
			// Deletes oldest batch
			samples[start] = 0;
			weights[start] = 0;
			discarded += batchSize;
			start++;
			if (start >= maxBatches) {
				start = 0;
			}
		}
		// Updates mean value
		samplesSum += tmpSample;
		weightsSum += weight;
		mean = samplesSum / weightsSum;
	}

	/**
	 * Returns a batch sample
	 * <br>PRECONDITION: index < getNumBatches()
	 * @param index position of the sample to be retreived
	 * @return requested batch sample
	 * @see this.getNumBatches()
	 */
	public double getBatchSample(int index) {
		if (start + index < maxBatches) {
			return samples[start + index];
		} else {
			return samples[start + index - maxBatches];
		}
	}

	/**
	 * Returns a batch samples' weight
	 * <br>PRECONDITION: index < getNumBatches()
	 * @param index  position of the sample to be retreived
	 * @return requested batch samples' weight
	 * @see this.getNumBatches()
	 */
	public double getBatchWeight(int index) {
		if (start + index < maxBatches) {
			return weights[start + index];
		} else {
			return weights[start + index - maxBatches];
		}
	}

	/**
	 * Deletes first n batches
	 * <br>PRECONDITION: index < getNumBatches()
	 * @param n number of batches to be deleted
	 * @see this.getNumBatches()
	 */
	public void deleteFirstBatches(int n) {
		if (start + n < maxBatches) {
			// start does not need to be resetted during deletion
			for (int i = start; i < start + n; i++) {
				samplesSum -= samples[i];
				weightsSum -= weights[i];
				samples[i] = 0;
				weights[i] = 0;
			}
			start += n;
		} else {
			// start need to be resetted during deletion
			for (int i = start; i < samples.length; i++) {
				samplesSum -= samples[i];
				weightsSum -= weights[i];
				samples[i] = 0;
				weights[i] = 0;
			}

			start = n - (maxBatches - start);

			for (int i = 0; i < start; i++) {
				samplesSum -= samples[i];
				weightsSum -= weights[i];
				samples[i] = 0;
				weights[i] = 0;
			}
		}
		discarded += n * batchSize;
		mean = samplesSum / weightsSum;
	}

	/**
	 * Deallocated all objects
	 */
	public void clean() {
		samples = null;
		weights = null;
	}

	/**
	 * Gets total number of batches
	 * @return total number of stored batches
	 */
	public int getNumBatches() {
		if (start < count / batchSize) {
			return count / batchSize - start;
		} else {
			return count / batchSize + maxBatches - start;
		}
	}

	/**
	 * Gets the size of batches
	 * @return size of batches
	 */
	public int getBatchSize() {
		return batchSize;
	}

	/**
	 * Gets the number of discarded samples (not batches)
	 * @return number of discareded samples
	 */
	public int getDiscarded() {
		return discarded;
	}

	/**
	 * Gets the mean value of samples.
	 * @return the mean value of the samples in the array
	 */
	public double getMeanValue() {
		return mean;
	}
}
