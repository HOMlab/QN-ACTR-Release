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

package jmt.engine.dataAnalysis;

import jmt.engine.dataAnalysis.sorting.HeapSort;
import jmt.engine.dataAnalysis.sorting.SortAlgorithm;
import jmt.engine.math.DoubleArrayList;
import jmt.engine.math.SampleMeanVar;

/**

 * Analyzes the data in order to calculate the requested quantiles.
 * @author Federico Granata
 * Date: 24-lug-2003
 * Time: 12.04.49

 */
public class QuantileDataAnalyzer extends DynamicDataAnalyzerImpl {
	//TODO: questa classe funziona anche nel caso di InverseMeasure????
	DoubleArrayList data;

	boolean ordered = false;

	double[] quantile;

	SortAlgorithm sorter;

	/**
	 * Creates a QuantileDataAnalyzer.
	 * @param  alfa    the quantile required for the confidence interval
	 * @param  precision   maximum amplitude of confidence interval
	 *                      (precision = maxamplitude / mean)
	 * @param maxData  maximum number of data to be analyzed
	 *
	 * @param quantile Requested quantiles //TODO: giusto??
	 * @param sorter Sorting algorithm used to manage data
	 */
	public QuantileDataAnalyzer(double alfa, double precision, int maxData, double[] quantile, SortAlgorithm sorter) {
		super(alfa, precision, maxData);
		this.quantile = quantile;
		this.sorter = sorter;
	}

	/**
	 * Creates a QuantileDataAnalyzer. A default sorting algorithm is
	 * used to manage data.
	 * @param  alfa    the quantile required for the confidence interval
	 * @param  precision   maximum amplitude of confidence interval
	 *                      (precision = maxamplitude / mean)
	 * @param maxData  maximum number of data to be analyzed
	 *
	 * @param quantile Requested quantiles
	 */
	public QuantileDataAnalyzer(double alfa, double precision, int maxData, double[] quantile) {
		super(alfa, precision, maxData);
		this.quantile = quantile;
		data = new DoubleArrayList(1024);
		data.add(0);
		sorter = new HeapSort();
	}

	/**
	 * Adds the new sample to the statistic.
	 * @param newSample the new sample
	 * @param Weight the weight of the newSample, if it is not needed put 1.
	 * @return true if the confidence interval is smaller than required by
	 *          the user, or the data analyzed are too many
	 */
	@Override
	public boolean addSample(double newSample, double Weight) {
		if (initialized) {
			data.add(newSample * Weight);
			ordered = false;
		}
		return super.addSample(newSample, Weight);
	}

	/**
	 * returns the quantile with the requested probability.
	 *
	 * @param prob probability of the quantile
	 * @return the estiamted quantile
	 */
	public double getQuantile(double prob) {
		if (ordered) {
			return data.get((int) (data.getSize() * prob));
		} else {
			sort();
			ordered = true;
			return data.get((int) (data.getSize() * prob));
		}
	}

	/**
	 * gets all requested quantiles.
	 * @return vector of quantiles.
	 */
	public double[] getQuantiles() {
		if (quantile != null) {
			double[] res = new double[quantile.length];
			for (int i = 0; i < res.length; i++) {
				res[i] = getQuantile(quantile[i]);
			}
			return res;
		} else {
			return null;
		}
	}

	/**
	 * returns the probability that a number extracted from the empirical
	 * distibution analyzed is greater then the quantile.
	 * @param quantile the requested quantile
	 * @return estiamted probability
	 */
	public double getProbability(double quantile) {
		if (ordered) {
			return search(quantile);
		}
		return Double.NaN;
	}

	protected double search(double element) {
		int l = 1, r = data.getSize() - 1, x;
		while (l < r) {
			x = (l + r) >> 1;
			if (element == data.get(x)) {
				return (x / (data.getSize() - 1.0));
			}
			if (element < data.get(x)) {
				r = x - 1;
			} else {
				l = x + 1;
			}
		}
		return (l / (data.getSize() - 1.0));
	}

	protected void sort() {
		double[] d = data.toArray(0, data.getSize() - 1);
		//		long start = System.currentTimeMillis();
		sorter.sort(d);
		//		System.out.println("tempo = "+ (System.currentTimeMillis() - start));
		data = new DoubleArrayList(d);

	}

	/** Applies the spetctral test to generate the Confidence Intervals.
	 *  see: P. Heidelberger, Peter D. Welch
	 * "A spectral method for confidence interval generation and run length
	 * control in simulations"
	 *
	 *
	 * @return true if the precision requirement met. false if not.
	 */
	@Override
	protected boolean HWtest() {
		sort();
		ordered = true;
		//		System.out.println("quantile " + getQuantileResults(0.75));
		//		System.out.println("nSamples = " + nSamples);
		return super.HWtest();
	}

	/**
	 *updates the variance
	 */
	@Override
	protected void calcVar() {
		double[] C;
		double[] tempBatch = new double[numBatch];
		double sampleVar = (new SampleMeanVar(batchMean)).getVar();
		K = numBatch / 4;
		C = calcConstants(K, polyOrder);
		C1 = C[0];
		C2 = (int) C[1];
		//DEK (Federico Granata)
		//si puo' fare array copy
		System.arraycopy(batchMean, 0, tempBatch, 0, numBatch - 1);
		//		for (int i = 0; i < numBatch; i++)
		//			tempBatch[i] = batchMean[i];

		extVar = calcVar(tempBatch, 0, batch, K, polyOrder);
		if (Math.abs(extVar - sampleVar) > sampleVar * precision * 2) {
			extVar = Double.MAX_VALUE;
		}
		if (extVar < sampleVar) {
			extVar = sampleVar;
		}
	}

}
