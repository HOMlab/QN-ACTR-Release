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

import jmt.engine.math.DoubleArrayList;
import jmt.engine.math.Printer;
import jmt.engine.math.SampleMean;
import jmt.engine.math.Spectrum;
import jmt.engine.math.TStudent;
import jmt.engine.math.WeightedMeanVar;
import Jama.Matrix;

/**
 * This class performs dynamic data analysis: it can remove the initial
 *  transient period and performs calculation of variance and confidence
 *	interval of the sample mean of a sequence. <br>It is robust to data correlation.
 *  <br><br>
 *	See:<br>
 * (1) K. Pawlikowski, "Steady­State Simulation of Queueing Processes: A Survey
 *	of Problems and Solutions", ACM Computing Surveys 22, 2 (1990), pp. 122--170
 *
 *
 * @version Modified 7/02/2003
 * @author  Federico Granata
 * @author Bertoli Marco - Bugfixing statistical part (R5 and Schruben) 18-11-2005
 */

public class DynamicDataAnalyzerImpl implements DynamicDataAnalyzer {

	private static final boolean DEBUG = false;
	private static final boolean ZERODEBUG = false;

	//NEW
	//@author Stefano Omini
	protected SimParameters parameters;
	//end NEW

	/**number of batches */
	protected int numBatch = 128;
	/**number of samples per batch */
	protected int batchLen = 8;

	/**mean of batches */
	protected double[] batchMean;
	/**mean of batches weighted */
	protected double[] weightBatchMean;

	/**the extimated mean */
	protected double extMean = 0;
	/**the extimated variance */
	protected double extVar = 0;
	/**the exitmated confidence intervals */
	protected double confInt = 0;

	/**the sum of samples of the last batch */
	protected double sumLastBatch;
	/** actual number of samples */
	protected int nSamples = 0;
	/**maximum number of data to be collected */
	protected int maxData;

	/**constant chosen to reduce bias of variance extimator */
	protected double C1;
	/**degrees of freeedom of the t-distribution */
	protected int C2;

	//TODO: serve oppure no?? Viene inizializzata ma poi non è aggiornata
	/**sample mean*/
	protected SampleMean sampleMean = new SampleMean();

	/**the quantile required for the confidence interval*/
	protected double alfa;
	/**maximum amplitude of confidence interval*/
	protected double precision;

	//protected DoubleArrayList initData = new DoubleArrayList();
	/**starting data, not steady state*/
	protected DoubleArrayList initData;
	/**starting weights, not steady state*/
	protected DoubleArrayList initWeight;

	//-------------------------STATE OF COMPUTATION---------------------------------//

	/**Transient period has been deleted or not*/
	protected boolean delet = false;
	/** initialized = true if the system is in steady state*/
	protected boolean initialized;
	/** end = true if the analysis has been completed (succesfully or not)*/
	protected boolean end;
	/** success = true if the analysis is successful, the required precision has
	been met*/
	protected boolean success;
	/** heuristic = true if the heuristic rule for startup period is successful.*/
	protected boolean heuristic = false;

	//-------------------------end STATE OF COMPUTATION-----------------------------//

	/**number of discarded data in deleting transient period    */
	protected int discarded;
	/**number of data that are replaced when detect initial
	test fails.*/
	protected int initial;

	//during test for stationarity, delta old observations will
	//be replaced by new ones
	protected int delta;

	/**number of points of the polyfit*/
	protected int K = numBatch / 4;

	// why numbatch/4 ? --> the signal spectral is computed using as the mean of two samples
	// therefore:
	// divide for 2, because having numbatch batches, we consider numbatch / 2
	// divide again for 2, because we want to use only the first half of the spectr
	// (the second half is sigmetric)

	/**the order of the polynomial*/
	protected int polyOrder = 2;

	/**
	 * First extimate of transient length: this
	 * number of samples is identified by the heuristic test
	 */
	public int eurDisc = 0;

	/**batches used to delete transient*/
	DoubleArrayList miniB = new DoubleArrayList();
	/**batches used to delete transient*/
	DoubleArrayList miniW = new DoubleArrayList();

	int initSamples = 0;

	double[] arrayData;

	/**minimum length (number of samples) of transient period */
	int minSamples = 0;
	/**sums of samples*/
	double sumS = 0;
	/**sums of weights.*/
	double sumW = 0;

	/**index of the batch*/
	int batch;

	//-----------------------NULL TEST HYPOTHESIS----------------------------------------//
	//NEW
	//@author Stefano Omini

	//NULL TEST is used to identify and stop all measures that are
	//very close to 0.0 (such measures cause troubles to the analyzer)

	//true if the analyzer must control whether the mean value is 0.0
	private boolean nullTestEnabled = true;
	//true if null test has determined that this measure is equal to zero
	private boolean measureIsZero = false;

	//contains the mean and the variance computed during the test
	private WeightedMeanVar nullTestData = new WeightedMeanVar();

	//the quantile required for the confidence interval of nullTest
	private double nullTestAlfa = 0.005;

	//to save time and resources, null test is not made for each sample, but repeated
	//every nullTestPeriod samples (testRate is equal to nullTestPeriod/maxData)
	private double nullTestRate = 0.01;
	//to save time and resources, null test is repeated every nullTestPeriod samples
	private int nullTestPeriod;

	//If measure is zero, this is the value of the upper limit of confidence interval
	private double nullMeasure_upperLimit = 0.0;

	//end NEW

	//--------------------end NULL TEST HYPOTHESIS----------------------------------------//

	//constant (instead of computing the same expression at each time)
	private final double sqrt_45 = Math.sqrt(45);

	/** Creates a new DynamicDataAnalyzer
	 *  @param alfa the quantile required for the confidence interval
	 *  @param precision maximum amplitude of confidence interval (precision = maxamplitude / mean )
	 *  @param maxData maximum number of data to be analyzed (at least 5000)
	 */
	public DynamicDataAnalyzerImpl(double alfa, double precision, int maxData) {
		if (alfa <= 0 || alfa >= 1 || precision <= 0 || precision >= 1 || maxData < 5000) {
			throw new IllegalArgumentException("Wrong Dynamic analyzer parameter  " + alfa + "  " + precision + "  " + maxData);
		}

		batchMean = new double[numBatch];
		weightBatchMean = new double[numBatch];
		this.maxData = maxData;
		this.alfa = alfa;
		this.precision = precision;
		initialized = false;
		end = false;
		success = false;
		initData = new DoubleArrayList();
		initWeight = new DoubleArrayList();

		//NEW
		//@author Stefano Omini
		if (nullTestEnabled) {
			//initializes nullTestPeriod (the period of null test repetition)
			nullTestPeriod = (int) (nullTestRate * maxData);
		}
		//end NEW
	}

	/**
	 * Adds the new sample to the statistic.
	 * @param newSample the new sample
	 * @param weight the weight of the newSample, if it is not needed put 1.
	 * @return true if the computation of confidence interval has finished (i.e. if the
	 * confidence interval is smaller than the one required by
	 * the user, or if the analyzed data are too many), false otherwise
	 */
	public synchronized boolean addSample(double newSample, double weight) {

		//DEK (Federico Granata)
		// why /5 ???  --> see documentation
		int index = nSamples / 5;

		nSamples++;

		//NEW
		//@author Stefano Omini

		//Measures which are often equal to zero are problematic for the
		//dynamic analyzer: in fact, in such cases, Schruben test for stationarity
		//may have no success.
		//For this reason a null test can be done in order to recognize if
		//a measure is almost always 0.

		if (nullTestEnabled) {

			//updates mean and var
			nullTestData.putNewSample(newSample, weight);

			//nullTest must be repeated every nullTestPeriod samples
			if ((nSamples % nullTestPeriod == 0)) {
				if (nullTest()) {
					//null test is true (the measure is almost always equal to 0.0)
					end = true;
					success = true;
					measureIsZero = true;
					return end;
				}
			}
		}
		//end NEW

		if (!delet) {

			//transient not deleted
			double temp = newSample * weight;

			sumS += temp;
			sumW += weight;
			if (index < miniB.getSize()) {
				miniB.set(index, miniB.get(index) + temp);
				miniW.set(index, miniW.get(index) + weight);
			} else {
				//adds at the end of the list
				miniB.add(temp);
				miniW.add(weight);
			}

		}

		/*
		TRANSIENT DETECTION
		When computation starts, transient must be detected: to do this,
		an heuristic rule is applied.

		The heuristic rule R5 approximately detects the length of transient, that is the number
		of observations to be deleted

		Then Shruben test is applied.
		If the sequence is stationary, transient is finished. Otherwise a part
		of the sequence is discarded, new data are collected and Shruben test is applied
		one more time.

		This iteration goes on until a stationary sequence is found.
		During this iteration, if the max number of data is reached, simulation is aborted.

		 */

		// transient not detected
		// initialized becomes true only after Schruben test has been passed
		if (!initialized) {

			if (nSamples >= maxData * 0.5) {
				// too many samples: ends analysis
				end = true;
				initialized = false;
				return end;
			}

			if (!heuristic) {
				//heuristic rule for transient detection hasn't finished yet
				heuristicR5(newSample, weight);
			} else {
				// heuristic = true  --> heuristic rule has finished,
				// computing a first approximation
				// of the number of observations to be deleted

				// now schruben test must be applied
				schrubenTest(newSample, weight);
			}

			//initialized = true  --> transient already detected
		} else if (nSamples > maxData) {
			//too many samples: ends simulation
			end = true;
			success = false;

		} else {

			if (!delet) {
				//transient detected but not deleted yet

				if (nSamples >= minSamples) {
					//min number of samples respected

					//Apply the heuristic rule MSR5 to discard the most biased samples of the
					//time series. It minimizes the Mean Square Error.
					if (deleteTransient()) {

						//transient deleted
						if ((batch == numBatch - 1) && (nSamples % batchLen == 0)) {

							//Applies the spectral test to generate the Confidence Intervals.
							//true if the precision has been reached
							end = HWtest();

						} else {
							end = false;
						}

					} else {
						//transient not deleted yet
						end = false;
					}
				}

			} else {
				//transient detected and deleted

				batchMean[batch] += newSample * weight;
				weightBatchMean[batch] += weight;

				if (nSamples % batchLen == 0) {
					batchMean[batch] /= weightBatchMean[batch];
					if (batch == (numBatch - 1)) {
						end = HWtest();
						if (DEBUG) {
							System.out.println("extMean = " + extMean + " samples = " + nSamples);
						}
					} else {
						batch++;
					}
				} else {
					end = false;
				}
			}
		}

		return end;
		//end = true if max number of data has been reached or if HWtest has been passed or if
		//transient hasn't been deleted
	}

	/**
	 * It uses the heuristic rule R5.
	 * Inserts a new value of the sequence, and approximately detects the initial
	 * transient period.
	 * <br>See
	 * <br>
	 *  K. Pawlikowski
	 * "Steady­State Simulation of Queueing Processes: A Survey of Problems and
	 * Solutions" ACM Computing Surveys 22, 2 (1990), pp. 122--170.
	 */
	protected boolean heuristicR5(double newSample, double Weight) {

		// Rule R5:
		// " The initial transient period is over after n observations,
		// if the time series x1, x2, ..., xn
		// crosses the mean X(n) k times."

		// actual number of crosses
		int counter = 0;

		//TODO: si potrebbe modificare (vedi tesi granata)
		// number of times that the time series initData should cross the initMean
		// to determine the end of transient
		int overTimes = 40; //OLD=40

		// actual mean of the initial samples.
		double mean = sumS / sumW;

		initData.add(newSample);
		initWeight.add(Weight);
		initSamples++;

		//apply the heuristic rule for initial transient detection
		//heuristic test R5

		//number of "for" cycles (uses a variable, instead of computing initData.getSize() for
		//each repetition
		int repeat_until = initData.getSize() - 1;

		//curr and next correspond to observation x(i) and x(i+1)
		//initializes curr with the first element
		double curr = initData.get(0);
		double next;

		//counts the number of crosses
		for (int i = 0; i < repeat_until; i++) {
			next = initData.get(i + 1);

			if (((curr >= mean) && (next <= mean)) || ((curr <= mean) && (next >= mean))) {
				//cross --> increase counter
				counter++;
				if (counter >= overTimes) {
					// if number of crosses >= overtimes exit from the cycle
					break;
				}
			}

			curr = next;
		}

		if (counter >= overTimes) {
			//heuristic rule has finished

			//initial transient detected all data to be discarded
			eurDisc = initSamples;
			heuristic = true;

			//initializes SampleMean object
			sampleMean.initialize();

			// when rule R5 finishes the number of initial samples is used
			// to extimate the initial transient length
			// (we need a part already steady of the sequence, because we need to
			// compute its variance)

			//  (an enormous number without any sense!!!!) (caused OutOfMemory too)
			initial = eurDisc * eurDisc / 10;

			// New -- Bertoli Marco
			if (initial > 5000) {
				initial = 5000;
			}

			if (eurDisc < initial) {
				delta = eurDisc / 2;
			} else {
				initData.delFirst(eurDisc - initial);
				initWeight.delFirst(eurDisc - initial);
				delta = initial / 2;
			}
			// Pawlikowski said that this must be around 200, larger values can help
			// euristic R5 problems, but will also reject legittimate measures
			if (delta > 200) {
				delta = 200;
				// END NEW
			}

			initData.ensureCapacity(initial);
			initWeight.ensureCapacity(initial);

			minSamples = initial * 2;

			numBatch = 1 << ((int) Math.ceil((Math.log(Math.sqrt(initial / 2))) / Math.log(2)));
			if (numBatch < 64) {
				numBatch = 64;
			}
			batchLen = (int) Math.ceil(initial / ((double) numBatch * 2));

			//during test for stationarity, delta old observations will
			//be replaced by new ones
			//OLD
			//delta = initSamples / 2;
			arrayData = new double[delta];
			initSamples = 0;

		} else {
			heuristic = false;
		}
		return heuristic;
	}

	/** inserts a new value of the sequence, and detects when the initial
	 *  transient period has ended.
	 */
	protected boolean schrubenTest(double newSample, double Weight) {

		//NEW
		//@author Stefano Omini

		//
		// test has been made stronger: on one hand, a stronger test requires more
		// samples to delete the transient (therefore the transient deletion phase
		// is longer).
		// On the other hand, after a stronger test, simulation will start computing the
		// confidence interval from a state nearer to the steady state: this makes
		// faster the next phase, which is typically more critical.

		//  The test is Two-sided

		//OLD
		//float alfaT = (float) 0.05;

		//significance of the test for stationarity
		float alfaT = (float) 0.001;

		//end NEW

		double var, mean, sum = 0, parSum = 0;
		double sumWeigths = 0;
		double[] C = new double[2];

		//internal array used to contain an arraylist, for speed

		initData.add(newSample);
		initWeight.add(Weight);
		initSamples++;
		//a.calcMeanStable(newSample);

		//append observations to the tested sequence
		//when init samples
		if (initSamples == minSamples) {
			//now the array is full
			arrayData = initData.toArray(0, delta);
			mean = 0;

			//OLD
			//for (int i = 0; i < initData.getSize(); i++) {
			//mean += initData.get(i) * initWeight.get(i);
			//	sumWeigths += initWeight.get(i);
			//}

			//NEW
			int size = initData.getSize();
			double tempWeight;
			for (int i = 0; i < size; i++) {
				tempWeight = initWeight.get(i);
				mean += initData.get(i) * tempWeight;
				sumWeigths += tempWeight;
			}
			//end NEW

			mean /= sumWeigths;
			int k = minSamples - numBatch * batchLen - 1;
			batchMean = new double[numBatch];

			for (int i = 0; i < numBatch; i++) {
				double weight = 0;
				for (int j = 0; j < batchLen; j++) {
					batchMean[i] += initData.get(k + j);
					weight += initWeight.get(k + j);
				}
				batchMean[i] /= weight;
				k += batchLen;
			}

			//computes the variance using the last collected observations
			var = batchLen * calcVar(batchMean, 0, numBatch, batchMean.length / 4, 2);

			// computes the degrees of freedom of the tStudent distribution (used in the
			// extimation of variance and confidence intervals)
			// and a constant used to depolarize the variance extinmator
			C = calcConstants(batchMean.length / 4, 2);

			//prepares the test for stationarity

			for (int j = 1; j < arrayData.length; j++) {
				parSum += arrayData[j];
				sum += j * delta * (1 - ((double) j / (double) delta)) * (mean - (parSum / j));
			}

			//OLD
			//double num = (Math.sqrt(45) * sum);
			//NEW
			double num = (sqrt_45 * sum);

			double den = (Math.pow(delta, 1.5) * Math.sqrt(numBatch * var));

			double T = 0;
			T = num / den;

			// the sign of the initial bias is difficult to predict
			// then prepare data for a two-sided test (less powerful)
			T = Math.abs(T);

			//otherwise use T = - T; if a positive bias is suspected

			//t is the upper (1-alfa) critical point from the t-distribution
			//with k=C[1] degrees of freedom
			double t = TStudent.tStudent(alfaT, C[1], false);

			//stationarity test
			if (T <= t) {
				//transitory ended after discarded samples

				initialized = true;
				initSamples = 0;
				minSamples = discarded * 2;
				initData.clear();
				initWeight.clear();
			} else {
				//after each unsuccessfully test for stationarity, delta old
				//observations are discarded: they will be replaced by new ones

				discarded += delta;
				initData.delFirst(delta - 1);
				initWeight.delFirst(delta - 1);
				initSamples -= delta;
			}
		}
		return initialized;
	}

	/**
	 * Apply the heuristic rule MSR5 to discard the most biased samples of the
	 * time series. It minimizes the Mean Square Error.
	 *
	 */
	protected boolean deleteTransient() {

		double sumB = 0;
		double sumW = 0;
		double min = Double.MAX_VALUE;
		double z = 0;
		int d = 0;
		double y;
		double mean = 0;
		double oldMean = 0;
		double sumSquare = 0;
		int data = miniB.getSize();
		//OLD
		//int index = miniB.getSize() / 2;
		//NEW
		int index = data / 2;

		//coefficients for the last part of data.
		sumB = miniB.get(data - 1);
		sumW = miniW.get(data - 1);
		oldMean = sumB / sumW;

		double miniW_elem, miniB_elem;

		for (int i = data - 2; i > index; i--) {

			//element i-th
			miniB_elem = miniB.get(i);
			//element i-th
			miniW_elem = miniW.get(i);

			sumB += miniB_elem;
			sumW += miniW_elem;
			y = (miniB_elem / miniW_elem) - oldMean;
			mean = oldMean + (miniW_elem * y / sumW);
			sumSquare += miniW_elem * y * y - miniW_elem * miniW_elem * y * y / sumW;
			oldMean = mean;
		}
		//coefficients for the first part of data
		for (int i = index; i >= 0; i--) {

			//element i-th
			miniB_elem = miniB.get(i);
			//element i-th
			miniW_elem = miniW.get(i);

			sumW += miniW_elem;
			y = (miniB_elem / miniW_elem) - oldMean;
			mean = oldMean + (miniW_elem * y / sumW);
			sumSquare += miniW_elem * y * y - miniW_elem * miniW_elem * y * y / sumW;
			oldMean = mean;
			z = sumSquare / sumW;
			if (z < min) {
				// find the minimum z
				min = z;
				d = i;
			}
		}

		//OLD
		// if (d - 1 <= nSamples / (5 * 5)) {
		if (d - 1 <= nSamples / (25)) {
			//the initial warm up period is short
			//cannot apply the algorithm to a sequence too polarized...
			discarded = d * 5;
			delet = true;
			miniB.delFirst(d);
			miniW.delFirst(d);

			nSamples = miniB.getSize() * 5;

			//heuristic to build the "optimal" number of batches.
			//numBatches is nearest 2 power to sqrt(nSamples)
			numBatch = 1 << ((int) Math.ceil((Math.log(Math.sqrt(nSamples))) / Math.log(2)));
			if (numBatch < 64) {
				numBatch = 64;
			}
			batchLen = 5 * (int) Math.ceil(miniB.getSize() / (double) numBatch);
			batch = (nSamples - 1) / batchLen;

			int k = 0;
			batchMean = new double[numBatch];
			weightBatchMean = new double[numBatch];
			for (int i = 0; i < miniB.getSize(); i++) {
				batchMean[k] += miniB.get(i);
				weightBatchMean[k] += miniW.get(i);
				if (((i + 1) % (batchLen / 5)) == 0) {
					batchMean[k] /= weightBatchMean[k];
					k++;
				}
			}
			miniB.clear();
			miniW.clear();
			return delet;
		} else {
			//transient can't be deleted (? ste)
			//TODO: come si arriva a questo punto??
			delet = false;
			minSamples += discarded;
			//TODO: forse è minSamples > maxData????
			if (minSamples < maxData) {
				end = true;
				if (DEBUG) {
					System.out.println("end = " + end);
				}
				success = false;
			}
			return delet;
		}
	}

	/**
	 * Applies the spectral test to generate the Confidence Intervals.
	 *
	 * <br>
	 * See:
	 * <br>
	 *
	 * P. Heidelberger,Peter D. Welch
	 * <br>"A spectral method for confidence interval generation and run length
	 * control in simulations"
	 *
	 *
	 * @return true if the precision requirement is reached. false if not.
	 */
	protected boolean HWtest() {
		//computes the mean
		sumS = 0;
		sumW = 0;
		for (int i = 0; i < numBatch; i++) {
			sumS += batchMean[i] * weightBatchMean[i];
			sumW += weightBatchMean[i];
		}
		extMean = sumS / sumW;

		calcVar();
		calcConfInterval();
		if (precision > confInt / extMean) {
			//simulation ended with success
			end = true;

			success = true;
		} else {
			if (batch == (numBatch - 1)) {
				//optimal number of batches
				int log2 = 1;
				log2 = 1 << ((int) Math.ceil((Math.log(Math.sqrt(nSamples))) / Math.log(2)));
				if (log2 < 64) {
					log2 = 64;
				}
				if (log2 > numBatch) {
					//doubles the number of batches
					numBatch = numBatch * 2;
					double[] tempBatch = new double[numBatch];
					batch++;

					try {
						System.arraycopy(batchMean, 0, tempBatch, 0, batchMean.length);
					} catch (Exception e) {
						System.out.println(e);
						System.out.println(batchMean.length + "  " + tempBatch.length);
					}

					batchMean = tempBatch;
					tempBatch = new double[numBatch];
					System.arraycopy(weightBatchMean, 0, tempBatch, 0, weightBatchMean.length);
					weightBatchMean = tempBatch;
				} else {
					//rebatch in n/2 batch of size batchLen
					int half = numBatch / 2;
					batch = half;
					for (int i = 0; i < half; i++) {
						batchMean[i] = (batchMean[2 * i] * weightBatchMean[2 * i] + batchMean[2 * i + 1] * weightBatchMean[2 * i + 1])
								/ (weightBatchMean[2 * i] + weightBatchMean[2 * i + 1]);
						weightBatchMean[i] = (weightBatchMean[2 * i] + weightBatchMean[2 * i + 1]) / 2;
					}
					for (int i = half; i < numBatch; i++) {
						batchMean[i] = 0;
						weightBatchMean[i] = 0;
					}
					batchLen *= 2;

				}

			}
		}
		return end;
	}

	/**
	 *updates the variance
	 */
	protected void calcVar() {
		double[] C;
		double[] tempBatch = new double[numBatch];
		K = numBatch / 4;
		C = calcConstants(K, polyOrder);
		C1 = C[0];
		C2 = (int) C[1];
		System.arraycopy(batchMean, 0, tempBatch, 0, numBatch);
		extVar = calcVar(tempBatch, 0, batch, K, polyOrder);
	}

	/** creates a string of the principals parameters calculated
	 *  @return the string
	 */
	@Override
	public String toString() {
		StringBuffer buf = new StringBuffer();
		buf.append(getClass().getName());
		buf.append("\n-------------------------");
		buf.append("\nAnalysis ");
		if (success) {
			buf.append("\nSuccesfully completed.\n");
		} else {
			buf.append("\nUnsuccesfully completed.\n");
		}
		buf.append("\nSamples analyzed : " + (nSamples + discarded));
		buf.append("\nSamples used : " + nSamples);
		buf.append("\n");
		buf.append("\nUpper limit : " + Printer.print(extMean + confInt, 6));
		buf.append("\nMean value  : " + Printer.print(extMean, 6));
		buf.append("\nLower limit : " + Printer.print(extMean - confInt, 6));
		buf.append("\nCI Width : " + Printer.print(confInt / extMean, 4) + " required : " + precision);
		buf.append("\n");
		buf.append("\nWith probablity : " + ((1 - alfa) * 100) + "%");
		buf.append("\nDiscarded samples :  " + discarded);
		buf.append("\nExtimated variance : " + Printer.print(getVar(), 6));
		buf.append("\nLast batch size : " + batchLen);
		buf.append("\nNumber of Batches : " + numBatch);
		return buf.toString();
	}

	/** number of samples elaborated up to now
	 * @return number of samples
	 */
	public int getSamples() {
		return nSamples + discarded;
	}

	/** gets the mean extimated of the monitored data stream
	 * @return the mean
	 */
	public double getMean() {

		//NEW
		//@author Stefano Omini
		if (isZero()) {
			return 0;
		} else if (initialized) {
			return extMean;
		} else {
			return 0;
		}
		//end NEW
	}

	/** gets the variance extimated of the monitored data stream
	 * @return the variance
	 */
	public double getVar() {
		if ((end) && (initialized)) {
			return extVar;
		} else {
			return 0;
		}
	}

	/** gets the half width confidence interval extimated of the monitored data
	 *  stream.
	 * @return the confidence interval requested
	 */
	public double getConfInt() {
		if ((confInt != 0) && (initialized)) {
			return confInt;
		} else {
			return 0;
		}
	}

	/** true if the analysis is successful
	 * @return true if the analysis respect all'users requests
	 */
	public boolean getSuccess() {
		return success;
	}

	/** calculates the variance through Heidelberger & Welch method, if the data
	 *  are the means of batches of size batchLen, the variance must be multipied
	 *  by batchLen.
	 *
	 *  @param  data        array contaning the sequence of data
	 *  @param  start       the starting sample
	 *  @param  n           number of data of the sequence to be estimated
	 *  @param  K           number of points used in the polynomial fitting
	 *                      must be <= n/4, tipically is 25, 50
	 *  @param  polyOrder   the order of the polynomial to be fitted to the getLog
	 *                      of the spectrum of data tipically is 2 or 3.
	 *  @return the extimated variance
	 */
	public double calcVar(double[] data, int start, int n, int K, int polyOrder) {

		K = Math.min(K, n / 4);
		double[] C;
		double[] fn = new double[K];
		double[] J = new double[K];
		double[] intData;
		double[] polyCoeff = new double[polyOrder + 1];

		if (n < 4 * K) {
			throw new Error("number of data must be at least 4 times K: " + data.length);
		}

		intData = Spectrum.periodogramOfReal(data, start, n);
		for (int i = 0; i < K; i++) {
			fn[i] = ((4 * ((double) i + 1)) - 1) / (2 * (double) n);
			J[i] = (Math.log((intData[((2 * (i + 1)) - 1)] + intData[(2 * (i + 1))]) / 2)) + 0.270;
		}
		//polynomial fitting
		polyCoeff = PolyFit.fit(K, fn, J, polyOrder);
		/*this function could be eliminated if the fit it is calculated through
		* the normal eqaution algorithm, because the (AA')^-1 it is created and
		* calulated to fit. But if the calculation is done trough other
		* algorithms then you have to do with this function.
		*/
		C = calcConstants(K, polyOrder);
		return C[0] * Math.exp(polyCoeff[0]);
	}

	/** computes the degrees of freedom of the tStudent distribution used in the
	 * extimation of variance and confidence intervals C2,
	 * and a constant used to depolarize the variance extinmator
	 * @param K number of points of the polyfit
	 * @param polyOrder the order of the polynomial
	 * @return the double array of dimension 2, at first position it is C1
	 * and at second C2
	 */
	public double[] calcConstants(int K, int polyOrder) {
		double constants[] = new double[2];
		double sigmaQ = calcExtimatorVar(K, polyOrder);//computes the variance
		//of the extimator of the variance
		constants[0] = Math.exp(-sigmaQ / 2);//C1
		constants[1] = Math.round((float) Math.rint(2 / (Math.exp(sigmaQ) - 1)));
		//C2
		return constants;
	}

	/**
	 * computes the variance of the extimator of the variance
	 */
	protected double calcExtimatorVar(int K, int polyOrder) {
		Matrix X = new Matrix(K, polyOrder + 1);
		Matrix Xt = new Matrix(polyOrder + 1, K);
		Matrix square = new Matrix(K, K);
		for (int i = 0; i < K; i++) {
			for (int j = 0; j <= polyOrder; j++) {
				X.set(i, j, (Math.pow((4 * ((double) i + 1) - 1) / (2 * (double) K), j)));
			}
		}
		Xt = X.transpose();
		square = Xt.times(X);
		square = square.inverse();
		//System.out.println("calcConst "+square.get(0,0));
		return (.645 * square.get(0, 0));
	}

	/**
	 * updates the conf intervals
	 */
	protected void calcConfInterval() {
		double t = TStudent.ICDF(alfa, C2);
		confInt = t * Math.sqrt(extVar / numBatch);
	}

	/**  gets the actual requested precision of the analyzer
	 *
	 * @return  the precision
	 */
	public double getPrecision() {
		return precision;
	}

	//NEW
	//@author Stefano Omini
	public double getAlfa() {
		return alfa;
	}

	//end NEW

	/** gets the maximum number of samples that can be analyzed.
	 *
	 * @return maxData
	 */
	public int getMaxData() {
		return maxData;
	}

	/** gets the number of discarded data.
	 *
	 * @return discarded data
	 */
	public int getDiscarded() {
		if (delet) {
			return discarded;
		} else {
			return 0;
		}
	}

	/**
	 * Gets the batch means. If the data is not over the transient period
	 * returns null.
	 * @return batch means
	 */
	public synchronized double[] getData() {
		if (initialized) {
			double[] data = new double[batch];
			if (DEBUG) {
				System.out.println("batch = " + batch);
				System.out.println("data.length = " + data.length);
				System.out.println("batchMean.length = " + batchMean.length);
				for (double element : batchMean) {
					System.out.print(element + "  ");
				}
				System.out.println("");
			}
			System.arraycopy(batchMean, 0, data, 0, batch);
			return data;
		} else {
			return null;
		}
	}

	//NEW
	//@author Stefano Omini

	/**
	 * Verifies whether the measure is almost always equal to zero
	 * @return true if the measure is almost always equal to zero
	 */
	protected boolean nullTest() {
		/*
		To test whether the population mean has a specific value, 0 , against
		the one-sided alternative that it is > 0, the confidence
		interval is converted to hypothesis-test form.

		The test is a one-sample t-test, and it is defined as:

		Null Hypothesis: mean = 0;
		Alternative Hypothesis: mean > 0;

		Test Statistic: T = (mean - 0) / sqrt(var / nSamples)

		Reject the null hypothesis if
		    |T| > t
		where t is the upper critical value of the t-distribution with (N - 1) degrees
		of freedom and with confidence coefficient equal to (1 - nullTestAlfa)
		*/

		//oneSided t-test  -->  t(2*alfa, N)
		//twoSided t-test  -->  t(alfa, N)
		//this method uses the two-sides test: make alfa * 2 because we want one-side test
		double t = TStudent.ICDF((2 * nullTestAlfa), nSamples);
		double mean = nullTestData.getMean();
		double var = nullTestData.getVar();
		double T;
		if (var != 0) {
			T = mean / (Math.sqrt(var / nSamples));
		} else {
			//when var = 0, the upper formula cannot be applied
			T = mean;
		}

		if (Math.abs(T) < t) {
			//the null hypothesis is true
			measureIsZero = true;

			//upper limit

			// we found that T < t with confidence coefficient equal to (1 - nullTestAlfa)
			// T = mean / sqrt(var / nSamples)
			// then mean < t * sqrt(var / nSamples)

			nullMeasure_upperLimit = t * (Math.sqrt(var / nSamples));

		} else {
			//the null hypothesis is false
			measureIsZero = false;
		}

		if (ZERODEBUG) {
			System.out.println("Number of samples: " + nSamples);
			System.out.println("t: " + Double.toString(t));
			System.out.println("T: " + Double.toString(T));
			System.out.println("Mean: " + Double.toString(nullTestData.getMean()));
			System.out.println("Var: " + Double.toString(nullTestData.getVar()));
			System.out.println("Null hypothesis is " + measureIsZero);
		}

		return measureIsZero;
	}

	/**
	 * Returns true if null test has found out that this measure is zero
	 * @return true if null test has found out that this measure is zero
	 */
	public boolean isZero() {
		return measureIsZero;
	}

	/**
	 * This method should be called only if mean is equal to zero (@see isZero()), that is
	 * whne nullTest() returns true. 
	 * @return the upper limit of confidence interval when mean is equal to 0.0
	 */
	public double getNullMeasure_upperLimit() {
		return nullMeasure_upperLimit;
	}

	/**
	 * Gets the extimated mean, even if the confidence requirements
	 * haven't been reached
	 * @return the extimated mean
	 */
	public double extimatedMean() {
		if (success) {
			//the measure computation was successful
			//returns the mean computed by the main stimator
			return getMean();
		}
		if (nullTestEnabled) {
			//the measure wasn't successful
			//returns the mean extimated by the null test, even if the confidence requirements
			//haven't been reached
			return nullTestData.getMean();
		}
		return 0.0;
	}

	//end NEW

	//NEW
	//@author Stefano Omini
	public synchronized void stopMeasure(boolean success) {
		this.end = true;
		this.success = success;
	}

	//end NEW

	/**
	 * Sets the reference to the class containing simulation
	 * parameters. The parameters are updated with the passed values.
	 *
	 * @param parameters
	 */
	public void setParameters(SimParameters parameters) {
		this.parameters = parameters;

		//set parameters

		//batches: number and size
		numBatch = parameters.getNumBatch();
		batchLen = parameters.getBatchLen();

		//null test: test accuracy and test rate
		nullTestAlfa = parameters.getNullTestAlfa();
		nullTestRate = parameters.getNullTestRate();
		//initializes nullTestPeriod (the period of null test repetition)
		nullTestPeriod = (int) (nullTestRate * maxData);

	}

}
