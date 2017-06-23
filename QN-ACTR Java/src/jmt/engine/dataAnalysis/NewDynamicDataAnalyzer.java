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

import jmt.engine.math.BatchCircularList;
import jmt.engine.math.Spectrum;
import jmt.engine.math.TStudent;
import jmt.engine.math.WeightedMeanVar;
import Jama.Matrix;

/**
 * <p>Title: New Dynamic Data Analyzer Class</p>
 * <p>Description: This class provides a new dynamic data analyzer method based
 * on Fisherman Rule, MSER-m Rule and Welch's Spectral test.</p>
 *
 * <p>Old one was slow, wrongly implemented and worked by "black magic".</p>
 *
 * @author Bertoli Marco (Fisman rule),
 *         Omini Stefano (null test),
 *         Granata Federico (spectral test)
 *
 *         Date: 11-gen-2006
 *         Time: 12.02.04
 */
public class NewDynamicDataAnalyzer implements DynamicDataAnalyzer {
	private double alpha, precision;
	private int maxData;
	// Minimum number of samples for an OK measure.
	private int minData;

	// end tells if computation is ended
	private boolean end;

	// success tells if measure was calculated correctly or failed
	private boolean success = false;

	// Number of analyzed samples
	private int nsamples = 0;

	// Tells if transient has been deleted
	private boolean delet = false;

	// ---- Variables for Fishman Means Rule -------------------------------------------------------
	// Number of crosses to be checked by Fishman Rule
	private static final int crossesNum = 19;
	// Number of samples to be collected to check Fishman Rule
	private static final int checkSize = 500;
	// Multiplier to space to be added when arrays are full
	private static final int hArrayMult = 4;
	// An epsilon added to the mean value to avoid problems with monothonic
	// measures tending to an asympthotic value
	private static final double hEpsilon = 1e-8;

	// Tells if heuristic was passed
	private boolean heuristicPassed = false;

	// Number of analyzed samples within this rule
	private int hSamplesNum = 0;

	// Arrays with mean values
	private double[] hMeans;

	// Transient Length
	private int transientLen = 0;
	// ---------------------------------------------------------------------------------------------

	// ---- Variables for MSER-m Rule --------------------------------------------------------------
	// Number of samples in a batch. 5 is raccomanded as will perform a MSER-5 rule that seems
	// the most stable
	private static final int batchSize = 5;

	// Maximum number of samples to be checked by the rule.
	// Big values are better but will REALLY slow simulation down
	private static final int maxRuleSamples = 5000;

	// Used to store transient datas to perform MSER-m rule and initialize spectral analisys
	private BatchCircularList batches = new BatchCircularList(batchSize, maxRuleSamples);

	/**minimum length (number of samples) of transient period */
	int minSamples = 0;
	// ---------------------------------------------------------------------------------------------

	// ---- Spectral Analysis ----------------------------------------------------------------------
	// Number of batches
	private int numBatch = 128;
	// Number of samples per batch
	private int batchLen = 8;

	// Mean of batches
	private double[] batchMean;
	// Mean of batches weighted
	private double[] weightBatchMean;

	/**index of the batch*/
	int batch;

	/**the extimated mean */
	protected double extMean = 0;
	/**the extimated variance */
	protected double extVar = 0;
	/**the exitmated confidence intervals */
	protected double confInt = 0;

	/**constant chosen to reduce bias of variance extimator */
	protected double C1;
	/**degrees of freeedom of the t-distribution */
	protected int C2;

	/**number of points of the polyfit*/
	protected int K = numBatch / 4;

	// why numbatch/4 ? --> the signal spectral is computed using as the mean of two samples
	// therefore:
	// divide for 2, because having numbatch batches, we consider numbatch / 2
	// divide again for 2, because we want to use only the first half of the spectr
	// (the second half is sigmetric)

	/**the order of the polynomial*/
	protected int polyOrder = 2;

	/**tells if estimated confidence interval is okay*/
	protected boolean confIntervalOk = false;
	protected boolean disableStatisticStop = false;
	// ---------------------------------------------------------------------------------------------

	// ---- NULL TEST HYPOTHESIS--------------------------------------------------------------------
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
	private static double nullMeasure_upperLimit = 0.0;

	// ---------------------------------------------------------------------------------------------

	/**
	 *  Creates a new Dynamic Data Analyzer
	 *  @param alpha the quantile required for the confidence interval
	 *  @param precision maximum relative error
	 *  @param maxData maximum number of data to be analyzed (at least 5000)
	 */
	public NewDynamicDataAnalyzer(double alpha, double precision, int maxData) {
		if (alpha <= 0 || alpha >= 1 || precision <= 0 || precision >= 1 || maxData < 5000) {
			throw new IllegalArgumentException("Wrong  Dynamic analyzer parameter  " + alpha + "  " + precision + "  " + maxData);
		}
		this.alpha = alpha;
		this.precision = precision;
		this.maxData = maxData;

		// Initialize variables
		hMeans = new double[checkSize * hArrayMult];

		//@author Stefano Omini
		if (nullTestEnabled) {
			//initializes nullTestPeriod (the period of null test repetition)
			nullTestPeriod = (int) (nullTestRate * maxData);
		}
	}

	/**
	 * Adds the new sample to the statistic.
	 *
	 * @param newSample the new sample
	 * @param weight    the weight of the newSample, if it is not needed put 1.
	 * @return true if the computation of confidence interval has finished (i.e. if the
	 *         confidence interval is smaller than the one required by
	 *         the user, or if the analyzed data are too many), false otherwise
	 */
	public boolean addSample(double newSample, double weight) {
		// Skips 0 weight samples
		if (weight == 0.0) {
			return end;
		}

		// Abort simulation when maxData is reached
		if (nsamples > maxData && !confIntervalOk) {
			success = false;
			end = true;
			// Free allocated memory
			hMeans = null;
			return true;
		}
		// Max samples are reached and confidence interval too, but statistic stop was disabled
		else if (nsamples > maxData && confIntervalOk) {
			success = true;
			end = true;
			// Free allocated memory
			hMeans = null;
			return true;
		}

		// Controls if analysis wasn't already terminated
		if (end) {
			return true;
		}

		nsamples++;

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
			if ((nsamples % nullTestPeriod == 0)) {
				if (nullTest()) {
					//null test is true (the measure is almost always equal to 0.0)
					if (canEnd()) {
						end = true;
					}
					confIntervalOk = true;
					success = true;
					measureIsZero = true;
					return end;
				}
			}
		}

		if (!delet) {
			//transient not deleted
			batches.add(newSample, weight);
		}

		// Execute heuristic
		if (!heuristicPassed) {
			heuristicPassed = FishmanRule();
			if (!heuristicPassed) {
				return false;
			} else {
				// Free allocated memory
				hMeans = null;

				// Heuristic to find spectral analysis parameters
				minSamples = transientLen;

				numBatch = 1 << ((int) Math.ceil((Math.log(Math.sqrt(transientLen / 4))) / Math.log(2)));
				if (numBatch < 64) {
					numBatch = 64;
				}
				batchLen = (int) Math.ceil(transientLen / ((double) numBatch * 4));

				// Next iteration will perform spectral analysis
				return false;
			}
		}
		// At this point Fisherman Rule was passed and biased data truncated. New samples will
		// be analyzed with spectral analisys method to find confidence interval

		if (!delet) {
			//transient detected but not deleted yet

			if (nsamples >= minSamples) {
				//min number of samples respected

				//Apply the heuristic rule MSR5 to discard the most biased samples of the
				//time series. It minimizes the Mean Square Error.
				if (deleteTransient()) {

					//transient deleted
					if ((batch == numBatch - 1) && (nsamples % batchLen == 0)) {

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
			// Correct problems with last batch after transient removal
			if (batch >= batchMean.length) {
				batch = batchMean.length - 1;
				batchMean[batch] *= weightBatchMean[batch];
			}

			//transient detected and deleted
			batchMean[batch] += newSample * weight;
			weightBatchMean[batch] += weight;

			if (nsamples % batchLen == 0) {
				batchMean[batch] /= weightBatchMean[batch];

				if (batch == (numBatch - 1)) {
					end = HWtest();
				} else {
					batch++;
				}
			} else {
				end = false;
			}
		}
		return end;
	}
	
	/**
	 * Checks minimum constraints to determine if data analysis can end
	 * @return true if the data analysis can end
	 */
	private boolean canEnd() {
		return !disableStatisticStop && nsamples > minData;
	}

	/**
	 * It uses the Fishman Means Rule
	 * Inserts a new value of the sequence, and approximately detects the initial
	 * transient period.
	 * <br>See
	 * <br>
	 *  Fishman G.S. 1973 �Concepts and Methods in discrete Event Digital Simulation�.
	 *  New York Wiley
	 *
	 * <br>
	 *
	 * An epsilon value was added to the rule to avoid problems when a measure has
	 * a monothonic behaviour to an asymptotic value
	 *
	 * @return true iff heuristic is verified
	 */
	private boolean FishmanRule() {
		//      Compute the running cumulative mean as data
		//      are generated. Count the number of crossings of
		//      the mean, looking backwards to the beginning. If
		//      the number of crossings reaches a pre-specified
		//      value, which means you have reached the truncation
		//      point.

		// Allocate more space if previous array is full
		if (hSamplesNum >= hMeans.length) {
			double[] tmpMeans;
			// Do not allocate too much memory...
			if (hSamplesNum * hArrayMult < maxData) {
				tmpMeans = new double[hSamplesNum * hArrayMult];
			} else {
				tmpMeans = new double[maxData];
			}
			System.arraycopy(hMeans, 0, tmpMeans, 0, hMeans.length);
			hMeans = tmpMeans;
		}

		// Gets previous two values
		double prev = 0, prevprev = 0;
		if (hSamplesNum >= 2) {
			prev = hMeans[hSamplesNum - 1];
			prevprev = hMeans[hSamplesNum - 2];
		}

		double mean = batches.getMeanValue();

		// Stores new mean value into array. Try to compress sequence avoiding
		// store of monothonic values
		if ((prevprev < prev * (1 - hEpsilon) && prev < mean * (1 - hEpsilon)) || (prevprev > prev * (1 + hEpsilon) && prev > mean * (1 + hEpsilon))) {
			// Sequence can be compressed, so overwrite prev value
			hMeans[hSamplesNum - 1] = mean;
		} else {
			hMeans[hSamplesNum] = mean;
			hSamplesNum++;
		}

		// Applies epsilon correction
		double lower = mean * (1 - hEpsilon);
		double upper = mean * (1 + hEpsilon);

		// every checkSize samples try to apply rule. This will spare a lot of cpu cycles
		if (hSamplesNum % checkSize == 0) {
			int crosses = 0; // Number of crosses of mean value
			for (int i = 0; i < hSamplesNum - 2; i++) {
				// Note that hMeans values are to be tested with this rule and NOT Samples
				// as we are studing the distribution of mean values and not of single samples
				if (hMeans[i] >= lower && hMeans[i + 1] <= upper || (hMeans[i] <= upper && hMeans[i + 1] >= lower)) {
					crosses++;
					if (crosses >= crossesNum) {
						transientLen = i;
						// Note: we discard all samples collected by now instead of i samples.
						// this is needed because otherwise we must cache every value of "newSample"
						// and "weight" and this will be prone to OutOfMemoryException
						// (that was tested, so this decision was taken). On the worst case it would be
						// a lost of (checkSize - 1) samples which isn't too much...
						return true;
					}
				}
			}
		}
		return false;
	}

	// --- Old Code by Granata ---------------------------------------------------------------------
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
		double sampleSum, weightSum;
		//computes the mean
		sampleSum = 0;
		weightSum = 0;
		for (int i = 0; i < numBatch; i++) {
			sampleSum += batchMean[i] * weightBatchMean[i];
			weightSum += weightBatchMean[i];
		}
		extMean = sampleSum / weightSum;

		calcVar();
		calcConfInterval();
		if (precision > confInt / extMean && canEnd()) {
			//simulation ended with success
			end = true;

			success = true;
		} else {
			// Handle if statistic stop was disabled or min samples was not reached.
			if (precision > confInt / extMean) {
				success = true;
				confIntervalOk = true;
			} else {
				success = false;
				confIntervalOk = false;
			}

			if (batch == (numBatch - 1)) {
				//optimal number of batches
				int log2;
				log2 = 1 << ((int) Math.ceil((Math.log(Math.sqrt(nsamples))) / Math.log(2)));
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
	 * updates the conf intervals
	 */
	protected void calcConfInterval() {
		double t = TStudent.ICDF(alpha, C2);
		confInt = t * Math.sqrt(extVar / numBatch);
	}

	/**
	 * updates the variance
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
		double[] polyCoeff;

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
		Matrix Xt;
		Matrix square;
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
	 * Apply the heuristic rule MSR5 to discard the most biased samples of the
	 * time series. It minimizes the Mean Square Error.
	 *
	 */
	protected boolean deleteTransient() {

		double sumB;
		double sumW;
		double min = Double.MAX_VALUE;
		double z;
		int d = 0;
		double y;
		double mean;
		double oldMean;
		double sumSquare = 0;
		int data = batches.getNumBatches();
		//OLD
		//int index = miniB.getSize() / 2;
		//NEW
		int index = data / 2;

		//coefficients for the last part of data.
		sumB = batches.getBatchSample(data - 1);
		sumW = batches.getBatchWeight(data - 1);
		oldMean = sumB / sumW;

		double miniW_elem, miniB_elem;

		for (int i = data - 2; i > index; i--) {

			//element i-th
			miniB_elem = batches.getBatchSample(i);
			//element i-th
			miniW_elem = batches.getBatchWeight(i);

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
			miniB_elem = batches.getBatchSample(i);
			//element i-th
			miniW_elem = batches.getBatchWeight(i);

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

		if (d - 1 <= nsamples / (batches.getBatchSize() * batches.getBatchSize())) {
			//the initial warm up period is short
			//cannot apply the algorithm to a sequence too polarized...
			delet = true;
			batches.deleteFirstBatches(d);

			nsamples = batches.getNumBatches() * batches.getBatchSize();

			//heuristic to build the "optimal" number of batches.
			//numBatches is nearest 2 power to sqrt(nsamples)
			numBatch = 1 << ((int) Math.ceil((Math.log(Math.sqrt(nsamples))) / Math.log(2)));
			if (numBatch < 64) {
				numBatch = 64;
			}
			batchLen = batches.getBatchSize() * (int) Math.ceil(batches.getNumBatches() / (double) numBatch);

			int k = 0;
			batchMean = new double[numBatch];
			weightBatchMean = new double[numBatch];
			for (int i = 0; i < batches.getNumBatches(); i++) {
				batchMean[k] += batches.getBatchSample(i);
				weightBatchMean[k] += batches.getBatchWeight(i);
				if (((i + 1) % (batchLen / batches.getBatchSize())) == 0) {
					batchMean[k] /= weightBatchMean[k];
					k++;
				}
			}
			batch = k;

			batches.clean();
			return delet;
		} else {
			//transient can't be deleted
			delet = false;
			minSamples += batches.getDiscarded();
			if (minSamples < maxData) {
				end = true;
				success = false;
			}
			return delet;
		}
	}

	// ---------------------------------------------------------------------------------------------

	// --- Old Code by Omini -----------------------------------------------------------------------
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
		double t = TStudent.ICDF((2 * nullTestAlfa), nsamples);
		double mean = nullTestData.getMean();
		double var = nullTestData.getVar();
		double T;
		if (var != 0) {
			T = mean / (Math.sqrt(var / nsamples));
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

			nullMeasure_upperLimit = t * (Math.sqrt(var / nsamples));

		} else {
			//the null hypothesis is false
			measureIsZero = false;
		}

		return measureIsZero;
	}

	// ---------------------------------------------------------------------------------------------

	/**
	 * Returns if the analysis was succesful
	 *
	 * @return true if the analysis respect all'users requests
	 */
	public boolean getSuccess() {
		return success;
	}

	/**
	 * Gets the mean estimate of the monitored data stream
	 *
	 * @return mean estimate of the monitored data stream
	 */
	public double getMean() {
		return extMean;
	}

	/**
	 * Gets the extimated mean, even if the confidence requirements
	 * haven't been reached
	 *
	 * @return the extimated mean
	 */
	public double extimatedMean() {
		if (!heuristicPassed || !delet) {
			return batches.getMeanValue();
		} else if (extMean < hEpsilon && batches.getMeanValue() >= hEpsilon) {
			return batches.getMeanValue();
		} else {
			return extMean;
		}
	}

	/**
	 * Returns true if null test has found out that this measure is zero
	 *
	 * @return true if null test has found out that this measure is zero
	 */
	public boolean isZero() {
		return measureIsZero;
	}

	/**
	 * Gets the half width confidence interval extimated of the monitored data
	 * stream.
	 *
	 * @return the confidence interval requested
	 */
	public double getConfInt() {
		if ((confInt != 0) && (heuristicPassed)) {
			return confInt;
		} else {
			return 0;
		}
	}

	/**
	 * This method should be called only if mean is equal to zero (@see isZero()), that is
	 * whne nullTest() returns true.
	 *
	 * @return the upper limit of confidence interval when mean is equal to 0.0
	 */
	public double getNullMeasure_upperLimit() {
		return nullMeasure_upperLimit;
	}

	/**
	 * Gets number of samples elaborated up to now
	 *
	 * @return number of samples elaborated
	 */
	public int getSamples() {
		return nsamples;
	}

	/**
	 * Gets the number of discarded data
	 *
	 * @return the number of discarded data
	 */
	public int getDiscarded() {
		return batches.getDiscarded();
	}

	/**
	 * Gets the maximum number of samples that can be analyzed
	 *
	 * @return maximum number of samples that can be analyzed
	 */
	public int getMaxData() {
		return maxData;
	}

	/**
	 * Gets measure precision (i.e. Maximum relative error)
	 *
	 * @return precision value
	 */
	public double getPrecision() {
		return precision;
	}

	/**
	 * Gets measure alpha (i.e. confidence interval)
	 *
	 * @return alpha value
	 */
	public double getAlfa() {
		return alpha;
	}

	/**
	 * Stops the measuring process
	 *
	 * @param success value to be reported by getSuccess()
	 */
	public void stopMeasure(boolean success) {
		this.success = success;
		end = true;
	}

	/**
	 * Sets the reference to the class containing simulation
	 * parameters. The parameters are updated with the passed values.
	 *
	 * @param parameters
	 */
	public void setParameters(SimParameters parameters) {
		this.maxData = parameters.getMaxSamples();
		this.numBatch = parameters.getNumBatch();
		this.batchLen = parameters.getBatchLen();
		this.nullTestAlfa = parameters.getNullTestAlfa();
		this.nullTestRate = parameters.getNullTestRate();
		this.disableStatisticStop = parameters.isDisableStatisticStop();
		this.minData = parameters.getMinSamples();
	}
}
