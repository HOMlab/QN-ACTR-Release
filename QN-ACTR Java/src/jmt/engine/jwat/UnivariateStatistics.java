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
package jmt.engine.jwat;

/**
 * Description: 
 * @author Brambilla Davide Matr 667986, Fumagalli Claudio 667971
 * Class created 1-ago-2006 10.01.22 Darksch
 * 
 */
public class UnivariateStatistics {
	// Mean value
	private double mean = 0;
	// Variance and Standard Deviation
	private double variance = 0;
	private double devStd = 0;
	private double minimum = 0;
	private double maximum = 0;
	// Skewness
	private double skewness = 0;
	// Kurtosis
	private double kurtosis = 0;
	// Index of quantili
	private int[] quantili = new int[99];
	// Observations list
	private Observation[] list = null;
	private int nVar;

	/**
	 * Constructor: calculates univariate statistics using list observation array for the nVar-th variable
	 * @param list list of observation sorted by nVar-th variable
	 * @param nVar variable used to calculate univariate statistics 
	 */
	public UnivariateStatistics(Observation[] list, int nVar) {
		this.list = list;
		this.nVar = nVar;
		calculateStatistics();
	}

	/**
	 * Returns Deviation Standard value
	 * @return Deviation Standard value
	 */
	public double getDevStd() {
		return devStd;
	}

	/**
	 * Return Kurtosis value
	 * @return Kurtosis value
	 */
	public double getKurtosis() {
		return kurtosis;
	}

	/**
	 * Returns Mean value
	 * @return Mean value
	 */
	public double getMean() {
		return mean;
	}

	/**
	 * Returns Quantili list
	 * @return Quantili list
	 */
	public int[] getQuantili() {
		return quantili;
	}

	/**
	 * Returns Skewness value
	 * @return Skewness value
	 */
	public double getSkewness() {
		return skewness;
	}

	/**
	 * Returns Variance value
	 * @return Variance value
	 */
	public double getVariance() {
		return variance;
	}

	//TODO Verificare con Fuma non sono sicuro
	/**
	 * Returns Median Value
	 * @return Median Value
	 */
	public double getMedian() {
		return list[quantili[49]].getIndex(nVar);
	}

	/**
	 * Returns Maximum value
	 * @return Maximum value
	 */
	public double getMaxValue() {
		return maximum;
	}

	/**
	 * REturns number of observations
	 * @return number of observations
	 */
	public int getNumObs() {
		return list.length;
	}

	/**
	 * Returns Minimum value
	 * @return Minimum value
	 */
	public double getMinValue() {
		return minimum;
	}

	/**
	 * Returns Range value
	 * @return Range value
	 */
	public double getRangeValue() {
		return getMaxValue() - getMinValue();
	}

	/**
	 * Restituisce la dimensione dell'intervallo per i grafici
	 * @return
	 */
	public double getDimInt1000() {
		return getRangeValue() / 1000;
	}

	/**
	 * Calculates univariate statistics 
	 */
	private void calculateStatistics() {
		// Calculate mean and variance and quantili and interval
		int qIndex = 1;
		int i = 0;
		minimum = list[0].getIndex(nVar);
		maximum = list[list.length - 1].getIndex(nVar);
		for (i = 0; i < list.length; i++) {
			mean += list[i].getIndex(nVar);
			variance += Math.pow(list[i].getIndex(nVar), 2);
			//Quantili index
			if (i == (qIndex * list.length / 100)) {
				quantili[qIndex - 1] = i;
				qIndex++;
			}
		}
		/*** MEAN ***/
		mean = mean / list.length;
		/*** VARIANCE ***/
		variance = (variance - (Math.pow(mean, 2) * list.length)) / (list.length - 1);
		/*** STANDARD DEVIATION ***/
		devStd = Math.sqrt(variance);
		// Calculates skewness and kurtosis
		double mu4 = 0;
		for (i = 0; i < list.length; i++) {
			skewness += Math.pow((list[i].getIndex(nVar) - mean), 3);
			mu4 += Math.pow((list[i].getIndex(nVar) - mean), 4);
		}
		/*** SKEWNESS ***/
		skewness *= list.length;
		skewness /= (list.length - 1);
		skewness /= (list.length - 2);
		skewness /= Math.sqrt(Math.pow(variance, 3));
		/*** KURTOSIS ***/
		kurtosis = (float) (mu4 * (list.length - 1));
		kurtosis /= Math.pow(list.length * variance, 2);
		kurtosis -= 3;
	}
}
