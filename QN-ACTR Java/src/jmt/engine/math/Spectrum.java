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
 * Spectrum.java
 *
 * Created on 3 novembre 2002, 23.14
 */

package jmt.engine.math;

/** This class perform spectral analysis and has also some useful functions for
 *  DSP
 *
 * @author  Federico Granata
 */
public class Spectrum {

	/** periodogramOfReal
	 * return the periodogram of a real sequence.
	 *  @param data the data sequence
	 *  @return the spectrum
	 */
	public static double[] periodogramOfReal(double[] data, int start, int n) {
		double[] intData = new double[2 * n];
		double[] spectrum;

		if (n > data.length) {
			throw new Error("number of data must be >= than n");
		}

		for (int i = 0; i < n; i++) {
			intData[2 * i] = data[i + start];
		}
		intData = zeroPad(intData);
		FFT.transform(intData);
		spectrum = new double[intData.length / 2];
		for (int i = 0; i < spectrum.length; i++) {
			spectrum[i] = (Math.pow(intData[2 * i], 2) + Math.pow(intData[2 * i + 1], 2)) / n;
		}
		return spectrum;
	}

	/** given a sequence it return the same sequence padded with zeros to the
	 *  nearest power of 2
	 *  @param  the sequence to be padded
	 *  @return the padded sequence
	 */
	public static double[] zeroPad(double[] data) {
		int log = 0;
		int n = data.length;
		double[] intData;

		for (int k = 1; k < n; k *= 2, log++) {
			;
		}
		if (n == (1 << log)) {
			return data;
		} else {
			intData = new double[(1 << log)];
			System.arraycopy(data, 0, intData, 0, n);
			return intData;
		}
	}
}
