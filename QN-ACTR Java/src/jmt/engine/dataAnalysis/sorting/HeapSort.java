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

package jmt.engine.dataAnalysis.sorting;

/**

 * Sorting algorithm for vectors of data.
 * @author Federico Granata
 * @version Date: 24-lug-2003 Time: 14.27.55

 */
public class HeapSort implements SortAlgorithm {

	/**
	 * Sorts data in a data vector & substitutes it.
	 * @param data
	 */
	public void sort(double[] data) {
		int i, ir, j, l;
		double rra;
		if ((data.length - 1) < 2) {
			return;
		}
		l = ((data.length - 1) >> 1) + 1;
		ir = (data.length - 1);

		for (;;) {
			if (l > 1) {
				rra = data[--l];
			} else {
				rra = data[ir];
				data[ir] = data[1];
				if (--ir == 1) {
					data[1] = rra;
					break;
				}
			}
			i = l;
			j = l + l;
			while (j <= ir) {
				if (j < ir && data[j] < data[j + 1]) {
					j++;
				}
				if (rra < data[j]) {
					data[i] = data[j];
					i = j;
					j <<= 1;
				} else {
					break;
				}
			}
			data[i] = rra;
		}
	}

}
