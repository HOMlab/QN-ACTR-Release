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
package jmt.engine.jwat.filters;

import jmt.engine.jwat.Observation;
import jmt.engine.random.engine.MersenneTwister;

public class RandomFilter implements FilterOnVariable {
	private boolean[] matcher = null;

	public RandomFilter(int total, int nObs) {
		generateRandom(total, nObs, matcher = new boolean[total]);
	}

	public boolean isMatching(Observation o, int pos) {
		return matcher[pos];
	}

	private int generateRandom(int numTotal, int numToRead, boolean[] catchVal) {
		int[] rndArray = new int[numTotal];
		int max = 0, i, dimArray, rndNum;
		MersenneTwister mst = new MersenneTwister();

		dimArray = numTotal;
		for (i = 0; i < dimArray; i++) {
			rndArray[i] = i;
		}

		for (i = 0; i < numToRead; i++) {
			rndNum = (int) (mst.nextDouble() * dimArray);
			catchVal[rndArray[rndNum]] = true;
			if (max < rndArray[rndNum]) {
				max = rndArray[rndNum];
			}
			rndArray[rndNum] = rndArray[--dimArray];
		}

		return max;
	}
}
