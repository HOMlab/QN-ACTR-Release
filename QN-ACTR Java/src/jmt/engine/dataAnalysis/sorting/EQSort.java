package jmt.engine.dataAnalysis.sorting;

//
// Modified by Lev Shereshevsky (leo@aeriesoft.ru) 98/03/20
//

/*
 * @(#)EQSort.java     1.0 95/06/26 Jim Boritz
 *
 * Copyright (c) 1995 UBC Microsystems, Inc. All Rights Reserved.
 *
 * Permission to use, copy, modify, and distribute this software
 * and its documentation for NON-COMMERCIAL purposes and without
 * fee is hereby granted provided that this copyright notice
 * appears in all copies. Please refer to the file "copyright.html"
 * for further important copyright and licensing information.
 *
 * UBC MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF
 * THE SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NON-INFRINGEMENT. UBC SHALL NOT BE LIABLE FOR
 * ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR
 * DISTRIBUTING THIS SOFTWARE OR ITS DERIVATIVES.
 */

/**
 * 19 Feb 1996: Fixed to avoid infinite loop discovered by Paul Haberli.
 *              Misbehaviour expressed when the pivot element was not unique.
 *              -Jason Harrison
 *
 * 21 Jun 1996: Modified code based on comments from Paul Haeberli, and
 *              Peter Schweizer (Peter.Schweizer@mni.fh-giessen.de).
 *              Used Daeron Meyer's (daeron@geom.umn.edu) code for the
 *              new pivoting code. - Jason Harrison
 *
 * 09 Jan 1998: Another set of bug fixes by Thomas Everth (everth@wave.co.nz)
 *              and John Brzustowski (jbrzusto@gpu.srv.ualberta.ca).
 */

/**
 * An enhanced quick sort demonstration algorithm.
 * SortAlgorithm.java, Thu Oct 27 10:32:35 1994
 *
 * @author Jim Boritz
 * @version     1.0, 26 Jun 1995
 */

public class EQSort implements SortAlgorithm {
	void brute(double a[], int lo, int hi) {
		if ((hi - lo) == 1) {
			if (a[hi] < a[lo]) {
				double T = a[lo];
				a[lo] = a[hi];
				a[hi] = T;
			}
		}
		if ((hi - lo) == 2) {
			int pmin = a[lo] < a[lo + 1] ? lo : lo + 1;
			pmin = a[pmin] < a[lo + 2] ? pmin : lo + 2;
			if (pmin != lo) {
				double T = a[lo];
				a[lo] = a[pmin];
				a[pmin] = T;
			}
			brute(a, lo + 1, hi);
		}
		if ((hi - lo) == 3) {
			int pmin = a[lo] < a[lo + 1] ? lo : lo + 1;
			pmin = a[pmin] < a[lo + 2] ? pmin : lo + 2;
			pmin = a[pmin] < a[lo + 3] ? pmin : lo + 3;
			if (pmin != lo) {
				double T = a[lo];
				a[lo] = a[pmin];
				a[pmin] = T;
			}
			int pmax = a[hi] > a[hi - 1] ? hi : hi - 1;
			pmax = a[pmax] > a[hi - 2] ? pmax : hi - 2;
			if (pmax != hi) {
				double T = a[hi];
				a[hi] = a[pmax];
				a[pmax] = T;
			}
			brute(a, lo + 1, hi - 1);
		}
	}

	void sort(double a[], int lo0, int hi0) {
		int lo = lo0;
		int hi = hi0;
		if ((hi - lo) <= 3) {
			brute(a, lo, hi);
			return;
		}

		/*
		 *  Pick a pivot and move it out of the way
		 */
		double pivot = a[(lo + hi) / 2];
		a[(lo + hi) / 2] = a[hi];
		a[hi] = pivot;

		while (lo < hi) {
			/*
			 *  Search forward from a[lo] until an element is found that
			 *  is greater than the pivot or lo >= hi
			 */
			while ((a[lo] <= pivot) && lo < hi) {
				lo++;
			}

			/*
			 *  Search backward from a[hi] until element is found that
			 *  is less than the pivot, or hi <= lo
			 */
			while ((pivot <= a[hi]) && lo < hi) {
				hi--;
			}

			/*
			 *  Swap elements a[lo] and a[hi]
			 */
			if (lo < hi) {
				double T = a[lo];
				a[lo] = a[hi];
				a[hi] = T;
			}
		}

		/*
		 *  Put the median in the "center" of the list
		 */
		a[hi0] = a[hi];
		a[hi] = pivot;

		/*
		 *  Recursive calls, elements a[lo0] to a[lo-1] are less than or
		 *  equal to pivot, elements a[hi+1] to a[hi0] are greater than
		 *  pivot.
		 */
		sort(a, lo0, lo - 1);
		sort(a, hi + 1, hi0);
	}

	public void sort(double[] data) {
		sort(data, 0, data.length - 1);
	}
}
