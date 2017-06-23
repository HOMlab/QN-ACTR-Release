package jmt.engine.dataAnalysis.sorting;

//
// Modified by Lev Shereshevsky (leo@aeriesoft.ru) 98/03/20
//

/*
 * @(#)MergeSort.java  1.0 95/06/23 Jason Harrison
 *
 * Copyright (c) 1995 University of British Columbia
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
 * A merge sort demonstration algorithm.
 * SortAlgorithm.java, Thu Oct 27 10:32:35 1994
 *
 * @author Jason Harrison@cs.ubc.ca
 * @version     1.1, 12 Jan 1998
 */
public class MergeSort implements SortAlgorithm {

	/**
	 *   pre :  0 <= p <= r <= A.length
	 *
	 *  post :  elements of A[p..r] are rearranged so that
	 *
	 *             A[p] <= A[p + 1] <= ... <= A[r]
	 */
	private static void sort(double[] A, int p, int r) {
		int q;

		if (p < r) {
			/* split the data into two halves */
			q = (p + r) / 2;

			/* recursively sort each half */
			sort(A, p, q);
			sort(A, q + 1, r);

			/* ``merge'' the two halves */
			merge(A, p, q, r);
		}
	}

	/**
	 *   pre :  (i)   0 <= p <= q < r <= A.length-1
	 *          (ii)  A[p] <= A[p+1] <= ... <= A[q]
	 *          (iii) A[q+1] <= A[q+2] <= ... <= A[r]
	 *
	 *  post :  elements of A[p..r] are rearranged so that
	 *
	 *             A[p] <= A[p+1] <= ... <= A[r]
	 */
	private static void merge(double[] A, int p, int q, int r) {
		int i, j, k;

		int N = r - p + 1;
		double[] copy = new double[N];

		/* copy data to be merged into array ``copy'' */
		for (i = 0; i < N; i++) {
			copy[i] = A[p + i];
		}

		int mid = q - p; // index of middle element in ``copy''
		int end = r - p; // index of last element in ``copy''

		i = 0;
		j = mid + 1;
		k = p;
		while (i <= mid && j <= end) {

			/* invariant:
			 *   A[p] <= A[p+1] <= ... <= A[k] <= min(copy[i], copy[j])
			 */

			if (copy[i] < copy[j]) {
				A[k] = copy[i];
				i++;
			} else {
				A[k] = copy[j];
				j++;
			}
			k++;
		}

		// copy left over stuff
		int a0, aN;
		if (i > mid) {
			a0 = j;
			aN = end;
		} else {
			a0 = i;
			aN = mid;
		}

		/* A[k-1] <= copy[a0] <= copy[a0+1] <= ... <= copy[aN] */

		// copy rest
		for (i = a0; i <= aN; i++) {
			A[k] = copy[i];
			k++;
		}
	}

	public void sort(double[] data) {
		sort(data, 0, data.length - 1);
	}

}
