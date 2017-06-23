package jmt.engine.dataAnalysis.sorting;

/**
 * A quick sort algorithm.
 *
 */
public class FastQSort implements SortAlgorithm {
	/** This is a generic version of C.A.R Hoare's Quick Sort
	 * algorithm.  This will handle arrays that are already
	 * sorted, and arrays with duplicate keys.<BR>
	 *
	 * If you think of a one dimensional array as going from
	 * the lowest index on the left to the highest index on the right
	 * then the parameters to this function are lowest index or
	 * left and highest index or right.  The first time you call
	 * this function it will be with the parameters 0, a.length - 1.
	 *
	 * @param a         an integer array
	 * @param l     left boundary of array partition
	 * @param r     right boundary of array partition
	 */
	private void quickSort(double a[], int l, int r) {
		int i, j;
		double pivot, temp;

		if (r > l) {
			pivot = a[r];
			i = l - 1;
			j = r;
			for (;;) {
				while (a[++i] < pivot) {
					;
				}
				while (a[--j] > pivot) {
					;
				}
				if (i >= j) {
					break;
				}
				//swap
				temp = a[i];
				a[i] = a[j];
				a[j] = temp;
			}
			temp = a[i];
			a[i] = a[r];
			a[r] = temp;
			quickSort(a, l, i - 1);
			quickSort(a, i + 1, r);
		}

	}

	public void sort(double a[]) {
		quickSort(a, 0, a.length - 1);
	}

}
