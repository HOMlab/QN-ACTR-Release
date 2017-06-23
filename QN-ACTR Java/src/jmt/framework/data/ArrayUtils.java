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

package jmt.framework.data;

import java.util.Arrays;

/**

 * @author alyf (Andrea Conti)
 * Date: 11-set-2003
 * Time: 16.46.05

 Modified by Bertoli Marco 18-may-2006

 */

/**
 * Assorted array mangling stuff.<br>
 * <li><code>resize</code> methods return a resized copy of an array, preserving data where possible and padding with a specified value
 * <li><code>copy</code> methods perform a deep copy of an array. They come in two flavors: one that copies into an existing array (that must have been properly initialized) and one that creates the copy from scratch
 * <li><code>delete</code> methods delete an element in an array, returning a reduced-size copy in which following elements have been shifted to fill the gap. For multidim arrays shallow copy is performed when possible - ie do not assume the new array to be independent from the old one
 * <li><code>toCSV</code> and <code>fromCSV</code> methods input/output single rows as Strings of comma-separated values.
 * <li><code>toString</code> methods return a somewhat useful String representation of an array.
 * <p><b><i>NOTE:</i></b> <code>fromCSV</code> uses the new regex String splitting methods introduced in 1.4. For use in pre-1.4 JDKs it must be rewritten using StringTokenizer.
 *
 */
public class ArrayUtils {

	private ArrayUtils() {
	}

	/**
	 * Resizes an array, preserving data and padding as needed
	 */
	public static int[] resize(int[] array, int newsize, int padding) {
		if (newsize < 0) {
			throw new RuntimeException("newsize must be >=0");
		}
		int oldsize = array.length;
		if (newsize == oldsize) {
			return array;
		}

		int[] newArray = new int[newsize];

		int count = (oldsize < newsize ? oldsize : newsize);

		System.arraycopy(array, 0, newArray, 0, count);

		if (padding != 0) {
			for (int i = count; i < newsize; i++) {
				newArray[i] = padding;
			}
		}

		return newArray;
	}

	/**
	 * Resizes an array, preserving data and padding as needed
	 */
	public static double[] resize(double[] array, int newsize, double padding) {
		if (newsize < 0) {
			throw new RuntimeException("newsize must be >=0");
		}
		int oldsize = array.length;
		if (newsize == oldsize) {
			return array;
		}
		double[] newArray = new double[newsize];
		int count = (oldsize < newsize ? oldsize : newsize);
		System.arraycopy(array, 0, newArray, 0, count);

		if (padding != 0.0) {
			for (int i = count; i < newsize; i++) {
				newArray[i] = padding;
			}
		}

		return newArray;
	}

	/**
	 * Resizes an array, preserving data and padding as needed
	 */
	public static String[] resize(String[] array, int newsize, String padding) {
		if (newsize < 0) {
			throw new RuntimeException("newsize must be >=0");
		}
		int oldsize = array.length;
		if (newsize == oldsize) {
			return array;
		}

		String[] newArray = new String[newsize];

		int count = (oldsize < newsize ? oldsize : newsize);
		System.arraycopy(array, 0, newArray, 0, count);

		if (padding != null) {
			for (int i = count; i < newsize; i++) {
				newArray[i] = padding;
			}
		}

		return newArray;
	}

	/**
	 * Resizes a 2-dimensional array preserving data and padding as needed
	 */
	public static double[][] resize2(double[][] array, int newsize1, int newsize2, double padding) {
		if (newsize1 < 0 || newsize2 < 0) {
			throw new RuntimeException("newsizes must be >=0");
		}

		int oldsize1 = array.length;
		int oldsize2 = array[0].length;

		if (oldsize1 == newsize1 && oldsize2 == newsize2) {
			return array;
		}

		//1st dimension

		double[][] newArray = new double[newsize1][];

		int count = (oldsize1 < newsize1 ? oldsize1 : newsize1);
		System.arraycopy(array, 0, newArray, 0, count);

		//2nd dimension
		for (int i = 0; i < count; i++) {
			newArray[i] = resize(array[i], newsize2, padding);
		}

		// fill in nulls
		for (int i = count; i < newsize1; i++) {
			newArray[i] = new double[newsize2];
		}

		//pad
		if (padding != 0.0) {
			for (int i = count; i < newsize1; i++) {
				Arrays.fill(newArray[i], padding);
			}
		}

		return newArray;
	}

	/**
	 * Resizes a 3-dimensional array with variable sizes preserving data and padding as needed
	 */
	public static double[][][] resize3var(double[][][] array, int newsize1, int newsize2, int[] newsize3, double padding) {
		if (newsize1 < 0 || newsize2 < 0) {
			throw new RuntimeException("newsizes must be >=0");
		}

		int oldsize1 = array.length;

		//1st dimension
		double[][][] newArray = new double[newsize1][][];

		int count = (oldsize1 < newsize1 ? oldsize1 : newsize1);
		System.arraycopy(array, 0, newArray, 0, count);

		//2nd & 3rd dimension
		for (int i = 0; i < count; i++) {
			newArray[i] = resize2(array[i], newsize2, newsize3[i], padding);
		}

		// fill in nulls
		for (int i = count; i < newsize1; i++) {
			newArray[i] = new double[newsize2][newsize3[i]];
		}

		//pad
		if (padding != 0.0) {
			for (int i = count; i < newsize1; i++) {
				for (int j = 0; j < newsize2; j++) {
					Arrays.fill(newArray[i][j], padding);
				}
			}
		}

		return newArray;
	}

	public static String[] copy(String[] array) {
		String[] newArray = new String[array.length];
		System.arraycopy(array, 0, newArray, 0, array.length);
		return newArray;
	}

	public static double[] copy(double[] array) {
		double[] newArray = new double[array.length];
		System.arraycopy(array, 0, newArray, 0, array.length);
		return newArray;
	}

	public static int[] copy(int[] array) {
		int[] newArray = new int[array.length];
		System.arraycopy(array, 0, newArray, 0, array.length);
		return newArray;
	}

	/* all multidim copy methods written explicitly to avoid some call overhead.
	Multidim copiers correctly handle non-square arrays */

	/**
	 * @return a deep copy of <code>array</code>
	 */
	public static double[][] copy2(double[][] array) {
		int len1, len2;
		len1 = array.length;
		double[][] newArray = new double[len1][];
		System.arraycopy(array, 0, newArray, 0, len1);
		for (int i = 0; i < len1; i++) {
			len2 = array[i].length;
			newArray[i] = new double[len2];
			System.arraycopy(array[i], 0, newArray[i], 0, len2);
		}

		return newArray;
	}

	/**
	 * @return a deep copy of <code>array</code>
	 */
	public static double[][][] copy3(double[][][] array) {

		int len1, len2, len3;
		len1 = array.length;
		double[][][] newArray = new double[len1][][];
		System.arraycopy(array, 0, newArray, 0, len1);
		for (int i = 0; i < len1; i++) {
			len2 = array[i].length;
			newArray[i] = new double[len2][];
			System.arraycopy(array[i], 0, newArray[i], 0, len2);
			for (int j = 0; j < len2; j++) {
				len3 = array[i][j].length;
				newArray[i][j] = new double[len3];
				System.arraycopy(array[i][j], 0, newArray[i][j], 0, len3);
			}
		}

		return newArray;
	}

	public static double[][][] copy3per2(double[][][] array3, double[][] factor) {
		double[][][] newArray = new double[array3.length][][];
		for (int i = 0; i < array3.length; i++) {
			newArray[i] = new double[array3[i].length][];
			if (i < factor.length) {
				for (int j = 0; j < array3[i].length; j++) {
					newArray[i][j] = new double[array3[i][j].length];
					if (j < factor[i].length) {
						for (int k = 0; k < array3[i][j].length; k++) {
							newArray[i][j][k] = array3[i][j][k] * factor[i][j];
						}
					} else {
						return copy3(array3);
					}
				}
			} else {
				return copy3(array3);
			}
		}
		return newArray;
	}

	public static void copy(String[] from, String[] to) {
		System.arraycopy(from, 0, to, 0, from.length);
	}

	public static void copy(double[] from, double[] to) {
		System.arraycopy(from, 0, to, 0, from.length);
	}

	public static void copy(int[] from, int[] to) {
		System.arraycopy(from, 0, to, 0, from.length);
	}

	public static void copy2(double[][] from, double[][] to) {
		int len1, len2;
		len1 = from.length;
		System.arraycopy(from, 0, to, 0, len1);
		for (int i = 0; i < len1; i++) {
			len2 = from[i].length;
			System.arraycopy(from[i], 0, to[i], 0, len2);
		}
	}

	public static void copy3(double[][][] from, double[][][] to) {

		int len1, len2, len3;
		len1 = from.length;
		System.arraycopy(from, 0, to, 0, len1);
		for (int i = 0; i < len1; i++) {
			len2 = from[i].length;
			System.arraycopy(from[i], 0, to[i], 0, len2);
			for (int j = 0; j < len2; j++) {
				len3 = from[i][j].length;
				System.arraycopy(from[i][j], 0, to[i][j], 0, len3);
			}
		}
	}

	public static String[] delete(String[] arr, int idx) {
		int newlen = arr.length - 1;
		if (newlen < 0) {
			throw new IllegalArgumentException("cannot delete from an empty array");
		}
		String[] res = new String[newlen];
		if (idx > 0) {
			System.arraycopy(arr, 0, res, 0, idx);
		}
		if (idx < newlen) {
			System.arraycopy(arr, idx + 1, res, idx, newlen - idx);
		}
		return res;
	}

	public static int[] delete(int[] arr, int idx) {
		int newlen = arr.length - 1;
		if (newlen < 0) {
			throw new IllegalArgumentException("cannot delete from an empty array");
		}
		int[] res = new int[newlen];
		if (idx > 0) {
			System.arraycopy(arr, 0, res, 0, idx);
		}
		if (idx < (newlen)) {
			System.arraycopy(arr, idx + 1, res, idx, newlen - idx);
		}
		return res;
	}

	public static double[] delete(double[] arr, int idx) {
		int newlen = arr.length - 1;
		if (newlen < 0) {
			throw new IllegalArgumentException("cannot delete from an empty array");
		}
		double[] res = new double[newlen];
		if (idx > 0) {
			System.arraycopy(arr, 0, res, 0, idx);
		}
		if (idx < (newlen)) {
			System.arraycopy(arr, idx + 1, res, idx, newlen - idx);
		}
		return res;
	}

	public static double[][] delete2_1(double[][] arr, int idx) {
		int newlen = arr.length - 1;
		if (newlen < 0) {
			throw new IllegalArgumentException("cannot delete from an empty array");
		}
		double[][] res = new double[newlen][];
		if (idx > 0) {
			System.arraycopy(arr, 0, res, 0, idx);
		}
		if (idx < (newlen)) {
			System.arraycopy(arr, idx + 1, res, idx, newlen - idx);
		}
		return res;
	}

	public static double[][] delete2_2(double[][] arr, int idx) {
		for (int i = 0; i < arr.length; i++) {
			arr[i] = delete(arr[i], idx);
		}
		return arr;
	}

	public static double[][][] delete3_1(double[][][] arr, int idx) {
		int newlen = arr.length - 1;
		if (newlen < 0) {
			throw new IllegalArgumentException("cannot delete from an empty array");
		}
		double[][][] res = new double[newlen][][];
		if (idx > 0) {
			System.arraycopy(arr, 0, res, 0, idx);
		}
		if (idx < (newlen)) {
			System.arraycopy(arr, idx + 1, res, idx, newlen - idx);
		}
		return res;
	}

	public static double[][][] delete3_2(double[][][] arr, int idx) {
		for (int i = 0; i < arr.length; i++) {
			arr[i] = delete2_1(arr[i], idx);
		}
		return arr;
	}

	public static double[][][] delete3_3(double[][][] arr, int idx) {
		for (int i = 0; i < arr.length; i++) {
			for (int j = 0; j < arr[i].length; j++) {
				arr[i][j] = delete(arr[i][j], idx);
			}
		}
		return arr;
	}

	/** for each i,j result[i][j] = array[i][index2][j-1] */
	public static double[][] extract13(double[][][] array, int index2) {
		int len1 = array.length;
		double[][] res = new double[len1][];

		for (int i = 0; i < len1; i++) {
			res[i] = array[i][index2];
		}

		return res;
	}

	/** for each i result[i] = array[i][index2] */
	public static double[] extract1(double[][] array, int index2) {
		int len = array.length;
		double[] res = new double[len];

		for (int i = 0; i < len; i++) {
			res[i] = array[i][index2];
		}
		return res;
	}

	/** create an empty double[len1][len2] */
	public static double[][] makeFilled(int len1, int len2, double val) {
		boolean fill = (val != 0);
		double[][] res = new double[len1][];
		for (int i = 0; i < len1; i++) {
			res[i] = new double[len2];
			if (fill) {
				Arrays.fill(res[i], val);
			}
		}
		return res;
	}

	/** for each i result[i][index2] = array[i] */
	public static void insert1(double[][] result, double[] array, int index2) {
		for (int i = 0; i < array.length; i++) {
			result[i][index2] = array[i];
		}
	}

	/** for each i result[i]=(int)array[i] */
	public static int[] toInt(double[] arr) {
		int len = arr.length;
		int[] res = new int[len];
		for (int i = 0; i < len; i++) {
			res[i] = (int) arr[i];
		}
		return res;
	}

	public static String toCSV(double[] array) {
		if (array.length == 0) {
			return "";
		}
		StringBuffer s = new StringBuffer();

		s.append(Double.toString(array[0]));

		for (int i = 1; i < array.length; i++) {
			s.append(";").append(Double.toString(array[i]));
		}
		return s.toString();
	}

	public static String toCSV3_2(double[][][] array, int index1, int index3) {
		int len2 = array[index1].length;
		if (len2 == 0) {
			return "";
		}
		StringBuffer s = new StringBuffer();

		s.append(Double.toString(array[index1][0][index3]));

		for (int i = 1; i < len2; i++) {
			s.append(";").append(Double.toString(array[index1][i][index3]));
		}
		return s.toString();
	}

	public static boolean fromCSV(double[] array, String str) {
		String[] tokens = str.split(";");
		int ntok = tokens.length;
		if (ntok != array.length) {
			return false;
		}
		for (int i = 0; i < ntok; i++) {
			array[i] = Double.parseDouble(tokens[i]);
		}
		return true;
	}

	/**
	 * Extract a double[] given a string separated by ';'
	 * @param str input string
	 * @return double array
	 * <br>
	 * Author: Bertoli Marco
	 */
	public static double[] fromCSV(String str) {
		String[] tokens = str.split(";");
		double[] out = new double[tokens.length];
		for (int i = 0; i < out.length; i++) {
			out[i] = Double.parseDouble(tokens[i]);
		}
		return out;
	}

	public static boolean fromCSV3_2(double[][][] array, int index1, int index3, String str) {
		String[] tokens = str.split(";");
		int ntok = tokens.length;
		if (ntok != array[index1].length) {
			return false;
		}

		for (int i = 0; i < ntok; i++) {
			array[index1][i][index3] = Double.parseDouble(tokens[i]);
		}
		return true;
	}

	/* good ol' Arrays only has 'em for 1 dimension...*/
	public static boolean equals2(double[][] arr1, double[][] arr2) {
		int len1 = arr1.length;
		if (len1 != arr2.length) {
			return false;
		}
		for (int i = 0; i < len1; i++) {
			if (!Arrays.equals(arr1[i], arr2[i])) {
				return false;
			}
		}
		return true;
	}

	public static boolean equals3(double[][][] arr1, double[][][] arr2) {
		int len1 = arr1.length;
		if (len1 != arr2.length) {
			return false;
		}
		for (int i = 0; i < len1; i++) {
			if (!equals2(arr1[i], arr2[i])) {
				return false;
			}
		}
		return true;
	}

	public static String toString(Object[] array) {
		StringBuffer s = new StringBuffer("[ ");
		for (Object element : array) {
			if (element == null) {
				s.append("null");
			} else {
				s.append(element.toString());
			}
			s.append(" ");
		}
		s.append("]");

		return s.toString();
	}

	public static String toString(int[] array) {
		StringBuffer s = new StringBuffer("[ ");
		for (int element : array) {
			s.append(element).append(" ");
		}
		s.append("]");

		return s.toString();
	}

	public static String toString(double[] array) {
		StringBuffer s = new StringBuffer("[ ");
		for (double element : array) {
			s.append(element).append(" ");
		}
		s.append("]");

		return s.toString();
	}

	public static String toString2(double[][] array) {
		StringBuffer s = new StringBuffer("[ ");
		for (double[] element : array) {
			s.append(toString(element)).append(" ");
		}
		s.append("]");

		return s.toString();
	}

	public static String toString3(double[][][] array) {
		StringBuffer s = new StringBuffer("[ ");
		for (double[][] element : array) {
			s.append(toString2(element)).append(" ");
		}
		s.append("]");

		return s.toString();
	}

	/**
	 * array[n]->result[n+1], where
	 *
	 * result[0]=0
	 * and
	 * for i=(0..n-1) result[i]=array[i-1]
	 *
	 */
	public static double[] prepend0(double[] array) {
		int len = array.length;
		double[] res = new double[len + 1];
		System.arraycopy(array, 0, res, 1, len);
		return res;
	}

	/**
	 * Copies a 2D array in a 3D one. Given source[i][j] elements are copied in
	 * position dest[i][j][k] where k is a given index.
	 * <br>
	 * Author: Bertoli Marco
	 * @param source source 2D array
	 * @param dest destination 3D array
	 * @param k third coordinate to be left unchanged
	 */
	public static void copy2to3(double[][] source, double[][][] dest, int k) {
		for (int i = 0; i < source.length; i++) {
			for (int j = 0; j < source[i].length; j++) {
				dest[i][j][k] = source[i][j];
			}
		}
	}

	/**
	 * Given an array <em>A</em> and a number <em>n</em> returns the product <em>A*n</em>
	 * <br>
	 * Author: Bertoli Marco
	 * @param source source array <em>A</em>
	 * @param num number to be multiplied <em>n</em>
	 * @return a new array with multiplied value
	 */
	public static double[] multiply(double[] source, double num) {
		double[] ret = new double[source.length];
		for (int i = 0; i < ret.length; i++) {
			ret[i] = source[i] * num;
		}
		return ret;
	}
}
