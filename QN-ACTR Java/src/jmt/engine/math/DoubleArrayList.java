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

package jmt.engine.math;

/** this class creates a dynamic list of double. It's useful to use when you want
 * to access the elements randomly, but you have often to cancel the first part of list
 *  or to add elements in the end but in general the size is approximately
 * constant.
 *
 * @author  Federico Granata
 */
public class DoubleArrayList {
	double[] data;
	int start;
	int end;
	int size;

	/** Creates a new instance of DoubleArrayList */
	public DoubleArrayList() {
		int dumbSize = 100;
		data = new double[dumbSize];
		start = 0;
		end = -1;
		size = 0;
	}

	/** Creates a new instance of DoubleArrayList with a minimum capacity.
	 * @param minCapacity the minimum capcity of the neew list
	 *
	 */
	public DoubleArrayList(int minCapacity) {
		data = new double[minCapacity];
		start = 0;
		end = -1;
		size = 0;
	}

	/** Creates a new instance of DoubleArrayList initialized
	 *  @param  data    the array of initializing data
	 */
	public DoubleArrayList(double[] data) {
		this.data = new double[data.length];
		System.arraycopy(data, 0, this.data, 0, data.length);
		start = 0;
		size = data.length;
		end = size - 1;
	}

	/** add a new element at the end of the list
	 *  @param  newD    the new element
	 */
	public boolean add(double newD) {
		if (data.length == size) {
			ensureCapacity(size * 2);
			end++;
			data[end] = newD;
			size++;
		} else if (end == (data.length - 1) && start != 0) {
			end = 0;
			data[end] = newD;
			size++;
		} else {
			end++;
			data[end] = newD;
			size++;
		}
		return true;
	}

	/**
	 *
	 * Ensures the minimum capacity of the List.
	 * If the lists has more element
	 * then dim it throws an exception.
	 *
	 * @param dim
	 */
	public void ensureCapacity(int dim) {
		if (dim < size) {
			throw new IndexOutOfBoundsException("dim < size");
		}
		if (dim > size && dim < data.length) {
			return;
		}
		double[] temp = new double[dim];
		if (end > start) {
			System.arraycopy(data, start, temp, 0, size);
		} else if (end < start) {
			int direct = data.length - start;
			System.arraycopy(data, start, temp, 0, direct);
			System.arraycopy(data, 0, temp, direct - 1, size - direct);
		}
		start = 0;
		end = size - 1;
		data = temp;
	}

	/** It  clears all the data inside the List.
	 *
	 */
	public void clear() {
		size = 0;
		start = 0;
		end = 0;
		//		System.out.println("Last size = "+data.length);
		data = new double[100];
	}

	/** Gets the data in position pos.
	 *
	 * @param pos
	 * @return the data
	 */
	public double get(int pos) {
		if (pos >= size) {
			throw new IndexOutOfBoundsException("pos >= size");
		}
		if (start < end) {
			return data[pos + start];
		} else {
			int direct = data.length - start;
			if (pos < direct) {
				return data[pos + start];
			} else {
				return data[pos - direct];
			}
		}
	}

	/** deletes the first dim data.
	 *
	 * @param dim
	 */
	public void delFirst(int dim) {
		if (dim > size) {
			throw new IndexOutOfBoundsException("dim > size");
		}
		if (size - dim == 0) {
			start = 0;
			end = -1;
			size = 0;
		} else if (size != 0) {
			if (start + dim < data.length) {
				start += dim;
			} else {
				start = dim - (data.length - start);
			}
			size -= dim;
		}
	}

	/** creates an array of double that contains the data from startPos
	 * to endPos.
	 *
	 * @param startPos
	 * @param endPos
	 * @return
	 */
	public double[] toArray(int startPos, int endPos) {
		double[] temp;
		if (endPos > startPos && startPos < size && endPos < size) {
			temp = new double[endPos - startPos + 1];
			if (endPos + start < data.length) {
				System.arraycopy(data, startPos + start, temp, 0, endPos - startPos + 1);
			} else if (startPos + start < data.length) {
				System.arraycopy(data, startPos + start, temp, 0, data.length - start);
				System.arraycopy(data, 0, temp, data.length - start, endPos - startPos - (data.length - start));
			} else {
				int direct = data.length - start;
				System.arraycopy(data, startPos - direct, temp, 0, endPos - startPos + 1);
			}
			return temp;
		} else {
			if (endPos < startPos) {
				throw new IndexOutOfBoundsException("endPos < startPos");
			}
			if (startPos > size) {
				throw new IndexOutOfBoundsException("startPos > size");
			}
			if (endPos > size) {
				throw new IndexOutOfBoundsException("endPos > size");
			}
			return null;
		}
	}

	/** sets the element in position pos
	 *
	 * @param pos
	 * @param newD
	 */
	public void set(int pos, double newD) {
		if (pos >= size) {
			throw new IndexOutOfBoundsException("pos >= size");
		}
		if (start < end) {
			data[pos + start] = newD;
		} else {
			int direct = data.length - start;
			if (pos < direct) {
				data[pos + start] = newD;
			} else {
				data[pos - direct] = newD;
			}
		}
	}

	/** gets the size of the List
	 *
	 * @return
	 */
	public int getSize() {
		return size;
	}

	/** test
	 *
	 * @return true if it's successfull
	 */
	public static boolean test() {
		DoubleArrayList dal = new DoubleArrayList();
		boolean success = true;
		Class<?> c;
		try {
			c = Class.forName("java.lang.IndexOutOfBoundsException");
			if (dal.getSize() != 0) {
				success = false;
			} else {
				try {
					dal.get(1);
				} catch (Exception e) {
					if (e.getClass() != c) {
						success = false;
					}
				}
				try {
					dal.delFirst(1);
				} catch (Exception e) {
					if (e.getClass() != c) {
						success = false;
					}
				}
				try {
					dal.toArray(0, 0);
				} catch (Exception e) {
					if (e.getClass() != c) {
						success = false;
					}
				}
				try {
					dal.set(1, 0.0);
				} catch (Exception e) {
					if (e.getClass() != c) {
						success = false;
					}
				}
			}
			try {
				for (int i = 0; i < 1000; i++) {
					dal.add(i);
				}
			} catch (Exception e) {
				success = false;
			}

			try {
				if (dal.get(100) != 100.0) {
					success = false;
				}
			} catch (Exception e) {
				if (e.getClass() != c) {
					success = false;
				}
			}

			if (dal.getSize() != 1000) {
				success = false;
			}
			try {
				dal.get(1001);
			} catch (Exception e) {
				if (e.getClass() != c) {
					success = false;
				}
			}

			double[] temp = dal.toArray(10, 20);
			for (int i = 0; i < 11; i++) {
				if (temp[i] != (double) i + 10) {
					success = false;
				}
			}

			dal.delFirst(50);
			if (dal.get(0) != 50) {
				success = false;
			}
			try {
				for (int i = 0; i < 50; i++) {
					dal.add(i);
				}
			} catch (Exception e) {
				success = false;
			}
			if (dal.get(990) != 40) {
				success = false;
			}

			dal.clear();
			dal.ensureCapacity(1000);
			for (int i = 0; i < 1000; i++) {
				dal.add(i);
			}
			for (int i = 0; i < 100; i++) {
				dal.delFirst(100);
				for (int j = 0; j < 100; j++) {
					dal.add(j);
				}
				if (dal.getSize() != 1000) {
					success = false;
				}
				if (dal.data.length != 1000) {
					success = false;
				}
			}

		} catch (ClassNotFoundException e) {
			e.printStackTrace();
		}
		return success;
	}

	public static void speed() {
		long initTime = System.currentTimeMillis();
		for (int i = 0; i < 1; i++) {
			DoubleArrayList dal = new DoubleArrayList();
			for (int j = 0; j < 10000; j++) {
				dal.add(j);
			}
			dal.ensureCapacity(100000);
			for (int k = 0; k < 100; k++) {
				for (int j = 0; dal.getSize() <= 100000; j++) {
					dal.add(j);
				}
				for (int j = 0; j < 50000; j++) {
					dal.get(j);
				}
				dal.toArray(0, 1000);
				dal.delFirst(10000);
			}
			dal.clear();
		}
		System.out.println("time Elapsed = " + (System.currentTimeMillis() - initTime));
		initTime = System.currentTimeMillis();
		for (int i = 0; i < 1; i++) {
			DoubleArrayListOld dal = new DoubleArrayListOld();
			for (int j = 0; j < 10000; j++) {
				dal.add(j);
			}
			dal.ensureCapacity(100000);
			for (int k = 0; k < 100; k++) {
				for (int j = 0; dal.size() <= 100000; j++) {
					dal.add(j);
				}
				for (int j = 0; j < 50000; j++) {
					dal.get(j);
				}
				dal.toArrayD(0, 1000);
				dal.removeR(0, 10000);
			}
			dal.clear();
		}
		System.out.println("time Elapsed = " + (System.currentTimeMillis() - initTime));
	}

	/*public void add(int pos, double newD) {

	}
	*/

}