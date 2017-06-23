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
 * DoubleArrayList.java
 *
 * Created on 20 ottobre 2002, 11.28
 */

package jmt.engine.math;

/** this class creats a dynamic list of double.
 *
 * @author  Federico Granata
 */
public class DoubleArrayListOld extends java.util.ArrayList {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/** Creates a new instance of DoubleArrayList */
	public DoubleArrayListOld() {
	}

	/** Creates a new instance of DoubleArrayList initialized
	 *  @param  data    the array of initializing data
	 */
	public DoubleArrayListOld(double[] data) {
		this.ensureCapacity(data.length);
		for (double element : data) {
			this.add(element);
		}
	}

	/** add a new element at the end of the list
	 *  @param  newD    the new element
	 */
	public boolean add(double newD) {
		return add(new Double(newD));
	}

	/** add a new element at the specified position
	 *  @param  pos     position
	 *  @param  newD    the new element
	 */
	public void add(int pos, double newD) {
		add(pos, new Double(newD));
	}

	/** get the element in the specified position.
	 *  @param  pos     position
	 *  @return         the element requested
	 */
	public double getD(int pos) {
		return ((Double) get(pos)).doubleValue();
	}

	/** remove all the elements from position start to end
	 *  @param  start
	 *  @param  end
	 */
	public void removeR(int start, int end) {
		removeRange(start, end);
	}

	/** set element at the specified position
	 *  @param  pos     position
	 *  @param  newD    the new value
	 */
	public void set(int pos, double newD) {
		Object obj = set(pos, new Double(newD));
	}

	/** creates an array of the elements of the list.
	 *  @return the array
	 */
	public double[] toArrayD() {
		double[] arrayD;
		Object[] arrayDouble = toArray();
		arrayD = new double[arrayDouble.length];
		for (int i = 0; i < arrayDouble.length; i++) {
			arrayD[i] = ((Double) arrayDouble[i]).doubleValue();
		}
		return arrayD;
	}

	/** creates an array of the elements of the list.
	 *  @param  start the starting point
	 *  @param  end the last data
	 *  @return the array
	 */
	public double[] toArrayD(int start, int end) {
		double[] arrayD = new double[end - start + 1];
		int k = 0;
		for (int i = start; i <= end; i++) {
			arrayD[k] = ((Double) this.get(i)).doubleValue();
			k++;
		}
		return arrayD;
	}

	/** create a string (attention it could be very large) with all the elements
	 *  of the list, one for each line.
	 *  @return the string
	 */
	@Override
	public String toString() {
		StringBuffer buf = new StringBuffer();
		for (int i = 0; i < size(); i++) {
			buf.append(getD(i) + "\n");
		}
		return buf.toString();
	}

}
