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
 * This class rapresents a single observation retrived from a log file. Every element is a double number
 * that rapresents either the true value or the converted value rapresenting a string, date or other types.
 * 
 * @author Brambilla Davide matr 667986, Fumagalli Claudio 667971
 *
 */
public class Observation implements Comparable {
	/* List of values belong to a single observation */
	private double[] observation = null;
	/* Identify the element of the observation used to accomplish the compareTo method */
	private static int sortIndex = 0;
	/* Validity */
	private boolean valid = true;
	private int ID;

	/**
	 * Constructor, creates an instance of observation which contains the values passed throught obsList 
	 * double list.
	 * @param obsList Elements of observation
	 */
	public Observation(double[] obsList, int ID) {
		observation = new double[obsList.length];
		this.ID = ID;
		for (int i = 0; i < observation.length; i++) {
			observation[i] = obsList[i];
		}
	}

	public int getID() {
		return ID;
	}

	public void setID(int ID) {
		this.ID = ID;
	}

	/**
	 * Comparable interface method overridden, to allow to sort observations according to sortIndex value
	 * @param obj element to compare to this instance
	 * @return a negative integer, zero, or a positive integer as this object 
	 *         is less than, equal to, or greater than the specified object.
	 */
	public int compareTo(Object obj) {
		if (observation[sortIndex] > ((Observation) obj).getIndex(sortIndex)) {
			return 1;
		}
		if (observation[sortIndex] == ((Observation) obj).getIndex(sortIndex)) {
			return 0;
		}
		return -1;
	}

	/**
	 * Return to Ith element stored in the observation instance
	 * @param Index element position
	 * @throws ArrayIndexOutOfBoundsException Index out of the range allowed
	 * @return Ith element
	 */
	public double getIndex(int Index) throws ArrayIndexOutOfBoundsException {
		/* Throws an exception if the Index value is out of range allowed */
		if (Index < 0 || Index > observation.length) {
			throw new ArrayIndexOutOfBoundsException();
			/* Return requested value */
		} else {
			return observation[Index];
		}
	}

	/**
	 * Stores the val value passed in the Index-th element of the instance
	 * @param Index element position
	 * @param val value to store
	 * @throws ArrayIndexOutOfBoundsException Index out of the range allowed
	 */
	public void setIndex(int Index, double val) throws ArrayIndexOutOfBoundsException {
		if (Index < 0 || Index > observation.length) {
			throw new ArrayIndexOutOfBoundsException();
		} else {
			observation[Index] = val;
		}
	}

	/**
	 * Returns the size ( number of elements ) of the observation
	 * @return Size of the observation
	 */
	public int getSize() {
		return observation.length;
	}

	/**
	 * Sets up element on which perform compareTo method 
	 * @param Index index of element
	 */
	public void setSorter(int Index) throws ArrayIndexOutOfBoundsException {
		if (Index >= 0 && Index < observation.length) {
			sortIndex = Index;
		} else {
			throw new ArrayIndexOutOfBoundsException();
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		String str = "[";
		for (int i = 0; i < observation.length; i++) {
			str = str + observation[i];
			if (i != observation.length - 1) {
				str = str + ",";
			}
		}
		return str + "]";
	}

	/**
	 * Sets the validity of the observation. This method is used to apply sampling operation
	 * without the loss of the original observation and to sync on these values all variables
	 * @param b validity value
	 */
	public void setValid(boolean b) {
		valid = b;
	}

	/**
	 * Getter
	 * @return validity of the observation
	 */
	public boolean isValid() {
		return valid;
	}
}
