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

package jmt.engine.random;

/**

 * Class used for the empirical distribution, through which the user provides
 * the data needed to construct the empirical distribution: for each
 * empirical entry the requested data are a value and the probability of that value.
 *
 * @author cattai
 * @version Date: 27-nov-2003 Time: 10.27.00

 */
public class EmpiricalEntry {
	/** The <code>EmpiricalEntry</code> value. */
	protected Object value;
	/** The <code>EmpiricalEntry</code> probability. */
	protected double probability;

	/**
	 * Creates a new instance of an <code>EmpiricalEntry</code> object with
	 * specified value and probability.
	 * @param value A given <code>String</code> representing a node value.
	 * @param probability A given <code>String</code> representing
	 * the value probability.
	 */
	public EmpiricalEntry(String value, String probability) {
		this.value = value;
		this.probability = Double.parseDouble(probability);
	}

	/**
	 * Creates a new instance of an <code>EmpiricalEntry</code> object with
	 * specified value and probability.
	 * @param value A given <code>String</code> representing a node value.
	 * @param probability A given <code>double</code> probability linked to the <i>value</i>.
	 */
	public EmpiricalEntry(String value, double probability) {
		this.value = value;
		this.probability = probability;
	}

	/**
	 * Creates a new instance of an <code>EmpiricalEntry</code> object with
	 * specified value and probability.
	 * @param value A given <code>String</code> representing a node value.
	 * @param probability A given <code>Double</code> probability linked to the <i>value</i>.
	 */
	public EmpiricalEntry(String value, Double probability) {
		this.value = value;
		this.probability = probability.doubleValue();
	}

	/**
	 * Returns the probability linked to the internal value.
	 * @return A <code>double</code> representing the probability.
	 */
	public double getProbability() {
		return probability;
	}

	/**
	 * Returns the internal value.
	 * @return An object representing the internal value.
	 */
	public Object getValue() {
		return value;
	}

	/**
	 * Sets the internal probability linked to the <i>value</i>.
	 * @param probability A given <code>Double</code> probability.
	 */
	public void setProbability(double probability) {
		this.probability = probability;
	}

	/**
	 * Sets the internal probability linked to the <i>value</i>.
	 * @param probability A given <code>String</code> representing a
	 * probability.
	 */
	public void setProbability(String probability) {
		this.probability = Double.parseDouble(probability);
	}

	/**
	 * Sets the internal <i>value</i> with the given <code>Object</code>.
	 * @param value A given <code>Object</code>.
	 */
	public void setValue(Object value) {
		this.value = value;
	}
}
