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
 * Double.java
 *
 * Created on 16 novembre 2002, 12.21
 */

package jmt.engine.math;

/** print some finite precision numbers
 *
 * @author  Federico Granata
 */
public class Printer {

	public static String print(double value, int digits) {
		if (digits > 12) {
			new IllegalArgumentException("digits must be lover than 12");
		}
		double d = Math.pow(10, digits);
		value = Math.round(value * d) / d;
		return Double.toString(value);
	}

	public static String print(float value, int digits) {
		if (digits > 8) {
			new IllegalArgumentException("digits must be lover than 8");
		}
		float d = (new Double(Math.pow(10, digits))).floatValue();
		value = Math.round(value * d) / d;
		return Float.toString(value);
	}

}
