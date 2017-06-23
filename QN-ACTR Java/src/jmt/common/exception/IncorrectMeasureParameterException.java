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

package jmt.common.exception;

/**
 *
 * @author Max
 * Date: 9-ott-2003
 * Time: 10.45.56

 */
public class IncorrectMeasureParameterException extends Exception {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Creates a new instance of <code>IncorrectMeasureParameterException</code> without detail message.
	 */
	public IncorrectMeasureParameterException() {
	}

	/**
	 * Creates a new instance of <code>IncorrectMeasureParameterException</code> with the specified detail message.
	 * @param message The detail message.
	 */
	public IncorrectMeasureParameterException(String message) {
		super(message);
	}
}
