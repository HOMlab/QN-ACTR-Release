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
 * IncorrectDistributionParameterException.java
 *
 * Created on 10 ottobre 2002, 17.22
 */

package jmt.common.exception;

/**
 *
 * @author  Lorenzo Muttoni
 */
public class IncorrectDistributionParameterException extends java.lang.Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Creates a new instance of <code>IncorrectDistributionParameterException</code> without detail message.
	 */
	public IncorrectDistributionParameterException() {
	}

	/**
	 * Constructs an instance of <code>IncorrectDistributionParameterException</code> with the specified detail message.
	 * @param msg the detail message.
	 */
	public IncorrectDistributionParameterException(String msg) {
		super(msg);
	}
}
