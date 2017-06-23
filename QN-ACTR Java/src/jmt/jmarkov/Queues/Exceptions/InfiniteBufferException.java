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
 * Created on 29-mar-2004 by Ernesto
 *
 */
package jmt.jmarkov.Queues.Exceptions;

/**
 * MMQueues
 * --------------------------------------
 * 29-mar-2004 - Queues.Exceptions/InfiniteBufferException.java
 * 
 * @author Ernesto
 */
public class InfiniteBufferException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * 
	 */
	public InfiniteBufferException() {
		super();
	}

	/**
	 * @param message
	 */
	public InfiniteBufferException(String message) {
		super(message);
	}

	/**
	 * @param cause
	 */
	public InfiniteBufferException(Throwable cause) {
		super(cause);
	}

	/**
	 * @param message
	 * @param cause
	 */
	public InfiniteBufferException(String message, Throwable cause) {
		super(message, cause);
	}

}
