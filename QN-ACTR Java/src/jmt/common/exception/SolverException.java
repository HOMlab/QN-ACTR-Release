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
 * @author alyf (Andrea Conti)
 * Date: 19-set-2003
 * Time: 10.16.48

 */
public class SolverException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public SolverException() {
	}

	public SolverException(Throwable cause) {
		super(cause);
	}

	public SolverException(String message) {
		super(message);
	}

	public SolverException(String message, Throwable cause) {
		super(message, cause);
	}
}
