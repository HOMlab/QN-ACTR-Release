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
 * This class implements a generic QueueNet exception.
 * @author Francesco Radaelli
 */
public class NetException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private Object ThrownBy;

	private int Type;

	private NetException HangingException;

	/** Constructs a new exception with the specified detail message.
	 *  @param ThrownBy	The object which throws the exception.
	 *  @param Type		Type of the exception (internal object type).
	 *  @param Message  The detail message.
	 */
	public NetException(Object ThrownBy, int Type, String Message) {
		super(Message);
		this.ThrownBy = ThrownBy;
		this.Type = Type;
	}

	/** Constructs a new exception with the specified detail message.
	 *  @param ThrownBy	The object which throws the exception.
	 *  @param Type		Type of the exception (internal object type).
	 *  @param Message  The detail message.
	 *  @param HangingException Hanging exception.
	 */
	public NetException(Object ThrownBy, int Type, String Message, NetException HangingException) {
		super(Message);
		this.ThrownBy = ThrownBy;
		this.Type = Type;
		this.HangingException = HangingException;
	}

	public NetException(String s) {
		super(s);
	}

	/** Gets the object which throws exception.
	 *	@return The object which throws this exception.
	 */
	public Object thrownBy() {
		return ThrownBy;
	}

	/** Gets the internal type of exception.
	 *	@return Type of the exception.
	 */
	public int getType() {
		return Type;
	}
}