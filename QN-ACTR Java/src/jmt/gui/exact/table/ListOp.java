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

package jmt.gui.exact.table;

/**

 * @author alyf (Andrea Conti)
 * Date: 17-set-2003
 * Time: 10.55.18

 */

/**
 * A class to keep track of row deletions/insertions in lists
 */
public class ListOp {

	private static final int RESIZE_OP = 0;
	private static final int DELETE_OP = 1;

	private static final String[] NAMES = { "resize", "delete" };

	private int type;
	private int data;

	private ListOp(int type, int data) {
		this.type = type;
		this.data = data;
	}

	public static ListOp createResizeOp(int newSize) {
		return new ListOp(RESIZE_OP, newSize);
	}

	public static ListOp createDeleteOp(int index) {
		return new ListOp(DELETE_OP, index);
	}

	public boolean isResizeOp() {
		return (type == RESIZE_OP);
	}

	public boolean isDeleteOp() {
		return (type == DELETE_OP);
	}

	public int getData() {
		return data;
	}

	@Override
	public String toString() {
		return "ListOp: " + NAMES[type] + "(" + data + ")";
	}

}
