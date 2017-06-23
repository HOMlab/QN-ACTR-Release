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
 * Date: 16-set-2003
 * Time: 10.36.11

 */

/**
 * A tagging interface for TableModels. A TableModel that implements this interface MUST have a getValueAt() method able to handle
 * value requests with column index=-1, as used by RowHeader. If the model also implements <code>PrototypedTableModel</code>,
 * its getPrototype() method must be header-aware, too.
 * @see jmt.gui.exact.table.RowHeader
 * @see jmt.gui.exact.table.PrototypedTableModel
 */
public interface ModelHandlesRowHeader {
}
