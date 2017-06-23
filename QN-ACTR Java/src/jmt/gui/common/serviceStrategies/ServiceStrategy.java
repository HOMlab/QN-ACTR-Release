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

package jmt.gui.common.serviceStrategies;

/**
 * <p>Title: Service Strategy Interface</p>
 * <p>Description: Provides methods used both on load dependent service strategy and service
 * time strategy</p>
 * 
 * @author Bertoli Marco
 *         Date: 18-ott-2005
 *         Time: 10.27.11
 */
public interface ServiceStrategy extends Cloneable {
	/**
	 * Overrides Cloneable 'clone' method, providing absence of exceptions
	 * @return a clone of current service strategy
	 */
	public ServiceStrategy clone();
}
