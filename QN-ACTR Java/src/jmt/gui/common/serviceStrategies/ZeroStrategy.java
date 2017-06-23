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
 * <p><b>Name:</b> ZeroStrategy</p> 
 * <p><b>Description: This is a special service strategy used when service time must be zero.</b> 
 * 
 * </p>
 * <p><b>Date:</b> 13-lug-2006
 * <b>Time:</b> 9.53.40</p>
 * @author Bertoli Marco
 * @version 1.0
 */
public class ZeroStrategy implements ServiceStrategy {

	/**
	 * Return engine classpacth for Zero Service strategy
	 * @return
	 */
	public static String getEngineClassPath() {
		return "jmt.engine.NetStrategies.ServiceStrategies.ZeroServiceTimeStrategy";
	}

	/* 
	 * Clones this strategy. In this case it simply returns a new ZeroStrategy
	 * @see java.lang.Object#clone()
	 */
	@Override
	public ZeroStrategy clone() {
		return new ZeroStrategy();
	}

	/* 
	 * Returns the value of this strategy
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "0";
	}
}
