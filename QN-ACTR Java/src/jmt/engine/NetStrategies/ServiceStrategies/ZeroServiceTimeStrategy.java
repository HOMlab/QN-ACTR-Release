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

package jmt.engine.NetStrategies.ServiceStrategies;

import jmt.engine.NetStrategies.ServiceStrategy;
import jmt.engine.QueueNet.NodeSection;

/**
 * This service strategy always returns a service time equal to 0.0.
 *
 * @author Stefano Omini
 */

public class ZeroServiceTimeStrategy extends ServiceStrategy {

	/** Creates a new instance of ServiceTime.*/
	public ZeroServiceTimeStrategy() {
	}

	/**
	 * The returned service time is always equal to 0.0.
	 * @param CallingSection The node section which is calling this method.
	 * @return the value of service time, which is always 0.0.
	 */
	@Override
	public double wait(NodeSection CallingSection) {
		return 0;
	}
}
