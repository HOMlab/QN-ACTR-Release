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

package jmt.engine.NetStrategies;

import jmt.common.AutoCheck;
import jmt.engine.QueueNet.NodeSection;

/**
 * Use this class to implement a specific server strategy. A server
 * strategy is a rule which calculates the service time for a job.
 * @author Francesco Radaelli
 */
public abstract class ServiceStrategy implements AutoCheck {

	/** This method should be overridden to implement a specific strategy.
	 * @param CallingSection The section which calls this strategy.
	 * @return Service time.
	 */
	public abstract double wait(NodeSection CallingSection) throws jmt.common.exception.NetException;

	public boolean check() {
		return true;
	}
}
