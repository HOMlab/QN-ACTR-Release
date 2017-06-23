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

/**
 * 2013. Modified for QN-Java project
 * 
 * [2013-07-09] peek()
 * 
 */

package jmt.engine.NetStrategies.QueueGetStrategies;

import jmt.engine.NetStrategies.QueueGetStrategy;
import jmt.engine.QueueNet.Job;
import jmt.engine.QueueNet.JobInfoList;
import jmt.engine.QueueNet.LinkedJobInfoList;
import jmt.engine.QueueNet.PSJobInfoList;

/**
 *	This class implements a LCFS strategy (Last Come First Served).
 * 	@author Francesco Radaelli
 */
public class LCFSstrategy implements QueueGetStrategy {

	/** Implements the LCFS strategy.
	 * @param Queue Jobs queue.
	 * @return Last job in the list.
	 */
	public Job get(JobInfoList Queue) throws jmt.common.exception.NetException {
		return Queue.removeLast().getJob();
	}

  public Job peek(JobInfoList Queue) throws jmt.common.exception.NetException {
    return ((LinkedJobInfoList)Queue).getInternalList().getLast().getJob();        
  }
	
	/* (non-Javadoc)
	 * @see jmt.common.AutoCheck#check()
	 */
	public boolean check() {
		return true;
	}
}