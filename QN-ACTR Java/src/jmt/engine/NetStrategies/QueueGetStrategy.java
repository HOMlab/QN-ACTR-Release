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

package jmt.engine.NetStrategies;

import jmt.common.AutoCheck;
import jmt.engine.QueueNet.Job;
import jmt.engine.QueueNet.JobInfoList;

/**
 * Use this class to implement a specific queue get strategy.
 * A queue get strategy is a rule which selects a job from a job
 * list, modifying the list: the implementation should remove the selected
 * job from the list.
 * @author Francesco Radaelli
 */
public interface QueueGetStrategy extends AutoCheck {

	/** This method should be overridden to implement a specific job strategy.
	 * @param Queue Jobs queue.
	 * @return Job selected and removed from the list.
	 * @throws jmt.common.exception.NetException
	 */
	public abstract Job get(JobInfoList Queue) throws jmt.common.exception.NetException;
	
	public abstract Job peek (JobInfoList Queue) throws jmt.common.exception.NetException;
}