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

package jmt.engine.NetStrategies.QueuePutStrategies;

import jmt.common.exception.NetException;
import jmt.engine.NetStrategies.QueuePutStrategy;
import jmt.engine.QueueNet.Job;
import jmt.engine.QueueNet.JobInfo;
import jmt.engine.QueueNet.JobInfoList;
import jmt.engine.QueueNet.NetNode;
import jmt.engine.QueueNet.NodeSection;

/**
 * This class implements a specific queue put strategy: all arriving jobs
 * are put at the end of the queue.
 * @author Francesco Radaelli
 */
public class TailStrategy implements QueuePutStrategy {

	/**
	 * The arriving job are inserted in last position
	 * @param job Job to be added to the queue.
	 * @param queue Queue.
	 * @param sourceSection Job source section.
	 * @param sourceNode Job source node.
	 * @param callingSection The section which calls this strategy.
	 */
	public void put(Job job, JobInfoList queue, byte sourceSection, NetNode sourceNode, NodeSection callingSection) throws NetException {
		queue.addLast(new JobInfo(job));
		return;
	}

	/* (non-Javadoc)
	 * @see jmt.common.AutoCheck#check()
	 */
	public boolean check() {
		return true;
	}
}