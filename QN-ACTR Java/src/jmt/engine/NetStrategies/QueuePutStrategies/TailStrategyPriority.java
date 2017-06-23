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

import java.util.List;
import java.util.ListIterator;

import jmt.common.exception.NetException;
import jmt.engine.NetStrategies.QueuePutStrategy;
import jmt.engine.QueueNet.Job;
import jmt.engine.QueueNet.JobInfo;
import jmt.engine.QueueNet.JobInfoList;
import jmt.engine.QueueNet.NetNode;
import jmt.engine.QueueNet.NodeSection;

/**
 * This class implements a specific queue put strategy: all arriving jobs
 * are ordered according JobClass priorities. Between jobs of the same
 * priority level, the last come is put at the end.
 * @author Stefano Omini
 */
public class TailStrategyPriority implements QueuePutStrategy {

	/**
	 * This method should be overridden to implement a specific job strategy.
	 * @param job job to be added to the queue.
	 * @param queue queue.
	 * @param sourceSection job source section.
	 * @param sourceNode job source node.
	 * @param callingSection The section which calls this strategy.
	 */
	public void put(Job job, JobInfoList queue, byte sourceSection, NetNode sourceNode, NodeSection callingSection) throws NetException {

		//priority of this job
		int priority = job.getJobClass().getPriority();

		//list of jobs in queue
		List<JobInfo> list = queue.getJobList();

		if (list.size() == 0) {
			//empty list: add first
			queue.addFirst(new JobInfo(job), true);
			return;
		}

		//else creates an iterator and find the correct position
		//according to the job priority

		ListIterator<JobInfo> iterator = list.listIterator();
		JobInfo current = null;
		int currentJobPriority = 0;
		int index = -1;

		//iterator starts from the first (i.e. the job with highest priority)
		while (iterator.hasNext()) {
			index++;
			current = iterator.next();
			currentJobPriority = current.getJob().getJobClass().getPriority();

			if (priority > currentJobPriority) {
				//the job to be added must be inserted before the current job
				//(because has greater priority)

				//index is the position of the current element, which will be shifted together
				//with the following ones
				queue.add(index, new JobInfo(job), true);
				return;
			}
			//else if priority is lower than current, continue iteration
		}

		//exiting from the "while" means that the job to be inserted is the job with
		//lowest priority or has equal priority to the current last job but it is the last come

		//add last
		queue.addLast(new JobInfo(job), true);
		return;

	}

	/* (non-Javadoc)
	 * @see jmt.common.AutoCheck#check()
	 */
	public boolean check() {
		return true;
	}
}