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

package jmt.engine.NodeSections;

import java.util.LinkedList;

import jmt.engine.NetStrategies.ServiceStrategy;
import jmt.engine.QueueNet.Job;
import jmt.engine.QueueNet.NetEvent;
import jmt.engine.QueueNet.NetMessage;

/**
 * This class implements a multi-class delay service. Every class has a
 * specific distribution and a own set of statistical parameters. The delay
 * service processes a job without being busy.
 * @author Francesco Radaelli
 */
public class Delay extends ServiceSection {

	private LinkedList<Job> waitingJobs;

	private boolean coolStart;

	private ServiceStrategy serviceStrategy[];

	/** Creates a new instance of Delay. Use this constructor to create a delay
	 * service for a open/closed network.
	 * @param serviceStrategy Array of service strategies, one per class.
	 * @throws jmt.common.exception.NetException
	 */
	public Delay(ServiceStrategy serviceStrategy[]) throws jmt.common.exception.NetException {
		this.serviceStrategy = serviceStrategy;
		waitingJobs = new LinkedList<Job>();
		coolStart = true;
	}

	@Override
	protected int process(NetMessage message) throws jmt.common.exception.NetException {
		Job job;
		switch (message.getEvent()) {

			case NetEvent.EVENT_JOB:

				//EVENT_JOB
				//If the message has been sent by this section, it means that the job
				//has been already delayed. Therefore, if there are no waiting jobs (coolStart true)
				//the job is forwarded and coolStart becomes false, otherwise the job is added to
				//the existing waiting jobs.
				//
				//If the message has been sent by the input section, delay section sends to
				//itself a message with the job and with a delay calculated using the
				//service strategy; then an ack is sent to the message source.

				double serviceTime;
				int c;
				// Gets the job from the message
				job = message.getJob();
				// If the message source is this section, the job has been
				// delayed and it should be forwarded to the next section
				if (isMine(message)) {
					if (coolStart) {
						// Sends job
						sendForward(job, 0.0);
						coolStart = false;
					} else {
						waitingJobs.add(job);
					}
				}
				// else delays the job
				else {
					// Gets the class of the job
					c = job.getJobClass().getId();
					// Calculates the service time of job
					serviceTime = serviceStrategy[c].wait(this);
					// Sends to itself the job with delay equal to "serviceTime"
					sendMe(job, serviceTime);
					// Sends backward the job ack
					sendBackward(NetEvent.EVENT_ACK, job, 0.0);
				}
				break;

			case NetEvent.EVENT_ACK:

				//EVENT_ACK
				//If there are waiting jobs, the first is get and forwarded,
				//otherwise coolStart is set to true.

				if (waitingJobs.size() != 0) {
					job = waitingJobs.removeFirst();
					sendForward(job, 0.0);
				} else {
					coolStart = true;
				}
				break;

			default:
				return MSG_NOT_PROCESSED;
		}
		return MSG_PROCESSED;
	}
}
