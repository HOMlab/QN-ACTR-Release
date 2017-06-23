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

import java.util.HashMap;

import jmt.common.exception.NetException;
import jmt.engine.QueueNet.ForkJob;
import jmt.engine.QueueNet.GlobalJobInfoList;
import jmt.engine.QueueNet.Job;
import jmt.engine.QueueNet.JobInfo;
import jmt.engine.QueueNet.JobInfoList;
import jmt.engine.QueueNet.NetEvent;
import jmt.engine.QueueNet.NetMessage;

/**
 * <p>Title: Join</p>
 * <p>Description: This class is a Join input section, used to re-assemble previously
 * forked jobs. Jobs are kept until all fragments are collected. If a not-fragmented
 * job is received, it is simply routed to service section.</p>
 *
 * @author Bertoli Marco
 *         Date: 13-mar-2006
 *         Time: 16.19.58
 */
public class Join extends InputSection {
	/** Data structure used to store received fragments for each job */
	private HashMap<Job, Integer> jobs;

	// --- Constructors -----------------------------------------------------------------------------
	/**
	 * Constructs a new Join
	 */
	public Join() {
		// Disables automatic handling of jobinfolists
		super(false);
		jobs = new HashMap<Job, Integer>();
	}

	// ----------------------------------------------------------------------------------------------

	/**
	 * Assemples splitted jobs and sends an EVENT_JOIN to reference fork when done
	 *
	 * @param message message to be processed.
	 * @throws NetException if something goes wrong
	 * @return message processing result.
	 */
	@Override
	protected int process(NetMessage message) throws NetException {
		switch (message.getEvent()) {
			case NetEvent.EVENT_JOB:
				Job job = message.getJob();
				// Sends ACK back
				send(NetEvent.EVENT_ACK, job, 0.0, message.getSourceSection(), message.getSource());

				if (job instanceof ForkJob) {
					ForkJob fJob = (ForkJob) job;

					// Removes job from global node list
					JobInfoList info = getOwnerNode().getJobInfoList();
					JobInfo jobData = info.lookFor(job);
					if (jobData != null) {
						info.remove(jobData);
					}

					// Removes job from system list
					GlobalJobInfoList global = getOwnerNode().getQueueNet().getJobInfoList();
					global.removeForkedJob(fJob);

					// Needed pieces
					int needed;
					if (jobs.containsKey(fJob.getForkedJob())) {
						needed = jobs.get(fJob.getForkedJob()).intValue();
					} else {
						needed = fJob.getForkedNumber();
						// As we are waiting for other fragments, adds merged job to global and local info list
						JobInfo merged = new JobInfo(fJob.getForkedJob());
						info.add(merged);
						jobsList.add(merged);
					}
					// Decrement needed as we received this job
					needed--;

					// If needed is zero, all pieces has been retrived and job can be
					// fowarded
					if (needed == 0) {
						jobs.remove(fJob.getForkedJob());
						// Adds original job, otherwise we will have one less job in the network
						global.addForkedJob(fJob.getForkedJob());
						// Sends job forward
						sendForward(fJob.getForkedJob(), 0.0);
						// Notify fork node (to support blocking)
						send(NetEvent.EVENT_JOIN, fJob.getForkedJob(), 0.0, fJob.getReferenceFork().getSectionID(), fJob.getReferenceFork()
								.getOwnerNode());
					} else {
						// We must wait for more fragments before sending this to
						// next section
						jobs.put(fJob.getForkedJob(), new Integer(needed));
					}
				} else {
					// If this is not a fork job, sends it forward
					sendForward(job, 0.0);
				}

				break;
			case NetEvent.EVENT_ACK:
				break;
		}

		// Everything was okay
		return MSG_PROCESSED;
	}

}
