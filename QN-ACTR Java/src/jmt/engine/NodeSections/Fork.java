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

import java.util.Iterator;

import jmt.common.exception.NetException;
import jmt.engine.QueueNet.ForkJob;
import jmt.engine.QueueNet.GlobalJobInfoList;
import jmt.engine.QueueNet.Job;
import jmt.engine.QueueNet.JobInfo;
import jmt.engine.QueueNet.JobInfoList;
import jmt.engine.QueueNet.NetEvent;
import jmt.engine.QueueNet.NetMessage;
import jmt.engine.QueueNet.NetNode;
import jmt.engine.QueueNet.NodeList;

/**
 * <p>Title: Fork</p>
 * <p>Description: This class is a fork output section, used to split a job on every
 * output link. Splitted job can be assembled back in a join input section.
 * A maximum number of jobs inside a fork-join section can be specified: when reached
 * this component will block until at least one job is joined.</p>
 *
 * @author Bertoli Marco
 *         Date: 13-mar-2006
 *         Time: 15.23.22
 */
public class Fork extends OutputSection {
	/** Maximum number of jobs allowed in a fork-join region (-1 or 0 is infinity) */
	private int block;

	/** Counts received ACK by following nodes */
	private int ackCount;

	/** Number of jobs to be routed on each link */
	private int jobsPerLink;

	/** Number of output nodes */
	private int numOut;

	/** Current number of jobs inside a fork-join region */
	private int jobNum;

	// --- Constructors -----------------------------------------------------------------------------
	/**
	 * Constructs a new Fork without blocking and with 1 job per link
	 */
	public Fork() {
		this(1, -1);
	}

	/**
	 * Construst a new Fork node
	 * @param jobsPerLink number of jobs to be routed on each link
	 * @param block maximum number of jobs allowed in a fork-join
	 * region (-1 or 0 is infinity)
	 */
	public Fork(Integer jobsPerLink, Integer block) {
		this(jobsPerLink.intValue(), block.intValue());
	}

	/**
	 * Construst a new Fork node
	 * @param jobsPerLink number of jobs to be routed on each link
	 * @param block maximum number of jobs allowed in a fork-join
	 * region (-1 or 0 is infinity)
	 */
	public Fork(int jobsPerLink, int block) {
		// Disables automatic handling of jobinfolists
		super(false);
		jobNum = 0;
		this.block = block;
		this.jobsPerLink = jobsPerLink;
	}

	// ----------------------------------------------------------------------------------------------

	/**
	 * Splits input job on every output link and waits if 'block' job number are
	 * not joined if and only if block is enabled.
	 *
	 * @param message message to be processed.
	 * @throws NetException if something goes wrong
	 * @return message processing result.
	 */
	@Override
	protected int process(NetMessage message) throws NetException {
		Job job;
		job = message.getJob();
		// Finds event type
		switch (message.getEvent()) {
			case NetEvent.EVENT_JOB:
				NodeList output = getOwnerNode().getOutputNodes();
				// Finds number of output jobs
				numOut = output.size() * jobsPerLink;
				// Resets ack count
				ackCount = 0;

				// Removes job from global node list
				JobInfoList info = getOwnerNode().getJobInfoList();
				JobInfo jobData = info.lookFor(job);
				if (jobData != null) {
					info.remove(jobData);
				}

				// Gets global jobInfoList
				GlobalJobInfoList global = getOwnerNode().getQueueNet().getJobInfoList();

				// Sends "jobsPerLink" jobs on each output link
				Iterator<NetNode> i = output.listIterator();
				while (i.hasNext()) {
					NetNode outNode = i.next();
					for (int n = 0; n < jobsPerLink; n++) {
						ForkJob newJob = new ForkJob(numOut, job, this);
						// Sends new job to all following stations
						send(newJob, 0.0, outNode);
						// Adds job to system jobinfolist
						global.addForkedJob(newJob);
					}
				}
				// Increment job counter
				jobNum++;

				// Removes original job, otherwise we will have one extra job in the network
				global.removeForkedJob(job);
				break;

			case NetEvent.EVENT_ACK:
				ackCount++;
				// If this fork doesn't block, sends ACK back when all messages are delivered
				// to unlock this fork
				if ((jobNum < block || block <= 0) && ackCount == numOut) {
					sendBackward(NetEvent.EVENT_ACK, job, 0.0);
				}
				break;
			case NetEvent.EVENT_JOIN:
				// If this fork blocks, finally sends ACK back and unlocks this
				if (jobNum == block) {
					sendBackward(NetEvent.EVENT_ACK, job, 0.0);
				}

				// Decrement job counter
				jobNum--;
				break;
		}
		// Everything was okay
		return MSG_PROCESSED;
	}
}
