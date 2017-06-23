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

import qnactr.objectDesigner.Enums;
import qnactr.sim.GlobalUtilities;
import jmt.engine.QueueNet.Job;
import jmt.engine.QueueNet.JobInfo;
import jmt.engine.QueueNet.JobInfoList;
import jmt.engine.QueueNet.NetEvent;
import jmt.engine.QueueNet.NetMessage;
import jmt.engine.QueueNet.NetNode;

/**
 *	This class implements a sink: every incoming job is destroyed.
 * 	@author Francesco Radaelli
 */
public class JobSink extends InputSection {

	private JobInfoList jobsList_node;

	/**
	 * Default Constructor creates the JobSink that can be used only as Input Section
	 */
	public JobSink() {
		super();

		//NEW
		//@author Stefano Omini
		//log = NetSystem.getLog();
		//end NEW

	}

	//NEW
	//@author Stefano Omini
	@Override
	protected void nodeLinked(NetNode node) {
		jobsList_node = this.getOwnerNode().getJobInfoList();

	}

	//end NEW

	/** This method implements a sink.
	 *  It eats all incoming jobs.
	 * @param message message to be processed.
	 * @throws jmt.common.exception.NetException
	 */
	@Override
	protected int process(NetMessage message) throws jmt.common.exception.NetException {
		switch (message.getEvent()) {

			case NetEvent.EVENT_START:
				//nothing to do
				return MSG_PROCESSED;

			case NetEvent.EVENT_JOB:

				//case EVENT_JOB
				//an ack message is sent to the source of the received message.
				//then the job is killed

				Job job = message.getJob();
				send(NetEvent.EVENT_ACK, job, 0.0, message.getSourceSection(), message.getSource());

				//NEW
				//@author Stefano Omini

				//BUG FIXED!

				//jobSink does not add JobInfo objects to its own list, so
				//there's no need to remove them

				//the owner node, on the contrary, adds them when the job message is received
				//before the jobinfo objects weren't removed (often causing outOfMemory errors!!!)

				if (jobsList_node == null) {
					jobsList_node = this.getOwnerNode().getJobInfoList();
				}
				JobInfo jobInfo_node = jobsList_node.lookFor(job);
				if (jobInfo_node != null) {
					//removes job from the jobInfoList of the node
					jobsList_node.removeAfterDrop(jobInfo_node);
				}
				//end NEW

				//log.write(NetLog.LEVEL_DEBUG, job, this, NetLog.JOB_DIED);

				// Signals to global jobInfoList removed job - Bertoli Marco
				this.getOwnerNode().getQueueNet().getJobInfoList().removeJob(job);

				job.qnactrEntity.updateCurrentPlace(Enums.EntityPlaceHeader.endedin, GlobalUtilities.fromGlobalNodeNameToEnumServerName(this.getOwnerNode().getName()), Enums.NodeSection.output); //QN-Java
				job.qnactrEntity.Trash = true;
				
				return MSG_PROCESSED;

			default:
				return MSG_NOT_PROCESSED;
		}
	}
}