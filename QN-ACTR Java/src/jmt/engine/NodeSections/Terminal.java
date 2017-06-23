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

import java.util.ListIterator;

import javax.swing.JOptionPane;

import jmt.common.exception.NetException;
import jmt.engine.QueueNet.Job;
import jmt.engine.QueueNet.JobClass;
import jmt.engine.QueueNet.JobInfoList;
import jmt.engine.QueueNet.LinkedJobInfoList;
import jmt.engine.QueueNet.NetEvent;
import jmt.engine.QueueNet.NetMessage;
import jmt.engine.QueueNet.NetNode;
import jmt.engine.QueueNet.WaitingRequest;
import jmt.engine.random.engine.RandomEngine;
import jmt.engine.simEngine.SimSystem;

/**
 * This class implements a terminal: each class has a finite number of customers in
 * the system.
 * @author  Stefano Omini
 */
public class Terminal extends InputSection {

	public static int PARAMETERS_EXCEPTION = 0x0001;

	private boolean coolStart;//when is true the waitingjobs queue is void

	private JobInfoList waitingRequests;

	private int jobsPerClass[]; //number of jobs per class

	/**
	 * Creates a terminal
	 * @param jobsPerClass number of jobs for each class
	 */
	public Terminal(int[] jobsPerClass) {
		super();
		coolStart = true;
		this.jobsPerClass = new int[jobsPerClass.length];
		for (int c = 0; c < this.jobsPerClass.length; c++) {
			this.jobsPerClass[c] = jobsPerClass[c];
		}

		//NEW
		//@author Stefano Omini
		//log = NetSystem.getLog();
		//end NEW
	}

	/**
	 * Creates a terminal
	 * @param jobsPerClass number of jobs for each class
	 */
	public Terminal(Integer[] jobsPerClass) {
		super();
		coolStart = true;
		this.jobsPerClass = new int[jobsPerClass.length];
		for (int c = 0; c < this.jobsPerClass.length; c++) {
			this.jobsPerClass[c] = jobsPerClass[c].intValue();
		}

		//NEW
		//@author Stefano Omini
		//log = NetSystem.getLog();
		//end NEW
	}

	@Override
	protected void nodeLinked(NetNode node) {
		// Sets netnode dependent properties
		waitingRequests = new LinkedJobInfoList(getJobClasses().size(), true);
		//TODO: togliere??
		//if (jobsList == null)
		//	jobsList = new JobInfoList(getJobClasses().size(), true);
	}

	@Override
	protected int process(NetMessage message) throws NetException {
		Job job;
		int c;
		
		//String showString = "clock: " + SimSystem.clock() + ". Terminal has Event type: " + message.getEvent(); //CAO
    //JOptionPane.showMessageDialog(null, showString, "Terminal.java process", JOptionPane.INFORMATION_MESSAGE); //CAO 
    
		
		switch (message.getEvent()) {

			case NetEvent.EVENT_START:

				//case EVENT_START:
				//the terminal creates all the jobs requested by each class.
				//for each job created, it sends to itself a message with delay 0

				//log.write(NetLog.LEVEL_RELEASE, null, this, NetLog.EVENT_START);
				ListIterator<JobClass> jobClasses = getJobClasses().listIterator();
				JobClass jobClass;

				//generator of random numbers (uses the same engine of
				//distributions and strategies) used to mix the order of
				//leaving jobs, otherwise they leave in order of class
				//(i.e. c1, c1, c1, ...c2, c2, c2, ... c3....)
				RandomEngine randomEng = RandomEngine.makeDefault();

				//delay used to mix leaving order
				double mixRandomDelay = 0.0;

				while (jobClasses.hasNext()) {
					jobClass = jobClasses.next();

					//NEW
					//@author Stefano Omini
					if (jobClass.getType() == JobClass.OPEN_CLASS) {
						//open class: no jobs to be generated
						continue;
					}
					//end NEW

					c = jobClass.getId();

					if (jobsPerClass != null) {

						//terminal of a closed system
						//generates all the jobs
						for (int i = 0; i < jobsPerClass[c]; i++) {
							//note that if jobsPerClass[c] = -1 (open class) the instructions
							//of this for are not performed

							//each job is created and sent to the terminal itself
							job = new Job(jobClass);

							//OLD
							// the delay of departure is set to 0
							//sendMe(job, 0);

							//NEW
							//@author Stefano Omini
							//sets a random (very small) delay to mix the jobs
							//of different classes

							//mixRandomDelay = (randomEng.nextDouble()) * 0.00001;
							mixRandomDelay = (randomEng.raw()) * 0.00001;

							sendMe(job, mixRandomDelay);
							//end NEW

							//log.write(NetLog.LEVEL_DEBUG, job, this, NetLog.JOB_CREATED);
						}
					}
				}
				break;

			case NetEvent.EVENT_JOB:

				//case EVENT_JOB
				//if coolStart=false adds the job to the list of waiting jobs.
				//
				//if coolStart=true (no waiting jobs) checks the source
				//If the message has been received from the terminal itself, the job's
				//bornTime is set, then the job is forwarded. Otherwise, if it has been
				//received from the outside, an ack message is sent to the source of the
				//message, the job's bornTime is set, then the job is forwarded.
				//

				//log.write(NetLog.LEVEL_DEBUG, message.getJob(), this, NetLog.JOB_IN);

				// Gets the job from the message
				job = message.getJob();

				//TODO: serve questa coolStart????
				if (coolStart) {
					//the queue of waiting jobs is empty

					if (message.getSource() == this.getOwnerNode() && message.getSourceSection() == this.getSectionID()) {

						//message sent by the terminal itself
						job.born();
						sendForward(job, 0.0);

						//log.write(NetLog.LEVEL_DEBUG, job, this, NetLog.JOB_OUT);

						coolStart = false;

						return MSG_PROCESSED;
					} else {

						//job received from the outside

						//send an ack
						send(NetEvent.EVENT_ACK, job, 0.0, message.getSourceSection(), message.getSource());

						//job goes on
						job.born();
						sendForward(job, 0.0);

						//log.write(NetLog.LEVEL_DEBUG, job, this, NetLog.JOB_OUT);

						coolStart = false;

						return MSG_PROCESSED;

					}

				} else {
					//coolStart is false: there are waiting jobs. Add the received job.
					waitingRequests.add(new WaitingRequest(message.getSource(), message.getSourceSection(), job));
				}
				break;

			case NetEvent.EVENT_ACK:

				//case EVENT_ACK:
				//if there are waiting jobs, takes the first, set its bornTime and
				//forwards it to the service section.
				//then it creates a new job and sends to itself a message whose delay is the time of
				//departure of that job.
				//otherwise, if there are no waiting jobs, sets coolstart=true

				if (waitingRequests.size() != 0) {
					WaitingRequest wr;
					wr = (WaitingRequest) waitingRequests.removeFirst();

					if (!isMyOwnerNode(wr.getNode())) {
						send(NetEvent.EVENT_ACK, wr.getJob(), 0.0, wr.getSection(), wr.getNode());

						//log.write(NetLog.LEVEL_ALL, message.getJob(), this, NetLog.ACK_JOB);
					}

					Job jobSent = wr.getJob();
					sendForward(jobSent, 0.0);

					//log.write(NetLog.LEVEL_DEBUG, jobSent, this, NetLog.JOB_OUT);
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
