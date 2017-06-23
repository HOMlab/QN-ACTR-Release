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
 * [2013-07-10] add "other tasks dummy"
 * 
 * [2013-06-23] This is new customer generator. By default, it generates a lot customers. QN-Java's 
 *              Sever "first trigger entity" needs just one. see QN-Java marks.
 * 
 */

package jmt.engine.NodeSections;

import java.util.LinkedList;
import java.util.ListIterator;

import javax.swing.JOptionPane;

import qnactr.objectDesigner.Enums;
import qnactr.sim.GlobalUtilities;
import qnactr.sim.QnactrSimulation;

import jmt.engine.NetStrategies.ServiceStrategy;
import jmt.engine.QueueNet.Job;
import jmt.engine.QueueNet.JobClass;
import jmt.engine.QueueNet.NetEvent;
import jmt.engine.QueueNet.NetMessage;
import jmt.engine.random.Parameter;
import jmt.engine.simEngine.SimSystem;

/**
 * This class implements a job source (generator): a job is created every
 * <i>t</i> simulation time units using a statisitcal distribution according
 * to its class.
 * @author Francesco Radaelli, Federico Granata
 *
 */
public class RandomSource extends InputSection {

  int totalJobGenerated = 0; // QN-Java
  
	public static int PARAMETERS_EXCEPTION = 0x0001;

	/** Service distribution: one for each job class. */
	//	protected Distribution distributions[];
	/** Distribution parameters: one for each job class. */
	protected Parameter parameters[];

	private boolean coolStart;//when is true the waitingjobs queue is void

	private LinkedList<Job> waitingJobs; //TODO: se riusciamo a convertire in job info list �meglio

	private ServiceStrategy[] strategy;

	/**
	 * Creates a new instance of inputSection.
	 * strategy[i] = null, if the i-th class is closed.
	 */
	public RandomSource(ServiceStrategy[] strategy) {
		super();
		this.strategy = strategy;
		waitingJobs = new LinkedList<Job>();
		coolStart = true;

		//NEW
		//@author Stefano Omini
		//log = NetSystem.getLog();
		//end NEW
	}

	@Override
	protected int process(NetMessage message) throws jmt.common.exception.NetException {
		Job job;
		double delay;
		int c;
		
		//String showString = "clock: " + SimSystem.clock() + ". Source has Event type: " + message.getEvent() + ". Name: " + this.getOwnerNode().getName(); //QN-Java
    //JOptionPane.showMessageDialog(null, showString, "RandomSource.java process", JOptionPane.INFORMATION_MESSAGE); //QN-Java 
    
		
		switch (message.getEvent()) {

			case NetEvent.EVENT_START:

				//case EVENT_START:
				//the random source creates all the jobs requested by each class.
				//for each job created, it sends to itself a message whose delay is the time of
				//departure of the job, calculated using the strategy of the corresponding class

				//log.write(NetLog.LEVEL_RELEASE, null, this, NetLog.EVENT_START);

				ListIterator<JobClass> jobClasses = getJobClasses().listIterator();
				JobClass jobClass;

				while (jobClasses.hasNext()) {
					jobClass = jobClasses.next();

					//NEW
					//@author Stefano Omini
					if (jobClass.getType() == JobClass.CLOSED_CLASS) {
						//closed class: no arrivals
						continue;
					}
					//end NEW

					c = jobClass.getId();

					// Calculates the delay of departure (1/lambda)
					if (strategy[c] != null) {
						job = new Job(jobClass);
						delay = strategy[c].wait(this);
						
						if (this.getOwnerNode().getName().equals("first trigger entity_0") || this.getOwnerNode().getName().equals("other tasks dummy_0")) {
						  job.qnactrEntity.hmiID = "0";
						  
						  
						  //job.qnactrEntity.Tag = QnactrSimulation.entityNumber; 
					    //QnactrSimulation.entityNumber++; //moved to new Entity()
						  
						  delay = 0.0; // QN-Java
						  
						  String serverLocalName = GlobalUtilities.stringLowNoSpace(GlobalUtilities.getServerOperatorNamesFromRawName(getOwnerNode().getName())[0]);
						  
						  job.qnactrEntity.updateCurrentPlace(Enums.EntityPlaceHeader.goingto, Enums.ServerName.valueOf(serverLocalName), Enums.NodeSection.server);
						}
						
						sendMe(job, delay);
						
						//log.write(NetLog.LEVEL_DEBUG, job, this, NetLog.JOB_CREATED);
					}

				}
				break;

			case NetEvent.EVENT_ACK:

				//case EVENT_ACK:
				//if there are waiting jobs, takes the first, set its bornTime and
				//forwards it to the service section.
				//then it creates a new job and sends to itself a message whose delay is the time of
				//departure of that job.
				//otherwise, if there are no waiting jobs, sets coolstart=true

				if (waitingJobs.size() != 0) {
					job = waitingJobs.removeFirst();
					c = job.getJobClass().getId();
					job.born();

					//NEW
					//@author Stefano Omini, Bertoli Marco

					// in RandomSource the job is created (--> SystemEnteringTime is initialized)
					// but then is delayed as long as the random interarrival time ("delay")
					//
					// to compute system response time, the job starting time must be
					// reset (otherwise it will correspond to the creation time and not to the
					// leaving time, which is "delay" seconds after)

					// Signals to global jobInfoList new added job
					this.getOwnerNode().getQueueNet().getJobInfoList().addJob(job);

					//end NEW

					sendForward(job, 0.0);

					//log.write(NetLog.LEVEL_DEBUG, job, this, NetLog.JOB_OUT);

					//QN-Java, do not need another job created from the source node.
					/*
					// Create a new job and send it to me delayed
					job = new Job(getJobClasses().get(c));
					delay = strategy[c].wait(this);
					sendMe(job, delay);
					*/
					
					//log.write(NetLog.LEVEL_DEBUG, job, this, NetLog.JOB_CREATED);

				} else {
					coolStart = true;
				}
				break;

			case NetEvent.EVENT_JOB:
				
			  int maxJobNeeded = 1; // QN-Java
			  if(totalJobGenerated < maxJobNeeded){ // QN-Java, control the total number of jobs/entities generated by the source node.
		       
			    
			    			  
  			  //case EVENT_JOB
  				//if coolStart=false adds the job to the list of waiting jobs.
  				//
  				//if coolStart=true (no waiting jobs)  the job is forwarded, an ack message
  				//is sent to the source of the message and a new job is created (the random source
  				//sends to itself a message, whose delay is the time of departure of the new job).
  
  				//log.write(NetLog.LEVEL_DEBUG, message.getJob(), this, NetLog.JOB_IN);
  
  				// Gets the job from the message
  				job = message.getJob();
  				
  				if(this.getOwnerNode().getName().equals("first trigger entity_0") || this.getOwnerNode().getName().equals("other tasks dummy_0")) {
            totalJobGenerated ++; // QN-Java
            job.qnactrEntity.updateCurrentPlace(Enums.EntityPlaceHeader.in, null , null);
          }
  				
  				if (coolStart) {
  					// Gets the class of the job
  					c = job.getJobClass().getId();
  
  					//no control is made on the number of jobs created
  					//it's an open class
  					job.born();
  
  					//NEW
  					//@author Stefano Omini, Bertoli Marco
  
  					// in RandomSource the job is created (--> SystemEnteringTime is initialized)
  					// but then is delayed as long as the random interarrival time ("delay")
  					//
  					// to compute system response time, the job starting time must be
  					// reset (otherwise it will correspond to the creation time and not to the
  					// leaving time, which is "delay" seconds after)
  
  					// Signals to global jobInfoList new added job
  					this.getOwnerNode().getQueueNet().getJobInfoList().addJob(job);
  
  					//end NEW
  
  					sendForward(job, 0.0);
  					send(NetEvent.EVENT_ACK, job, 0.0, message.getSourceSection(), message.getSource());
  
  					//log.write(NetLog.LEVEL_DEBUG, job, this, NetLog.JOB_OUT);
  
  					//QN-Java do not need another entity from the source node.
  					/*
  					job = new Job(job.getJobClass());
  					delay = strategy[c].wait(this);
  					sendMe(job, delay);
  					 */
  					
  					// Sets coolStart to false, next job should wait ack
  					coolStart = false;
  
  					//log.write(NetLog.LEVEL_DEBUG, job, this, NetLog.JOB_CREATED);
  					
  					
  					return MSG_PROCESSED;
  
  				} else {
  					//coolStart is false: there are waiting jobs. Add the received job.
  					waitingJobs.add(job);
  				}
  				
			  } // QN-Java
				
				break;
				
			  
			default:
				return MSG_NOT_PROCESSED;
		}
		return MSG_PROCESSED;
	}
}
