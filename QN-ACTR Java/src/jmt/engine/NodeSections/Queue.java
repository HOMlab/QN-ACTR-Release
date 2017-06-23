/**    
  * Copyright (C) 2009, Laboratorio di Valutazione delle Prestazioni - Politecnico di Milano

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

import java.util.Arrays;

import javax.swing.JOptionPane;

import qnactr.sim.GlobalUtilities;
import qnactr.sim.QnactrSimulation;

import jmt.common.exception.NetException;
import jmt.engine.NetStrategies.QueueGetStrategy;
import jmt.engine.NetStrategies.QueuePutStrategy;
import jmt.engine.NetStrategies.QueueGetStrategies.FCFSstrategy;
import jmt.engine.NetStrategies.QueuePutStrategies.TailStrategy;
import jmt.engine.QueueNet.BlockingRegion;
import jmt.engine.QueueNet.Job;
import jmt.engine.QueueNet.JobClass;
import jmt.engine.QueueNet.JobClassList;
import jmt.engine.QueueNet.JobInfo;
import jmt.engine.QueueNet.JobInfoList;
import jmt.engine.QueueNet.LinkedJobInfoList;
import jmt.engine.QueueNet.NetEvent;
import jmt.engine.QueueNet.NetMessage;
import jmt.engine.QueueNet.NetNode;
import jmt.engine.QueueNet.NodeSection;
import jmt.engine.QueueNet.PSJobInfoList;
import jmt.engine.QueueNet.WaitingRequest;
import jmt.engine.random.engine.RandomEngine;
import jmt.engine.simEngine.SimSystem;

import qnactr.objectDesigner.Enums;

/**
 * This class implements a generic finite/infinite queue. In finite queue, if
 * the queue is full, new jobs could be dropped or not. It could implement
 * different job strategy and/or waiting requests strategy.
 *
 * <br><br>
 * It can also define the queue of a station which is inside a blocking region.
 * When a job arrives at this node section, the source node of the message is found out.
 * If the source node is inside the same region, there are no problems and the message
 * is processed as usual.
 * Otherwise, if the source node is outside the blocking region, this message is not
 * processed but redirected to the fictitious station (called "region input station")
 * which controls the access to the blocking region.
 * <br><br>
 *
 * The class has different constructors to create a generic queue or a redirecting queue.
 * <br>
 * However it's also possible to create a generic queue and then to turn on/off the
 * "redirecting queue" behaviour using the <tt>redirectionTurnON(..)</tt> and
 * <tt>redirectionTurnOFF()</tt> methods.
 *
 * @author Francesco Radaelli, Stefano Omini, Bertoli Marco
 * 
 * Modified by Ashanka (Oct 2009) for FCR Bug fix: Events are created with job instead of null for EVENT_JOB_OUT_OF_REGION
 */
/**
 * <p><b>Name:</b> Queue</p> 
 * <p><b>Description:</b> 
 * 
 * </p>
 * <p><b>Date:</b> 15/nov/2009
 * <b>Time:</b> 23.08.16</p>
 * @author Bertoli Marco [marco.bertoli@neptuny.com]
 * @version 3.0
 */
public class Queue extends InputSection {

  //QN-Java
  boolean queueDebugPopupFlag = false;
  
	/** Property Identifier: infinite. */
	public static final int PROPERTY_ID_INFINITE = 0x0101;
	/** Property Identifier: drop.*/
	public static final int PROPERTY_ID_DROP = 0x0102;
	/** Property Identifier: size.*/
	public static final int PROPERTY_ID_SIZE = 0x0103;
	/** Property Identifier: Waiting request.*/
	public static final int PROPERTY_ID_WAITING_REQUESTS = 0x0104;
	/** Property Identifier: Queue get strategy.*/
	public static final int PROPERTY_ID_GET_STRATEGY = 0x0105;
	/** Property Identifier: Queue put strategy.*/
	public static final int PROPERTY_ID_PUT_STRATEGY = 0x0106;
	/** Property Identifier: Dropped jobs.*/
	public static final int PROPERTY_ID_DROPPED_JOBS = 0x0107;

	public static final String FINITE_DROP = "drop";
	public static final String FINITE_BLOCK = "BAS blocking";
	public static final String FINITE_WAITING = "waiting queue";

	private int size;

	//coolStart is true if there are no waiting jobs when the queue is started
	private boolean coolStart, infinite;

	private boolean[] drop, block;

	//the JobInfoList of the owner NetNode (use to control the number of jobs in
	//case of finite queue)
	private JobInfoList nodeJobsList;

	/** This jobinfolist should be used instead of jobsList to support Processor Sharing */
	private JobInfoList queueJobInfoList;

	//number of dropped jobs
	private int droppedJobs;
	private int[] droppedJobsPerClass;

	private JobInfoList waitingRequests;

	private QueueGetStrategy getStrategy;

	private QueuePutStrategy putStrategy[];

	//-------------------BLOCKING REGION PROPERTIES----------------------------//
	//@author Stefano Omini

	//true if the queue belongs to a blocking region and has to redirect the jobs
	//arriving from the outside of the region
	private boolean redirectionON;
	//the blocking region the node belongs to
	private BlockingRegion myRegion;
	//the input station of the blocking region
	private NetNode regionInputStation;

	//-------------------end BLOCKING REGION PROPERTIES----------------------------//

	/**
	 * Creates a new instance of finite Queue.
	 * @param size Queue size (-1 = infinite queue).
	 * @param getStrategy Queue get strategy: if null FCFS strategy is used.
	 * @param putStrategy Queue put strategy: if null Tail strategy is used.
	 * @param drop True if the queue should rejects new jobs when it's full,
	 * false otherwise.
	 */
	public Queue(int size, boolean drop, QueueGetStrategy getStrategy, QueuePutStrategy putStrategy[]) {

		//OLD
		//super();

		//NEW
		//auto = false, otherwise when a JOB message is received,
		//the corresponding Job object is automatically added to
		//JobInfoList

		super(false);
		//end NEW

		if (size == -1) {
			infinite = true;
		} else {
			this.size = size;
			infinite = false;
		}
		if (getStrategy == null) {
			this.getStrategy = new FCFSstrategy();
		} else {
			this.getStrategy = getStrategy;
		}
		this.putStrategy = putStrategy;
		// Uses putstrategy.length to extimate number of classes. It's a bit unclean but we are forced for compatibility.
		this.drop = new boolean[putStrategy.length];
		this.block = new boolean[putStrategy.length];
		Arrays.fill(this.drop, drop);
		Arrays.fill(this.block, false);
		coolStart = true;

		//this node doesn't belong to any blocking region
		redirectionON = false;
		myRegion = null;
		regionInputStation = null;

		//NEW
		//@author Stefano Omini
		//log = NetSystem.getLog();
		//end NEW
	}

	/**
	 * Creates a new instance of finite Queue.
	 * @param size Queue size (-1 = infinite queue).
	 * @param getStrategy Queue get strategy: if null FCFS strategy is used.
	 * @param putStrategy Queue put strategy: if null Tail strategy is used.
	 * @param drop True if the queue should rejects new jobs when it's full,
	 * false otherwise.
	 */
	public Queue(Integer size, Boolean drop, QueueGetStrategy getStrategy, QueuePutStrategy putStrategy[]) {
		this(size.intValue(), drop.booleanValue(), getStrategy, putStrategy);
	}

	/** Creates a new instance of finite redirecting Queue.
	 * @param size Queue size (-1 = infinite queue).
	 * @param getStrategy Queue get strategy: if null FCFS strategy is used.
	 * @param putStrategy Queue put strategy: if null Tail strategy is used.
	 * @param drop True if the queue should rejects new jobs when it's full,
	 * false otherwise.
	 * @param myReg the blocking region to which the owner node of this queue belongs
	 */
	public Queue(int size, boolean drop, QueueGetStrategy getStrategy, QueuePutStrategy putStrategy[], BlockingRegion myReg) {
		//uses constructor for generic queue
		this(size, drop, getStrategy, putStrategy);

		//sets blocking region properties
		redirectionON = true;
		myRegion = myReg;
		regionInputStation = myRegion.getInputStation();
	}

	/** Creates a new instance of finite redirecting Queue.
	 * @param size Queue size (-1 = infinite queue).
	 * @param getStrategy Queue get strategy: if null FCFS strategy is used.
	 * @param putStrategy Queue put strategy: if null Tail strategy is used.
	 * @param drop True if the queue should rejects new jobs when it's full,
	 * false otherwise.
	 * @param myReg the blocking region to which the owner node of this queue belongs
	 */
	public Queue(Integer size, Boolean drop, QueueGetStrategy getStrategy, QueuePutStrategy putStrategy[], BlockingRegion myReg) {
		this(size.intValue(), drop.booleanValue(), getStrategy, putStrategy, myReg);
	}

	/**
	 * Creates a new instance of finite Queue. This is the newwst constructor that supports
	 * differend drop strategies. Other constructors are left for compatibility.
	 * @param size Queue size (-1 = infinite queue).
	 * @param getStrategy Queue get strategy: if null FCFS strategy is used.
	 * @param putStrategy Queue put strategy: if null Tail strategy is used.
	 * @param dropStrategies  
	 */
	public Queue(Integer size, String[] dropStrategies, QueueGetStrategy getStrategy, QueuePutStrategy putStrategy[]) {
		this(size.intValue(), false, getStrategy, putStrategy);
		// Decodes drop strategies
		for (int i = 0; i < dropStrategies.length; i++) {
			if (dropStrategies[i].equals(FINITE_DROP)) {
				drop[i] = true;
				block[i] = false;
			} else if (dropStrategies[i].equals(FINITE_BLOCK)) {
				drop[i] = false;
				block[i] = true;
			} else if (dropStrategies[i].equals(FINITE_WAITING)) {
				drop[i] = false;
				block[i] = false;
			}
		}
	}

	/**
	 * Turns on the "redirecting queue" behaviour.
	 * @param region the blocking region to which the owner node
	 * of this queue belongs
	 */
	public void redirectionTurnON(BlockingRegion region) {
		//sets blocking region properties
		redirectionON = true;
		myRegion = region;
		regionInputStation = myRegion.getInputStation();
	}

	/**
	 * Turns off the "redirecting queue" behaviour.
	 */
	public void redirectionTurnOFF() {
		//sets blocking region properties
		redirectionON = false;
		myRegion = null;
		regionInputStation = null;
	}

	/**
	 * Tells whether the "redirecting queue" behaviour has been turned on.
	 * @return true, if the "redirecting queue" behaviour is on; false otherwise.
	 */
	public boolean isRedirectionON() {
		return redirectionON;
	}

	//end NEW

	@Override
	public boolean isEnabled(int id) throws jmt.common.exception.NetException {
		switch (id) {
			case PROPERTY_ID_INFINITE:
				return infinite;
			default:
				return super.isEnabled(id);
		}
	}

	@Override
	public int getIntSectionProperty(int id) throws jmt.common.exception.NetException {
		switch (id) {
			case PROPERTY_ID_SIZE:
				return size;
			case PROPERTY_ID_WAITING_REQUESTS:
				return waitingRequests.size();
				//NEW
				//@author Stefano Omini
			case PROPERTY_ID_DROPPED_JOBS:
				return droppedJobs;
				//end NEW
			default:
				return super.getIntSectionProperty(id);
		}
	}

	@Override
	public int getIntSectionProperty(int id, JobClass jobClass) throws jmt.common.exception.NetException {
		switch (id) {
			case PROPERTY_ID_WAITING_REQUESTS:
				return waitingRequests.size(jobClass);
				//NEW
				//@author Stefano Omini
			case PROPERTY_ID_DROPPED_JOBS:
				return droppedJobsPerClass[jobClass.getId()];
				//end NEW
			default:
				return super.getIntSectionProperty(id, jobClass);
		}
	}

	@Override
	public Object getObject(int id) throws jmt.common.exception.NetException {
		switch (id) {
			case PROPERTY_ID_GET_STRATEGY:
				return getStrategy;
			case PROPERTY_ID_PUT_STRATEGY:
				return putStrategy;
			default:
				return super.getObject(id);
		}
	}

	public boolean hasInfiniteQueue() {
		return infinite;
	}

	@Override
	protected void nodeLinked(NetNode node) {
		// Sets netnode dependent properties
		waitingRequests = new LinkedJobInfoList(getJobClasses().size(), true);
		if (putStrategy == null) {
			putStrategy = new QueuePutStrategy[getJobClasses().size()];
			for (int i = 0; i < getJobClasses().size(); i++) {
				putStrategy[i] = new TailStrategy();
			}
		}

		// TODO the following line its not clean. The correct behavior should be implemented without this hack.
		try {
			if (getOwnerNode().getSection(SERVICE) instanceof PSServer) {
				jobsList = new PSJobInfoList(getJobClasses().size(), true);
				queueJobInfoList = ((PSJobInfoList) jobsList).getInternalList();
			}
		} catch (NetException ex) {
			logger.error(ex);
		}
		if (jobsList == null) {
			jobsList = new LinkedJobInfoList(getJobClasses().size(), true);
		}
		if (queueJobInfoList == null) {
			queueJobInfoList = jobsList;
		}

		if (!infinite) {
			droppedJobs = 0;
			droppedJobsPerClass = new int[getJobClasses().size()];
			for (int i = 0; i < droppedJobsPerClass.length; i++) {
				droppedJobsPerClass[i] = 0;
			}
		}

		//retrieves the job info list of the owner node
		nodeJobsList = getOwnerNode().getJobInfoList();

	}

	/** This method implements a generic finite/infinite queue
	 * @param message message to be processed.
	 * @throws jmt.common.exception.NetException
	 */
	@Override
	protected int process(NetMessage message) throws jmt.common.exception.NetException {
		Job job;
		switch (message.getEvent()) {

			case NetEvent.EVENT_START:

				//EVENT_START
				//If there are jobs in queue, the first (chosen using the specified
				//get strategy) is forwarded and coolStart becomes false.

				if (queueJobInfoList.size() > 0) {
					//the first job is forwarded to service section
					forward(getStrategy.get(queueJobInfoList));
					coolStart = false;
				}

				break;

			case NetEvent.EVENT_ACK:

				//EVENT_ACK
				//If there are waiting requests, the first is taken (if the source node of this request
				//is the owner node of this section, an ack message is sent).
				//The job contained is put into the queue using the specified put strategy.
				//
				//At this point, if there are jobs in queue, the first is taken (using the
				//specified get strategy) and forwarded. Otherwise, if there are no jobs, coolStart
				//is set true.

				// if there is a waiting request send ack to the first node
				//(note that with infinite queue there are no waitinq requests)
				if (waitingRequests.size() != 0) {
					WaitingRequest wr;
					wr = (WaitingRequest) waitingRequests.removeFirst();

					// If the source is not the owner node sends ack if blocking is enabled. Otherwise 
					// ack was already sent.
					if (!isMyOwnerNode(wr.getNode()) && block[wr.getJob().getJobClass().getId()]) {
						send(NetEvent.EVENT_ACK, wr.getJob(), 0.0, wr.getSection(), wr.getNode());
					}

					//the class ID of this job
					int c = wr.getJob().getJobClass().getId();
					//the job is put into the queue according to its own class put strategy
					putStrategy[c].put(wr.getJob(), queueJobInfoList, message.getSourceSection(), message.getSource(), this);
				}

				//debug method
				//if(this.getOwnerNode().getName().equals("Motor Initiation_1"))
				//JOptionPane.showMessageDialog(null, "Queue EVENT ACK \n + clock: " + SimSystem.clock() + "\n Queue Name: " + this.getOwnerNode().getName() + ". queueJobInfoList.size(): " + queueJobInfoList.size(), "Queue.java process" , JOptionPane.INFORMATION_MESSAGE); //QN-Java
				
				// if there is at least one job, sends it
				if (queueJobInfoList.size() > 0) {
				  
				  
				  ////////////////////////////////
				  //  releaseCondition  1 // QN-Java
				  ////////////////////////////////
					// QN-Java
					String[] soName = GlobalUtilities.getServerOperatorNamesFromRawName(this.getOwnerNode().getName());
					String serverName = soName[0];
					String operatorID = soName[1];
					QnactrSimulation simQnactr = this.getOwnerNode().getSimJMT().getQnactrSims()[ Integer.parseInt(operatorID) - 1];
					Job jobPeek = getStrategy.peek(queueJobInfoList);
	        
					if(queueDebugPopupFlag)JOptionPane.showMessageDialog(null, "Queue releaseCondition  1  \n + clock: " + SimSystem.clock() + "\n Queue Name: " + this.getOwnerNode().getName() + ". queueJobInfoList.size(): " + queueJobInfoList.size() + ". job entity tag: " + jobPeek.qnactrEntity.Tag, "Queue.java process" , JOptionPane.INFORMATION_MESSAGE); //QN-Java
	        
					boolean releaseCondition = (boolean)simQnactr.logics.Enums(Enums.ServerName.valueOf(GlobalUtilities.stringLowNoSpace(serverName)), Enums.ServiceStage.Release, jobPeek.qnactrEntity); // QN-Java

				  if(releaseCondition){
				    // Gets job using a specific strategy and sends job
				    Job jobSent = getStrategy.get(queueJobInfoList);
				    
				    forward(jobSent);
				    
				    jobSent.qnactrEntity.updateCurrentPlace(Enums.EntityPlaceHeader.goingto, null, Enums.NodeSection.server); //QN-Java
				  }
				  
				  
				  
				} else {
					// else set coolStart to true
					coolStart = true;

				}
				break;

			case NetEvent.EVENT_JOB:

				//EVENT_JOB
				//If the queue is a redirecting queue, jobs arriving from the outside of
				//the blocking region must be redirected to the region input station
				//
				//Otherwise the job is processed as usual.
				//
				//If coolStart is true, the queue is empty, so the job is added to the job list
				//and immediately forwarded to the next section. An ack is sent and coolStart is
				//set to false.
				//
				//If the queue is not empty, it should be distinguished between finite/infinite queue.
				//
				//If the queue is finite, checks the size: if it's not full the job is put into the
				//queue and an ack is sent. Else, if it's full, checks the owner node: if the
				//source node is the owner node of this section, an ack is sent and a waiting
				//request is created. If the source is another node the waiting request is created
				//only if drop is false, otherwise an ack is sent but the job is rejected.
				//
				//If the queue is infinite, the job is put into the queue and an ack is sent

				job = message.getJob();
				
				job.qnactrEntity.updateCurrentPlace(Enums.EntityPlaceHeader.in, GlobalUtilities.fromGlobalNodeNameToEnumServerName(this.getOwnerNode().getName()), Enums.NodeSection.queue); //QN-Java
				
				//JOptionPane.showMessageDialog(null, "Queue EVENT JOB \n + clock: " + SimSystem.clock() + "\n Queue Name: " + this.getOwnerNode().getName() + ". queueJobInfoList.size(): " + queueJobInfoList.size() + ". job entity tag: " + job.qnactrEntity.Tag, "Queue.java process" , JOptionPane.INFORMATION_MESSAGE); //QN-Java
        
				//----REDIRECTION BEHAVIOUR----------//
				if (isRedirectionON()) {

					NetNode source = message.getSource();
					boolean fromTheInside = myRegion.belongsToRegion(source);

					//the first time input station isn't known yet
					if (regionInputStation == null) {
						regionInputStation = myRegion.getInputStation();
					}

					if (!fromTheInside) {
						//this message has arrived from the outside of the blocking region
						if ((source != regionInputStation)) {
							//the external source is not the input station
							//the message must be redirected to the input station,
							//without processing it

							//redirects the message to the inputStation
							redirect(NetEvent.EVENT_JOB, job, 0.0, NodeSection.INPUT, regionInputStation);
							//send a ack to the source
							send(NetEvent.EVENT_ACK, job, 0.0, message.getSourceSection(), message.getSource());

							return MSG_PROCESSED;
						}
					}
				}
				//----END REDIRECTION BEHAVIOUR-------//

				//
				//two possible cases:
				//1 - the queue is a generic queue (redirectionOn == false)
				//2 - the queue is a redirecting queue, but the message has arrived
				//from the inside of the region or from the inputStation:
				//in this case the redirecting queue acts as a normal queue
				//
				//therefore in both cases the behaviour is the same
				//

				// Check if there is still capacity.
				// <= size because the arrived job hasn't been inserted in Queue
				// job list but has been inserted in NetNode job list !!
				if (infinite || nodeJobsList.size() <= size) {
					// Queue is not full. Okay.

					// If coolStart is true, this is the first job received or the
					// queue was empty: this job is sent immediately to the next
					// section and coolStart set to false.
				  
					if (coolStart) {
					  
			  ////////////////////////////////
	          //  releaseCondition  2 // QN-Java
	          ////////////////////////////////
	        // QN-Java
	          String[] soName = GlobalUtilities.getServerOperatorNamesFromRawName(this.getOwnerNode().getName());
	          String serverName = soName[0];
	          String operatorID = soName[1];
	          boolean releaseCondition;
	          
	          
	          if(queueDebugPopupFlag)JOptionPane.showMessageDialog(null, "Queue releaseCondition  2  \n + clock: " + SimSystem.clock() + "\n Queue Name: " + this.getOwnerNode().getName() + ". queueJobInfoList.size(): " + queueJobInfoList.size() , "Queue.java process" , JOptionPane.INFORMATION_MESSAGE); //QN-Java
	          
	          
	          if(operatorID.equals("0")){
	            releaseCondition = true;
	          }
	          else{
	            QnactrSimulation simQnactr = this.getOwnerNode().getSimJMT().getQnactrSims()[ Integer.parseInt(operatorID) - 1];
	            releaseCondition = (boolean)simQnactr.logics.Enums(Enums.ServerName.valueOf(GlobalUtilities.stringLowNoSpace(serverName)), Enums.ServiceStage.Release, job.qnactrEntity); // QN-Java
	          }
	          if(releaseCondition){
	            	            
	            // No jobs in queue: Refresh jobsList and sends job (don't use put strategy, because queue is empty)
	            queueJobInfoList.add(new JobInfo(job));
	            //forward without any delay
	            forward(queueJobInfoList.removeFirst().getJob());
	            
	            job.qnactrEntity.updateCurrentPlace(Enums.EntityPlaceHeader.goingto, null, Enums.NodeSection.server); //QN-Java
//	            JOptionPane.showMessageDialog(null, "Queue EVENT JOB coolStart Truely \n + clock: " + SimSystem.clock() + "\n Queue Name: " + this.getOwnerNode().getName() + ". queueJobInfoList.size(): " + queueJobInfoList.size() + ". job entity tag: " + job.qnactrEntity.Tag, "Queue.java process" , JOptionPane.INFORMATION_MESSAGE); //QN-Java
	            
	          }
	          else{
	            queueJobInfoList.add(new JobInfo(job));
	            
//	            JOptionPane.showMessageDialog(null, "Queue EVENT JOB coolStart but false releaseCondition \n + clock: " + SimSystem.clock() + "\n Queue Name: " + this.getOwnerNode().getName() + ". queueJobInfoList.size(): " + queueJobInfoList.size() + ". job entity tag: " + job.qnactrEntity.Tag, "Queue.java process" , JOptionPane.INFORMATION_MESSAGE); //QN-Java
              
	          }
					

            coolStart = false;
            
					} else {
						putStrategy[job.getJobClass().getId()].put(job, queueJobInfoList, message.getSourceSection(), message.getSource(), this);
						
//						JOptionPane.showMessageDialog(null, "Queue EVENT JOB _NOT_ coolStart \n + clock: " + SimSystem.clock() + "\n Queue Name: " + this.getOwnerNode().getName() + ". queueJobInfoList.size(): " + queueJobInfoList.size() + ". job entity tag: " + job.qnactrEntity.Tag, "Queue.java process" , JOptionPane.INFORMATION_MESSAGE); //QN-Java
						
					}
					//sends an ACK backward
					send(NetEvent.EVENT_ACK, job, 0.0, message.getSourceSection(), message.getSource());
				} else {
					// Queue is full. Now we use an additional queue or drop.

					// if the job has been sent by the owner node of this queue section
					if (isMyOwnerNode(message.getSource())) {
						send(NetEvent.EVENT_ACK, job, 0.0, message.getSourceSection(), message.getSource());

						waitingRequests.add(new WaitingRequest(message.getSource(), message.getSourceSection(), job));
					}
					// otherwise if job has been sent by another node
					else if (!drop[job.getJobClass().getId()]) {
						// if drop is true reject the job, else add the job to waitingRequests
						waitingRequests.add(new WaitingRequest(message.getSource(), message.getSourceSection(), job));
						//if blocking is disabled, sends ack otherwise router of the previous node remains busy
						if (!block[job.getJobClass().getId()]) {
							send(NetEvent.EVENT_ACK, job, 0.0, message.getSourceSection(), message.getSource());
						}
					} else {
						// if drop, send ack event to source
						droppedJobs++;
						droppedJobsPerClass[job.getJobClass().getId()]++;

						// Removes job from global jobInfoList - Bertoli Marco
						getOwnerNode().getQueueNet().getJobInfoList().dropJob(job);

						//after arriving to this section, the job has been inserted in the job
						//lists of both node section and node.
						//If drop = true, the job must be removed if the queue is full.
						//Using the "general" send method, however, the dropped job wasn't removed
						//from the job info list of node section and of node, then it was
						//sent later, after receiving one or more ack.

						sendAckAfterDrop(job, 0.0, message.getSourceSection(), message.getSource());

						//if the queue is inside a blocking region, the jobs
						//counter must be decreased
						if (isRedirectionON()) {
							//decrease the number of jobs
							myRegion.decreaseOccupation(job.getJobClass());
							//sends an event to the input station (which may be blocked)
							send(NetEvent.EVENT_JOB_OUT_OF_REGION, job, 0.0, NodeSection.INPUT, regionInputStation);
							//Since now for blocking regions the job dropping is handles manually at node 
							//level hence need to create events with Jobs ..Modified for FCR Bug Fix
						}
					}
				}

				break;

			default:
				return MSG_NOT_PROCESSED;
		}
		return MSG_PROCESSED;
	}

	private void forward(Job job) throws jmt.common.exception.NetException {
		sendForward(job, 0.0);
	}

	/**
	 * Gets the total number of dropped jobs
	 * @return the total number of dropped jobs, -1 otherwise
	 */
	public int getDroppedJobs() {
		if (!infinite) {
			return droppedJobs;
		} else {
			return -1;
		}
	}

	/**
	 * Gets the numbers of dropped jobs for each class
	 * @return the numbers of dropped jobs for each class, null otherwise
	 */
	public int[] getDroppedJobsPerClass() {
		if (!infinite) {
			return droppedJobsPerClass;
		} else {
			return null;
		}

	}

	/**	 * Gets the number of dropped jobs for the specified class
	 * @return the number of dropped jobs for the specified class, -1 otherwise
	 */
	public int getDroppedJobPerClass(int jobClass) {
		if (!infinite) {
			return droppedJobsPerClass[jobClass];
		} else {
			return -1;
		}

	}

	/**
	 * Adds the specified numbers of jobs for each class
	 * @param preload_jobPerClass the numbers of jobs for each class
	 */
	public void preloadJobs(int[] preload_jobPerClass) {

		//total jobs
		int totJobs = 0;

		//number of classes
		int classNumber = preload_jobPerClass.length;

		//jobs that haven't been inserted yet
		int[] residualClassJobs = new int[classNumber];

		//first of all computes the total number of jobs to be added
		for (int c = 0; c < classNumber; c++) {
			totJobs += preload_jobPerClass[c];
			residualClassJobs[c] = preload_jobPerClass[c];
		}

		RandomEngine randomEng = RandomEngine.makeDefault();
		int randomClassIndex;

		JobClassList jobClasses = getJobClasses();

		while (totJobs > 0) {
			//jobs of different classes must be mixed.. use random numbers

			randomClassIndex = (int) Math.floor((randomEng.raw()) * classNumber);

			if (residualClassJobs[randomClassIndex] > 0) {
				//other jobs to be added
				Job newJob = new Job(jobClasses.get(randomClassIndex));
				queueJobInfoList.add(new JobInfo(newJob));

				//job has been added: decrease class e total counters
				residualClassJobs[randomClassIndex]--;
				totJobs--;

				// Signals to global jobInfoList new added job - Bertoli Marco
				this.getOwnerNode().getQueueNet().getJobInfoList().addJob(newJob);

			}

			//else no other jobs of this class must be added

			//continue

		}

	}

	//QN-Java
	public JobInfoList getQueueJobInfoList(){
	  return queueJobInfoList;
	}
}
