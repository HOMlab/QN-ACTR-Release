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
 * [2013-06-27] add publicSendMe
 * 
 */


package jmt.engine.QueueNet;

import jmt.engine.dataAnalysis.InverseMeasure;
import jmt.engine.dataAnalysis.Measure;
import jmt.engine.log.JSimLogger;
import jmt.engine.random.Parameter;
import jmt.engine.simEngine.RemoveToken;

/**
 * This class implements a generic section of a NetNode.
 * @author Francesco Radaelli, Stefano Omini
 */
public abstract class NodeSection {

	/*------------------------------ADDRESSES---------------------------------*/

	/** Section without address.*/
	public static final byte NO_ADDRESS = 0x00;
	/** Input section address */
	public static final byte INPUT = 0x01;
	/** Service section address. */
	public static final byte SERVICE = 0x02;
	/** Output section address.*/
	public static final byte OUTPUT = 0x03;

	/*-----------------------NODE SECTION RETURNED VALUES---------------------*/

	/** Message passed has not been processed (general event).*/
	public static final int MSG_NOT_PROCESSED = 0x0000;

	/** Message passed has been processed (all events).*/
	public static final int MSG_PROCESSED = 0x0001;

	/*------------------------------BIT MASK----------------------------------*/

	static final int SOURCE_MASK = 0xFF000000;

	static final int DESTINATION_MASK = 0x00FF0000;

	static final int SOURCE_SHIFT = 24;

	static final int DESTINATION_SHIFT = 16;

	/*------------------------------------------------------------------------*/

	/** Exception ID: Required measure doesn't exist. */
	public final int EXCEPTION_MEASURE_DOES_NOT_EXIST = 0x0001;
	/** Exception ID: Required property is not available. */
	public static final int EXCEPTION_PROPERTY_NOT_AVAILABLE = 0x0002;

	//TODO: attenzione, non tutte queste propriet�hanno senso per ogni tipo di sezione
	/** Property ID: number of jobs inside */
	public static final int PROPERTY_ID_RESIDENT_JOBS = 0x0001;
	/** Property ID: number of jobs which arrived to this NodeSection  */
	public static final int PROPERTY_ID_ARRIVED_JOBS = 0x0002;
	/** Property ID: number of jobs which left this NodeSection  */
	public static final int PROPERTY_ID_LEAVING_JOBS = 0x0003;
	/** Property ID: residence time  */
	public static final int PROPERTY_ID_RESIDENCE_TIME = 0x0004;
	/** Property ID: throughput  */
	public static final int PROPERTY_ID_THROUGHPUT = 0x0005;
	/** Property ID: utilization  */
	public static final int PROPERTY_ID_UTILIZATION = 0x0006;
	/** Property Identifier: JobInfoList.*/
	public static final int PROPERTY_ID_JOBINFOLIST = 0x0007;

	//TODO: rendere pi�efficiente il meccanismo di rilevamneto dell'owner node? (per evitare troppi getNode)
	/** Owner of this NodeSection. */
	private NetNode ownerNode;

	/** Identifier of this NodeSection. */
	private byte sectionID;

	protected JobInfoList jobsList;

	protected JSimLogger logger = JSimLogger.getLogger(this.getClass());

	private boolean auto; //auto refresh of the jobsList attribute.
	private boolean nodeAuto; // auto refresh the joblist attribute at node level.

	/** Creates a new instance of this NodeSection.
	 * Note that, while building a
	 * new node section, node owner informations are not available. To
	 * set node section properties depending on the owner node ones, the
	 * "nodeLiked(...)" protected method should be used.
	 *  @param id    NodeSection identifier.
	 */
	public NodeSection(byte id) {
		this.sectionID = id;
		auto = true;
		nodeAuto = true;
	}

	/** Creates a new instance of this NodeSection.
	 * Note that, while building a
	 * new node section, node owner informations are not available. To
	 * set node section properties depending on the owner node ones, the
	 * "nodeLiked(...)" protected method should be used.
	 *  @param id    NodeSection identifier.
	 *  @param auto  auto refresh of the jobsList attribute.
	 */
	public NodeSection(byte id, boolean auto) {
		this.sectionID = id;
		this.auto = auto;
		nodeAuto = true;
	}

	/** Creates a new instance of this NodeSection.
	 * Note that, while building a
	 * new node section, node owner informations are not available. To
	 * set node section properties depending on the owner node ones, the
	 * "nodeLiked(...)" protected method should be used.
	 *  @param id    NodeSection identifier.
	 *  @param auto  auto refresh of the jobsList attribute.
	 *  @param nodeAuto auto refresh the jobsList attribute at node level
	 */
	public NodeSection(byte id, boolean auto, boolean nodeAuto) {
		this.sectionID = id;
		this.auto = auto;
		this.nodeAuto = nodeAuto;
	}

	/** This method should be overriden to implement a specific beahviour of the
	 * node section when the node section itself is linked to the owner node.
	 * This method should be used to set node section properties depending on
	 * the owner node ones.
	 * @param node Owner node.
	 */
	protected void nodeLinked(NetNode node) {
	};

	/** Gets the id of the node section.
	 * @return node section identifier.
	 */
	public byte getSectionID() {
		return sectionID;
	}

	//TODO: eventualmente si pu�fare un overriding per componenti particolari (terminal, ..)
	/** Analyzes a measure in the node section. Ovveride this method to
	 * analyze a measure depending on the node section implementation. Note that
	 * the first 256 identifiers are reserved by NodeSection class.
	 * @param name name of the measure to be activated (see measures constants).
	 * @param jobClass Job class to be analyzed.
	 * @param measurement Set of measure to be activated.
	 * @throws jmt.common.exception.NetException
	 */
	public void analyze(int name, JobClass jobClass, Measure measurement) throws jmt.common.exception.NetException {
		switch (name) {

			case SimConstants.LIST_RESIDENCE_TIME:
				jobsList.analyzeResidenceTime(jobClass, measurement);
				break;

			case SimConstants.LIST_THROUGHPUT:
				//OLD
				//jobsList.analyzeThroughput(jobClass, measurement);
				//NEW
				//an InverseMeasured object is required to analyze throughput
				//@author Stefano Omini
				if (measurement instanceof InverseMeasure) {
					InverseMeasure inv = (InverseMeasure) measurement;
					jobsList.analyzeThroughput(jobClass, inv);
				} else {
					throw new jmt.common.exception.NetException(this, EXCEPTION_MEASURE_DOES_NOT_EXIST,
							"An InverseMeasure object is required to analyze throughput!");
				}
				//end NEW
				break;

			case SimConstants.LIST_NUMBER_OF_JOBS:
				jobsList.analyzeUtilization(jobClass, measurement);
				break;

			case SimConstants.LIST_DROP_RATE:
				jobsList.analyzeDropRate(jobClass, (InverseMeasure) measurement);
				break;

			default:
				throw new jmt.common.exception.NetException(this, EXCEPTION_MEASURE_DOES_NOT_EXIST, "required analyzer does not exist!");
		}
	}

	/** Checks if the specified node is the owner node of this section. Note that this method
	 * could be called <b>only after</b> that the owner node has been linked to this
	 * section.
	 * @param node node to be checked.
	 * @return True if the node is the owner node, false otherwise.
	 */
	public boolean isMyOwnerNode(NetNode node) {
		return node == this.ownerNode;
	}

	/** Gets owner node of this section.
	 * @return Owner node.
	 */
	public NetNode getOwnerNode() {
		return ownerNode;
	}

	/** Gets the list of the job classes of the owner queue network.
	 * @return Queue network job classes.
	 */
	public JobClassList getJobClasses() {
		return ownerNode.getJobClasses();
	}

	/** Gets an integer type property of this node. Note that the first 256
	 * identifiers are reserved by NodeSection class.
	 * @param id Property identifier (see properties constants).
	 * @return Property value.
	 * @throws jmt.common.exception.NetException if the requested property is not available.
	 */
	public int getIntSectionProperty(int id) throws jmt.common.exception.NetException {
		try {
			switch (id) {
				case PROPERTY_ID_ARRIVED_JOBS:
					return jobsList.getJobsIn();
				case PROPERTY_ID_LEAVING_JOBS:
					return jobsList.getJobsOut();
				case PROPERTY_ID_RESIDENT_JOBS:
					return jobsList.size();
			}
		} catch (jmt.common.exception.NetException exc) {
			throw new jmt.common.exception.NetException(this, EXCEPTION_PROPERTY_NOT_AVAILABLE, "required property is not available.", exc);
		}
		throw new jmt.common.exception.NetException(this, EXCEPTION_PROPERTY_NOT_AVAILABLE, "required property is not available.");
	}

	/** Gets an integer type property of this node depending on a specified
	 * job class. Note that the first 256 identifiers are reserved by
	 * NodeSection class.
	 * @param id Property identifier (see properties constants).
	 * @param jobClass jobClass.
	 * @return Property value.
	 * @throws jmt.common.exception.NetException  if the requested property is not available.
	 */
	public int getIntSectionProperty(int id, JobClass jobClass) throws jmt.common.exception.NetException {
		try {
			switch (id) {
				case PROPERTY_ID_ARRIVED_JOBS:
					return jobsList.getJobsInPerClass(jobClass);
				case PROPERTY_ID_LEAVING_JOBS:
					return jobsList.getJobsOutPerClass(jobClass);
				case PROPERTY_ID_RESIDENT_JOBS:
					return jobsList.size(jobClass);
			}
		} catch (jmt.common.exception.NetException exc) {
			throw new jmt.common.exception.NetException(this, EXCEPTION_PROPERTY_NOT_AVAILABLE, "required property is not available.", exc);
		}
		throw new jmt.common.exception.NetException(this, EXCEPTION_PROPERTY_NOT_AVAILABLE, "required property is not available.");
	}

	/** Gets a double type property of this node. Note that the first 256
	 * identifiers are reserved by NodeSection class.
	 * @param id Property identifier (see properties constants).
	 * @return Property value.
	 * @throws jmt.common.exception.NetException if the requested property is not available.
	 */
	public double getDoubleSectionProperty(int id) throws jmt.common.exception.NetException {
		try {
			switch (id) {
				case PROPERTY_ID_RESIDENCE_TIME:
					return jobsList.getBusyTime() / jobsList.getJobsOut();
				case PROPERTY_ID_THROUGHPUT:
					return jobsList.getJobsOut() / NetSystem.getTime();
				case PROPERTY_ID_UTILIZATION:
					return jobsList.getBusyTime() / NetSystem.getTime();
			}
		} catch (jmt.common.exception.NetException exc) {
			throw new jmt.common.exception.NetException(this, EXCEPTION_PROPERTY_NOT_AVAILABLE, "required property is not available.", exc);
		}
		throw new jmt.common.exception.NetException(this, EXCEPTION_PROPERTY_NOT_AVAILABLE, "required property is not available.");
	}

	/** Gets a double type property of this node depending on a specified
	 * job class. Note that the first 256 identifiers are reserved by NodeSection
	 * class.
	 * @param id Property identifier (see properties constants).
	 * @param jobClass jobClass.
	 * @return Property value.
	 * @throws jmt.common.exception.NetException  if the requested property is not available.
	 */
	public double getDoubleSectionProperty(int id, JobClass jobClass) throws jmt.common.exception.NetException {
		try {
			switch (id) {
				case PROPERTY_ID_RESIDENCE_TIME:
					return jobsList.getBusyTimePerClass(jobClass) / jobsList.getJobsOutPerClass(jobClass);
				case PROPERTY_ID_THROUGHPUT:
					return jobsList.getJobsOutPerClass(jobClass) / NetSystem.getTime();
				case PROPERTY_ID_UTILIZATION:
					return jobsList.getBusyTimePerClass(jobClass) / NetSystem.getTime();
			}
		} catch (jmt.common.exception.NetException exc) {
			throw new jmt.common.exception.NetException(this, EXCEPTION_PROPERTY_NOT_AVAILABLE, "required property is not available.", exc);
		}
		throw new jmt.common.exception.NetException(this, EXCEPTION_PROPERTY_NOT_AVAILABLE, "required property is not available.");
	}

	/** Gets a parameter type property of this node depending on a
	 * specified job class. Note that the first 256identifiers are reserved by
	 * NodeSection class.
	 * @param id Property identifier.
	 * @param jobClass jobClass.
	 * @return Property value.
	 * @throws jmt.common.exception.NetException if the requested property is not available.
	 */
	public Parameter getParameter(int id, JobClass jobClass) throws jmt.common.exception.NetException {
		switch (id) {
			default:
				throw new jmt.common.exception.NetException(this, EXCEPTION_PROPERTY_NOT_AVAILABLE, "required property is not available.");
		}
	}

	/** Gets a generic object type property of this node. Note that the first
	 * 256 identifiers are reserved by NodeSection class.
	 * @param id Property identifier.
	 * @return Property value.
	 * @throws jmt.common.exception.NetException if the requested property is not available.
	 */
	public Object getObject(int id) throws jmt.common.exception.NetException {
		switch (id) {
			case PROPERTY_ID_JOBINFOLIST:
				return jobsList;
			default:
				throw new jmt.common.exception.NetException(this, EXCEPTION_PROPERTY_NOT_AVAILABLE, "required property is not available.");
		}
	}

	/** Gets a generic object type property of this node depending on a
	 * specified job class. Note that the first 256 identifiers are reserved by
	 * NodeSection class.
	 * @param id Property identifier.
	 * @param jobClass the jobClass which property refers to.
	 * @return Property value.
	 * @throws jmt.common.exception.NetException if the requested property is not available.
	 */
	public Object getObject(int id, JobClass jobClass) throws jmt.common.exception.NetException {
		switch (id) {
			default:
				throw new jmt.common.exception.NetException(this, EXCEPTION_PROPERTY_NOT_AVAILABLE, "required property is not available.");
		}
	}

	/** Checks if a property is enabled. Note that the first 256
	 * identifiers are reserved by NodeSection class.
	 * @param id Property identifier.
	 * @return Property value.
	 * @throws jmt.common.exception.NetException if the requested property is not available.
	 */
	public boolean isEnabled(int id) throws jmt.common.exception.NetException {
		switch (id) {
			default:
				throw new jmt.common.exception.NetException(this, EXCEPTION_PROPERTY_NOT_AVAILABLE, "required property is not available.");
		}
	}

	/** Checks if a property  depending on a specified job class is enable.
	 * Note that the first 256 identifiers are reserved by NodeSection
	 * class.
	 * @param id Property identifier.
	 * @param jobClass jobClass.
	 * @return Property value.
	 * @throws jmt.common.exception.NetException if the requested property is not available.
	 */
	public boolean isEnabled(int id, JobClass jobClass) throws jmt.common.exception.NetException {
		switch (id) {
			default:
				throw new jmt.common.exception.NetException(this, EXCEPTION_PROPERTY_NOT_AVAILABLE, "required property is not available.");
		}
	}

	/** This method should be overridden to implement a specific NetMessage
	 * processor.
	 * @param message message to be processed.
	 * @return message processing result.
	 */
	protected abstract int process(NetMessage message) throws jmt.common.exception.NetException;

	/** Sends an event to a section of a node.
	 * @param event event to be sent.
	 * @param data  data to be attached to the message.
	 * @param delay Scheduling delay.
	 * @param destinationSection destination section.
	 * @param destination destination node.
	 * @return a token to remove sent event
	 * @throws jmt.common.exception.NetException
	 */
	protected RemoveToken send(int event, Object data, double delay, byte destinationSection, NetNode destination)
			throws jmt.common.exception.NetException {
		if ((event == NetEvent.EVENT_JOB) && ((destination != ownerNode) || ((destination == ownerNode) && (destinationSection != sectionID)))) {
			//it's a JOB event and the destination is not the owner node or it's the owner
			//node but the dest section is not this section
			updateJobsList((Job) data);
		}
		return ownerNode.send(event, data, delay, sectionID, destinationSection, destination);
	}

	/** Sends an event to the <b>input</b> section of a node.
	 * @param event event to be sent.
	 * @param data  data to be attached to the message.
	 * @param delay Scheduling delay.
	 * @param destination destination node.
	 * @return a token to remove sent event
	 * @throws jmt.common.exception.NetException
	 */
	protected RemoveToken send(int event, Object data, double delay, NetNode destination) throws jmt.common.exception.NetException {
		if (auto && event == NetEvent.EVENT_JOB) {
			updateJobsList((Job) data);
		}
		return ownerNode.send(event, data, delay, sectionID, NodeSection.INPUT, destination);
	}

	/** Sends an event to a section of this node.
	 * @param event event to be sent.
	 * @param data  data to be attached to the message.
	 * @param delay Scheduling delay.
	 * @param destinationSection Destination section.
	 * @return a token to remove sent event
	 * @throws jmt.common.exception.NetException
	 */
	protected RemoveToken send(int event, Object data, double delay, byte destinationSection) throws jmt.common.exception.NetException {
		if (auto && (event == NetEvent.EVENT_JOB) && (destinationSection != sectionID)) {
			updateJobsList((Job) data);
		}
		return ownerNode.send(event, data, delay, sectionID, destinationSection, ownerNode);
	}

	//TODO: NOT USED
	/** Sends an event to this section.
	 * @param event event to be sent.
	 * @param data  data to be attached to the message.
	 * @param delay Scheduling delay.
	 * @return a token to remove sent event
	 * @throws jmt.common.exception.NetException
	 */
	protected RemoveToken sendMe(int event, Object data, double delay) throws jmt.common.exception.NetException {
		return ownerNode.send(event, data, delay, sectionID, sectionID, ownerNode);
	}

	//TODO: NOT USED
	/** Sends a job to a section of a node.
	 * @param job job to be attached to the message.
	 * @param delay Scheduling delay.
	 * @param destinationSection destination section.
	 * @param destination destination node.
	 * @return a token to remove sent event
	 * @throws jmt.common.exception.NetException
	 */
	protected RemoveToken send(Job job, double delay, byte destinationSection, NetNode destination) throws jmt.common.exception.NetException {
		if (auto && destinationSection != sectionID) {
			updateJobsList(job);
		}
		return ownerNode.send(NetEvent.EVENT_JOB, job, delay, sectionID, destinationSection, destination);

	}

	/** Sends a job to the <b>input</b> section of a node.
	 * @param job job to be attached to the message.
	 * @param delay Scheduling delay.
	 * @param destination destination node.
	 * @return a token to remove sent event
	 * @throws jmt.common.exception.NetException
	 */
	protected RemoveToken send(Job job, double delay, NetNode destination) throws jmt.common.exception.NetException {
		if (auto) {
			updateJobsList(job);
		}
		return ownerNode.send(NetEvent.EVENT_JOB, job, delay, sectionID, NodeSection.INPUT, destination);
	}

	/** Sends a job to a section of this node.
	 * @param job job to be attached to the message.
	 * @param delay Scheduling delay.
	 * @param destinationSection Destination section.
	 * @return a token to remove sent event
	 * @throws jmt.common.exception.NetException
	 */
	protected RemoveToken send(Job job, double delay, byte destinationSection) throws jmt.common.exception.NetException {
		if (auto && destinationSection != sectionID) {
			updateJobsList(job);
		}
		return ownerNode.send(NetEvent.EVENT_JOB, job, delay, sectionID, destinationSection, ownerNode);
	}

	/** Sends a job to this section.
	 * @param job job to be attached to the message.
	 * @param delay Scheduling delay.
	 * @return a token to remove sent event
	 * @throws jmt.common.exception.NetException
	 */
	protected RemoveToken sendMe(Job job, double delay) throws jmt.common.exception.NetException {
		return ownerNode.send(NetEvent.EVENT_JOB, job, delay, sectionID, sectionID, ownerNode);
	}
	
	/**
	 * QN-Java
	 * public version of sendMe
	 * @param job
	 * @param delay
	 * @return
	 * @throws jmt.common.exception.NetException
	 */
	public RemoveToken publicSendMe(Job job, double delay) throws jmt.common.exception.NetException{
	  return sendMe(job, delay);
	}
	
	/**
	 * QN-Java
	 * public send me an Ack event
	 * @param job
	 * @param delay
	 * @return
	 * @throws jmt.common.exception.NetException
	 */
	public RemoveToken publicAckMe(Job job, double delay) throws jmt.common.exception.NetException{
	  return ownerNode.send(NetEvent.EVENT_ACK, job, delay, sectionID, sectionID, ownerNode);
	}

	/**
	 * Removes a message previously sent
	 * @param token the token to remove the message
	 * @return true if message was removed, false otherwise
	 */
	protected boolean removeMessage(RemoveToken token) {
		return ownerNode.removeMessage(token);
	}

	/** Sends a message to a section of all the NetNodes of the QueueNetwork.
	 * @param event event tag.
	 * @param data  data to be attached to the message.
	 * @param delay Scheduling delay.
	 * @param destinationSection The destination section.
	 * @param nodeType Type of the node.
	 * @throws jmt.common.exception.NetException
	 */
	//TODO: not used
	protected void sendBroadcast(int event, Object data, double delay, byte destinationSection, int nodeType)
			throws jmt.common.exception.NetException {
		ownerNode.sendBroadcast(event, data, delay, sectionID, destinationSection, nodeType);
	}

	/** Checks if a message has been sent by this section. Note that this method
	 * could be called only after that the owner node has been linked to this
	 * section.
	 * @param message message to be checked.
	 * @return True if the message has been sent by this section,
	 * false otherwise.
	 */
	protected boolean isMine(NetMessage message) {
		return message.sentBy(sectionID, ownerNode);
	}

	private void updateJobsList(Job job) throws jmt.common.exception.NetException {
		JobInfo jobInfo = jobsList.lookFor(job);
		if (jobInfo != null) {
			jobsList.remove(jobInfo);
		}
	}

	void setOwnerNode(NetNode ownerNode) {
		this.ownerNode = ownerNode;
		//if (auto)
		jobsList = new LinkedJobInfoList(getJobClasses().size(), ownerNode.getOutputNodes().size() > 0);
		nodeLinked(ownerNode);
	}

	int receive(NetMessage message) throws jmt.common.exception.NetException {
		if (auto && (message.getEvent() == NetEvent.EVENT_JOB) && (message.getSourceSection()) != sectionID) {
			Job job = message.getJob();
			jobsList.add(new JobInfo(job));
		}
		switch (message.getEvent()) {
			default:
				return process(message);
		}
	}

	//NEW
	//@author Stefano Omini

	/**
	 * Redirects an event to a section of a node, without updating jobInfoList measures.
	 * @param event event to be sent.
	 * @param data  data to be attached to the message.
	 * @param delay Scheduling delay.
	 * @param destinationSection destination section.
	 * @param destination destination node.
	 * @return a token to remove sent event
	 * @throws jmt.common.exception.NetException
	 */
	protected RemoveToken redirect(int event, Object data, double delay, byte destinationSection, NetNode destination)
			throws jmt.common.exception.NetException {
		if ((event == NetEvent.EVENT_JOB) && ((destination != ownerNode) || ((destination == ownerNode) && (destinationSection != sectionID)))) {
			//it's a JOB event and the destination is not the owner node or it's the owner
			//node but the dest section is not this section
			updateJobsListAfterRedirect((Job) data);
		}
		return ownerNode.redirect(event, data, delay, sectionID, destinationSection, destination);
	}

	/**
	 * Updates the JobInfoList by removing this job, without updating measures
	 *
	 * @param job job to be removed.
	 * @throws jmt.common.exception.NetException
	 */
	private void updateJobsListAfterRedirect(Job job) throws jmt.common.exception.NetException {
		JobInfo jobInfo = jobsList.lookFor(job);
		if (jobInfo != null) {
			//removes the job without updating measures

			jobsList.removeAfterRedirect(jobInfo);

		}
	}

	/**
	 * Updates the JobInfoList by removing this job, without updating measures
	 *
	 * @param job job to be removed.
	 * @throws jmt.common.exception.NetException
	 */
	private void updateJobsListAfterDrop(Job job) throws jmt.common.exception.NetException {
		JobInfo jobInfo = jobsList.lookFor(job);
		if (jobInfo != null) {
			//removes the job
			jobsList.removeAfterDrop(jobInfo);
		} else {
			jobsList.dropJob(job);
		}
	}

	/**
	 * Sends an "ack" event to a section of a node, to inform it that
	 * the job previously sent has been dropped.
	 * The dropped job is removed from the job info list.
	 * <br>
	 * The general "send" method cannot be used, because it updates
	 * the job info list (by removing the job) only if the message contains
	 * a job event (on the contrary, after dropping a job an ack event is sent!)
	 *
	 * @param data  data to be attached to the message.
	 * @param delay Scheduling delay.
	 * @param destinationSection destination section.
	 * @param destination destination node.
	 * @return a token to remove sent event
	 * @throws jmt.common.exception.NetException
	 */
	protected RemoveToken sendAckAfterDrop(Object data, double delay, byte destinationSection, NetNode destination)
			throws jmt.common.exception.NetException {

		if (((destination != ownerNode) || ((destination == ownerNode) && (destinationSection != sectionID)))) {
			//the destination is not the owner node or it's the owner
			//node but the dest section is not this section

			/*
			the job can be dropped:
			- by a finite queue
			- by a blocking router (i.e. a router which controls the access of a blocking
			region)

			theorically the job should be inserted in the job list only if there is further space
			in queue or in blocking region
			on the contrary, in the simulator, while analyzing the situation (queue/region full or
			not) the job has been already inserted in the job info list
			however, when it's stated that the job must be dropped, it must be removed from
			the list and measures shouldn't be updated, otherwise they would consider
			this job in excess

			*/

			//removes job but does not update measures
			updateJobsListAfterDrop((Job) data);
		}
		return ownerNode.sendAckAfterDrop(NetEvent.EVENT_ACK, data, delay, sectionID, destinationSection, destination);
	}

	//end NEW

	/**
	 * Tells if jobinfolist at node section should be updated automatically
	 * @return true if it should be handled automatically, false otherwise
	 */
	boolean automaticUpdateNodeJobinfolist() {
		return nodeAuto;
	}
}
