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


package jmt.engine.QueueNet;

import java.util.LinkedList;
import java.util.ListIterator;

import javax.swing.JOptionPane;

import jmt.common.exception.NetException;
import jmt.engine.NodeSections.BlockingQueue;
import jmt.engine.NodeSections.Queue;
import jmt.engine.dataAnalysis.Measure;

/**
 * This class implements a queue network.
 * @author Francesco Radaelli, Bertoli Marco (Added support for global measures)
 */
public class QueueNetwork {

	/* Symbolic name for reference node in the network: it should be used
	* within broadcast communication.
	*/
	public static final int REFERENCE_NODE = 0x0001;

	/* Symbolic name for normal node in the network: it should be used
	* within broadcast communication.
	*/
	public static final int NODE = 0x0002;

	/** Behaviour ID: Aborts the simulation when a measure has been obtained.*/
	public final static int BEHAVIOUR_ABORT = 0x001;
	/** Behaviour ID: Stops the simulation when a measure has been obtained.*/
	public final static int BEHAVIOUR_STOP = 0x002;
	/** Behaviour ID: Continues the simulation when a measure has been obtained.*/
	public final static int BEHAVIOUR_CONTINUE = 0x003;
	/** Behaviour ID: Continues and waits other measures when a measure has been obtained.*/
	public final static int BEHAVIOUR_OBTAIN_ALL_MEASURES_THEN_ABORT = 0x004;
	/** Behaviour ID: Continues and waits other measures when a measure has been obtained.*/
	public final static int BEHAVIOUR_OBTAIN_ALL_MEASURES_THEN_STOP = 0x005;

	/** State ID: Initial state, the network is ready to be started.*/
	public final static int STATE_READY = 0x0001;
	/** State ID: The network is running.*/
	public final static int STATE_RUNNING = 0x0002;
	/** State ID: The network has been stopped.*/
	public final static int STATE_STOPPED = 0x0003;
	/** State ID: The network has been aborted.*/
	public final static int STATE_ABORTED = 0x0004;
	/** State ID: The network is in final state.*/
	public final static int STATE_FINAL = 0x0005;

	//dekkar (Federico Granata)
	/** Measure ID: queue length */
	public final static int QUEUE_LENGTH = 0x001;
	/** Measure ID: residence time */
	public final static int RESIDENCE_TIME = 0x002;
	/** Measure ID: residence time */
	public final static int SERVICE_TIME = 0x003;

	private NodeList referenceNodes, nodes;

	private LinkedList<Measure> measures;

	private JobClassList jobClasses;

	private String name;

	private int behaviour, state;

	private GlobalJobInfoList jobInfoList;

	/** Creates a new instance of QueueNetwork. */
	public QueueNetwork(String name) {
		nodes = new NodeList();
		referenceNodes = new NodeList();
		jobClasses = new JobClassList();
		this.name = name;
		measures = new LinkedList<Measure>();
		behaviour = BEHAVIOUR_OBTAIN_ALL_MEASURES_THEN_STOP;
		state = STATE_READY;
	}

	/** Adds a generic node to the newtork. If the node has no inputs the node
	 * is set as reference node.
	 * @param node node to be added.
	 */
	public void addNode(NetNode node) throws jmt.common.exception.NetException {
		nodes.add(node);

		if (node.getInputNodes().size() == 0) {
			//if no input nodes are present, it's presumed that this node
			//must be a source of jobs
			referenceNodes.add(node);
		}

		node.setNetwork(this);
		
		//JOptionPane.showMessageDialog(null, "addNode: " + node.getName() , "QueueNetwork.java", JOptionPane.INFORMATION_MESSAGE); //CAO
	}

	/** Adds a node to the newtork and forces it to be a reference node.
	 * @param node node to be added.
	 */
	public void addReferenceNode(NetNode node) throws jmt.common.exception.NetException {
		nodes.add(node);
		referenceNodes.add(node);
		node.setNetwork(this);
	}

	/** Adds a new job class to the network.
	 *  @param jobClass Job class to be added.
	 */
	public void addJobClass(JobClass jobClass) {
		jobClasses.add(jobClass);
		jobClass.setId(jobClasses.indexOf(jobClass));
	}

	/**
	 * Gets the list of the queue network nodes.
	 * @return Queue network nodes.
	 */
	public NodeList getNodes() {
		return nodes;
	}

	//dekkar (Federico Granata)
	/**
	 * Gets the NetNode with the specified name...
	 *
	 * @param name the name of the node
	 * @return the specified node
	 */
	public NetNode getNode(String name) {
		return nodes.get(name);
	}

	//NEW
	//@author Stefano Omini

	/**
	 * Gets the JobClass with the specified name...
	 *
	 * @param name the name of the JobClass
	 * @return the job class. Null if it doesn't exist.
	 */
	public JobClass getJobClass(String name) {
		return jobClasses.get(name);
	}

	//end NEW

	//dekkar (Federico Granata)
	/**Gets the specified measure from the node with the specified name.
	 *
	 * @param nodeName name of the node
	 * @param measureID the measure id (chosen from QUEUE_LENGTH, RESIDENCE_TIME, SERVICE_TIME)
	 * @return the measure
	 * @throws jmt.common.exception.NetException
	 */
	public double getMeasure(String nodeName, int measureID) throws jmt.common.exception.NetException {
		double measure = 0;
		boolean found = false;

		switch (measureID) {
			case QUEUE_LENGTH:
				measure = nodes.get(nodeName).getDoubleNodeProperty(NetNode.PROPERTY_ID_RESIDENT_JOBS);
				found = true;
				break;
			case RESIDENCE_TIME:
				measure = nodes.get(nodeName).getDoubleNodeProperty(NetNode.PROPERTY_ID_RESIDENCE_TIME);
				found = true;
				break;
			case SERVICE_TIME:
				measure = nodes.get(nodeName).getSection(NodeSection.SERVICE).getDoubleSectionProperty(NodeSection.PROPERTY_ID_RESIDENCE_TIME);
				found = true;
				break;
		}
		if (found) {
			return measure;
		} else {
			throw new jmt.common.exception.NetException(this, 0, "measure not available");
		}
	}

	/** Gets the list of the queue network reference nodes.
	 * @return Queue network reference nodes.
	 */
	public NodeList getReferenceNodes() {
		return referenceNodes;
	}

	/** Gets the list of the queue network job classes.
	 * @return Queue network job classes.
	 */
	public JobClassList getJobClasses() {
		return jobClasses;
	}

	/** Gets network name
	 * @return Network name.
	 */
	public String getName() {
		return name;
	}

	/** Adds a new measure to the network.
	 * @param measure Reference to the measure to be added.
	 */
	public void addMeasure(Measure measure) {
		// If GlobalJobInfoList is not set, creates it
		// (at this point all classes should be declared)
		if (jobInfoList == null) {
			jobInfoList = new GlobalJobInfoList(jobClasses.size());
		}

		//sets the reference to network
		measure.setNetwork(this);

		measures.add(measure);
	}

	/**
	 * Returns Global jobInfoList associated with this queue network. This is used
	 * to calculate global measures
	 * @return Global JobInfoList of this network
	 */
	public GlobalJobInfoList getJobInfoList() {
		return jobInfoList;
	}

	/**
	 * Gets the measures
	 */
	public LinkedList<Measure> getMeasures() {
		return measures;
	}

	/**
	 * Gets the network behaviour (see behaviour constants).
	 *
	 */
	public int getBehaviour() {
		return behaviour;
	}

	/**
	 * Sets the network behaviour (see behaviour constants).
	 */
	public void setBehaviuor(int behaviour) {
		this.behaviour = behaviour;
	}

	/**
	 * Sets the network state (see state constants).
	 */
	void setState(int state) {
		this.state = state;
	}

	/**
	 * Gets the network state (see state constants).
	 */
	public int getState() {
		if (state == STATE_FINAL || state == STATE_READY) {
			return state;
		}
		boolean flag = false;
		ListIterator<NetNode> nodeList = nodes.listIterator();
		NetNode node;
		while (nodeList.hasNext()) {
			node = nodeList.next();
			if (node.isRunning()) {
				flag = true;
			}
		}
		if (flag) {
			return state;
		} else {
			state = STATE_FINAL;
			return STATE_FINAL;
		}
	}

	//NEW
	//@author Stefano Omini
	/**
	 * Get the total number of dropped jobs in the whole network for the specified
	 * class
	 * @param jobClass the job class
	 * @return the total number of dropped jobs in the whole network for the specified
	 * class
	 */
	public int getDroppedJobs(JobClass jobClass) {

		//dropped jobs
		int dropped = 0;
		int classIndex = jobClass.getId();

		for (int s = 0; s < nodes.size(); s++) {
			NetNode node = nodes.get(s);
			NodeSection inputSection;
			try {
				inputSection = node.getSection(NodeSection.INPUT);

				//jobs can be dropped by a finite queue or by a blocking region

				if (inputSection instanceof BlockingQueue) {

					//input section of a blocking region
					dropped += ((BlockingQueue) inputSection).getDroppedJobPerClass(classIndex);

				} else if (inputSection instanceof Queue) {

					//check
					boolean infinite = ((Queue) inputSection).hasInfiniteQueue();
					if (!infinite) {
						//finite queue
						dropped += ((Queue) inputSection).getDroppedJobPerClass(classIndex);
					}

				}
			} catch (NetException ne) {
				continue;
			}
		}

		return dropped;

	}

	//end NEW

	//NEW
	//@author Stefano Omini
	/**
	 * Get the total number of dropped jobs in the whole network
	 * @return the total number of dropped jobs in the whole network
	 */
	public int getDroppedJobs() {

		//dropped jobs
		int dropped = 0;

		for (int s = 0; s < nodes.size(); s++) {
			NetNode node = nodes.get(s);
			NodeSection inputSection;
			try {
				inputSection = node.getSection(NodeSection.INPUT);

				//jobs can be dropped by a finite queue or by a blocking region

				if (inputSection instanceof BlockingQueue) {

					//input section of a blocking region
					dropped += ((BlockingQueue) inputSection).getDroppedJobs();

				} else if (inputSection instanceof Queue) {

					//check
					boolean infinite = ((Queue) inputSection).hasInfiniteQueue();
					if (!infinite) {
						//finite queue
						dropped += ((Queue) inputSection).getDroppedJobs();
					}

				}
			} catch (NetException ne) {
				continue;
			}
		}

		return dropped;

	}
	//end NEW

}
