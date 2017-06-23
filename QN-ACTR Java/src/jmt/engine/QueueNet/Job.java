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
 * [2013-06-24] add qnactrEntity
 * 
 */

package jmt.engine.QueueNet;

import javax.swing.JOptionPane;

import qnactr.objectDesigner.Entity;
import qnactr.sim.QnactrSimulation;

import jmt.engine.simEngine.SimSystem;

/**
 *	This class implements a generic job of a queue network.
 * 	@author Francesco Radaelli
 */
public class Job implements Cloneable {

  public Entity qnactrEntity; // QN-Java 
  
	//counter used to generate id
	private static int counter;
	//job ID
	private int Id;

	//Class of this job
	private JobClass JobClass;
	//used to compute residence time: born time is reset when a job enters a station
	private double BornTime;
	//used to compute system response time
	protected double systemEnteringTime;

	//NEW
	//@author Stefano Omini
	/*
	This fields are used with blocking region.
	The presence of an input station, in fact, modifies the route of some jobs:
	instead of being processed directly by the destination node, they are first
	redirected to the region input station (which check the capability of the
	blocking region) and then returned to the destination node, using the
	informations contained in this object.
	*/

	//true if this job has been redirected
	private boolean redirected = false;
	// the original destination of the job message
	private NetNode originalDestinationNode = null;

	//end NEW

	/** Creates a new instance of Job.
	 *  @param JobClass Reference to the class of the job.
	 */
	public Job(JobClass JobClass) {
		this.JobClass = JobClass;
		// Job Id is used only for logging
		this.Id = counter++;

		//NEW
		//@author Stefano Omini
		resetSystemEnteringTime();
		//end NEW
		
		//JOptionPane.showMessageDialog(null, "clock: " + SimSystem.clock() + "\nNew Job (Entity) created with ID: " + Id, "Job.java", JOptionPane.INFORMATION_MESSAGE); //CAO
		qnactrEntity = new Entity(); // QN-Java
		
		QnactrSimulation.globalAllEntitiesListAddLast(qnactrEntity);
		
		qnactrEntity.ownerJob = this;
		
	}

	/** Gets the class of this job.
	 * @return Value of property Class.
	 */
	public JobClass getJobClass() {
		return JobClass;
	}

	/** Sets born time.*/
	public void born() {
		BornTime = NetSystem.getTime();
	}

	/** Gets born time.
	 * @return Born time.
	 */
	public double getBornTime() {
		return BornTime;
	}

	/** Resets born time of the job. */
	void reborn() {
		BornTime = NetSystem.getTime();
	}

	/** Gets job id: job id is used mainly for logging.
	 * @return Job Id.
	 */
	public int getId() {
		return Id;
	}

	//NEW
	//@author Stefano Omini
	public void resetSystemEnteringTime() {
		systemEnteringTime = NetSystem.getTime();
	}

	public double getSystemEnteringTime() {
		return systemEnteringTime;
	}

	//end NEW

	//NEW
	//@author Stefano Omini

	/**
	 * Tells whether this job has been redirected (used for blocking regions)
	 * @return true if the job has been redirected
	 */
	public boolean isRedirected() {
		return redirected;
	}

	/**
	 * Sets <tt>redirected</tt> attribute
	 * @param redirected true to mark the job as redirected
	 */
	public void setRedirected(boolean redirected) {
		this.redirected = redirected;
	}

	/**
	 * Gets the destination node of this redirected job
	 * @return the destination node, if this job has been redirected, null otherwise
	 */
	public NetNode getOriginalDestinationNode() {
		if (redirected) {
			return originalDestinationNode;
		} else {
			return null;
		}
	}

	/**
	 * Sets the destination node of a redirected job and sets <tt>redirected</tt> to true
	 * @param originalDestinationNode the destination node
	 */
	public void setOriginalDestinationNode(NetNode originalDestinationNode) {
		this.originalDestinationNode = originalDestinationNode;
		redirected = true;
	}

	//end NEW

	/**
	 * 
	 * @return, a Job whose Entity is cloned with a new Tag
	 */
	 public Job cloneWithNewTag() {
	    Job returnJob = new Job ( this.getJobClass());
	    
	    returnJob.qnactrEntity = this.qnactrEntity.cloneWithNewTag();
	    returnJob.qnactrEntity.ownerJob = returnJob;
	    
	    return returnJob;
	  }
	
}
