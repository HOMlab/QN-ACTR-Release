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

package jmt.jmarkov;

import jmt.jmarkov.Queues.Processor;

public class Job implements Cloneable {
	private int jobId;
	//creationTime: when this object is created(when the previous one is entered in the queue
	private double creationTime;
	//in the creation of the job it is assigned
	private double enteringQueueTime;
	//the time which the job start to get service 
	private double enteringCpuTime;
	//ending of the service
	private double systemExitTime;
	//nextEvent: this can be equal to enteringQueueTime or enteringCpu or 0->if the job exit the system
	private double nextEventTime;
	//
	private int currentStateType;
	//if there are at least two server name(id) of the server
	private int processorId;

	private Processor processor;

	public static final int CURRENT_STATE_CREATED = 0;//created but not enter the Q
	public static final int CURRENT_STATE_IN_QUEUE = 1;//wait for an empty CPU	
	public static final int CURRENT_STATE_IN_CPU = 2;//it is running in the cpu
	public static final int CURRENT_STATE_EXIT_SYSTEM = 3;//job has serviced and exit from the system
	public static final int CURRENT_STATE_ANIMATION = 4;//job has serviced and exit from the system

	@Override
	public Object clone() {
		// TODO Auto-generated method stub
		Object clone = null;
		try {
			clone = super.clone();
		} catch (CloneNotSupportedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return clone;

	}

	public Job(int jobId, double time) {
		super();
		setCreationTime(time);
		setNextEventTime(0);
		setJobId(jobId);
		processor = null;
	}

	private void setCreationTime(double creationTime) {
		this.creationTime = creationTime;
		setCurrentStateType(CURRENT_STATE_CREATED);
	}

	//in the creation of the job it is assigned
	public void setEnteringQueueTime(double enteringQueueTime) {
		this.enteringQueueTime = enteringQueueTime;
		setNextEventTime(enteringQueueTime);
	}

	public void setEnteringCpuTime(double enteringCpuTime, Processor newProcessor) {
		this.enteringCpuTime = enteringCpuTime;
		setCurrentStateType(CURRENT_STATE_IN_CPU);
		setProcessor(newProcessor);
		setProcessorId(processor.getProcesserId());
	}

	public void setSystemExitTime(double exitingSystem) {
		this.systemExitTime = exitingSystem;
		setNextEventTime(exitingSystem);
	}

	public void setStateEnterQueue() {
		setCurrentStateType(CURRENT_STATE_IN_QUEUE);
		setNextEventTime(Double.MAX_VALUE);
	}

	public void setStateExitSystem() {
		setCurrentStateType(CURRENT_STATE_EXIT_SYSTEM);
		setNextEventTime(Double.MAX_VALUE);
		processor = null;
	}

	private void setNextEventTime(double nextEventTime) {
		this.nextEventTime = nextEventTime;
	}

	private void setCurrentStateType(int currentStateType) {
		this.currentStateType = currentStateType;
	}

	private void setProcessorId(int processorId) {
		this.processorId = processorId;
	}

	private void setProcessor(Processor processor) {
		this.processor = processor;
	}

	public double getCreationTime() {
		return creationTime;
	}

	public double getEnteringQueueTime() {
		return enteringQueueTime;
	}

	public double getEnteringCpuTime() {
		return enteringCpuTime;
	}

	public double getSystemExitTime() {
		return systemExitTime;
	}

	public double getNextEventTime() {
		return nextEventTime;
	}

	public int getCurrentStateType() {
		return currentStateType;
	}

	public int getProcessorId() {
		return processorId;
	}

	public Processor getProcessor() {
		return processor;
	}

	public int getJobId() {
		return jobId;
	}

	private void setJobId(int jobId) {
		this.jobId = jobId;
	}

	public void setAnimationTime(double animationTime) {
		setNextEventTime(animationTime);
		setCurrentStateType(Job.CURRENT_STATE_ANIMATION);
	}

}
