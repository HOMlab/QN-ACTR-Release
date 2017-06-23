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

/*
 * Created on 11-mar-2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package jmt.jmarkov.Queues;

import jmt.jmarkov.Job;
import jmt.jmarkov.Simulator;
import jmt.jmarkov.Graphics.Notifier;
import jmt.jmarkov.Queues.Exceptions.InfiniteBufferException;
import jmt.jmarkov.Queues.Exceptions.NoJobsException;

/**
 * 
 * Representing the arrival of the job. 
 * it generates the job and enqueue to the queue
 * 
 */
public final class Arrivals {

	//if the arrival is limited then jobsToDo must be greater than zero
	private boolean limited = false;
	private int jobsToDo = 0;

	private QueueLogic ql;

	public Simulator sim;

	//	private boolean noJobs = true;	

	private Notifier n[];

	private JobQueue q;

	//	private double at;	

	private int jobIdCounter;

	public void createJob(double currentTime) {
		double interarrivalTime;
		if ((jobsToDo > 0) || (!limited)) {
			try {
				interarrivalTime = getInterarrivalTime();
			} catch (NoJobsException e) {
				// lambda is zero
				sim.setLambdaZero(true);
				return;
			}
			jobIdCounter++;
			Job job = new Job(jobIdCounter, currentTime);
			job.setEnteringQueueTime(interarrivalTime + currentTime);
			sim.enqueueJob(job);
		}
	}

	public void addQ(Job job, double time) {
		try {
			if (limited) {
				jobsToDo--;
			}
			if (ql.getMaxStates() == 0 || q.size() < ql.getMaxStates()) {
				q.addToQueueVoid(job);

				job.setStateEnterQueue();
				notifyGraphics("addQ", job.getJobId(), time);
			} else {
				notifyGraphics("lostJob", job.getJobId(), time);
			}
		} catch (InfiniteBufferException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public void animate(Job job, double time) {
		notifyGraphics("animate", job.getJobId(), time);

	}

	public Arrivals(QueueLogic ql, JobQueue q) {
		setParameters(ql, q);
	}

	public Arrivals(QueueLogic ql, JobQueue q, Notifier n[], int jobsToDo) {
		setParameters(ql, q, n, jobsToDo);
	}

	private void setParameters(QueueLogic ql, JobQueue q) {
		this.ql = ql;
		this.q = q;
		this.jobsToDo = 0;
		this.limited = false;
		jobIdCounter = 0;
	}

	private void setParameters(QueueLogic ql, JobQueue q, Notifier n[], int jobsToDo) {
		setParameters(ql, q);
		this.n = n;
		if (jobsToDo > 0) {
			this.limited = true;
			this.jobsToDo = jobsToDo;
		}
	}

	/**
	 * Notify the changes to the user interface
	 * 
	 */
	private void notifyGraphics(String gi, int jobId, double time) {
		if (gi == "addQ") {
			for (Notifier element : n) {
				element.enterQueue(jobId, time);
			}
		} else if (gi == "lostJob") {
			for (Notifier element : n) {
				element.jobLost(jobId, time);
			}
		} else if (gi == "animate") {
			for (Notifier element : n) {
				element.updateQueue(jobId, time);
			}
		}
	}

	/**
	 * Get the interarrival time from queue logic
	 * 
	 */
	private double getInterarrivalTime() throws NoJobsException {
		return ql.getArrivalTime();

	}

	/**
	 * returns if there are more jobs to create
	 * 
	 */
	public boolean moreJobToDo() {
		return ((jobsToDo > 0) || (!limited));
	}

}
