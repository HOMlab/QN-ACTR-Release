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

import java.util.Date;
import java.util.LinkedList;
import java.util.Random;

import jmt.jmarkov.Graphics.Notifier;
import jmt.jmarkov.Queues.Arrivals;
import jmt.jmarkov.Queues.Processor;

public class Simulator implements Runnable {

	//list of jobs which contains jobs waiting in the simulator.
	private LinkedList<Job> jobList;
	//this contains list of jobs which exits from the system
	//it is no use anymore 
	//this could be used get the statistics of the exiting job but 
	//the statistics gathering by other component 
	//private LinkedList endJobList;

	private Notifier[] n;

	//current simulation time
	private double currentTime;// in millisecond
	//if lambda is zero this value is set to true
	//(if lambda set to zero new job will not create 
	private boolean lambdaZero = false;
	//Arrival class
	private Arrivals arrival;
	//array of processor
	private Processor[] processors;
	//it saves the data if the simulator is paused or not
	private boolean paused = false;
	//multiplier is used for multiplier for timer.(comes from simulation time slide bar)
	private double timeMultiplier = 1.0;

	private boolean running = false;

	private boolean started = false;

	public Simulator(Arrivals arrival, Processor[] processors, double timeMultiplier, Notifier[] n) {
		super();
		jobList = new LinkedList<Job>();
		// waitingJobList = new LinkedList();
		//endJobList = new LinkedList();
		this.arrival = arrival;
		currentTime = 0;
		this.processors = processors;
		// numberOfTotalProcessor = processors.length;
		setTimeMultiplier(timeMultiplier);
		this.n = n;
	}

	public void run() {
		running = true;
		started = true;
		// this is the simulation time till run command is called
		double currentTimeMultiplied;
		//when calling run getting the current real time
		long realTimeStart;
		//this is the time after return the thread.sleep
		long realTimeCurrent;
		currentTimeMultiplied = 0;
		realTimeStart = new Date().getTime();

		//this is the first job which is created
		//if job list is not empty this means run is called from the paused situation
		if (jobList.isEmpty()) {
			arrival.createJob(currentTime); // this is the first job which is created after pressed start
			//it is called in order calculate first arrival time to the system
		}

		//if there is still some job is waiting for processing it is running recursive
		//if paused the running will stop.
		while (jobList.size() > 0 && !paused) {
			//this is calculating how long system will sleep
			currentTimeMultiplied += (peekJob().getNextEventTime() - currentTime) / timeMultiplier;
			//this is calculating how long system will sleep
			realTimeCurrent = new Date().getTime() - realTimeStart;

			//this is for calculating if the system will pause or not?
			if ((long) currentTimeMultiplied > realTimeCurrent) {
				//System.out.println(currentTime + "\t" + currentTimeMultiplied + "\t" + realTimeCurrent);
				/*				try {
									Thread.sleep((long) currentTimeMultiplied - realTimeCurrent);
								} catch (InterruptedException e) {
									e.printStackTrace();
								}*/

				//System.out.println("\t\tSLEEP:\t" + ((long) currentTimeMultiplied - realTimeCurrent));
				realTimeCurrent = new Date().getTime() - realTimeStart;
			}
			//System.out.println(currentTime + "\t" + currentTimeMultiplied + "\t" + realTimeCurrent);

			Job job = dequeueJob();
			currentTime = job.getNextEventTime();
			switch (job.getCurrentStateType()) {
				case Job.CURRENT_STATE_CREATED:
					newJobArrival(job);
					break;
				case Job.CURRENT_STATE_ANIMATION:
					if ((long) currentTimeMultiplied + 300 > realTimeCurrent) {
						animate(job);
					}
					//				else
					//					System.out.println("no animation");
					break;
				case Job.CURRENT_STATE_IN_CPU:
					exitProcessor(job);
					break;

			}
		}

		running = false;
	}

	private void newJobArrival(Job job) {
		//the job whose arrival time has calculated entering the queue
		arrival.addQ(job, currentTime);
		//the new job is creating for calculating arrival time
		arrival.createJob(currentTime);
		// if (numberOfTotalProcessor > numberOfWorkingProcessor) {
		startProcess();// if there is an empty processor start it
		//for the red circle animation 
		createAnimation(job);
	}

	private void createAnimation(Job job) {
		Job cloneJob;
		double nextEventTime;
		if (!jobList.isEmpty()) {
			nextEventTime = peekJob().getNextEventTime();
		} else {
			nextEventTime = currentTime + 100;
		}
		//if (job.getCurrentStateType() != Job.CURRENT_STATE_EXIT_SYSTEM) {
		cloneJob = (Job) job.clone();
		cloneJob.setAnimationTime(currentTime + (nextEventTime - currentTime) * 1 / 4);
		this.enqueueJob(cloneJob);
		cloneJob = (Job) job.clone();
		cloneJob.setAnimationTime(currentTime + (nextEventTime - currentTime) * 2 / 4);
		this.enqueueJob(cloneJob);
		cloneJob = (Job) job.clone();
		cloneJob.setAnimationTime(currentTime + (nextEventTime - currentTime) * 3 / 4);
		this.enqueueJob(cloneJob);
		//}
	}

	private void animate(Job job) {
		arrival.animate(job, currentTime);
		for (Processor processor : processors) {
			processor.animate(currentTime);
		}

	}

	private void startProcess() {// if there is an empty processor start it
		int i;
		//assign randomly to free one
		// if (numberOfTotalProcessor > numberOfWorkingProcessor
		// &&
		// if (waitingJobList.size() > 0) {
		int numFreeProcessor = 0;
		for (i = 0; i < processors.length; i++) {
			if (!processors[i].isProcessing()) {
				numFreeProcessor++;
				// numberOfWorkingProcessor++;
				// break;
			}
		}
		if (numFreeProcessor != 0) {
			int assigned = new Random().nextInt(numFreeProcessor);
			for (i = 0; i < processors.length; i++) {
				if (!processors[i].isProcessing()) {
					if (assigned == 0) {
						processors[i].process(currentTime);
						break;
					}
					assigned--;
				}
			}
		}

	}

	private void exitProcessor(Job job) {
		job.getProcessor().endProcessing(currentTime);// this processor is stopped
		job.setStateExitSystem();// set the state of the job as a
		// finished
		//endJobList.add(job); // add exit list
		for (Notifier element : n) {
			element.exitSystem(job.getJobId(), job.getProcessorId(), job.getEnteringQueueTime(), job.getEnteringCpuTime(), job.getSystemExitTime());
		}
		startProcess();
		createAnimation(job);
	}

	public void enqueueJob(Job newJob) {// priority queue wrt their next job
		int i;
		for (i = 0; i < jobList.size(); i++) {
			if (jobList.get(i).getNextEventTime() > newJob.getNextEventTime()) {
				break;
			}
		}
		jobList.add(i, newJob);
	}

	public Job dequeueJob() {
		return jobList.removeFirst();
	}

	public Job peekJob() {
		return jobList.element();
	}

	// public void enqueueWaitingJob(Job job) {
	// waitingJobList.add(job);
	// }
	//
	// public Job dequeueWaitingJob() {
	// return (Job) waitingJobList.removeFirst();
	// }
	//
	// public Job peekWaitingJob() {
	// return (Job) waitingJobList.element();
	// }

	public boolean isLambdaZero() {
		return lambdaZero;
	}

	public void setLambdaZero(boolean lambdaZero) {
		this.lambdaZero = lambdaZero;
	}

	public void pause() {
		if (paused) {
			paused = false;
			start();
		} else {
			paused = true;
		}

	}

	public void start() {
		Thread simt = new Thread(this);
		simt.setDaemon(true);
		simt.start();
	}

	public void stop() {
		paused = true;
		started = false;
	}

	public double getTimeMultiplier() {
		return timeMultiplier;
	}

	public void setTimeMultiplier(double timeMultiplier) {
		this.timeMultiplier = timeMultiplier;
	}

	public boolean isRunning() {
		return running;
	}

	public boolean isStarted() {
		return started;
	}

}
