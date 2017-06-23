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

/**
 * It represent the "Processor" (Server). This calculates how much time 
 * necessary in processor and each processor in the system one object is
 * generated  
 * 
 */
public final class Processor {

	private Notifier[] n;

	private JobQueue q;

	private QueueLogic ql;

	public Simulator sim;

	private boolean processing;// this show if this processor is doing a job or not

	private int processerId;

	private Job processingJob;

	public void process(double currentTime) {
		try {
			if (!this.isProcessing()) {
				if (!q.isEmpty()) {
					Job job = q.removeFromQueue();
					double rt = getExecutionTime();
					notifyGraphics("enterCpu", job.getJobId(), currentTime, rt);
					job.setEnteringCpuTime(currentTime, this);
					job.setSystemExitTime(currentTime + rt);
					sim.enqueueJob(job);
					setProcessing(true);
					processingJob = job;
				}
			}

		} catch (Exception e) {
			e.printStackTrace();
		}

	}

	public Processor(QueueLogic ql, JobQueue q, Notifier n[], int processorId/*, int jobsToDo*/) {
		this.ql = ql;
		this.q = q;
		this.n = n;
		setProcessing(false);
		processingJob = null;
		setProcesserId(processorId);
	}

	private double getExecutionTime() {
		return ql.getRunTime();
	}

	/**
	 * Notify the changes to the user interface
	 * 
	 */
	private void notifyGraphics(String gi, int jobId, double time, double executionTime) {
		int processorId = this.getProcesserId();
		if (gi == "enterCpu") {
			for (Notifier element : n) {
				element.exitQueue(jobId, time);
				element.enterProcessor(jobId, processorId, time, executionTime);
			}
		} else if (gi == "exitProcessor") {
			for (Notifier element : n) {
				element.exitProcessor(jobId, processorId, time);
			}
		} else if (gi == "animate") {
			for (Notifier element : n) {
				element.updateProcessor(jobId, this.getProcesserId(), processingJob.getSystemExitTime() - time, time);
			}
		}

	}

	public void animate(double time) {
		if (processingJob != null) {
			notifyGraphics("animate", processingJob.getJobId(), time, 0);
		}

	}

	public boolean isProcessing() {
		return processing;
	}

	private void setProcessing(boolean running) {
		if (!running) {
			processingJob = null;
		}
		this.processing = running;
	}

	public void endProcessing(double time) {
		notifyGraphics("exitProcessor", this.processingJob.getJobId(), time, 0);
		setProcessing(false);
	}

	public int getProcesserId() {
		return processerId;
	}

	public void setProcesserId(int processerId) {
		this.processerId = processerId;
	}
}
