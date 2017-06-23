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

import jmt.engine.NodeSections.Fork;

/**
 * <p>Title: ForkJob</p>
 * <p>Description: This is a special job created by Fork component. This is designed
 * to hold a reference to fork node and forked job.</p>
 *
 * @author Bertoli Marco
 *         Date: 13-mar-2006
 *         Time: 15.48.33
 */
public class ForkJob extends Job {
	/** Reference to original job */
	protected Job forkedJob;
	/** Number of child jobs */
	protected int jobNum;
	/** Reference to Fork node */
	protected Fork forkNode;

	/**
	 * Creates a new ForkJob, given number of jobs, original job and reference fork
	 * @param jobNum number of jobs in which this one must be splitted
	 * @param forkedJob original job to split
	 * @param forkNode reference to splitting node (used for blocking)
	 */
	public ForkJob(int jobNum, Job forkedJob, Fork forkNode) {
		super(forkedJob.getJobClass());
		this.forkedJob = forkedJob;
		this.jobNum = jobNum;
		this.forkNode = forkNode;
		this.systemEnteringTime = forkedJob.systemEnteringTime;
		this.born();
	}

	/**
	 * Gets original job that was splitted in this piece
	 * @return original job that was splitted in this piece
	 */
	public Job getForkedJob() {
		return forkedJob;
	}

	/**
	 * Gets number of split created from original job
	 * @return number number of split created from original job
	 */
	public int getForkedNumber() {
		return jobNum;
	}

	/**
	 * Gets the Fork node that created this split
	 * @return reference to Fork node that created this split
	 */
	public Fork getReferenceFork() {
		return forkNode;
	}
}
