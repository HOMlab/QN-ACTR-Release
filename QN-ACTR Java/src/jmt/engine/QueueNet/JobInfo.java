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

/**
 * This class is a generic job information storage class.
 * @see LinkedJobInfoList
 * @author Francesco Radaelli
 */
public class JobInfo {

	private Job job;

	private double time;

	/** Creates a new instance of JobInfo object.
	 * @param Job Reference to the job to be described.
	 */
	public JobInfo(Job Job) {
		this.job = Job;
		time = NetSystem.getTime();
	}

	/** Gets the job referenced by this JobInfo
	 * @return Referenced job.
	 */
	public Job getJob() {
		return job;
	}

	/** Gets Time property value .
	 * @return Time property value.
	 */
	public double getTime() {
		return time;
	}

	/** Sets Time property value .
	 * @param time Time property value.
	 */
	protected void setTime(double time) {
		this.time = time;
	}

}
