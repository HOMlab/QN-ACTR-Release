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

import java.util.LinkedList;

import jmt.jmarkov.Job;

/**
 * Represent the queue (the queue of the system) and this queue contains 
 * the jobs waiting for processing
 *
 */
public class JobQueue extends LinkedList {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Initializer of JobQueue class 
	 *  
	 *  
	 */
	public JobQueue() {
		super();
	}

	/**
	 * add job in to system
	 */
	public synchronized void addToQueueVoid(Job newJob) {
		super.add(newJob);
	}

	/**
	 * Remove a job from the system
	 * 
	 * @return returns the first element in the queue
	 */
	public Job removeFromQueue() {
		return (Job) super.remove();
	}

	public void clearQueue() {
		super.clear();
	}

}
