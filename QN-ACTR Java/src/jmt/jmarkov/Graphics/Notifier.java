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
 * Created on 16-mar-2004 by Ernesto
 *
 */
package jmt.jmarkov.Graphics;

/**
 * MMQueues
 * --------------------------------------
 * 16-mar-2004 - Graphics/Notifier.java
 * 
 * this is used for notifying graphical 
 * panels. each panel is implemented 
 * with this class 
 * 
 */

public interface Notifier {

	//when new job adding to queue
	public void enterQueue(int jobId, double time);

	//when new job come to system and queue is full
	public void jobLost(int jobId, double time);

	//when new job removing to queue
	public void exitQueue(int jobId, double time);

	//when new job entering to processor
	public void enterProcessor(int jobId, int processorId, double time, double executionTime);

	//when job remove from the queue it should be enter one of the processor

	//when job exiting from the processor
	public void exitProcessor(int jobId, int processorId, double time);

	//when job exiting from the system
	public void exitSystem(int jobId, int processorId, double enterQueueTime, double enterCpuTime, double exitSystemTime);

	//when the job exit from the processor they whould be exit from the system.

	//resetting all the data in the ui
	public void reset();

	//this is for refreshing the job in the system(in the cpu) 
	public void updateProcessor(int jobId, int processorId, double remainingTime, double time);

	//this is for refreshing the job in the system(for queue animation)
	public void updateQueue(int jobId, double time);

}
