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

import jmt.jmarkov.Queues.Exceptions.InfiniteBufferException;
import jmt.jmarkov.Queues.Exceptions.NoJobsException;
import jmt.jmarkov.Queues.Exceptions.NonErgodicException;

/**
 * This interface represent the logic of the queue types: the arrival 
 * and the execution of the process/job. This changed with respect to 
 * inputs arrival rate(lambda), service time, number of station capacity
 * or number of servers. 
 * Each queue type could have different inputs. 
 * 
 */
public interface QueueLogic {

	//methods:

	/**
	 * Calculates the probability of each status. 
	 * @return the probability of the status
	 * @param number of elements in the status
	 */
	public double getStatusProbability(int status) throws NonErgodicException;

	/**
	 * Calculates the service time of the job.(ms) 
	 * @return service time
	 */
	public double getRunTime();

	/**
	 * Calculates the arrival of the next job.(ms)
	 * @return interarrival time
	 */
	public double getArrivalTime() throws NoJobsException;

	/**
	 * 
	 * Returns the maximum number of the jobs in the queue(only buffer)
	 * @return 0 if the buffer is ideal(infinite), a positive number,  otherwise
	 */
	public int getMaxStates() throws InfiniteBufferException;

	/**
	 * Calculates the average number of the jobs in the queue(Q)
	 * 
	 */
	public double mediaJobs() throws NonErgodicException;

	/**
	 * Calculates the average utilization of the queue(U)
	 */
	public double utilization() throws NonErgodicException;

	/**
	 * Calculates the average  throughput(X) 
	 *  
	 */
	public double throughput() throws NonErgodicException;

	/**
	 * Calculates the average response time(R) 
	 * 
	 */
	public double responseTime() throws NonErgodicException;

	/**
	 * Returns the number of the servers.  
	 * 
	 */
	public int getNumberServer();

}
