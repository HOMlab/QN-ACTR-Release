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
 */
package jmt.jmarkov.Queues;

import java.util.Random;

import jmt.jmarkov.Queues.Exceptions.NoJobsException;
import jmt.jmarkov.Queues.Exceptions.NonErgodicException;

/**
 * Contains the algorithm of the queue M/M/1  where lambda represent the arrival of the job
 * in second and s as service time of the job in the processor.
 * M/M/1 is the poisson process of arrival and service time.   
 *
 */
public class MM1Logic implements QueueLogic {

	/**
	 * arrival rate [job/ms]
	 */
	protected double lambda;

	/**
	 * service time in the processor[ms]
	 */
	protected double s;

	/**
	 * random number generator
	 */

	protected Random rnd;

	/**
	 * Initialize the queue 
	 * @param lambda represent the medium of arrival of <i>Poisson process</i> <b>[job/s]</b>
	 * @param s represent the medium of the service time of <i>Poisson process</i> <b>[ms]</b>
	 */
	public MM1Logic(double lambda, double s) {
		this.lambda = lambda;
		this.s = s;
		rnd = new Random();
	}

	/* (non-Javadoc)
	 * @see Queues.QueueLogic#getArrivalTime()
	 */
	public double getArrivalTime() throws NoJobsException {
		if (lambda == 0) {
			throw new NoJobsException();
		}
		return (this.getTime(1000.0 / lambda));
	}

	/* (non-Javadoc)
	 * @see Queues.QueueLogic#getRunTime()
	 */
	public double getRunTime() {
		return (this.getTime(s * 1000.0));
	}

	/* (non-Javadoc)
	 * @see Queues.QueueLogic#getStatusProbability(int)
	 */
	public double getStatusProbability(int status) throws NonErgodicException {
		return ((1 - utilization()) * Math.pow(utilization(), status));
	}

	/**
	 * Calculate the interarrival time of poisson distribution with given medium 
	 */
	private double getTime(double media) {
		//forma esplicita della legge di distribuzione Poisson
		return ((-media * Math.log(rnd.nextDouble())));

	}

	/**
	 * setting the value of <b>lambda</b>  
	 * @param lambda new value <b>[job/s]</b>
	 */
	public void setLambda(double lambda) {
		this.lambda = lambda;
	}

	/**
	 * setting the value of <b>s</b> 
	 * @param s new value <b>[s]</b>
	 */
	public void setS(double s) {
		this.s = s;
	}

	/**
	 * Calculate the average jobs in the station 
	 * with respect to lambda and s
	 * 
	 * @return number of average jobs in the station
	 * 
	 * @exception NonErgodicException queue is not ergodic (U > 1 o U < 0)
	 */
	public double mediaJobs() throws NonErgodicException {
		return (utilization()) / (1.0 - utilization());
	}

	/**
	 * Calculate the utilizaion of the server
	 * with respect to parameters lambda and s
	 * 
	 * @return utilization
	 * 
	 * @exception NonErgodicException queue is not ergodic (U > 1 o U < 0)
	 */
	public double utilization() throws NonErgodicException {
		if ((lambda * s) > 1) {
			throw new NonErgodicException();
		} else {
			return (lambda * s);
		}
	}

	/* (non-Javadoc)
	 * @see Queues.QueueLogic#getMaxStates()
	 */
	public int getMaxStates() {
		return 0;
	}

	/**
	 * @param buffer
	 */
	public void setMaxStates(int buffer) {
		// the maximum queue length is infinite
	}

	/* (non-Javadoc)
	 * @see Queues.QueueLogic#throughput()
	 */
	public double throughput() throws NonErgodicException {
		return mediaJobs() / responseTime();
	}

	/* (non-Javadoc)
	 * @see Queues.QueueLogic#responseTime()
	 */
	public double responseTime() throws NonErgodicException {
		return s / (1.0 - utilization());
	}

	public double getLambda() {
		return lambda;
	}

	public double getS() {
		return s;
	}

	public int getNumberServer() {
		return 1;
	}
}
