/**    
  * Copyright (C) 2007, Laboratorio di Valutazione delle Prestazioni - Politecnico di Milano

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
 * Created on 25-mar-2004 by Ernesto
 *
 */
package jmt.jmarkov.Queues;

/**
 * MMQueues
 * --------------------------------------
 * 25-mar-2004 - Queues/MM1dLogic.java
 * 
 * @author Ernesto
 */
public class MM1dLogic extends MM1Logic {

	private int max;

	/**
	 * Initializes the queue 
	 * @param lambda represent the medium of arrival of <i>Poisson process</i> <b>[job/s]</b>
	 * @param s represent the medium of the service time of <i>Poisson process</i> <b>[ms]</b>
	 * @param max represent the maximum length of the queue
	 */
	public MM1dLogic(double lambda, double s, int max) {
		super(lambda, s);
		this.max = max;
	}

	@Override
	public double getStatusProbability(int status) {
		double p = lambda * s;
		if ((status > max + 1) && (max > 0)) {
			return 0.0;
		}
		if (p == 1) {
			return 1.0 / (max + 2.0);
		}
		return ((1.0 - p) / (1.0 - Math.pow(p, max + 2)) * (Math.pow(p, status)));
	}

	@Override
	public int getMaxStates() {
		return max;
	}

	@Override
	public void setMaxStates(int max) {
		this.max = max;
	}

	@Override
	public double mediaJobs() {
		double p = lambda * s;
		if (p == 1) {
			return (max + 1.0) / 2.0;
		}
		return p / (1 - Math.pow(p, max + 2)) * ((1 - Math.pow(p, max + 1)) / (1 - p) - (max + 1) * Math.pow(p, max + 1));
	}

	private double pb() {//status probability of the last status
	//        double p = lambda * s;
	//        return ((1-p) * Math.pow(p, max+1)) / (1 - Math.pow(p, max+2));
		return getStatusProbability(max + 1);

	}

	@Override
	public double responseTime() {
		//        return  mediaJobs()/lambda/  (1 - pb()) ;
		return mediaJobs() / throughput();
	}

	@Override
	public double throughput() {
		//        double p = lambda * s;
		//        return lambda * (1 - Math.pow(p, max+1)) / (1 - Math.pow(p, max+2));
		return lambda * (1.0 - pb());
	}

	@Override
	public double utilization() {
		//        double p = lambda * s;
		//        return p * (1 - Math.pow(p, max+1)) / (1 - Math.pow(p, max+2));
		return lambda * (1 - pb()) * s;
	}

}
