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
package jmt.engine.math;

/**
 * <p><b>Name:</b> Erlang</p> 
 * <p><b>Description:</b> 
 * This class provides methods to compute Erlang-B and Erlang-C 
 * formulas with a numerically stable algorithm.
 * </p>
 * <p><b>Date:</b> 13/apr/07
 * <b>Time:</b> 10:43:39</p>
 * @author Bertoli Marco
 * @version 1.0
 */
public class Erlang {

	/**
	 * Computes Erlang-B formula (probability of blocking) with a 
	 * recursive method to avoid numeric instability
	 * @param traffic The total traffic offered (in erlangs) 
	 * that is <em>arrival rate * service demand</em>. Must be greater than zero.
	 * @param servers number of servers. Must be greater than one.
	 * @return computed value
	 * @throws IllegalArgumentException if <code>traffic <= 0</code> or <code>servers <= 0</code>
	 */
	public static double erlangB(double traffic, int servers) throws IllegalArgumentException {
		if (traffic < 0) {
			throw new IllegalArgumentException("Traffic parameter must be greater than zero");
		}
		if (servers < 0) {
			throw new IllegalArgumentException("Number of servers must be greater than zero");
		}
		double a = 1.0, inv = 1.0;
		for (int i = 0; i < servers; i++) {
			a *= (servers - i) / traffic;
			inv += a;
		}
		return 1.0 / inv;
	}

	/**
	 * Computes Erlang-C formula (probability of queueing) 
	 * basing on Erlang-B evaluation
	 * @param traffic The total traffic offered (in erlangs) 
	 * that is <em>arrival rate * service demand</em>. Must be greater than zero.
	 * @param servers number of servers. Must be greater than one.
	 * @return computed value
	 * @throws IllegalArgumentException if traffic <= 0 or servers <= 0
	 */
	public static double erlangC(double traffic, int servers) throws IllegalArgumentException {
		double b = erlangB(traffic, servers);
		double rho = traffic / servers;
		return b / (1 - rho + rho * b);
	}
}
