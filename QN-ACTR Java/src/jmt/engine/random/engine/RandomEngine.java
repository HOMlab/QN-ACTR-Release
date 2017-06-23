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
 * RandomEngine.java
 *
 * Created on 11 novembre 2002, 11.51
 */

package jmt.engine.random.engine;

/**
 * Interface for all random numbers generators.
 *
 * @author  Federico Granata
 */
public abstract class RandomEngine {

	private static RandomEngine defaultEngine = new MersenneTwister();

	//NEW
	//@author Stefano Omini
	//should be set by the class Simulation to initialize the default RandomEngine
	private static long simulationSeed;

	//end NEW

	/**
	 * Makes this class non instantiable, but still lets others inherit from it.
	 */
	protected RandomEngine() {
	}

	/**
	 * Returns the default engine of uniform random number: the engine is seeded with the
	 * current time.
	 * Currently this engine is {@link jmt.engine.random.engine.MersenneTwister}.
	 */
	public static RandomEngine makeDefault() {
		return defaultEngine;
	}

	//NEW
	//@author Stefano Omini
	/**
	 * Sets the seed of the default engine of uniform random number.
	 * WARNING: this method must be called only by the class Simulation and only
	 * before starting the simulation, otherwise the engine is seeded using the
	 * current time.
	 *
	 */
	public static void setSeed(long seed) {
		simulationSeed = seed;
		//set the new seed
		((MersenneTwister) defaultEngine).setNewSeed(simulationSeed);
		return;
	}

	//end NEW

	/**
	 * Returns a 64 bit uniformly distributed random number in the open unit
	 * interval (0.0,1.0) (excluding 0.0 and 1.0).
	 */
	public abstract double nextDouble();

	/**
	 *Returns a 32 bit uniformly distributed random number in the closed interval
	 * [Integer.MIN_VALUE,Integer.MAX_VALUE]
	 * (including Integer.MIN_VALUE and  Integer.MAX_VALUE)
	 */
	public abstract int nextInt();

	/**
	 *Returns a 64 bit uniformly distributed random number in the closed interval
	 * [Long.MIN_VALUE,Long.MAX_VALUE]
	 * (including Long.MIN_VALUE and  Long.MAX_VALUE)
	 */
	public abstract long nextLong64();

	/**
	 * Returns a 32 bit uniformly distributed random number in the open unit
	 * interval (0.0,1.0)  (excluding 0.0 and 1.0).
	 */
	public abstract double raw();

} //end RandomStrategy Engine
