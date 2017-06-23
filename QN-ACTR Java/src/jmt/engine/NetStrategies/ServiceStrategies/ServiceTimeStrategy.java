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

package jmt.engine.NetStrategies.ServiceStrategies;

import jmt.common.exception.IncorrectDistributionParameterException;
import jmt.engine.NetStrategies.ServiceStrategy;
import jmt.engine.QueueNet.NodeSection;
import jmt.engine.random.Distribution;
import jmt.engine.random.Parameter;

/**
 * This strategy calculates the time of service of a job, using the specified
 * distribution and parameters.
 */

public class ServiceTimeStrategy extends ServiceStrategy {

	/** Service time distribution. */
	protected Distribution Distribution;

	/** Distribution parameters. */
	protected Parameter Parameter;

	/** Creates a new instance of ServiceTime.*/
	public ServiceTimeStrategy() {
	}

	/** Creates a new instance of ServiceTime.
	 * @param distribution service time distribution.
	 * @param parameter distribution parameter.
	 */
	public ServiceTimeStrategy(Distribution distribution, Parameter parameter) {
		this.Distribution = distribution;
		this.Parameter = parameter;
	}

	/**
	 * Sets the distribution
	 * @param Distribution the distribution to be used by this strategy.
	 * @return true, if distribution has been set.
	 */
	public boolean setDistribution(Distribution Distribution) {
		this.Distribution = Distribution;
		return true;
	}

	/**
	 * Sets the parameters of the distribution
	 * @param Parameter the parameter object
	 * @return true, if parameters have been set.
	 */
	public boolean setParameter(Parameter Parameter) {
		this.Parameter = Parameter;
		return true;
	}

	/**
	 * Calculates the service time using the specified distribution.
	 * @param CallingSection The node section which is calling this method.
	 * @return the value of service time.
	 */
	@Override
	public double wait(NodeSection CallingSection) {
		try {
			return Distribution.nextRand(Parameter);
		} catch (IncorrectDistributionParameterException e) {
			e.printStackTrace();

		}
		return 0;
	}
}
