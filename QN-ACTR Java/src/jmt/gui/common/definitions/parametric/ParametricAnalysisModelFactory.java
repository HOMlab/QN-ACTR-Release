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

package jmt.gui.common.definitions.parametric;

import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.definitions.SimulationDefinition;
import jmt.gui.common.definitions.StationDefinition;

/**
 * <p>Title: ParametricAnalysisModelFactory</p>
 * <p>Description: this class is used only to create instances of ParametricAnalysisDefinition</p>
 *
 * @author Francesco D'Aquino
 *         Date: 31-gen-2006
 *         Time: 16.59.46
 */

public class ParametricAnalysisModelFactory implements ParametricAnalysis {

	/**
	 *
	 * @param param the string describing the PAD to be created
	 * @param cd class definition
	 * @param sd station definition
	 * @param simd simulation definition
	 * @return a new ParametricAnalysisDefinition
	 */
	public static ParametricAnalysisDefinition createParametricAnalysisModel(String param, ClassDefinition cd, StationDefinition sd,
			SimulationDefinition simd) {
		if (param.equals(ParametricAnalysis.PA_TYPE_NUMBER_OF_CUSTOMERS)) {
			return new NumberOfCustomerParametricAnalysis(cd, sd, simd);
		} else if (param.equals(ParametricAnalysis.PA_TYPE_POPULATION_MIX)) {
			return new PopulationMixParametricAnalysis(cd, sd, simd);
		} else if (param.equals(ParametricAnalysis.PA_TYPE_SERVICE_TIMES)) {
			return new ServiceTimesParametricAnalysis(cd, sd, simd);
		} else if (param.equals(ParametricAnalysis.PA_TYPE_ARRIVAL_RATE)) {
			return new ArrivalRateParametricAnalysis(cd, sd, simd);
		} else if (param.equals(ParametricAnalysis.PA_TYPE_SEED)) {
			return new SeedParametricAnalysis(cd, sd, simd);
		} else {
			return null;
		}
	}

}
