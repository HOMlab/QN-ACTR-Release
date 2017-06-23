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

package jmt.test.engine;

import jmt.engine.testSystem.BatchTest;
import jmt.engine.testSystem.RandomMVAModelGenerator;

/**
 * Created by IntelliJ IDEA.
 * User: Stefano
 * Date: 26-apr-2005
 * Time: 16.41.43
 * To change this template use File | Settings | File Templates.
 */
public class BatchTestSimulation {

	public static void main(String[] args) {

		//parameters: classes - stations - models - model type - simulation runs
		//comboTest(1, 3, 5, RandomMVAModelGenerator.OPEN_MODEL, 1);

		//parameters: classes - stations - model seed - model type - simulation runs - simseed

		//Modello con una stazione poco visitata
		//test_fixedSimSeedAndModelSeed_runs(2, 3, 123123, 1, 1, 232323);

		//test_fixedSimSeedAndModelSeed_runs(1, 1, 123123, 1, 1, 232323);

		//dura circa 60 secondi
		//test_fixedSimSeedAndModelSeed_runs(3,3,123,0,1,123123);

		//String path = "D://JMTtest//ERRrandomModel_1.xml";
		//String path = "D://JMTtest//randomModel_5.xml";
		//String path = "D://JMTtest//2_randomModel_5.xml";

		//String path = "D://JMTtest//senza_preload.xml";
		//String path = "D://JMTtest//preload.xml";
		//String path = "D://JMTtest//preload3.xml";
		//String path = "D://JMTtest//limite_szero_2c2s.xml";
		//String path = "D://JMTtest//limite_vzero_2c2s.xml";
		//String path = "D://JMTtest//solverstep0.xml";
		//String path = "D://JMTtest//test_nonFCFS.xml";
		//String path = "D://JMTtest//randomModel_open_1.xml";

		//String path = "D://JMTtest//visite_mva_prova.xml";
		//String path = "D://JMTtest//2c2s.xml";
		//String path = "D://JMTtest//mva_closed.xml";
		//String path = "D://JMTtest//sim_test_base.xml";
		//BatchTest.test_fixedPath(path);

		BatchTest.comboTest(2, 2, 4, RandomMVAModelGenerator.OPEN_MODEL, 1);
		//BatchTest.comboTest(2, 2, 4, RandomMVAModelGenerator.CLOSED_MODEL, 1);
		//BatchTest.comboTest(2, 2, 4, RandomMVAModelGenerator.MIXED_MODEL, 1);

		//BatchTest.test_fixedSimSeedAndModelSeed(2,2,232323,RandomMVAModelGenerator.OPEN_MODEL, 232323);
		//BatchTest.comboTest(2, 2, 10, RandomMVAModelGenerator.OPEN_MODEL, 1);

		//BatchTest.test_fixedSimSeedAndModelSeed(2,2,222,RandomMVAModelGenerator.CLOSED_MODEL,333);

	}
}
