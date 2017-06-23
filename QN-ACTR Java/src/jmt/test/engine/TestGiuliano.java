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

import jmt.common.exception.IncorrectDistributionParameterException;
import jmt.engine.NetStrategies.ServiceStrategy;
import jmt.engine.NetStrategies.RoutingStrategies.RandomStrategy;
import jmt.engine.NetStrategies.ServiceStrategies.ServiceTimeStrategy;
import jmt.engine.NodeSections.Delay;
import jmt.engine.NodeSections.Queue;
import jmt.engine.NodeSections.Router;
import jmt.engine.NodeSections.Server;
import jmt.engine.NodeSections.Terminal;
import jmt.engine.QueueNet.JobClass;
import jmt.engine.QueueNet.NetSystem;
import jmt.engine.QueueNet.SimConstants;
import jmt.engine.dataAnalysis.InverseMeasure;
import jmt.engine.random.Distribution;
import jmt.engine.random.Exponential;
import jmt.engine.random.ExponentialPar;
import jmt.engine.random.Parameter;
import jmt.engine.simEngine.Simulation;

/**
 * Tests examples with singleclass models.
 *
 * @author Stefano Omini
 *
 *
 */
public class TestGiuliano {

	/**
	 * Close model, with only one class and two stations: terminal, server.
	 */
	public static void closed_2s_seq(int N, double S2) {

		String stringS2 = Double.toString(S2).replace('.', '_');
		String testName = "pop_" + N + "_S2_" + stringS2;

		Distribution[] TerminalExp = new Distribution[1];

		Distribution[] serverExp1 = new Distribution[1];
		Parameter[] serverPar1 = new Parameter[1];

		Distribution[] serverExp2 = new Distribution[1];
		Parameter[] serverPar2 = new Parameter[1];

		Distribution[] DelayExp = new Distribution[1];
		Parameter[] DelayPar = new Parameter[1];

		DelayExp[0] = new Exponential();
		TerminalExp[0] = new Exponential();
		serverExp1[0] = new Exponential();
		serverExp2[0] = new Exponential();

		try {

			double servTime1 = 1.0; //fixed
			double servRate1 = 1 / servTime1;

			double servTime2 = S2; //variable
			double servRate2 = 1 / servTime2;

			double think_time = 0.0; //no delay
			double term_rate = 1 / think_time;

			DelayPar[0] = new ExponentialPar(term_rate);
			serverPar1[0] = new ExponentialPar(servRate1);
			serverPar2[0] = new ExponentialPar(servRate2);

		} catch (IncorrectDistributionParameterException e) {
			e.printStackTrace();
		}

		try {
			//add class
			JobClass[] classes = new JobClass[1];
			classes[0] = new JobClass("prima");
			Simulation sim = new Simulation(-1, testName);
			sim.addClasses(classes);

			//terminal
			RandomStrategy[] rrs_term = new RandomStrategy[1];
			rrs_term[0] = new RandomStrategy();

			int[] numJobs = { N }; //variable population

			ServiceTimeStrategy[] ds = { new ServiceTimeStrategy(DelayExp[0], DelayPar[0]) };

			sim.addNode("terminal", new Terminal(numJobs), new Delay(ds), new Router(rrs_term));

			//server 1
			ServiceStrategy[] serv_strat1 = new ServiceStrategy[1];
			serv_strat1[0] = new ServiceTimeStrategy(serverExp1[0], serverPar1[0]);

			RandomStrategy[] rrs_serv1 = new RandomStrategy[1];
			rrs_serv1[0] = new RandomStrategy();

			sim.addNode("server1", new Queue(-1, false, null, null), new Server(1, null, serv_strat1), new Router(rrs_serv1));

			//server 2
			ServiceStrategy[] serv_strat2 = new ServiceStrategy[1];
			serv_strat2[0] = new ServiceTimeStrategy(serverExp2[0], serverPar2[0]);

			RandomStrategy[] rrs_serv2 = new RandomStrategy[1];
			rrs_serv2[0] = new RandomStrategy();

			sim.addNode("server2", new Queue(-1, false, null, null), new Server(1, null, serv_strat2), new Router(rrs_serv2));

			//connections
			sim.addConnection("terminal", "server1");
			sim.addConnection("server1", "server2");
			sim.addConnection("server2", "terminal");

			//measure

			jmt.engine.dataAnalysis.Measure mis3_s1 = new InverseMeasure(testName + "_x1.out", .1, .1, 1000000, true);
			jmt.engine.dataAnalysis.Measure mis3_s2 = new InverseMeasure(testName + "_x2.out", .1, .1, 1000000, true);

			int through = SimConstants.THROUGHPUT;

			sim.addMeasure(through, "server1", mis3_s1, classes[0].getName());
			//sim.addMeasure(through, "server2", mis3_s2, classes[0].getName());

			sim.initialize();
			sim.run();

			System.out.println("tot time = " + NetSystem.getElapsedTime());

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public static void main(String[] args) {

		//closed_2s_seq();

		int[] vect_N = { 1, 2, 3, 5, 10, 20, 40, 60 };
		double[] vect_S2 = { 0, 0.25, 0.5, 0.75, 1 };

		int i, j;

		for (i = 0; i < vect_N.length; i++) {
			for (j = 0; j < vect_S2.length; j++) {

				System.out.println("simulazione con pop=" + vect_N[i] + " e con S2=" + vect_S2[j]);
				closed_2s_seq(vect_N[i], vect_S2[j]);

			}

		}

	}
}
