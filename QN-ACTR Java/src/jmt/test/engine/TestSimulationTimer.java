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
import jmt.engine.NetStrategies.RoutingStrategies.RoundRobinStrategy;
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
 * @author Stefano
 * @version 4-ott-2004 15.23.14
 */
public class TestSimulationTimer {
	/**
	 * Close model, with only one class and two stations: terminal, server.
	 */
	public static void closed_terminal() {

		Distribution[] serverExp = new Distribution[1];
		Parameter[] serverPar = new Parameter[1];

		Distribution[] DelayExp = new Distribution[1];
		Parameter[] DelayPar = new Parameter[1];

		DelayExp[0] = new Exponential();
		//TerminalExp[0] = new Exponential();
		serverExp[0] = new Exponential();

		try {
			double servTime = 1.0;
			double servRate = 1 / servTime;

			double think_time = 5.0;
			double term_rate = 1 / think_time;

			DelayPar[0] = new ExponentialPar(term_rate);
			//TerminalPar[0] = new ExponentialPar(lambda_terminal);
			serverPar[0] = new ExponentialPar(servRate);

		} catch (IncorrectDistributionParameterException e) {
			e.printStackTrace();
		}

		try {
			//add class
			JobClass[] classes = new JobClass[1];
			classes[0] = new JobClass("prima");

			long simTime = 200;
			Simulation sim = new Simulation(-1, "SimulationWithTimer", simTime);

			sim.addClasses(classes);

			//terminal
			RoundRobinStrategy[] rrs_term = new RoundRobinStrategy[1];
			rrs_term[0] = new RoundRobinStrategy();

			int[] numJobs = { 6 };

			ServiceTimeStrategy[] ds = { new ServiceTimeStrategy(DelayExp[0], DelayPar[0]) };

			sim.addNode("terminal", new Terminal(numJobs), new Delay(ds), new Router(rrs_term));

			//server
			ServiceStrategy[] serv_strat = new ServiceStrategy[1];
			serv_strat[0] = new ServiceTimeStrategy(serverExp[0], serverPar[0]);

			RoundRobinStrategy[] rrs_serv = new RoundRobinStrategy[1];
			rrs_serv[0] = new RoundRobinStrategy();

			sim.addNode("server", new Queue(-1, false, null, null), new Server(1, null, serv_strat), new Router(rrs_serv));

			//connections
			sim.addConnection("terminal", "server");
			sim.addConnection("server", "terminal");

			//measure

			jmt.engine.dataAnalysis.Measure mis1 = new jmt.engine.dataAnalysis.Measure("queuelength_s.out", .1, .1, 1000000, false, null);
			jmt.engine.dataAnalysis.Measure mis11 = new jmt.engine.dataAnalysis.Measure("queuelength_t.out", .1, .1, 1000000, false, null);
			jmt.engine.dataAnalysis.Measure mis2 = new jmt.engine.dataAnalysis.Measure("utilization_s.out", .1, .1, 1000000, false, null);
			jmt.engine.dataAnalysis.Measure mis3 = new InverseMeasure("throughput_s.out", .1, .1, 1000000, false);
			jmt.engine.dataAnalysis.Measure mis4 = new jmt.engine.dataAnalysis.Measure("queuetime_s.out", .1, .1, 1000000, false, null);
			jmt.engine.dataAnalysis.Measure mis5 = new jmt.engine.dataAnalysis.Measure("residencetime_s.out", .1, .1, 1000000, false, null);

			jmt.engine.dataAnalysis.Measure mis6 = new jmt.engine.dataAnalysis.Measure("utilization_t.out", .1, .1, 1000000, false, null);
			jmt.engine.dataAnalysis.Measure mis7 = new InverseMeasure("throughput_t.out", .1, .1, 1000000, false);
			jmt.engine.dataAnalysis.Measure mis8 = new jmt.engine.dataAnalysis.Measure("residencetime_t.out", .1, .1, 1000000, false, null);
			jmt.engine.dataAnalysis.Measure mis9 = new jmt.engine.dataAnalysis.Measure("responsetime_s.out", .1, .1, 1000000, false, null);
			jmt.engine.dataAnalysis.Measure mis10 = new jmt.engine.dataAnalysis.Measure("responsetime_t.out", .1, .1, 1000000, false, null);

			int queue_l = SimConstants.QUEUE_LENGTH;
			int utiliz = SimConstants.UTILIZATION;
			int through = SimConstants.THROUGHPUT;
			int queue_t = SimConstants.QUEUE_TIME;
			int resid_t = SimConstants.RESIDENCE_TIME;;
			int resp_t = SimConstants.RESPONSE_TIME;

			//sim.addMeasure(queue_l, "server", mis1, classes[0].getName());
			//sim.addMeasure(queue_l, "terminal", mis11, classes[0].getName());
			sim.addMeasure(utiliz, "server", mis2, classes[0].getName());
			sim.addMeasure(utiliz, "terminal", mis6, classes[0].getName());
			sim.addMeasure(through, "server", mis3, classes[0].getName());
			sim.addMeasure(through, "terminal", mis7, classes[0].getName());
			//sim.addMeasure(resid_t, "server", mis5, classes[0].getName());
			//sim.addMeasure(resid_t, "terminal", mis8, classes[0].getName());
			//sim.addMeasure(resp_t, "server", mis9, classes[0].getName());
			//sim.addMeasure(resp_t, "terminal", mis10, classes[0].getName());

			sim.initialize();
			sim.run();

			System.out.println("tot time = " + NetSystem.getElapsedTime());

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public static void main(String[] argsv) {
		closed_terminal();
	}
}
