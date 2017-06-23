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

package jmt.gui.common;

import java.awt.Dimension;

import jmt.framework.data.ConstMap;
import jmt.gui.common.routingStrategies.FastestServiceRouting;
import jmt.gui.common.routingStrategies.LeastUtilizationRouting;
import jmt.gui.common.routingStrategies.ProbabilityRouting;
import jmt.gui.common.routingStrategies.RandomRouting;
import jmt.gui.common.routingStrategies.RoundRobinRouting;
import jmt.gui.common.routingStrategies.RoutingStrategy;
import jmt.gui.common.routingStrategies.ShortestQueueLengthRouting;
import jmt.gui.common.routingStrategies.ShortestResponseTimeRouting;

/**
 * Created by IntelliJ IDEA.
 * User: OrsotronIII
 * Date: 17-mag-2005
 * Time: 11.15.51
 * Modified by Bertoli Marco
 * 
 * Modified by Ashanka (May 2010):
 * Desc: Renamed the default of Queueing Station to Queue Stat. 
 *       in the inner class : ConstMap function: fill 
 */
public interface CommonConstants {

	public final static int CLASS_TYPE_OPEN = 0;
	public final static int CLASS_TYPE_CLOSED = 1;

	/** Table row height */
	public final static int ROW_HEIGHT = 19;

	public final static String STATION_TYPE_SOURCE = "Source";
	public final static String STATION_TYPE_SINK = "Sink";
	public final static String STATION_TYPE_TERMINAL = "Terminal";
	public final static String STATION_TYPE_ROUTER = "RoutingStation";
	public final static String STATION_TYPE_DELAY = "Delay";
	public final static String STATION_TYPE_SERVER = "Server";
	public final static String STATION_TYPE_FORK = "Fork";
	public final static String STATION_TYPE_JOIN = "Join";
	public final static String STATION_TYPE_LOGGER = "Logger";

	/** A map that decodes from station type to station name for the GUI */

	public static final ConstMap<String, String> STATION_NAMES = new ConstMap<String, String>() {
		@Override
		protected void fill() {
			putConst(STATION_TYPE_SOURCE, STATION_TYPE_SOURCE);
			putConst(STATION_TYPE_SINK, STATION_TYPE_SINK);
			putConst(STATION_TYPE_TERMINAL, STATION_TYPE_TERMINAL);
			putConst(STATION_TYPE_ROUTER, "Routing Station");
			putConst(STATION_TYPE_DELAY, STATION_TYPE_DELAY);
			//putConst(STATION_TYPE_SERVER, "Queueing Station");
			putConst(STATION_TYPE_SERVER, "Queue Stat.");
			putConst(STATION_TYPE_FORK, STATION_TYPE_FORK);
			putConst(STATION_TYPE_JOIN, STATION_TYPE_JOIN);
			putConst(STATION_TYPE_LOGGER, STATION_TYPE_LOGGER);
		}
	};

	/**Constants for selection of queueing strategy*/
	public final static String QUEUE_STRATEGY_LCFS = "LCFS";
	public final static String QUEUE_STRATEGY_FCFS = "FCFS";
	public final static String QUEUE_STRATEGY_STATION_PS = "Processor Sharing";
	public final static String QUEUE_STRATEGY_STATION_QUEUE = "Non-preemptive Scheduling";
	public final static String QUEUE_STRATEGY_STATION_QUEUE_PRIORITY = "Non-preemptive Scheduling (Priority)";

	/**Constants used for service time distributions*/
	public static final String SERVICE_LOAD_INDEPENDENT = "Load Independent";
	public static final String SERVICE_LOAD_DEPENDENT = "Load Dependent";
	public static final String SERVICE_ZERO = "Zero Service Time";

	/**Constants for selection of distributions*/
	public final static String DISTRIBUTION_CONSTANT = "Constant";
	public final static String DISTRIBUTION_EXPONENTIAL = "Exponential";
	public final static String DISTRIBUTION_NORMAL = "Normal";
	public final static String DISTRIBUTION_PARETO = "Pareto";
	public final static String DISTRIBUTION_ERLANG = "Erlang";
	public final static String DISTRIBUTION_HYPEREXPONENTIAL = "Hyperexponential";
	public final static String DISTRIBUTION_BURST = "Burst";
	//public final static String DISTRIBUTION_MAP= "MAP";
	public final static String DISTRIBUTION_MMPP2 = "MMPP2";
	public final static String DISTRIBUTION_GAMMA = "Gamma";
	public final static String DISTRIBUTION_UNIFORM = "Uniform";
	public final static String DISTRIBUTION_STUDENTT = "StudentT";
	public final static String DISTRIBUTION_POISSON = "Poisson";
	public final static String DISTRIBUTION_REPLAYER = "Replayer";

	public final static RoutingStrategy ROUTING_RANDOM = new RandomRouting();
	public final static RoutingStrategy ROUTING_ROUNDROBIN = new RoundRobinRouting();
	public final static RoutingStrategy ROUTING_PROBABILITIES = new ProbabilityRouting();
	public final static RoutingStrategy ROUTING_SHORTESTQL = new ShortestQueueLengthRouting();
	public final static RoutingStrategy ROUTING_SHORTESTRT = new ShortestResponseTimeRouting();
	public final static RoutingStrategy ROUTING_LEASTUTILIZATION = new LeastUtilizationRouting();
	public final static RoutingStrategy ROUTING_FASTESTSERVICE = new FastestServiceRouting();

	/**HTML formats for panels descriptions*/
	final static String HTML_START = "<html><body align=\"left\">";
	final static String HTML_END = "</body></html>";
	final static String HTML_FONT_TITLE = "<font size=\"4\"><b>";
	final static String HTML_FONT_NORM = "<font size=\"3\">";
	final static String HTML_FONT_TIT_END = "</b></font><br>";
	final static String HTML_FONT_NOR_END = "</font>";

	final static Dimension DIM_BUTTON_XS = new Dimension(60, 20);
	final static Dimension DIM_BUTTON_S = new Dimension(80, 20);
	final static Dimension DIM_BUTTON_M = new Dimension(110, 20);
	final static Dimension DIM_BUTTON_L = new Dimension(140, 20);

	public static final String FINITE_DROP = "Drop";
	public static final String FINITE_BLOCK = "BAS blocking";
	public static final String FINITE_WAITING = "Waiting Queue (no drop)";

	public final static String CLASSES_DESCRIPTION = HTML_START + HTML_FONT_TITLE + "Classes Characteristics" + HTML_FONT_TIT_END + HTML_FONT_NORM
			+ "Define type, name and parameters for each customer " + "class." + HTML_FONT_NOR_END + HTML_END;
	public final static String STATIONS_DESCRIPTION = HTML_START + HTML_FONT_TITLE + "Station Characteristics" + HTML_FONT_TIT_END + HTML_FONT_NORM
			+ "Define type and name for each station" + HTML_FONT_NOR_END + HTML_END;
	public final static String CONNECTIONS_DESCRIPTION = HTML_START + HTML_FONT_TITLE + "Station Connections" + HTML_FONT_TIT_END + HTML_FONT_NORM
			+ "Click on table entry (i,j) to connect station i to station j." + HTML_FONT_NOR_END + HTML_END;
	public final static String STATIONS_PAR_DESCRIPTION = HTML_START + HTML_FONT_TITLE + "Station Parameters" + HTML_FONT_TIT_END + HTML_FONT_NORM
			+ "For each station in the list, define the requested parameters" + HTML_FONT_NOR_END + HTML_END;
	public final static String MEASURES_DESCRIPTION = HTML_START + HTML_FONT_TITLE + "Performance Indices" + HTML_FONT_TIT_END + HTML_FONT_NORM
			+ "Define system performance indices to be collected and " + "plotted by the simulation engine." + HTML_FONT_NOR_END + HTML_END;

	public final static String SIMULATION_DESCRIPTION = HTML_START + HTML_FONT_TITLE + "Simulation Parameters" + HTML_FONT_TIT_END + HTML_FONT_NORM
			+ "Define simulation parameters and initial state." + HTML_FONT_NOR_END + HTML_END;

	public final static String BATCH_SIMULATION_DESCRIPTION = HTML_START + HTML_FONT_TITLE + "What-if analysis" + HTML_FONT_TIT_END + HTML_FONT_NORM
			+ "Enable parametric analysis and customize it." + HTML_FONT_NOR_END + HTML_END;

	public final static String REFSOURCE_DESCRIPTION = HTML_START + HTML_FONT_TITLE + "Reference Station" + HTML_FONT_TIT_END + HTML_FONT_NORM
			+ "Define reference station for each closed class. This is used " + "to calculate system throughput for that class." + HTML_FONT_NOR_END
			+ HTML_END;

	public final static String LDSERVICE_DESCRIPTION = HTML_START + HTML_FONT_TITLE + "Load Dependent Service Time Distribution" + HTML_FONT_TIT_END
			+ HTML_FONT_NORM + "Define the distribution and the values of service times for a range of number of jobs inside "
			+ "the station. Mean value of the distribution can be specified with an arithmetic "
			+ "expression, as a function of the current value of 'n' (see Help for operators)." + HTML_FONT_NOR_END + HTML_END;

	public final static String PARAMETRIC_ANALYSIS_DESCRIPTION = HTML_START + HTML_FONT_TITLE + "What-if analysis" + HTML_FONT_TIT_END
			+ HTML_FONT_NORM + "Define the type of what-if analysis to be performed and modify parameter options." + HTML_FONT_NOR_END + HTML_END;

	public final static String BLOCKING_DESCRIPTION = HTML_START + HTML_FONT_TITLE + "Finite Capacity Region Characteristics" + HTML_FONT_TIT_END
			+ HTML_FONT_NORM + "Define number, name, composition, global and class specific constraints for finite capacity regions."
			+ HTML_FONT_NOR_END + HTML_END;

	public final static String CONVERSION_WARNING_DESCRIPTION = HTML_START + HTML_FONT_TITLE + "Conversion performed" + HTML_FONT_TIT_END
			+ HTML_FONT_NORM + "Input model was automatically converted from <b>%PAR1%</b> to <b>%PAR2%</b>. "
			+ "The conversion was completed with the warnings shown below" + HTML_FONT_NOR_END + HTML_END;

	// String tokens
	public static final String PAR1 = "%PAR1%";
	public static final String PAR2 = "%PAR2%";

	public static final int MAX_NUMBER_OF_CLASSES = 30;
	public static final int MAX_NUMBER_OF_STATIONS = 50;
	public static final int MAX_NUMBER_OF_REGIONS = 15;

	public static final String ALL_CLASSES = "--- All Classes ---";
	public static final String INFINITE_CAPACITY = "Infinite Capacity";

	// Warnings for missing resources
	public static final String WARNING_CLASS_STATION = "User classes and stations have to be defined first";
	public static final String WARNING_CLASS = "User classes have to be defined first";
	public static final String WARNING_STATIONS = "Stations have to be defined first";
	public static final String WARNING_ROUTING = "Station outgoing connections undefined";

	// Application names (used for input file format conversion messages)
	public static final String JMVA = "JMVA";
	public static final String JSIM = "JSIM";
	public static final String JABA = "JABA";
	public static final String JWAT = "JWAT";
	public static final String SIMENGINE = "Simulation Engine XML data";
}
