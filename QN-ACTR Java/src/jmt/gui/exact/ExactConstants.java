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

package jmt.gui.exact;

/**
 * This class contains some constants for exact models.
 * @author alyf (Andrea Conti), Bertoli Marco
 * @version Date: 11-set-2003 Time: 16.40.25
 * 
 * @author Ashanka 
 * Added modifications regarding the renaming of QueueLength to Customer Number
 * @version Date: Aug-2009
 * 
 * @author Ashanka 
 * Added modifications regarding the renaming of Customer Number to Number of Customers which is station based
 * Added modifications regarding the renaming of Number of Customers to System Customer Number which is for global
 * @version Date: Sep-2009
 * 
 * @author Ashanka 
 * Added modifications regarding the renaming of System Customer Number to System Number of Customers
 * @version Date: Sep-2009
 */
public interface ExactConstants {

	public static final int STATION_DELAY = 0;
	public static final int STATION_LI = 1; //load independent
	public static final int STATION_LD = 2; //load dependent
	/** HTML gray text START*/
	public static final String GRAY_S = "<html><font color=\"aaaaaa\">";
	/** HTML gray text END*/
	public static final String GRAY_E = "</font></html>";

	public static final String[] STATION_TYPENAMES_LD_ENABLED = { "Delay (Infinite Server)", "Load Independent", "Load Dependent" };
	//BEGIN Federico Dall'Orso 7/3/2005
	/*OLD
	public static final String[] STATION_TYPENAMES = {"Delay (Infinite Server)", "Load Independent"};
	*/
	//NEW
	public static final String[] STATION_TYPENAMES = { "Delay (Infinite Server)", "Load Independent", GRAY_S + "Load Dependent" + GRAY_E };
	//END Federico Dall'Orso 7/3/2005

	public static final int CLASS_CLOSED = 0;
	public static final int CLASS_OPEN = 1;
	public static final String[] CLASS_TYPENAMES = { "closed", "open" };

	public static final int MAX_CLASSES = 30;
	public static final int MAX_STATIONS = 50;

	// Performance indices
	public static final String[] INDICES_TYPES = { "Throughput", "Number of Customers", "Residence Times", "Utilization", "System Power" };
	// Aggregate performance indices
	public static final String[] AGGREGATE_TYPES = { "System Response Time", "System Throughput", "System Number of Customers" };

	public static final String DESCRIPTION_CLASSES = "<html><body align=\"left\"><font size=\"4\"><b>Classes characteristics</b>"
			+ "</font><font size=\"3\"><br>Number, customized name, type of classes and number of "
			+ "customers (closed class) or arrival rate (open class). "
			+ "Add classes one by one or define total number at once.</font></body></html>";
	public static final String DESCRIPTION_STATIONS = "<html><body align=\"left\"><font size=\"4\"><b>Stations characteristics</b>"
			+ "</font><font size=\"3\"><br>Number, customized name and type of stations. "
			+ "Add stations one by one or define total number at once.</font></body></html>";
	public static final String DESCRIPTION_SERVICETIMES = "<html><body align=\"left\"><font size=\"4\"><b>Service Times</b>"
			+ "</font><font size=\"3\"><br>Input service times of each station for each class.<br>"
			+ "If the station is \"Load Dependent\" you can set the service times"
			+ " for each number of customers by double-click on \"LD Settings...\" button.<br>"
			+ "Press \"Service Demands\" button to enter service demands instead of service times and visits.</font></body></html>";
	public static final String DESCRIPTION_VISITS = "<html><body align=\"left\"><font size=\"4\"><b>Visits</b>"
			+ "</font><font size=\"3\"><br>Average number of visits to each station per class.</font></body></html>";
	public static final String DESCRIPTION_SERVICEDEMANDS = "<html><body align=\"left\"><font size=\"4\"><b>Service Demands</b>"
			+ "</font><font size=\"3\"><br>Input service demands of each station and class.<br>"
			+ "If the station is \"Load Dependent\" you can set the service demands"
			+ " for each number of customers by double-click on \"LD Settings...\" button.<br>"
			+ " Press \"Service Times and Visits\" button to enter service times and visits instead of service demands.</font></body></html>";
	public static final String DESCRIPTION_COMMENT = "<html><body align=\"left\"><font size=\"4\"><b>Comment</b>"
			+ "</font><font size=\"3\"><br>Input an optional short comment.<br></body></html>";
	public static final String DESCRIPTION_QUEUELENGTHS = "<html><body align=\"left\"><font size=\"4\"><b>Number of Customers</b>"
			+ "</font><font size=\"3\"><br>Average number of customers for each class at each station.</body></html>";
	public static final String DESCRIPTION_THROUGHPUTS = "<html><body align=\"left\"><font size=\"4\"><b>Throughput</b>"
			+ "</font><font size=\"3\"><br>Throughput for each class at each station.</body></html>";
	public static final String DESCRIPTION_RESPONSETIMES = "<html><body align=\"left\"><font size=\"4\"><b>Residence Times</b>"
			+ "</font><font size=\"3\"><br>Total time spent by each customer class at each station. Note that the aggregate values are weighted by relative per-class throughput."
			+ " The global aggregate is the system response time.</body></html>";
	public static final String DESCRIPTION_UTILIZATIONS = "<html><body align=\"left\"><font size=\"4\"><b>Utilization</b>"
			+ "</font><font size=\"3\"><br>Utilization of a customer class at the selected station. "
			+ "The utilization of a delay station is the average number of customers in the station (it may be greater than 1)</body></html>";
	public static final String DESCRIPTION_WHATIF_NONE = "<html><body align=\"left\"><font size=\"4\"><b>What-if analysis</b>"
			+ "</font><font size=\"3\"><br>Select a control parameter if you want to solve several models with its values "
			+ "changing in the selected range. " + "The performance indices will be shown " + "in a graph. </font></body></html>";
	public static final String DESCRIPTION_WHATIF_ARRIVAL_ALL = "<html><body align=\"left\"><font size=\"4\"><b>What-if analysis</b>"
			+ "</font><font size=\"3\"><br>Solve models increasing proportionally the arrival rates of the open classes. "
			+ "Starting from the actual value, the arrival rates are increased (or decreased) " + "by the percentage expressed in the 'to' value."
			+ "</font></body></html>";
	public static final String DESCRIPTION_WHATIF_ARRIVAL_ONE = "<html><body align=\"left\"><font size=\"4\"><b>What-if analysis</b>"
			+ "</font><font size=\"3\"><br>Solve models with increasing (or decreasing) arrival rate of selected open class."
			+ "</font></body></html>";
	public static final String DESCRIPTION_WHATIF_CUSTOMERS_ALL = "<html><body align=\"left\"><font size=\"4\"><b>What-if analysis</b>"
			+ "</font><font size=\"3\"><br>Solve models increasing proportionally the number of customers of the closed classes. "
			+ "Starting from the actual value, the population is increased (or decreased) " + "by the percentage expressed in the 'to' value.<br>"
			+ "Since only integer population values are allowed, the number of models executed can be very small." + "</font></body></html>";
	public static final String DESCRIPTION_WHATIF_CUSTOMERS_ONE = "<html><body align=\"left\"><font size=\"4\"><b>What-if analysis</b>"
			+ "</font><font size=\"3\"><br>Solve models with increasing (or decreasing) number of customers of selected closed class."
			+ "</font></body></html>";
	public static final String DESCRIPTION_WHATIF_DEMANDS_ALL = "<html><body align=\"left\"><font size=\"4\"><b>What-if analysis</b>"
			+ "</font><font size=\"3\"><br>Solve models increasing proportionally "
			+ "service demands at selected station for all classes. Starting from the actual value, service demands are increased (or decreased) "
			+ "by the percentage expressed in the 'to' value." + "</font></body></html>";
	public static final String DESCRIPTION_WHATIF_DEMANDS_ONE = "<html><body align=\"left\"><font size=\"4\"><b>What-if analysis</b>"
			+ "</font><font size=\"3\"><br>Solve models with increasing (or decreasing) service demand at selected station for selected class."
			+ "</font></body></html>";
	public static final String DESCRIPTION_WHATIF_MIX = "<html><body align=\"left\"><font size=\"4\"><b>What-if analysis</b>"
			+ "</font><font size=\"3\"><br>Solve models with different proportion of jobs between two closed classes, "
			+ "keeping constant the total number of jobs N (�i = Ni / N). It is required that Ni > 0."
			+ "<br>Since only integer Ni values are allowed, the number of models executed can be very small." + "</font></body></html>";
	public static final String DESCRIPTION_GRAPH = "<html><body align=\"left\"><font size=\"4\"><b>Graphical Results</b>"
			+ "</font><font size=\"3\"><br>Select performance indices to be plotted. Left-click and drag on the graph to zoom "
			+ "it, right-click to save it in EPS or PNG format.</body></html>";

	//Added by ASHANKA START
	public static final String DESCRIPTION_SYSPOWER = "<html><body align=\"left\"><font size=\"4\"><b>System Power</b>"
			+ "</font><font size=\"3\"><br>System Power (Aggregate or Per Class): Throughout Xi /Response Time Ri</body></html>";

	//Added by ASHANKA STOP

	/** What-if Analysis type constants */
	public static final String WHAT_IF_ARRIVAL = "Arrival Rates";
	public static final String WHAT_IF_CUSTOMERS = "Customer Numbers";
	public static final String WHAT_IF_MIX = "Population Mix";
	public static final String WHAT_IF_DEMANDS = "Service Demands";
}
