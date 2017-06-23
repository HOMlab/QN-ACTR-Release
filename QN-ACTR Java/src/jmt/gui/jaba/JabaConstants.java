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

package jmt.gui.jaba;

/**
 * This class contains some constants for jaba models.
 * @author alyf (Andrea Conti)
 * @version Date: 11-set-2003 Time: 16.40.25
 */
public interface JabaConstants {

	public static final int STATION_DELAY = 2;
	public static final int STATION_LI = 0; //load independent
	public static final int STATION_LD = 1; //load dependent

	public static final int SERVICE_DEMANDS_PROP = 100;

	/* Original Exact
	public static final String[] STATION_TYPENAMES_LD_ENABLED = {"Delay (Infinite Server)", "Load Independent", "Load Dependent"};
	*/

	// Jaba
	public static final String[] STATION_TYPENAMES_LD_ENABLED = { "Load Independent" }; //, "Load Dependent"};
	public static final String[] STATION_TYPENAMES = { "Load Independent" }; //, "Load Dependent"};
	//BEGIN Federico Dall'Orso 7/3/2005
	/*OLD
	public static final String[] STATION_TYPENAMES = {"Delay (Infinite Server)", "Load Independent"};
	*/
	//NEW
	//public static final String[] STATION_TYPENAMES = {"Delay (Infinite Server)","Load Independent","<HTML><font color=\"aaaaaa\">Load Dependent</font></HTML>"};
	//END Federico Dall'Orso 7/3/2005

	public static final int CLASS_CLOSED = 0;
	public static final int CLASS_OPEN = 1;

	//todo aggiungere classi chiuse quando possibile
	//public static final String[] CLASS_TYPENAMES = {"closed", "open"};
	public static final String[] CLASS_TYPENAMES = { "closed" };

	//todo limitazione delle classi a 3
	public static final int MAX_CLASSES = 3; // prima era limitato a 100
	public static final int MAX_STATIONS = 500;

	public static final String DESCRIPTION_CLASSES = "<html><body align=\"left\"><font size=\"4\"><b>Classes characteristics</b>"
			+ "</font><font size=\"3\"><br>Number, customized name, type of classes and number of "
			+ "customers (closed class) or arrival rate (open class).<br>"
			+ "You can add classes one by one or define total number at once.</font></body></html>";
	public static final String DESCRIPTION_STATIONS = "<html><body align=\"left\"><font size=\"4\"><b>Stations characteristics</b>"
			+ "</font><font size=\"3\"><br>Number, customized name and type of stations.<br>"
			+ "You can add stations one by one or define total number at once.</font></body></html>";
	public static final String DESCRIPTION_SERVICETIMES = "<html><body align=\"left\"><font size=\"4\"><b>Service Times</b>"
			+ "</font><font size=\"3\"><br>Input service times of each station for each class.<br>"
			+ "If station type is set to \"Load Dependent\", you can set values of service times"
			+ " for each number of customers by double-clicking on any button in station's row."
			+ "If you whish to input service demands instead of service times and visits, press "
			+ "\"Service Demands button.\"</font></body></html>";
	public static final String DESCRIPTION_VISITS = "<html><body align=\"left\"><font size=\"4\"><b>Visits</b>"
			+ "</font><font size=\"3\"><br>Average number of accesses of each class to the " + "station.</font></body></html>";
	public static final String DESCRIPTION_SERVICEDEMANDS = "<html><body align=\"left\"><font size=\"4\"><b>Service Demands</b>"
			+ "</font><font size=\"3\"><br>Input service demands of each station for each class.<br>"
			+ "If station type is set to \"Load Dependent\", you can set values of service demands"
			+ " for each number of customers by double-clicking on any button in station's row."
			+ "If you whish to input service times and visits instead of service demands, press "
			+ "\"Service Times and Visits button.\"</font></body></html>";
	public static final String DESCRIPTION_COMMENT = "<html><body align=\"left\"><font size=\"4\"><b>Comment</b>"
			+ "</font><font size=\"3\"><br>Input an optional short comment.<br></body></html>";
	public static final String DESCRIPTION_QUEUELENGTHS = "<html><body align=\"left\"><font size=\"4\"><b>QueueLength</b>"
			+ "</font><font size=\"3\"><br>Average number of customers in queue at each station for each class.<br></body></html>";
	public static final String DESCRIPTION_THROUGHPUTS = "<html><body align=\"left\"><font size=\"4\"><b>Throughput</b>"
			+ "</font><font size=\"3\"><br>Throughput for each class and each station.<br></body></html>";
	public static final String DESCRIPTION_RESPONSETIMES = "<html><body align=\"left\"><font size=\"4\"><b>Residence Times</b>"
			+ "</font><font size=\"3\"><br>Residence Time for each class and each station. The global aggregate is the response time of entire system.</body></html>";
	public static final String DESCRIPTION_UTILIZATIONS = "<html><body align=\"left\"><font size=\"4\"><b>Utilization</b>"
			+ "</font><font size=\"3\"><br>Average Utilization for each class and each station.<br></body></html>";
	public static final String DESCRIPTION_EMPTY = "<html><body align=\"left\"><font size=\"4\"><b>Results</b>"
			+ "</font><font size=\"3\"><br>Please solve the model to get results..<br></body></html>";

}
