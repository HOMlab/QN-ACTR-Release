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

package jmt.gui.common.definitions;

import java.util.Vector;

/**
 * Created by IntelliJ IDEA.
 * User: OrsotronIII
 * Date: 27-mag-2005
 * Time: 9.52.46
 * This interface provides methods for editing of set of stations for JSIM models.
 * Each station is assigned a search key that can be used to retrieve each parameter.
 */
public interface StationDefinition {

	/**Code for station name retrieval*/
	public static final int STATION_NAME = 0;

	/**Code for station type retrieval*/
	public static final int STATION_TYPE = 1;

	/**Code for station type retrieval*/
	public static final int STATION_QUEUE_CAPACITY = 2;

	/**Code for station type retrieval*/
	public static final int STATION_NUMBER_OF_SERVERS = 3;

	/**
	 * This method returns the key set of sources
	 *
	 * Author: Francesco D'Aquino
	 */
	public Vector<Object> getStationKeysSource();

	/**
	 * This method returns the key set of servers
	 *
	 * @return an array containing the entire set of server keys
	 *
	 * Author: Francesco D'Aquino
	 */
	public Vector<Object> getStationKeysServer();

	/**
	 * This method returns the key set of delays
	 *
	 * @return an array containing the entire set of delay keys
	 *
	 * Author: Francesco D'Aquino
	 */
	public Vector<Object> getStationKeysDelay();

	/**
	 * This method returns the entire set of station keys.
	 */
	public Vector<Object> getStationKeys();

	/**
	 * This method returns all station keys except source and sink ones.
	 */
	public Vector<Object> getStationKeysNoSourceSink();

	/**
	 * This method returns all station (without sources and sinks) and blocking region keys.
	 */
	public Vector<Object> getStationRegionKeysNoSourceSink();
	
	public Vector<Object> getStationKeysSink();

	/** Returns name of the station in <code>String</code> representation, given the search key.*/
	public String getStationName(Object key);

	/** Sets name of the station, given the search key.*/
	public void setStationName(String name, Object key);

	/** Returns type of the station in <code>String</code> representation,
	 * given the search key.*/
	public String getStationType(Object key);

	/** Sets type of the station, given the search key.*/
	public void setStationType(String type, Object key);

	/** Returns queue capacity given the search key.*/
	public Integer getStationQueueCapacity(Object key);

	/** Sets queue capacity of the station, given the search key.*/
	public void setStationQueueCapacity(Integer queueCapacity, Object key);

	/** Returns number of servers or number of forked job for the station given the search key.*/
	public Integer getStationNumberOfServers(Object key);

	/** Sets number of servers or number of forked job for the station, given the search key.*/
	public void setStationNumberOfServers(Integer numberOfServers, Object key);

	/** Returns parameter for the station given the search key and parameter code.*/
	public Object getStationParameter(Object key, int parameterCode);

	/** Sets parameter for the station, given the search key and parameter code.*/
	public void setStationParameter(Object value, Object key, int parameterCode);

	/**Adds a new station to the model. Name and type must be specified.
	 * @param name: name of the new station
	 * @param type: string representing station type. It's value is contained in
	 * <code>JSIMConstants</code> interface.
	 * @return : key of search for this class*/
	public Object addStation(String name, String type);

	/**Deletes station given a search key.*/
	public void deleteStation(Object key);

	/**
	 * Tells if a fork is blocking
	 * <br>Author: Bertoli Marco
	 * @param key search's key for fork
	 * @return maximum number of jobs allowed in a fork-join
	 * region (-1 is infinity)
	 */
	public Integer getForkBlock(Object key);

	/**
	 * Sets if a fork is blocking
	 * <br>Author: Bertoli Marco
	 * @param key search's key for fork
	 * @param value maximum number of jobs allowed in a fork-join
	 * region (-1 is infinity)
	 */
	public void setForkBlock(Object key, Integer value);

	/*------------------------------------------------------------------------------
	 *---------------- Methods for setup of class-station parameters ---------------
	 *------------------------------------------------------------------------------*/

	/**Sets queue strategy for a station and a class, given their search keys.
	 * If specified station cannot accept this kind of parameter, no value will
	 * be set.
	 * @param stationKey: search key for station.
	 * @param classKey: search key for class.
	 * @param queueStrategy: string name for queue strategy*/
	public void setQueueStrategy(Object stationKey, Object classKey, String queueStrategy);

	/**Returns queue strategy for a station and a class, given their search keys.
	 * If specified station cannot accept this kind of parameter, null value is
	 * returned.
	 * @param stationKey: search key for station.
	 * @param classKey: search key for class.
	 * @return : string name for queue strategy*/
	public String getQueueStrategy(Object stationKey, Object classKey);

	/**Sets Service Time distribution for class and station, given their search keys.
	 * If specified station cannot accept this kind of parameter, no value will
	 * be set.
	 * @param stationKey: search key for station.
	 * @param classKey: search key for class.
	 * @param distribution: distribution to be set for specified class and station.
	 */
	public void setServiceTimeDistribution(Object stationKey, Object classKey, Object distribution);

	/**Sets Service Time distribution for class and station, given their search keys.
	 * If specified station cannot accept this kind of parameter, null value is
	 * returned.
	 * @param stationKey: search key for station.
	 * @param classKey: search key for class.
	 * @return distribution for specified class and station.
	 */
	public Object getServiceTimeDistribution(Object stationKey, Object classKey);

	/**Sets routing startegy for class and station, given their search keys.
	 * If specified station cannot accept this kind of parameter, no value will
	 * be set.
	 * @param stationKey: search key for station.
	 * @param classKey: search key for class.
	 * @param routingStrategy: distribution to be set for specified class and station.
	 */
	public void setRoutingStrategy(Object stationKey, Object classKey, Object routingStrategy);

	/**Returns routing strategy for class and station, given their search keys.
	 * If specified station cannot accept this kind of parameter, null value is
	 * returned.
	 * @param stationKey: search key for station.
	 * @param classKey: search key for class.
	 * @return routing strategy for specified class and station.
	 */
	public Object getRoutingStrategy(Object stationKey, Object classKey);

	/** Sets (Returns) logging parameters for the logger.
	 * @param stationKey: search key for station.
	 * @param loggerParameters: local LoggerParameters.
	 */
	public void setLoggingParameters(Object stationKey, Object loggerParameters);

	/** Returns logging parameters for the logger. <I>MF'08 0.7.4</I>
	 * @param stationKey: search key for station.
	 */
	public Object getLoggingParameters(Object stationKey);

	/** Returns logging parameters for the Global logfile. <I>MF'08 0.7.4</I>
	 * @param selector: either "path", "delim", or "autoAppend" 
	 */
	public String getLoggingGlbParameter(String selector);

	/** Sets a global logging parameter as a CommonModel variable. <I>MF'08 0.7.4</I>
	 * @param selector: either "path", "delim", "decimalSeparator", or "autoAppend" 
	 * @param value: String to assign to variable named by Selector.
	 */
	public void setLoggingGlbParameter(String selector, String value);

	/**
	 * Normalizes the routing probabilities
	 */
	public void manageProbabilities();

	/**
	 * Returns drop rule associated with given station queue section if capacity is finite
	 * @param stationKey: search key for station.
	 * @param classKey: search key for class.
	 * @return FINITE_DROP || FINITE_BLOCK || FINITE_WAITING
	 */
	public String getDropRule(Object stationKey, Object classKey);

	/**
	 * Sets drop rule associated with given station queue section if capacity is finite
	 * @param stationKey: search key for station.
	 * @param classKey: search key for class.
	 * @param rule FINITE_DROP || FINITE_BLOCK || FINITE_WAITING
	 */
	public void setDropRule(Object stationKey, Object classKey, String rule);

	/*------------------------------------------------------------------------------
	*-------------  methods for inter-station connections definition  --------------
	*-------------------------------------------------------------------------------*/
	/**Adds a connection between two stations in this model, given search keys of
	 * source and target stations. If connection could not be created (if, for example,
	 * target station's type is "Source")false value is returned.
	 * @param sourceKey: search key for source station
	 * @param targetKey: search key for target station
	 * @param areConnected: true if stations must be connected, false otherwise.
	 * @return : true if connection was created, false otherwise.
	 * */
	public boolean setConnected(Object sourceKey, Object targetKey, boolean areConnected);

	/**Tells wether two stations are connected
	 * @param sourceKey: search key for source station
	 * @param targetKey: search key for target station
	 * @return : true if stations are connected, false otherwise.
	 */
	public boolean areConnected(Object sourceKey, Object targetKey);

	/**Tells wether two stations can be connected
	 * @param sourceKey: search key for source station
	 * @param targetKey: search key for target station
	 * @return : true if stations are connectable, false otherwise.
	 */
	public boolean areConnectable(Object sourceKey, Object targetKey);

	/**Returns a set of station keys specified station is connected to as a source.
	 * @param stationKey: source station for which (target)connected stations must be
	 * returned.
	 * @return Vector containing keys for connected stations.
	 */
	public Vector<Object> getForwardConnections(Object stationKey);

	/**Returns a set of station keys specified station is connected to as a target.
	 * @param stationKey: source station for which (source)connected stations must be
	 * returned.
	 * @return Vector containing keys for connected stations.
	 */
	public Vector<Object> getBackwardConnections(Object stationKey);

	/**
	 * Returns the key of the station whose name is <code>stationName</code>. It
	 * returns <code>null</code> if no such station is found.
	 * @param stationName the name of the station
	 * @return the key of the station
	 */
	public Object getStationByName(String stationName);

	public void setStationQueueStrategy(Object stationKey, String strategy);

	public String getStationQueueStrategy(Object stationKey);
	
	public boolean isSinkProbabilityUpdated();
	
	public void setSinkProbabilityUpdatedVar(boolean param);
	
	public Vector<String> getsinkProbabilityUpdateStations();
	
	public void resetSinkProbabilityUpdateStations();
}
