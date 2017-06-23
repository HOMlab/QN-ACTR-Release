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

import java.util.Set;
import java.util.Vector;

/**
 * <p>Title: Blocking Region Definition Interface</p>
 * <p>Description: This interface will enumerate methods used to specify a blocking
 * region on the queue network.</p>
 *
 * @author Bertoli Marco
 *         Date: 26-gen-2006
 *         Time: 16.21.26
 */
public interface BlockingRegionDefinition {
	/**
	 * Adds a new blocking region to the model
	 * @param name name of new blocking region
	 * @param type type of new blocking region
	 * @return search's key for new blocking region
	 */
	public Object addBlockingRegion(String name, String type);

	/**
	 * Removes a blocking region from the model
	 * @param key Search's key for region to be deleted
	 */
	public void deleteBlockingRegion(Object key);

	/**
	 * Adds a new station to specified blocking region
	 * @param regionKey Search's key for region
	 * @param stationKey Search's key for station
	 * @return true if station can be put inside specified region
	 */
	public boolean addRegionStation(Object regionKey, Object stationKey);

	/**
	 * Sets the name of a blocking region
	 * @param regionKey Search's key for region
	 * @param name name of the blocking region
	 */
	public void setRegionName(Object regionKey, String name);

	/**
	 * Returns the name of a blocking region
	 * @param regionKey Search's key for region
	 * @return name of the blocking region
	 */
	public String getRegionName(Object regionKey);

	/**
	 * Sets the type of a blocking region
	 * @param regionKey Search's key for region
	 * @param type type of the blocking region
	 */
	public void setRegionType(Object regionKey, String type);

	/**
	 * Returns the type of a blocking region
	 * @param regionKey Search's key for region
	 * @return type of the blocking region
	 */
	public String getRegionType(Object regionKey);

	/**
	 * Tells if a station can be added to specified blocking region
	 * @param regionKey Search's key for region
	 * @param stationKey Search's key for station
	 * @return true if station can be put inside specified region
	 */
	public boolean canRegionStationBeAdded(Object regionKey, Object stationKey);

	/**
	 * Removes a station from a specified blocking region
	 * @param regionKey Search's key for region
	 * @param stationKey Search's key for station
	 */
	public void removeRegionStation(Object regionKey, Object stationKey);

	/**
	 * Returns the entire set of blocking region keys
	 * @return the entire set of blocking region keys
	 */
	public Vector<Object> getRegionKeys();

	/**
	 * Sets a customer number constraint for a given region and an user class
	 * @param regionKey search's key for blocking region
	 * @param classKey search's key for customer class
	 * @param maxJobsPerClass maximum number of allowed customer. -1 means infinity.
	 */
	public void setRegionClassCustomerConstraint(Object regionKey, Object classKey, Integer maxJobsPerClass);

	/**
	 * Sets a drop rule for a given region and an user class
	 * @param regionKey search's key for blocking region
	 * @param classKey search's key for customer class
	 * @param drop true if jobs of specified class can be dropped, false otherwise
	 */
	public void setRegionClassDropRule(Object regionKey, Object classKey, Boolean drop);

	/**
	 * Sets a global customer number constraint for a given region
	 * @param regionKey search's key for blocking region
	 * @param maxJobs maximum number of allowed customer. -1 means infinity.
	 */
	public void setRegionCustomerConstraint(Object regionKey, Integer maxJobs);

	/**
	 * Gets customer number constraint for a given region and an user class
	 * @param regionKey search's key for blocking region
	 * @param classKey search's key for customer class
	 * @return maximum number of allowed customer. -1 means infinity.
	 */
	public Integer getRegionClassCustomerConstraint(Object regionKey, Object classKey);

	/**
	 * Gets drop rule for a given region and an user class
	 * @param regionKey search's key for blocking region
	 * @param classKey search's key for customer class
	 * @return true if jobs of specified class can be dropped, false otherwise
	 */
	public Boolean getRegionClassDropRule(Object regionKey, Object classKey);

	/**
	 * Gets global customer number constraint for a given region
	 * @param regionKey search's key for blocking region
	 * @return maximum number of allowed customer. -1 means infinity.
	 */
	public Integer getRegionCustomerConstraint(Object regionKey);

	/**
	 * Returns a list of every station inside a blocking region
	 * @param regionKey search's key for given blocking region
	 * @return a set with every station inside given blocking region
	 */
	public Set<Object> getBlockingRegionStations(Object regionKey);

	/**
	 * Gets blocking region of a given station
	 * @param stationKey search's key for given station
	 * @return search's key for its blocking region or null if it's undefined
	 */
	public Object getStationBlockingRegion(Object stationKey);

	/**
	 * Gets a vector with every station that can be added to a blocking region
	 * @return a vector of search's keys of every station that can be added
	 * to a blocking region
	 */
	public Vector<Object> getBlockableStationKeys();

	/**
	 * Returns name of the station in <code>String</code> representation,
	 * given the search key.
	 */
	public String getStationName(Object key);

}
