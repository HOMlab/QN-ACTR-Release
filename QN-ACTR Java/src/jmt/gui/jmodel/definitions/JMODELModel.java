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

package jmt.gui.jmodel.definitions;

import java.awt.Color;
import java.util.HashMap;
import java.util.TreeSet;
import java.util.Vector;

import jmt.gui.common.Defaults;
import jmt.gui.common.definitions.CommonModel;
import jmt.gui.common.routingStrategies.RoutingStrategy;
import jmt.gui.common.serviceStrategies.ServiceStrategy;
import jmt.gui.jmodel.JMODELConstants;

/**
 * <p>Title: JMODEL-Model </p>
 * <p>Description: Main data structure used to store model definition data,
 * to save and load model and to compile xml files for the simulator and
 * as a bridge to connect JMODEL with JSIM and JMVA</p>
 *
 * @author Bertoli Marco
 * Date: 2-giu-2005
 * Time: 10.27.54
 *
 *
 * Modified by Francesco D'Aquino 11/11/2005
 */
//todo MMM JMVA bridge, undo/redo
public class JMODELModel extends CommonModel implements JmodelClassDefinition, JmodelStationDefinition, JMODELConstants {

	// ----- Variables -------------------------------------------------------------
	protected HashMap<String, Long> objectNumber = new HashMap<String, Long>(); // Used to generate progressive unique default names
	protected HashMap<Object, Color> classColor = new HashMap<Object, Color>(); // Used to store classes color
	protected HashMap<Object, JMTPoint> stationPositions = new HashMap<Object, JMTPoint>(); // Used to store station positions
	protected Color[] defaultColor = new Color[] { Color.blue, Color.red, Color.green, Color.magenta, Color.orange, Color.cyan, Color.yellow }; // Defaults color prompted when inserting new class (next ones are generated random)

	//Francesco D'Aquino
	protected boolean animationEnabled = Defaults.getAsBoolean("isWithAnimation").booleanValue();

	// end Francesco D'Aquino

	// ----- Methods----------------------------------------------------------------
	/**
	 * Default constructor, creates a new instance of <code>JMODELModel</code>
	 */
	public JMODELModel() {
		super();
	}

	// ----- Class Definition Methods ---------------------------------------------------------------------
	/**
	 * Creates a new Class with default parameters and default generated name
	 * @return search key for newly created class
	 */
	public Object addClass() {
		Long num;
		if (!objectNumber.containsKey(Defaults.get("className"))) {
			// This if first station of that type
			num = new Long(0L);
		} else {
			// Uses next number
			num = objectNumber.get(Defaults.get("className"));
			num = new Long(num.longValue() + 1);
		}
		objectNumber.put(Defaults.get("className"), num);
		return addClass(getUniqueClassName(Defaults.get("className") + num.toString()), Defaults.getAsInteger("classType").intValue(), Defaults
				.getAsInteger("classPriority").intValue(), Defaults.getAsInteger("classPopulation"), Defaults.getAsNewInstance("classDistribution"));
	}

	/**
	 * Delete spefified class
	 * @param key search key for class to be deleted
	 * Overrides default one to add "Color"
	 */
	@Override
	public void deleteClass(Object key) {
		super.deleteClass(key);
		classColor.remove(key);
	}

	/**
	 * Adds a class and sets all class parameters at once.
	 * @param name name of the class to be added
	 * @param type type of class
	 * @param priority value of priority field
	 * @param population population of this class
	 * @param distribution initial distribution type
	 * @return search key identifying the new class.
	 * Overrides default one to add "Color"
	 */
	@Override
	public Object addClass(String name, int type, int priority, Integer population, Object distribution) {
		Object tmp = super.addClass(getUniqueClassName(name), type, priority, population, distribution);
		this.setClassColor(tmp, getNewColor());
		return tmp;
	}

	/**
	 * Sets name of the class linked to the specific key
	 * @param name name to be set to selected class
	 * @param key search's key for selected class
	 */
	@Override
	public void setClassName(String name, Object key) {
		super.setClassName(getUniqueClassName(name), key);
	}

	/**
	 * Returns a class parameter, given the class key of search, and the parameter name
	 * specified by proper code.
	 * @param key of search for class
	 * @param parameterName name of parameter to be returned
	 * @return selected parameter's value
	 * Overrides default one to add "Color"
	 */
	@Override
	public Object getClassParameter(Object key, int parameterName) {
		if (parameterName == CLASS_COLOR) {
			return getClassColor(key);
		} else if (parameterName == REFERENCE_SOURCE_NAME) {
			return getClassRefStation(key);
		} else {
			return super.getClassParameter(key, parameterName);
		}
	}

	/**
	 * Sets a class parameter, given the class key of search, and the parameter name
	 * specified by proper code.
	 * @param key key of search for class
	 * @param parameterName name of parameter to be changed
	 * @param value selected parameter should be setted to this value
	 * Overrides default one to add "Color"
	 */
	@Override
	public void setClassParameter(Object key, int parameterName, Object value) {
		if (parameterName == CLASS_COLOR) {
			setClassColor(key, (Color) value);
		} else if (parameterName == REFERENCE_SOURCE_NAME) {
			setClassRefStation(key, value);
		} else {
			super.setClassParameter(key, parameterName, value);
		}
	}

	/**
	 * Sets type of the class linked to the specific key. Type of class is represented by
	 * an int number whose value is contained in <code>JMODELConstants</code>.
	 * */
	@Override
	public synchronized void setClassType(int type, Object key) {
		// If a class type changes, resets its reference station
		int old = getClassType(key);
		super.setClassType(type, key);
		// If type has changed sets ref station to first found source if class is open
		if (old != type) {
			setClassRefStation(key, null);
			if (type == CLASS_TYPE_OPEN) {
				Vector stations = this.getStationKeys();
				for (int i = 0; i < stations.size(); i++) {
					if (this.getStationType(stations.get(i)).equals(STATION_TYPE_SOURCE)) {
						setClassRefStation(key, stations.get(i));
						break;
					}
				}
			}
		}
	}

	/**
	 * Sets the color of a class
	 * @param key search key for class
	 * @param color Color that should be associated with given class
	 */
	public void setClassColor(Object key, Color color) {
		classColor.put(key, color);
	}

	/**
	 * Returns a class color
	 * @param key search key for class
	 * @return Color associated with given class, or null if class does not exist
	 */
	public Color getClassColor(Object key) {
		return classColor.get(key);
	}

	/**
	 * Generates a color for a new class. First enumerates all elements of defaultColor array
	 * then generates new colors at random.
	 * @return Newly generated color
	 */
	public Color getNewColor() {
		Long num;
		if (!objectNumber.containsKey(COLOR_NAME)) {
			// This if first station of that type
			num = new Long(0L);
		} else {
			// Uses next number
			num = objectNumber.get(COLOR_NAME);
			num = new Long(num.longValue() + 1);
		}
		objectNumber.put(COLOR_NAME, num);
		if (num.longValue() < defaultColor.length) {
			return defaultColor[num.intValue()];
		} else {
			return new Color((int) Math.floor(Math.random() * 256), (int) Math.floor(Math.random() * 256), (int) Math.floor(Math.random() * 256));
		}
	}

	//Francesco D'Aquino ---------------------------
	/**
	 * Return true if queue animation is enabled
	 *
	 * @return true if the animation is enabled
	 */
	@Override
	public boolean isAnimationEnabled() {
		return this.animationEnabled;
	}

	/**
	 * Enable or disable queue animation
	 *
	 * @param isEnabled - set it to true to enable queue animation
	 */
	@Override
	public void setAnimationEnabled(boolean isEnabled) {
		animationEnabled = isEnabled;
	}

	// end Francesco D'Aquino ----------------------------

	/**
	 * Returns a serialized object for a given class, used to support cut/paste,
	 * undo/redo operations
	 * @param classKey Search's key for station
	 * @return Serialized class object
	 */
	public Object serializeClass(Object classKey) {
		return new SerializedClass(classKey);
	}

	/**
	 * Deserializes given class
	 * @param serializedForm class's Serialized form got from <code>serializeClass</code>
	 * method.
	 * @return search's key for inserted class
	 */
	public Object deserializeClass(Object serializedForm) {
		SerializedClass sc = (SerializedClass) serializedForm;
		Object newkey = addClass(sc.name, sc.type, sc.priority, sc.population, sc.distribution);
		setClassColor(newkey, sc.color);
		setClassRefStation(newkey, sc.referenceStation);
		return newkey;
	}

	/**
	 * Object returned when asking for serializedClass
	 */
	public class SerializedClass {
		// Attributes of this class
		public Object key;
		public String name;
		public Color color;
		public int type;
		public int priority;
		public Integer population;
		public Object distribution;
		public Object referenceStation;

		/**
		 * Constructs a new serialized form of specified class
		 * @param classkey key of search for specified class
		 */
		public SerializedClass(Object classkey) {
			key = classkey;
			name = getClassName(classkey);
			color = getClassColor(classkey);
			type = getClassType(classkey);
			priority = getClassPriority(classkey);
			population = getClassPopulation(classkey);
			distribution = getClassDistribution(classkey);
			referenceStation = getClassRefStation(classkey);
		}
	}

	/**
	 * Returns given name if a class with the same name does not exists or makes it unique
	 * @param name class name
	 * @return unique name
	 */
	private String getUniqueClassName(String name) {
		TreeSet<String> names = new TreeSet<String>(); // Map of all unique names with their first users
		Vector keys = getClassKeys();
		// Finds all used names
		for (int i = 0; i < keys.size(); i++) {
			names.add(this.getClassName(keys.get(i)));
		}

		// If name is new, returns it
		if (!names.contains(name)) {
			return name;
		}

		int num;
		// If format is already '*_[number]' increment number
		char[] charname = name.toCharArray();
		int n = charname.length - 1;
		while (charname[n] >= '0' && charname[n] <= '9' && n > 0) {
			n--;
		}
		if (charname[n] == '_') {
			num = Integer.parseInt(name.substring(n + 1));
			name = name.substring(0, n); // Removes suffix
		}
		// Otherwise uses number 1
		else {
			num = 1;
		}
		// Finds unique number
		while (names.contains(name + "_" + num)) {
			num++;
		}
		return name + "_" + num;
	}

	// ----------------------------------------------------------------------------------------------------

	// ----- Station Definition Methods -------------------------------------------------------------------
	/**
	 * Adds a new station to the model. Only type is required as a default name will be set.
	 * @param type Constant string from JSIMConstants interface.
	 * @return key of search for newly created station
	 */
	public Object addStation(String type) {
		Long num;
		if (!objectNumber.containsKey(type)) {
			// This if first station of that type
			num = new Long(0L);
		} else {
			// Uses next number
			num = objectNumber.get(type);
			num = new Long(num.longValue() + 1);
		}
		objectNumber.put(type, num);
		return super.addStation(getUniqueStationName(STATION_NAMES.get(type) + " " + num), type);
	}

	/**
	 * Sets type of the station, given the search key.
	 * @param key: search key for station to be modified
	 * @param name: name of station.
	 */
	@Override
	public void setStationName(String name, Object key) {
		// Changes station name only if needed
		if (!name.equals(getStationName(key))) {
			super.setStationName(getUniqueStationName(name), key);
		}
	}

	/**
	 * Deletes station given a search key
	 * @param key search key for station to be deleted
	 */
	@Override
	public synchronized void deleteStation(Object key) {
		super.deleteStation(key);
		stationPositions.remove(key);
	}

	/**
	 * Sets position for a target station into jgraph window. This method is used to
	 * implement save routine
	 * @param key key of search for target station
	 * @param position position where target station is placed
	 */
	public void setStationPosition(Object key, JMTPoint position) {
		stationPositions.put(key, position);
	}

	/**
	 * Returns stored position for a given station into jgraph window
	 * (Note that this is stored position, not original position. This method
	 * is used to implement load routine)
	 * @param key key of search for target station
	 * @return position where target station has to be placed
	 */
	public JMTPoint getStationPosition(Object key) {
		return stationPositions.get(key);
	}

	/**
	 * Gets a preview of name that will be assigned automatically to new created object of
	 * specified type. This method is used to calculate dimensions of caption to be displayed
	 * under component on jgraph window
	 * @param type Constant string from JMODELConstants interface.
	 * @return name assigned to next station of 'type' type.
	 */
	public String previewStationName(String type) {
		if (!objectNumber.containsKey(type)) {
			return getUniqueStationName(type + "0");
		} else {
			return getUniqueStationName(type + objectNumber.get(type).toString());
		}
	}

	/**
	 * Returns a serialized form of a given station to support cut/copy, undo/redo operations
	 * @param key Search's key for station
	 * @return serialized station or null if station key does not exists
	 */
	public Object serializeStation(Object key) {
		return new SerializedStation(key);
	}

	/**
	 * Inserts a serialized station into data structure, used to support paste operation.
	 * Note that Empirical routing strategy can be inconsistent with station's current connections.
	 * (will be resolved in <code>JmtClipboard</code>
	 * @param serializedForm station's Serialized form got from <code>serializeStation</code>
	 * method.
	 * @return search's key for inserted station
	 */
	public Object deserializeStation(Object serializedForm) {
		SerializedStation ss = (SerializedStation) ((SerializedStation) serializedForm).clone();
		ss.name = getUniqueStationName(ss.name);
		Object key = this.addStation(ss.name, ss.type);
		setStationQueueCapacity(ss.queueCapacity, key);
		setStationNumberOfServers(ss.numOfServers, key);
		// Now sets class-dependant parameters ONLY if stored class exists (so has not been deleted
		// in the meantime)
		Object[] classkeyset = ss.serviceStrategy.keySet().toArray();
		Vector currentclasses = getClassKeys();
		for (Object element : classkeyset) {
			if (currentclasses.contains(element)) {
				setQueueStrategy(key, element, ss.queueStrategy.get(element));
				setServiceTimeDistribution(key, element, ss.serviceStrategy.get(element));
				setRoutingStrategy(key, element, ss.routingStrategy.get(element));
			}
		}
		return key;
	}

	/**
	 * Object returned when asking for serializedStation
	 */
	public class SerializedStation {
		// Attributes of this station only
		protected Object key;
		public String name;
		public String type;
		public Integer numOfServers;
		public Integer queueCapacity;
		// Attributes of the tuple (class, this station)
		public HashMap<Object, String> queueStrategy = new HashMap<Object, String>();
		public HashMap<Object, ServiceStrategy> serviceStrategy = new HashMap<Object, ServiceStrategy>();
		public HashMap<Object, RoutingStrategy> routingStrategy = new HashMap<Object, RoutingStrategy>();

		public SerializedStation() {

		}

		public SerializedStation(Object key) {
			this.key = key;
			name = getStationName(key);
			type = getStationType(key);
			queueCapacity = getStationQueueCapacity(key);
			numOfServers = getStationNumberOfServers(key);
			// Now sets all attributes of the (station/class) touple
			Vector classes = getClassKeys();
			Object currclass;
			for (int i = 0; i < classes.size(); i++) {
				currclass = classes.get(i);
				queueStrategy.put(currclass, getQueueStrategy(key, currclass));
				serviceStrategy.put(currclass, ((ServiceStrategy) getServiceTimeDistribution(key, currclass)).clone());
				routingStrategy.put(currclass, ((RoutingStrategy) getRoutingStrategy(key, currclass)).clone());
			}
		}

		/**
		 * Returns a deep copy of this object
		 * @return A deep copy of this object
		 */
		@Override
		public Object clone() {
			SerializedStation ss = new SerializedStation();
			ss.key = key;
			ss.name = name;
			ss.type = type;
			ss.numOfServers = numOfServers;
			ss.queueCapacity = queueCapacity;
			Object[] classes = serviceStrategy.keySet().toArray();
			// QueueStrategy can share values as they are strings (immutable). On the other hand,
			// as Distribution and RoutingStrategy are mutable, they have to be cloned explicitly
			ss.queueStrategy = new HashMap<Object, String>(queueStrategy);
			for (Object classe : classes) {
				ss.serviceStrategy.put(classe, serviceStrategy.get(classe).clone());
				ss.routingStrategy.put(classe, routingStrategy.get(classe).clone());
			}
			return ss;
		}
	}

	/**
	 * Returns given name if a station with the same name does not exists or makes it unique
	 * @param name station name
	 * @return unique name
	 */
	private String getUniqueStationName(String name) {
		TreeSet<String> names = new TreeSet<String>(); // Map of all unique names with their first users
		Vector keys = getStationKeys();
		// Finds all used names
		for (int i = 0; i < keys.size(); i++) {
			names.add(this.getStationName(keys.get(i)));
		}

		// If name is new, returns it
		if (!names.contains(name)) {
			return name;
		}

		int num;
		// If format is already '*_[number]' increment number
		char[] charname = name.toCharArray();
		int n = charname.length - 1;
		while (charname[n] >= '0' && charname[n] <= '9' && n > 0) {
			n--;
		}
		if (charname[n] == '_') {
			num = Integer.parseInt(name.substring(n + 1));
			name = name.substring(0, n); // Removes suffix
		}
		// Otherwise uses number 1
		else {
			num = 1;
		}
		// Finds unique number
		while (names.contains(name + "_" + num)) {
			num++;
		}
		return name + "_" + num;
	}

	// ----------------------------------------------------------------------------------------------------
	// ----- Blocking Region Definition Methods -----------------------------------------------------------

	/**
	 * Adds a new blocking region with unique name to current model
	 * @return search's key for created blocking region
	 */
	public Object addBlockingRegion() {
		long num = 0L;
		String baseName = Defaults.get("blockingRegionName");

		// Gets all blocking region names to find an unique name
		TreeSet<String> names = new TreeSet<String>();
		Vector keys = getRegionKeys();
		for (int i = 0; i < keys.size(); i++) {
			names.add(getRegionName(keys.get(i)));
		}

		String name = baseName + num;
		while (names.contains(name)) {
			name = baseName + (++num);
		}

		return addBlockingRegion(name, Defaults.get("blockingRegionType"));
	}
	// ----------------------------------------------------------------------------------------------------
}
