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

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.text.DecimalFormatSymbols;
import java.util.Locale;
import java.util.Properties;

import jmt.framework.gui.components.JMTFrame;

/**
 * <p>Title: Simulation Defaults</p>
 * <p>Description: This class will provide methods to get default values of every
 * parameter that can be set by the user. Default values can be overridden and
 * will be saved on a config file.</p>
 *
 * @author Bertoli Marco
 *         Date: 12-lug-2005
 *         Time: 14.04.02
 *
 * Modified by Francesco D'Aquino 15/11/2005
 * 
 * Modified by Ashanka (July 2010)
 * Desc: Added new defaults control of a Random CheckBox.
 * 
 */
public class Defaults implements CommonConstants {
	protected static final String FILENAME = "defaults.conf";
	protected static Properties prop;
	// Initialize properties
	static {
		reload();
	}

	/**
	 * Sets default properties. This function is called only once before initializing
	 * internal data structure. Upon inserting a new property, this function MUST be
	 * extended.
	 * @return default (factory hard coded) initial properties
	 */
	protected static Properties getDefaults() {
		Properties def = new Properties();

		// Station parameters defaults
		def.setProperty("stationName", "Station");
		def.setProperty("stationType", STATION_TYPE_SERVER);
		def.setProperty("stationCapacity", "-1");
		def.setProperty("stationServers", "1");
		def.setProperty("stationStationQueueStrategy", QUEUE_STRATEGY_STATION_QUEUE);
		def.setProperty("stationQueueStrategy", QUEUE_STRATEGY_FCFS);
		def.setProperty("stationServiceStrategy", "jmt.gui.common.distributions.Exponential");
		def.setProperty("stationDelayServiceStrategy", "jmt.gui.common.distributions.Exponential");
		def.setProperty("stationRoutingStrategy", "jmt.gui.common.routingStrategies.RandomRouting");
		def.setProperty("forkBlock", "-1");
		def.setProperty("forkJobsPerLink", "1");
		def.setProperty("dropRule", "Drop");

		// Classes parameters defaults
		def.setProperty("className", "Class");
		def.setProperty("classType", "" + CLASS_TYPE_CLOSED);
		def.setProperty("classPriority", "0");
		def.setProperty("classPopulation", "1");
		def.setProperty("classDistribution", "jmt.gui.common.distributions.Exponential");

		// Blocking Region Parameters
		def.setProperty("blockingMaxJobsPerClass", "-1");
		def.setProperty("blockingDropPerClass", "true");
		def.setProperty("blockingMaxJobs", "-1");
		def.setProperty("blockingRegionName", "FCRegion");
		// Not used parameter...
		def.setProperty("blockingRegionType", "default");

		// Logger Parameters (for global log)
		def.setProperty("loggerDelimiter", ";");
		Locale locale = JMTFrame.getPlatformDefaultLocale();
		DecimalFormatSymbols symbols = new DecimalFormatSymbols(locale);
		def.setProperty("loggerDecimalSeparator", String.valueOf(symbols.getDecimalSeparator()));
		def.setProperty("loggerAutoAppend", "" + jmt.engine.log.LoggerParameters.LOGGER_AR_ASK);
		def.setProperty("loggerFilePath", "");

		//Simulation Defaults parameters
		def.setProperty("measureAlpha", "0.99");
		def.setProperty("measurePrecision", "0.03");
		def.setProperty("simulationSeed", "23000");
		def.setProperty("isSimulationSeedRandom", "true");
		def.setProperty("maxSimulationSamples", "1000000");
		def.setProperty("simulationMaxDuration", "-1");
		def.setProperty("simulationPolling", "1");
		// Francesco D'Aquino
		def.setProperty("isWithAnimation", "true");
		def.setProperty("representableClasses", "10");
		// end Francesco D'Aquino

		//GUI default parameters.
		def.setProperty("JSIMWindowWidth", "800");
		def.setProperty("JSIMWindowHeight", "600");

		return def;
	}

	/**
	 * Returns inner properties data structure
	 * @return inner properties data structure
	 */
	public static Properties getProperties() {
		return prop;
	}

	/**
	 * Returns default value of chosen parameter
	 * @param parameterName name of the parameter to be selected
	 * @return selected parameter default value in String format
	 */
	public static String get(String parameterName) {
		return prop.getProperty(parameterName);
	}

	/**
	 * Returns default value of chosen parameter in Integer form
	 * @param parameterName name of the parameter to be selected
	 * @return selected parameter default value in Integer format
	 * @throws NumberFormatException if the parameter does not contain a parsable integer
	 */
	public static Integer getAsInteger(String parameterName) {
		return new Integer(get(parameterName));
	}

	/**
	 * Returns default value of chosen parameter in Long form
	 * @param parameterName name of the parameter to be selected
	 * @return selected parameter default value in Long format
	 * @throws NumberFormatException if the parameter does not contain a parsable long
	 */
	public static Long getAsLong(String parameterName) {
		return new Long(get(parameterName));
	}

	/**
	 * Returns default value of chosen parameter in Double form
	 * @param parameterName name of the parameter to be selected
	 * @return selected parameter default value in Double format
	 * @throws NumberFormatException if the parameter does not contain a parsable integer
	 */
	public static Double getAsDouble(String parameterName) {
		return new Double(get(parameterName));
	}

	/**
	 * Returns default value of chosen parameter in Boolean form
	 * @param parameterName name of the parameter to be selected
	 * @return selected parameter default value in Boolean format
	 */
	public static Boolean getAsBoolean(String parameterName) {
		return new Boolean(get(parameterName));
	}

	/**
	 * Returns a new instance of default value of chosen parameter. Uses reflection to instantiate
	 * selected parameter's class.
	 * @param parameterName name of the parameter to be selected
	 * @return new instance of default value for selected parameter or null if value of
	 * this parameter is not a class to be instantiated
	 */
	public static Object getAsNewInstance(String parameterName) {
		String className = get(parameterName);
		Object newClass = null;
		try {
			newClass = Class.forName(className).newInstance();
		} catch (InstantiationException e) {
		} catch (IllegalAccessException e) {
		} catch (ClassNotFoundException e) {
			System.err.println("Error: Default class not found");
			e.printStackTrace();
		} catch (Exception e) {
		}
		return newClass;
	}

	/**
	 * Sets a default value for a chosen parameter
	 * @param parameterName name of the parameter to be chosen
	 * @param value new default value to be associated with given parameter
	 */
	public static void set(String parameterName, String value) {
		prop.setProperty(parameterName, value);
	}

	/**
	 * Saves current default values into <CODE>FILENAME</CODE>
	 * @return true iff file was correctly saved
	 */
	public static boolean save() {
		boolean saved = false;
		try {
			OutputStream output = new FileOutputStream(FILENAME);
			prop.store(output, "Default parameters definition");
			saved = true;
		} catch (IOException e) {
		} catch (SecurityException ex) {
		}
		return saved;
	}

	/**
	 * Reloads current default values from <CODE>FILENAME</CODE>
	 */
	public static void reload() {
		prop = new Properties(getDefaults());
		// Try to load properties from default config file, otherwise uses defaults one
		try {
			InputStream input = new FileInputStream(FILENAME);
			prop.load(input);
		} catch (IOException e) {
		} catch (SecurityException ex) {
		}
	}

	/**
	 * Reverts current values to original default ones (hard coded)
	 */
	public static void revertToDefaults() {
		prop = new Properties(getDefaults());
	}
}
