package jmt.common;

import java.io.IOException;
import java.io.Serializable;
import java.util.Properties;

public class GlobalSettings implements Serializable {
	// Some settings
	public static final String VERSION = "jmt.version";

	private static final long serialVersionUID = 1L;
	private static final String FILENAME = "globalsettings.conf";
	/**
	 * The data structure. Remember to access them in a thread-safe manner. 
	 */
	private static Properties props;

	/**
	 * Initializes this class loading all the properties
	 */
	private synchronized static void load() {
		props = new Properties();
		try {
			props.load(GlobalSettings.class.getResourceAsStream(FILENAME));
		} catch (IOException ex) {
			System.err.println("Fatal error: unable to load global settings");
			throw new Error("Fatal error: unable to load global settings. Aborting JMT.");
		}
	}

	static {
		load();
	}

	/**
	 * Returns a setting. Searches first on System properties, then on globalsettings.
	 * @param name the name of the property
	 * @return the property value or null if not found.
	 */
	public static String getSetting(String name) {
		if (name == null || name.length() == 0) {
			return null;
		}
		String value = null;
		try {
			value = System.getProperty(name);
		} catch (SecurityException th) {
			// A security manager blocked the property get
			value = null;
		}
		// If system property is null, searches  on configuration properties.
		if (value == null) {
			value = props.getProperty(name);
		}
		return value;
	}

	/**
	 * Reloads properties cache. May be used for debugging purposes.
	 */
	public static void reload() {
		load();
	}
}
