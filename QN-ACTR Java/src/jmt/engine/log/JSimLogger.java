/**    
  * Copyright (C) 2009, Laboratorio di Valutazione delle Prestazioni - Politecnico di Milano

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

package jmt.engine.log;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.PrintWriter;
import java.io.Serializable;
import java.io.StringWriter;
import java.net.URL;
import java.util.Properties;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;

/**
 * <p><b>Name:</b> JSimLogger</p> 
 * <p><b>Description:</b> 
 * A wrapper aroung log4j API to provide logging capabilities.
 * </p>
 * <p><b>Date:</b> 11/dic/2009
 * <b>Time:</b> 20.17.54</p>
 * @author Bertoli Marco [marco.bertoli@neptuny.com]
 * @version 1.0
 */
public class JSimLogger implements Serializable {
	private static final long serialVersionUID = 1L;
	public static final String STD_LOGGER = "jmt.engine";
	public static final String LOG4J_CONF = "log4j.conf";

	private transient org.apache.log4j.Logger logger;
	static {
		// Initialize only if somebody didn't already initialize this.
		if (!LogManager.getCurrentLoggers().hasMoreElements()) {
			URL props = JSimLogger.class.getResource(LOG4J_CONF);
			if (props != null) {
				PropertyConfigurator.configure(props);
			} else {
				System.out.println("Cannot find logProperties, using defaults");
				//set stdout defaults
				Properties p = new Properties();
				p.setProperty("log4j.rootLogger", "DEBUG, stdout");
				//p.setProperty("log4j.rootLogger","ALL, stdout");
				p.setProperty("log4j.appender.stdout", "org.apache.log4j.ConsoleAppender");
				p.setProperty("log4j.appender.stdout.layout", "org.apache.log4j.PatternLayout");
				p.setProperty("log4j.appender.stdout.layout.ConversionPattern", "%-5p [%t]- %m (%F:%L)%n");
				PropertyConfigurator.configure(p);
			}
		}
	}

	private JSimLogger() {
		logger = Logger.getRootLogger();
	}

	private JSimLogger(String loggerName) {
		logger = Logger.getLogger(loggerName);
	}

	public static JSimLogger getRootLogger() {
		JSimLogger mylogger = new JSimLogger();
		return mylogger;
	}

	public static JSimLogger getLogger() {
		return getLogger(JSimLogger.STD_LOGGER);
	}

	public static JSimLogger getLogger(Object caller) {
		JSimLogger mylogger = null;
		if (caller != null) {
			mylogger = JSimLogger.getLogger(caller.getClass());
		} else {
			mylogger = JSimLogger.getLogger();
		}
		return mylogger;
	}

	public static JSimLogger getLogger(Class<?> callerClass) {
		JSimLogger mylogger = new JSimLogger(callerClass.getName());
		return mylogger;
	}

	public static JSimLogger getLogger(String loggerName) {
		JSimLogger mylogger = new JSimLogger(loggerName);
		return mylogger;
	}

	// printing methods:
	public void debug(Object message) {
		logger.debug(message);
	}

	public void info(Object message) {
		logger.info(message);
	}

	public void warn(Object message) {
		logger.warn(message);
	}

	public void error(Object message) {
		logger.error(message);
	}

	public void error(Throwable th) {
		StringWriter sw = new StringWriter();
		PrintWriter pw = new PrintWriter(sw);
		th.printStackTrace(pw);
		String buffer = sw.toString();
		logger.error(buffer);
	}

	public void fatal(Object message) {
		logger.fatal(message);
	}

	public boolean isDebugEnabled() {
		return logger.isDebugEnabled();

	}

	public boolean isInfoEnabled() {
		return logger.isInfoEnabled();
	}
	
	// --- Following methods handles recreation of transient logger object after serialization ------------------
    private void writeObject(ObjectOutputStream out) throws IOException {
    	out.defaultWriteObject();
    	out.writeObject(logger.getName());
    }

    private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException {
    	in.defaultReadObject();
    	String name = (String) in.readObject();
		if (name == null) {
			logger = Logger.getRootLogger();
		} else {
			logger = Logger.getLogger(name);
		}
    }
// ----------------------------------------------------------------------------------------------------------    


}
