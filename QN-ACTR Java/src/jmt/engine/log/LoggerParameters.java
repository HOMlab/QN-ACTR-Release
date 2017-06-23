/**    
  * Copyright (C) 2008, Laboratorio di Valutazione delle Prestazioni - Politecnico di Milano

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

import java.io.File;

public class LoggerParameters {
	public volatile boolean isEnabled; // the variable is left public for fast external read access - don't write to it.
	public Boolean boolExecTimestamp;
	public Boolean boolLoggername;
	public Boolean boolTimeStamp;
	public Boolean boolJobID;
	public Boolean boolJobClass;
	public Boolean boolTimeSameClass;
	public Boolean boolTimeAnyClass;
	public String name;
	public String path;

	/* Constants for name passing */
	public final static int LOGGER_AR_ASK = 0;
	public final static int LOGGER_AR_REPLACE = 1;
	public final static int LOGGER_AR_APPEND = 2;
	public final static String GLOBALLOGNAME = "global.csv";

	/**
	 * Creates a logger with default parameters.
	 */
	public LoggerParameters() {
		boolExecTimestamp = new Boolean(false);
		boolLoggername = new Boolean(true);
		boolTimeStamp = new Boolean(true);
		boolJobID = new Boolean(true);
		boolJobClass = new Boolean(false);
		boolTimeSameClass = new Boolean(false);
		boolTimeAnyClass = new Boolean(false);
		name = new String(GLOBALLOGNAME);
		path = new String("./");
		enable();
	}

	/**
	 * Creates a logger with most parameters specified as a string.
	 * Used by jmt.engine.NodeSections.LogTunnel class.
	 */
	public LoggerParameters(Boolean ET, String FN, String FP, Boolean LN, Boolean TS, Boolean JID, Boolean JC, Boolean TSC, Boolean TAC) {
		boolExecTimestamp = ET;
		boolLoggername = LN;
		boolTimeStamp = TS;
		boolJobID = JID;
		boolJobClass = JC;
		boolTimeSameClass = TSC;
		boolTimeAnyClass = TAC;
		name = FN;
		path = FP;
		enable();
	}

	/**
	 * @returns Returns the path, preferably in a relative format.
	 */
	public final String getpath() {
		String syspath = System.getProperty("user.dir");

		try {

			if (path == null) {
				return "./";
			} else if (path.length() == 0 || path.startsWith(".")) {
				return "./";
			} else if (path.equalsIgnoreCase(syspath) || path.equalsIgnoreCase(syspath + File.separator)) {
				return "./";
			} else if ((System.getProperty("os.name").startsWith("Windows")) && (path.startsWith(syspath.substring(0, 2)) == false)) {
				return path; // if not on the same drive.. return the absolute path
			} else if ((path.startsWith(syspath))) {
				String newpath = "";
				newpath = "./" + path.substring(syspath.length() + 1);
				newpath = newpath.replaceAll("\\\\", "/");
				return newpath;
			}

			return path;

		} catch (Exception e) {
			return "./";
		}
	}

	/**
	 * @returns String representation of all variables of LoggerParameters instance.
	 */
	@Override
	public final String toString() {
		String s = "";
		s += isEnabled() ? "1" : "0";
		s += "|";
		s += (boolExecTimestamp.booleanValue() == true) ? "1" : "0";
		s += (boolLoggername.booleanValue() == true) ? "1" : "0";
		s += (boolTimeStamp.booleanValue() == true) ? "1" : "0";
		s += (boolJobID.booleanValue() == true) ? "1" : "0";
		s += (boolJobClass.booleanValue() == true) ? "1" : "0";
		s += (boolTimeSameClass.booleanValue() == true) ? "1" : "0";
		s += (boolTimeAnyClass.booleanValue() == true) ? "1" : "0";
		s += "|";
		s += name;
		s += "|";

		return s;
	}

	/**
	 * Convenience: Calculates if the logger should log to a file
	 * @return Returns <em>true</em> if (1) the logger is enabled, and (2) if there is something to log
	 */
	public boolean isEnabled() {
		try {
			if (isEnabled == false) {
				return isEnabled;
			}

			if ((boolExecTimestamp.booleanValue() == false) && (boolLoggername.booleanValue() == false) && (boolTimeStamp.booleanValue() == false)
					&& (boolJobID.booleanValue() == false) && (boolJobClass.booleanValue() == false) && (boolTimeSameClass.booleanValue() == false)
					&& (boolTimeAnyClass.booleanValue() == false)) // then..
			{
				this.disable();
				return isEnabled;
			}
		} catch (NullPointerException npe) {
			this.disable();
			npe.printStackTrace();
			return isEnabled;
		}

		// The above proves there is no disabling reason, so enable.
		return isEnabled;
	}

	public boolean enable() {
		isEnabled = true;
		return isEnabled();
	}

	public void disable() {
		isEnabled = false;
	}

	public boolean isGlobal() {
		if (name.equalsIgnoreCase(GLOBALLOGNAME)) {
			return true;
		} else {
			return false;
		}
	}
}
