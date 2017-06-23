package jmt.engine.jwat.workloadAnalysis.utils;

import java.awt.Window;
import java.util.Vector;

public class JWatWorkloadManager {
	public static Vector<Window> windows = new Vector<Window>();

	/**
	 * Adds a new Window to be checked for termination before terminating JVM
	 * @param application JMT application to be checked
	 */
	public static void addJMTWindow(Window application) {
		if (!windows.contains(application)) {
			windows.add(application);
		}
	}

	/**
	 * This method has to be called each time a JMT application is terminated. If all applications
	 * are terminated, JVM is killed.
	 * @param application application to be terminated. If application was not disposed,
	 * this method will do 'dispose()' too.
	 */
	public static void exit(Window application) {
		// Disposes application window if user didn't do it.
		if (application.isDisplayable()) {
			application.dispose();
		}
		windows.remove(application);
	}

	/**
	 * This method has to be called when application JWat->WorkloadAnalysis is terminated. All windows that
	 * are open must be closed
	 */
	public static void closeAll() {
		while (windows.size() != 0) {
			windows.remove(windows.size() - 1).dispose();
		}
	}
}
