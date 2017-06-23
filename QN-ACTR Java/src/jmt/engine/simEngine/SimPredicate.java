/* Sim_predicate.java
 */

package jmt.engine.simEngine;

/**
 * Predicates are used to select events from the deferred queue.
 * This class is abstract and must be subclassed when writing new
 * predicate.
 */

public abstract class SimPredicate {
	/**
	 * The match function which must be overridden when writing a new
	 * predicate. The function is called with each event in the deferred
	 * queue as its parameter when a <tt>SimSystem.simSelect()</tt>
	 * call is made by the user.
	 * @param event The event to test for a match.
	 * @return The function should return <tt>true</tt> if the
	 *         event matches and shoult be selected, of <tt>false</tt>
	 *         if it doesn't
	 */
	public abstract boolean match(SimEvent event);
}
