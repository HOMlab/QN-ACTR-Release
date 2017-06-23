/* Sim_any_p.java
 */

package jmt.engine.simEngine;

/**
 * A predicate which will match any event on the deferred event queue.
 * There is a publicly accessible instance of this predicate in the
 * Sim_system class, called Sim_system.SIM_ANY, so the user does
 * not need to create any new instances.
 * @see         eduni.simjava.Sim_predicate
 * @see         eduni.simjava.Sim_system
 * @version     1.0, 4 September 1996
 * @author      Ross McNab
 */

public class SimAnyP extends SimPredicate {
	/** Constructor.
	 */
	public SimAnyP() {
	};

	/** The match function called by Sim_system.simSelect(),
	 * not used directly by the user
	 */
	@Override
	public boolean match(SimEvent ev) {
		return true;
	}
}
