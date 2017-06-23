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

/* SimEntity.java
*/

package jmt.engine.simEngine;

/**
 * This class represents entities, or processes, running in the system.
 * To create a new type of entity, it should be <tt>extended</tt> and
 * a definition for the <tt>body()</tt> method given. The <tt>body()</tt>
 * method is called by the <tt>SimSystem</tt> and defines the behaviour of
 * the entity during the simulation. <p>
 *
 * @author      Federico Granata
 */

public abstract class SimEntity {
	// Private data members
	private String name; // The entities name
	private int me; // Unique id
	private SimEvent evbuf; // For incoming events
	protected int state; // Our current state from list below
	private SimPredicate waitingPred; //the predicate that the SimWaitFor is waiting

	//
	// Public library interface
	//

	// Public constructor
	/** The standard constructor.
	 * @param name The name to be associated with this entity
	 */
	public SimEntity(String name) {
		this.name = name;
		me = -1;
		state = RUNNABLE;
		// Adding this to SimSystem automatically
		SimSystem.add(this);
	}

	// Public access methods
	/** Get the name of this entity
	 * @return The entity's name
	 */
	public String getName() {
		return name;
	}

	/** Get the unique id number assigned to this entity
	 * @return The id number
	 */
	public int getId() {
		return me;
	}

	// The body function which should be overridden
	/** The method which defines the behaviour of the entity. This method
	 * should be overridden in subclasses of SimEntity.
	 */
	protected abstract void body();

	/**
	 * check that the body of Entity is closed correctly
	 */
	private void exit() throws jmt.common.exception.NetException {
		if (state == SimEntity.RUNNABLE) {
			throw new jmt.common.exception.NetException("body of entity " + getName() + " ended incorrectly the status at the end of the body "
					+ "must be waiting or holding");

		}
	}

	// Runtime methods
	/** Causes the entity to hold for <tt>delay</tt> units of simulation time.
	 * @param delay The amount of time to hold
	 */
	public final void simHold(double delay) {
		setState(HOLDING);
		SimSystem.hold(me, delay);
	}

	//TODO: METTERE A POSTO
	/** An interruptable hold.
	 * Causes the entity to hold for <tt>delay</tt> units of simulation time.
	 * @param delay The amount of time to hold
	 * @param ev Returns the event if hold interrupted
	 * @return The amount of time left on the hold (0.0 if no interruptions)
	 */
	//
	public final double simHoldFor(double delay) {
		double start_t = SimSystem.getClock();
		simSchedule(me, delay, 9999); // Send self 'hold done' msg
		// SimEvent ev2 = new SimEvent();
		simWaitFor(SimSystem.SIM_ANY);
		if (evbuf.getTag() == 9999) {
			return 0.0;
		} else { // interrupted
			SimTypeP p = new SimTypeP(9999);
			double time_left = delay - (evbuf.eventTime() - start_t);
			simCancel(p, null);
			return time_left;
		}
	}

	// The schedule functions
	/** Send an event to another entity, by id number with data.
	 * @param dest The unique id number of the destination entity
	 * @param delay How long from the current simulation time the event
	 *              should be sent
	 * @param tag An user-defined number representing the type of event.
	 * @param data A reference to data to be sent with the event.
	 * @return a token to remove scheduled message
	 */
	public final RemoveToken simSchedule(int dest, double delay, int tag, Object data) {
		return SimSystem.send(me, dest, delay, tag, data);
	}

	/** Send an event to another entity, by id number and with <b>no</b> data.
	 * @param dest The unique id number of the destination entity
	 * @param delay How long from the current simulation time the event
	 *              should be sent
	 * @param tag An user-defined number representing the type of event.
	 * @return a token to remove scheduled message
	 */
	public final RemoveToken simSchedule(int dest, double delay, int tag) {
		return SimSystem.send(me, dest, delay, tag, null);
	}

	/**
	 * Removes a scheduled event from future or deferred queue
	 * @param token the token to remove the element
	 * @return true if event was found and removed, false if it was not found
	 */
	public final boolean simUnschedule(RemoveToken token) {
		return SimSystem.remove(token);
	}

	/**
	 * Hold until the entity receives an event.
	 */
	public final void simWait() {
		setState(SimEntity.WAITING);
		SimSystem.wait(me);
	}

	/** Count how many events matching a predicate are waiting
	 * for this entity on the deferred queue.
	 * @param p The event selection predicate
	 * @return The count of matching events
	 */
	public final int simWaiting(SimPredicate p) {
		return SimSystem.waiting(me, p);
	}

	/** Count how many events are waiting for this entity on the deferred queue
	 * @return The count of events
	 */
	public final int simWaiting() {
		return SimSystem.waiting(me, SimSystem.SIM_ANY);
	}

	/** Extract the first event waiting for this entity on the deferred
	 * queue, matched by the predicate <tt>p</tt>.
	 * @param p An event selection predicate
	 */
	public final void simSelect(SimPredicate p) {
		SimSystem.select(me, p);
	}

	/** Cancel the first event waiting for this entity on the future
	 * queue, matched by the predicate <tt>p</tt>. Returns the
	 * number of events cancelled (0 or 1).
	 * @param p    An event selection predicate
	 * @param ev   The event matched is copied into <tt>ev</tt> if
	 *             it points to a blank event, or discarded if <tt>ev</tt> is
	 *             <tt>null</tt>
	 */
	public final int simCancel(SimPredicate p, SimEvent ev) {
		SimSystem.cancel(me, p);
		if ((ev != null) && (evbuf != null)) {
			ev.copy(evbuf);
		}
		if (evbuf != null) {
			return 1;
		} else {
			return 0;
		}
	}

	/** Repeatedly <tt>simWait()</tt> until the entity receives an event
	 * matched by the specified predicate, all the other received events
	 * are discarded.
	 * @param p The event selection predicate
	 */
	public final void simWaitFor(SimPredicate p) {
		waitingPred = p;
		simWait();
	}

	/** Puts an event back on the deferred queue.
	 * @param ev The event to reinsert
	 * @return a token to remove scheduled message
	 */
	public final RemoveToken simPutback(SimEvent ev) {
		return SimSystem.putback((SimEvent) ev.clone());
	}

	/** Gets the first event matching a predicate from the deferred queue,
	 * or, if none match, wait for a matching event to arrive.
	 * @param p The predicate to match
	 */
	public final void simGetNext(SimPredicate p) {
		if (simWaiting(p) > 0) {
			simSelect(p);
		} else {
			simWaitFor(p);
		}
	}

	/** Get the first event from the deferred queue waiting on the entity,
	 * or, if there are none, wait for an event to arrive.
	 */
	public final void simGetNext() {
		simGetNext(SimSystem.SIM_ANY);
	}

	/** Get the id of the currently running entity
	 * @return A unique entity id number
	 */
	public final int simCurrent() {
		return this.getId();
	}

	//
	// Package level methods
	//

	// Package access methods
	final int getState() {
		return state;
	}

	public final SimEvent getEvbuf() {
		return evbuf;
	}

	// The states

	/**
	 * State RUNNING: the entity can be run by the simulation
	 */
	protected static final int RUNNABLE = 0;

	/**
	 *  State WAITING: the entity is waiting for an event.
	 */
	protected static final int WAITING = 1;

	/**
	 * State HOLDING: the entity has been caused to wait for a specified number of units of simulation time.
	 */
	protected static final int HOLDING = 2;

	/**
	 * State FINISHED: the entity has finished its behaviour.
	 */
	protected static final int FINISHED = 3;

	// Package update methods
	/**
	 * Restarts the entity after an hold period
	 */
	public abstract void restart();

	final void setState(int state) {
		this.state = state;
	}

	final void setId(int id) {
		me = id;
	}

	final void setEvbuf(SimEvent e) {
		evbuf = e;
	}

	/**
	 * Changes the state of the entity into FINISHED.
	 */
	public void poison() {
		setState(FINISHED);
	}

	/**
	 * Starts the activity of the SimEntity, call it the first time before
	 * calling execute.
	 */
	public abstract void start();

	/**
	 * Returns the Predicate which this entity is waiting for
	 * @return
	 */

	public final SimPredicate getWaitingPred() {
		return waitingPred;
	}

	/**
	 *
	 * /TODO: cosa significa??
	 * runs the activity of this SimEntiy remember that at the end of the action
	 * the SimEntiy must be waiting holding or not exiting. otherwise the
	 * simulation will be interrupted (read aborted).
	 */
	final void execute() throws jmt.common.exception.NetException {
		body();
		exit();
	}
	


}
