/* SimEvent.java
 */

/**
 * 2013. Modified for QN-Java project
 * 
 * [2013-08-09] setEventTime
 * 
 */

package jmt.engine.simEngine;

/**
 * This class represents events which are passed between the entities
 * in the simulation.
 */

public class SimEvent implements Cloneable {
	// Private data members
	private int etype; // internal event type
	private double time; // simulation time in which event should occur
	private int entSrc; // id of entity which scheduled event
	private int entDst; // id of entity which event will be sent to
	private int tag; // the user defined type of event
	private Object data; // any data the event is carrying
	/** This field should never be set explicitly. It's used by the EventQueue for ordering purposes. */
	int internalOrdering;

	//
	// Public library interface
	//

	// Internal event types

	//initial value when the event has just been created
	static final int ENULL = 0;
	//the event is a message to another entity
	static final int SEND = 1;
	//used to let an entity know that its waiting time has finished
	static final int HOLD_DONE = 2;
	//this event creates dinamically a new entity
	static final int CREATE = 3;

	// Constructors
	/** Contructor, create a blank event. Useful for fetching events
	 * using methods such as <tt>SimEntity.simWait(ev)</tt>.
	 */
	public SimEvent() {
		etype = ENULL;
		this.time = -1.0;
		entSrc = -1;
		entDst = -1;
		this.tag = -1;
		data = null;
	}

	// Package level constructors
	SimEvent(int evtype, double time, int src, int dest, int tag, Object edata) {
		etype = evtype;
		this.time = time;
		entSrc = src;
		entDst = dest;
		this.tag = tag;
		data = edata;
	}

	SimEvent(int evtype, double time, int src) {
		etype = evtype;
		this.time = time;
		entSrc = src;
		entDst = -1;
		this.tag = -1;
		data = null;
	}

	// Public access methods
	/** Get the unique id number of the entity which is the destination of this event.
	 * @return the id number
	 */
	public int getDest() {
		return entDst;
	}

	/** Get the unique id number of the entity which scheduled this event.
	 * @return the id number
	 */
	public int getSrc() {
		return entSrc;
	}

	/** Get the simulation time which this event was scheduled for.
	 * @return The simulation time
	 */
	public double eventTime() {
		return time;
	}

	/** Get the user-defined tag in this event
	 * @return The tag
	 */
	public int type() {
		return tag;
	} // The user defined type

	/** Get the unique id number of the entity which scheduled this event.
	 * @return the id number
	 */
	public int scheduledBy() {
		return entSrc;
	}

	/** Get the user-defined tag in this event.
	 * @return The tag
	 */
	public int getTag() {
		return tag;
	}

	/** Get the data passed in this event.
	 * @return A reference to the data
	 */
	public Object getData() {
		return data;
	}

	// Public modifying methods
	/** Create an exact copy of this event.
	 * @return A reference to the copy
	 */
	@Override
	public Object clone() {
		return new SimEvent(etype, time, entSrc, entDst, tag, data);
	}

	/** Set the source entity of this event.
	 * @param s The unique id number of the entity
	 */
	public void setSrc(int s) {
		entSrc = s;
	}

	/** Set the destination entity of this event.
	 * @param d The unique id number of the entity
	 */
	public void setDest(int d) {
		entDst = d;
	}

	//
	// Package level methods
	//

	int getType() {
		return etype;
	} // The internal type

	void copy(SimEvent ev) {
		entDst = ev.getDest();
		entSrc = ev.getSrc();
		time = ev.eventTime();
		etype = ev.getType();
		tag = ev.getTag();
		data = ev.getData();
	}

	/**
	 * Returns a string representation of the object. In general, the
	 * <code>toString</code> method returns a string that
	 * "textually represents" this object. The result should
	 * be a concise but informative representation that is easy for a
	 * person to read.
	 * It is recommended that all subclasses override this method.
	 * <p>
	 * The <code>toString</code> method for class <code>Object</code>
	 * returns a string consisting of the name of the class of which the
	 * object is an instance, the at-sign character `<code>@</code>', and
	 * the unsigned hexadecimal representation of the hash code of the
	 * object. In other words, this method returns a string equal to the
	 * value of:
	 * <blockquote>
	 * <pre>
	 * getClass().getName() + '@' + Integer.toHexString(hashCode())
	 * </pre></blockquote>
	 *
	 * @return  a string representation of the object.
	 */
	@Override
	public String toString() {
		return "type = " + etype + "; time = " + time + "; src = " + entSrc + "; dest = " + entDst + "; tag = " + tag + "; data = " + data + "\n";
	}
	
	//QN-Java
	public void setEventTime(double newTime){
	  time = newTime;
	}
}
