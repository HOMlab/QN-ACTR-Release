package jmt.engine.simEngine;

import java.util.Comparator;
import java.util.Iterator;
import java.util.PriorityQueue;
import java.util.Queue;

import qnactr.sim.GlobalUtilities;

import jmt.framework.data.CircularList;

/**
 * <p><b>Name:</b> HybridEventQueue</p> 
 * <p><b>Description:</b> 
 * An hybrid implementation for the EventQueue interface. Future events are inserted and retrieved in a sorted
 * binary heap with log(n) performances, while events for the current time interval are inserted and retrieved
 * using an unbounded circular list. 
 * </p>
 * <p><b>Date:</b> 12/mag/2009
 * <b>Time:</b> 18:40:26</p>
 * @author Bertoli Marco
 * @version 1.1
 */

/**
 * 2013. Modified for QN-Java project
 * 
 * [2013-07-20] getCurrentList
 * 
 */
public class HybridEventQueue implements EventQueue {
	private int DEFAULT_INITIAL_CAPACITY = 111;
	/** Current events */
	private CircularList<SimEvent> current;
	/** Future events*/
	private Queue<SimEvent> future;
	/** Current time. All events in current event queue will have this time. */
	private double currentTime;
	/** A counter used to order future events basing on event time and insertion order */
	private int order;

	public HybridEventQueue() {
		clear();
	}

	/* (non-Javadoc)
	 * @see jmt.engine.simEngine.EventQueue#add(jmt.engine.simEngine.SimEvent)
	 */
	public boolean add(SimEvent event) {
		double eventTime = event.eventTime();
		
		//QN-Java, make sure all event times are accurate to 0.001, avoiding difference between 1.0000001 s and 1.0000002 s
		eventTime = GlobalUtilities.round(eventTime, 3);
		event.setEventTime(eventTime);
		//
		
		if (eventTime == currentTime) {
			current.add(event);
		} else if (eventTime > currentTime) {
			addToFuture(event);
		} else {
			// Probably this condition will never happen but we are robust to it. We leaqve to the simulation
			// engine the choice to deal with past events.
			moveCurrentToFuture();
			currentTime = eventTime;
			current.add(event);
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see jmt.engine.simEngine.EventQueue#clear()
	 */
	public void clear() {
		current = new CircularList<SimEvent>();
		future = new PriorityQueue<SimEvent>(DEFAULT_INITIAL_CAPACITY, new SimEventComparator());
		currentTime = 0.0;
		order = Integer.MIN_VALUE;
	}

	/* (non-Javadoc)
	 * @see jmt.engine.simEngine.EventQueue#iterator()
	 */
	public Iterator<SimEvent> iterator() {
		return new Iter();
	}

	/* (non-Javadoc)
	 * @see jmt.engine.simEngine.EventQueue#peek()
	 */
	public SimEvent peek() {
		handleCurrent();
		if (current.size() > 0) {
			return current.getFirst();
		} else {
			return null;
		}
	}

	/* (non-Javadoc)
	 * @see jmt.engine.simEngine.EventQueue#pop()
	 */
	public SimEvent pop() {
		handleCurrent();
		if (current.size() > 0) {
			return current.removeFirst();
		} else {
			return null;
		}
	}

	/**
	 * This method will move events from future to current queue if it's empty.
	 */
	private void handleCurrent() {
		if (current.size() == 0 && future.size() > 0) {
			SimEvent first = future.remove();
			currentTime = first.eventTime();
			current.add(first);
			while (future.size() > 0 && future.peek().eventTime() == currentTime) {
				current.add(future.poll());
			}
		}
	}

	/**
	 * This method will move the entire current queue to future. This is needed when an event 
	 * older than current one is queued. Probably will never happen because the simulator currently 
	 * avoid this.
	 */
	private void moveCurrentToFuture() {
		while (current.size() > 0) {
			addToFuture(current.removeFirst());
		}
	}

	/**
	 * This method will build a new future buffer with new order indices. As the total number of indices is
	 * 2^32 this method probably will never be called.
	 */
	private void rebuildOrderIndices() {
		Queue<SimEvent> tmp = future;
		future = new PriorityQueue<SimEvent>(DEFAULT_INITIAL_CAPACITY, new SimEventComparator());
		order = Integer.MIN_VALUE;
		while (tmp.size() > 0) {
			addToFuture(tmp.remove());
		}
	}

	/**
	 * Adds an event to future queue.
	 * @param event
	 */
	private void addToFuture(SimEvent event) {
		if (order == Integer.MAX_VALUE) {
			// Need to rebuild future events queue becauseorder indices were finished. Probably this case will never happen
			// but we are robust to it.
			rebuildOrderIndices();
		}
		event.internalOrdering = order++;
		future.add(event);
	}

	/* (non-Javadoc)
	 * @see jmt.engine.simEngine.EventQueue#remove(jmt.engine.simEngine.SimEvent)
	 */
	public boolean remove(SimEvent ev) {
		if (ev.eventTime() == currentTime) {
			return current.remove(ev);
		} else {
			return future.remove(ev);
		}
	}

	/* (non-Javadoc)
	 * @see jmt.engine.simEngine.EventQueue#size()
	 */
	public int size() {
		return current.size() + future.size();
	}	

	/**
	 * Internal Iterator implementation
	 */
	private class Iter implements Iterator<SimEvent> {
		private Iterator<SimEvent> currentIter = current.iterator();
		private Iterator<SimEvent> futureIter = future.iterator();
		private boolean isCurrent = true;

		/* (non-Javadoc)
		 * @see java.util.Iterator#hasNext()
		 */
		public boolean hasNext() {
			return currentIter.hasNext() || futureIter.hasNext();
		}

		/* (non-Javadoc)
		 * @see java.util.Iterator#next()
		 */
		public SimEvent next() {
			if (currentIter.hasNext()) {
				isCurrent = true;
				return currentIter.next();
			} else {
				isCurrent = false;
				return futureIter.next();
			}
		}

		/* (non-Javadoc)
		 * @see java.util.Iterator#remove()
		 */
		public void remove() {
			if (isCurrent) {
				currentIter.remove();
			} else {
				futureIter.remove();
			}
		}

	}

	private static class SimEventComparator implements Comparator<SimEvent> {
		/* (non-Javadoc)
		 * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
		 */
		public int compare(SimEvent e1, SimEvent e2) {
			if (e1.eventTime() > e2.eventTime()) {
				return 1;
			} else if (e1.eventTime() < e2.eventTime()) {
				return -1;
			} else {
				if (e1.internalOrdering > e2.internalOrdering) {
					return 1;
				} else if (e1.internalOrdering < e2.internalOrdering) {
					return -1;
				} else {
					return 0;
				}
			}
		}

	}

	
	
	//CAO add
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
    String str = new String();
    
    str += "current event list:\n";
    str +=  current.toString() + "\n";
    
    str += "future event list:\n";
    str +=  future.toString() + "\n";
    
    return str;
  }
	
  public CircularList<SimEvent> getCurrentList (){
    return current;
  }
}
