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

package jmt.engine.simEngine;

import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

//TODO: testare mettere a posto...classe di prova!
/**
 * This class implements an event queue used internally by the Sim_system to
 * manage the list of Sim_events.
 * @author Federico Granata
 * @version 7-ott-2003 14.50.16
 */
public class CircularEventQueue implements EventQueue {

	private static final boolean DEBUG = false;

	private SimEvent[] data;//data Objects

	private int start = -1;//first element pointer

	private int end = -1;//last element pointer

	private int size = 0;//size

	private SimEvent buffer;//buffer element

	/**
	 * Allocates a new EventQueue object, with an initial capacity.
	 * @param initialCapacity	The initial capacity of the queue.
	 */
	public CircularEventQueue(int initialCapacity) {
		if (initialCapacity <= 0) {
			data = new SimEvent[2];
		} else {
			data = new SimEvent[initialCapacity];
		}
	}

	/**
	 * Allocates a new EventQueue object.
	 */
	public CircularEventQueue() {
		data = new SimEvent[2];
	}

	/**
	 * Returns the number of components in this vector.
	 *
	 * @return  the number of components in this vector.
	 */
	public final synchronized int size() {
		return size;
	}

	/**
	 * Inserts the specified object as a component in this vector at the
	 * specified <code>index</code>. Each component in this vector with
	 * an index greater or equal to the specified <code>index</code> is
	 * shifted upward to have an index one greater than the value it had
	 * previously. <p>
	 *
	 * The index must be a value greater than or equal to <code>0</code>
	 * and less than or equal to the current size of the vector. (If the
	 * index is equal to the current size of the vector, the new element
	 * is appended to the Vector.)<p>
	 *
	 * This method is identical in functionality to the add(Object, int) method
	 * (which is part of the List interface). Note that the add method reverses
	 * the order of the parameters, to more closely match array usage.
	 *
	 * @param      event     the event to insert.
	 * @param      index   where to insert the new component.
	 * @exception  ArrayIndexOutOfBoundsException  if the index was invalid.
	 * @see        #size()
	 * @see	   List
	 */
	private synchronized void insertElementAt(SimEvent event, int index) {
		if (size != 0) {
			if (index == start) {
				size++;
				if (start != 0) {//insert the elemnt at start -1;
					start--;
					data[start] = event;
				} else {
					if (size > data.length) {//need to resize
						//move data in another array
						SimEvent[] oldData = data;
						//double the size of array
						data = new SimEvent[data.length * 2];
						//copy all old data
						System.arraycopy(oldData, 0, data, 1, size - 1);
						//insert the new element at first place
						data[0] = event;
						end++;
					} else {
						//moves data one place  up the array
						System.arraycopy(data, 0, data, 1, size - 1);
						//insert the new element at first place
						data[0] = event;
						end++;
					}
				}
			} else if (index == end + 1) {//need to insert at the end of the data
				size++;
				end++;
				if (end < data.length) {//last place free
					data[end] = event;
				} else {
					if (size > data.length) {
						//data vector too short
						//move data in another array
						SimEvent[] oldData = data;
						//double the size of array
						data = new SimEvent[data.length * 2];
						//copy all old data end put it int right place (size -1 because we have counted the element we have not inseted)
						System.arraycopy(oldData, start, data, 0, size - 1);
						start = 0;
						//the new element is considered inserted(see next instruction)
						end = size - 1;
						//insert the elemnt in last position
						data[end] = event;
					} else {
						//move data at the start of the array to find place
						System.arraycopy(data, start, data, 0, size - 1);
						//insert the elemnt in last position
						start = 0;
						end = size - 1;
						data[end] = event;
					}
				}
			} else {//insert inside the middle of the data
				size++;
				if (size < data.length) {
					end++;
					if (end < data.length) {
						//move data from index of 1 position
						System.arraycopy(data, index, data, index + 1, end - index);
						data[index] = event;
					} else {
						end--;
						//move the first part of the array at pos 0..
						System.arraycopy(data, start, data, 0, index - start);
						//move second part of data
						System.arraycopy(data, index, data, index - start + 1, end - index + 1);
						data[index - start] = event;
						//						end = end - start;
						end = size - 1;
						start = 0;
					}
				} else {//data vector too short
					//move data in another array
					SimEvent[] oldData = data;
					//double the size of array
					data = new SimEvent[data.length * 2];
					//move the first part of the array at pos 0..
					System.arraycopy(oldData, start, data, 0, index - start);
					//move second part of data
					System.arraycopy(oldData, index, data, index - start + 1, end - index + 1);
					data[index - start] = event;
					start = 0;
					end = size - 1;
				}
			}
		} else {
			size++;
			start = 0;
			end = 0;
			data[start] = event;
		}
	}

	/**
	 * Add a new event to the queue, preserving the temporal order of the
	 * events in the queue.
	 * @param new_event	The event to be put on the queue.
	 */
	public final boolean add(SimEvent new_event) {
		int i = 0;
		if (size != 0) {
			double evTime = new_event.eventTime();

			for (i = start; i <= end; i++) {
				buffer = data[i];
				if (buffer.eventTime() > evTime) {
					break;
				}
			}
		}
		insertElementAt(new_event, i);
		if (DEBUG) {
			for (i = start; i < end - 1; i++) {
				if (data[i].eventTime() > data[i + 1].eventTime()) {
					throw new RuntimeException("position incorrect  " + this);
				}
			}
		}
		return true;
	}

	void push(SimEvent newEvent) {
		insertElementAt(newEvent, end + 1);
		if (DEBUG) {
			for (int i = start; i < end - 1; i++) {
				if (data[i].eventTime() > data[i + 1].eventTime()) {
					throw new RuntimeException("position incorrect");
				}
			}
		}
	}

	/**
	 * Remove and return the event at the top of the queue.
	 * @return           The next event.
	 */
	public final SimEvent pop() {
		if (size != 0) {
			buffer = data[start];
			if (size == 1) {//last element
				size = 0;
				start = -1;
				end = -1;
				if (DEBUG) {
					if (start != -1 && buffer.eventTime() > data[start].eventTime()) {
						throw new RuntimeException("position incorrect");
					}
					for (int i = start; i < end - 1; i++) {
						if (data[i].eventTime() > data[i + 1].eventTime()) {
							throw new RuntimeException("position incorrect");
						}
					}
				}
			} else {//move the start pointer up
				size--;
				start++;
				if (DEBUG) {
					if (start != -1 && buffer.eventTime() > data[start].eventTime()) {
						throw new RuntimeException("position incorrect");
					}
					for (int i = start; i < end - 1; i++) {
						if (data[i].eventTime() > data[i + 1].eventTime()) {
							throw new RuntimeException("position incorrect");
						}
					}
				}
			}

			//return the last element
			return buffer;
		} else {
			return null;
		}
	}

	/**
	 * Return the event at the top of the queue, without removing it.
	 * @return	The next event.
	 */
	public final SimEvent top() {
		if (size != 0) {
			return data[start];
		} else {
			return null;
		}
	}

	/**
	 * removes the selected element
	 * @param event
	 */
	public boolean removeElement(SimEvent event) {
		int i = -1;
		for (i = start; i <= end; i++) {
			if (data[i].equals(event)) {
				break;
			}
		}
		if (i >= 0) {
			remove(i);
			return true;
		} else {
			return false;
		}
	}

	private void remove(int index) {
		if (index >= start) {
			if (index < end) {
				end--;
				size--;
			} else if (index == end) {
				end--;
				size--;
			}
		}
		if (size == 0) {
			start = -1;
			end = -1;
		}
	}

	/** clears the queue of all its elements
	 *
	 */
	public void clear() {
		size = 0;
		start = -1;
		end = -1;
	}

	/**
	 * Returns an enumeration of the components of this vector. The
	 * returned <tt>Enumeration</tt> object will generate all items in
	 * this vector. The first item generated is the item at index <tt>0</tt>,
	 * then the item at index <tt>1</tt>, and so on.
	 *
	 * @return  an enumeration of the components of this vector.
	 * @see     Enumeration
	 * @see     java.util.Iterator
	 */
	public Enumeration<SimEvent> elements() {
		return new Enumeration<SimEvent>() {
			int count = start;

			public boolean hasMoreElements() {
				return count < size;
			}

			public SimEvent nextElement() {
				synchronized (CircularEventQueue.this) {
					if (count < size) {
						return data[count++];
					}
				}
				throw new NoSuchElementException("Vector Enumeration");
			}
		};
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
		String str = new String();
		str += "start = " + start;
		str += "; end = " + end;
		str += "; size = " + size;
		str += "; length = " + data.length;
		str += "\n";
		if (start > -1) {
			for (int i = start; i <= end; i++) {
				str += "pos = " + (i - start) + "  " + data[i];
			}
		}
		return str;
	}

	public static void test() {
		CircularEventQueue q = new CircularEventQueue();

		for (int i = 0; i < 5; i++) {
			q.add(new SimEvent(0, i, 0, 0, 0, null));
			System.out.println("q = \n" + q);
			System.out.println("");
		}
		for (int i = 0; i < 10; i++) {
			q.add(new SimEvent(0, 3, 0, 0, i, null));
			System.out.println("q = \n" + q);
			System.out.println("");
		}
		for (int i = 11; i < 30; i++) {
			q.pop();
			q.add(new SimEvent(0, 3, 0, 0, i, null));
			System.out.println("q = \n" + q);
			System.out.println("");
		}

		//	   System.out.println("q = \n" + q);

	}

	public Iterator<SimEvent> iterator() {
		return new Iter();
	}

	public SimEvent peek() {
		return top();
	}

	public boolean remove(SimEvent ev) {
		return removeElement(ev);
	}

	private class Iter implements Iterator<SimEvent> {
		Enumeration<SimEvent> en = elements();
		SimEvent lastEvent;

		public boolean hasNext() {
			return en.hasMoreElements();
		}

		public SimEvent next() {
			lastEvent = en.nextElement();
			return lastEvent;
		}

		public void remove() {
			CircularEventQueue.this.remove(lastEvent);
		}

	}

}
