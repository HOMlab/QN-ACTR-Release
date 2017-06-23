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
package jmt.framework.data;

import java.io.Serializable;
import java.util.AbstractList;
import java.util.Collection;
import java.util.ConcurrentModificationException;
import java.util.Iterator;
import java.util.List;
import java.util.RandomAccess;

/**
 * <p><b>Name:</b> CircularList</p> 
 * <p><b>Description:</b> 
 * A random access circular list based on an array, optimized for speed in append and remove 
 * operations in the head or tail of the list.
 * The array is able to grow when internal capacity is finished, with an O(n) complexity.
 * <br>
 * CircularQueue is optimized to have O(1) complexity on each get operation and in add and remove operations
 * at the beginning and at the end of the list. Other operations are O(n). This implementation is good for
 * unordered, unbounded LIFO/FIFO queues.
 * </p>
 * <p><b>Date:</b> 10/giu/2009
 * <b>Time:</b> 19:22:03</p>
 * @author Bertoli Marco
 * @version 1.0
 */
public class CircularList<E> extends AbstractList<E> implements List<E>, RandomAccess, Cloneable, Serializable {
	private static final long serialVersionUID = 1L;
	private static final int DEFAULT_SIZE = 10;

	private transient E[] elements;
	private transient int head, tail;
	private int size;

	/**
	 * Builds a new Circual list, using the default size
	 */
	public CircularList() {
		this(DEFAULT_SIZE);
	}

	/**
	 * Builds a new CircularList with the given initial size
	 * @param initialSize the initial size
	 */
	public CircularList(int initialSize) {
		init(initialSize);
	}

	/**
	 * Initializes the internal array with the given size
	 * @param size the size for the inner array
	 */
	@SuppressWarnings("unchecked")
	private void init(int size) {
		elements = (E[]) new Object[size];
		head = 0;
		tail = -1;
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractCollection#size()
	 */
	@Override
	public int size() {
		return size;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#add(java.lang.Object)
	 */
	@Override
	public boolean add(final E o) {
		ensureCapacity(size + 1);
		tail = increment(tail, 1);
		elements[tail] = o;
		size++;
		modCount++;
		return true;
	}

	/**
	 * Adds an object at the beginning of the list
	 * @param o the object to add
	 */
	public void addFirst(final E o) {
		ensureCapacity(size + 1);
		head = increment(head, -1);
		elements[head] = o;
		size++;
		modCount++;
	}

	/**
	 * Adds an object to the end of the list. It's a copy of add method.
	 * @param o the object to add
	 * @see #add(Object)
	 */
	public void addLast(final E o) {
		add(o);
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractList#addAll(int, java.util.Collection)
	 */
	@Override
	public boolean addAll(int index, final Collection<? extends E> c) {
		ensureCapacity(size + c.size());

		for (Iterator<? extends E> it = c.iterator(); it.hasNext();) {
			tail = increment(tail, 1);
			elements[tail] = it.next();
			size++;
		}

		if (c.size() > 0) {
			modCount++;
			return true;
		} else {
			return false;
		}
	}

	/**
	 * Ensures capacity for the internal buffer. If capacity is not enough, doubles it.
	 * @param capacity the capacity to ensure
	 */
	@SuppressWarnings("unchecked")
	private void ensureCapacity(int capacity) {
		if (capacity > elements.length) {
			E[] tmp = (E[]) new Object[(elements.length * 2)];

			int j = 0, i = head;
			while (j < size) {
				// We copy and null the reference to avoid out of memory problems.
				tmp[j] = elements[i];
				elements[i] = null;

				j++;
				i++;
				if (i == elements.length) {
					i = 0;
				}
			}

			head = 0;
			tail = j - 1;
			elements = tmp;
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#clear()
	 */
	@Override
	public void clear() {
		init(elements.length);
		modCount++;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#clone()
	 */
	@Override
	public Object clone() {
		CircularList<E> clone = new CircularList<E>(elements.length);
		clone.addAll(this);
		return clone;
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractList#get(int)
	 */
	@Override
	public E get(int index) {
		return elements[translateIndex(index)];
	}

	/**
	 * @return the first element of the list
	 * @throws IllegalArgumentException if the list is empty
	 */
	public E getFirst() {
		if (size == 0) {
			throw new IndexOutOfBoundsException("Size is 0");
		}
		return elements[head];
	}

	/**
	 * @return the last element of the list
	 * @throws IllegalArgumentException if the list is empty
	 */
	public E getLast() {
		if (size == 0) {
			throw new IndexOutOfBoundsException("Size is 0");
		}
		return elements[tail];
	}

	/**
	 * Translates an index from external to internal one
	 * @param externalIndex the external index
	 * @param the size used for the IndexOutOfBoundsException check
	 * @return translated index
	 */
	private int translateIndex(int externalIndex, int size) {
		if (externalIndex < 0 || externalIndex >= size) {
			throw new IndexOutOfBoundsException("size: " + size() + ", requested index: " + externalIndex);
		}
		return doTranslateIndex(externalIndex);
	}

	/**
	 * Translates an index from external to internal one
	 * @param externalIndex the external index
	 * @return translated index
	 */
	private int translateIndex(int externalIndex) {
		return translateIndex(externalIndex, size);
	}

	/**
	 * Translates an index from external to internal one. Do not perform boundary checks.
	 * @param externalIndex the external index
	 * @return translated index
	 */
	private int doTranslateIndex(int externalIndex) {
		return increment(head, externalIndex);
	}

	/**
	 * Module increment (or decrement) given number using internal buffer size as a module
	 * @param what what should be incremented
	 * @param how increment/decrement factor
	 * @return the result
	 */
	private int increment(int what, int how) {
		int ret = what + how;
		if (ret >= elements.length) {
			return ret - elements.length;
		} else if (ret < 0) {
			return elements.length + ret;
		} else {
			return ret;
		}
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractList#add(int, java.lang.Object)
	 */
	@Override
	public void add(int index, final E element) {
		if (index == size) {
			add(element);
			return;
		} else if (index == 0) {
			addFirst(element);
			return;
		}

		int pos = translateIndex(index, size + 1);

		ensureCapacity(size + 1);

		// Shifts forwards all elements
		for (int i = size; i > index; i--) {
			elements[doTranslateIndex(i)] = elements[doTranslateIndex(i - 1)];
		}
		elements[pos] = element;
		tail = increment(tail, 1);
		size++;
		modCount++;
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractList#indexOf(java.lang.Object)
	 */
	@Override
	public int indexOf(final Object o) {
		if (o == null) {
			for (int i = 0; i < size; i++) {
				if (elements[doTranslateIndex(i)] == null) {
					return i;
				}
			}
		} else {
			for (int i = 0; i < size; i++) {
				if (elements[doTranslateIndex(i)].equals(o)) {
					return i;
				}
			}
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractList#lastIndexOf(java.lang.Object)
	 */
	@Override
	public int lastIndexOf(final Object o) {
		if (o == null) {
			for (int i = size - 1; i >= 0; i--) {
				if (elements[doTranslateIndex(i)] == null) {
					return i;
				}
			}
		} else {
			for (int i = size - 1; i >= 0; i--) {
				if (elements[doTranslateIndex(i)].equals(o)) {
					return i;
				}
			}
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractList#remove(int)
	 */
	@Override
	public E remove(int index) {
		if (index == 0) {
			return removeFirst();
		} else if (index == size - 1) {
			return removeLast();
		} else if (index < 0 || index >= size) {
			throw new IndexOutOfBoundsException("size: " + size() + ", requested index: " + index);
		}
		E obj = elements[doTranslateIndex(index)];
		// Shifts backwards elements
		for (int i = index; i < size - 1; i++) {
			elements[doTranslateIndex(i)] = elements[doTranslateIndex(i + 1)];
		}
		elements[tail] = null;
		tail = increment(tail, -1);
		size--;
		modCount++;
		return obj;
	}

	/**
	 * Removes the first element of the list and returns it.
	 * @return the first element of the list
	 * @throws IndexOutOfBoundsException if size is zero
	 */
	public E removeFirst() {
		if (size == 0) {
			throw new IndexOutOfBoundsException("Size is 0");
		}
		E obj = elements[head];
		elements[head] = null;
		head = increment(head, 1);
		size--;
		modCount++;
		return obj;
	}

	/**
	 * Removes the last element of the list and returns it
	 * @return the last element of the list
	 * @throws IndexOutOfBoundsException if size is zero
	 */
	public E removeLast() {
		if (size == 0) {
			throw new IndexOutOfBoundsException("Size is 0");
		}
		E obj = elements[tail];
		elements[tail] = null;
		tail = increment(tail, -1);
		size--;
		modCount++;
		return obj;
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractList#removeRange(int, int)
	 */
	@Override
	protected void removeRange(int fromIndex, int toIndex) {
		if (fromIndex == toIndex) {
			return;
		} else if (fromIndex == toIndex + 1) {
			remove(fromIndex);
		}
		modCount++;

		int num = toIndex - fromIndex;

		if (fromIndex == 0) {
			// Removes from the head of the list
			for (int i = fromIndex; i < toIndex; i++) {
				elements[doTranslateIndex(i)] = null;
			}

			head = increment(head, num);
		} else {
			// Removes from the tail of the list
			for (int i = fromIndex; i < size - 1; i++) {
				if (num + i < size) {
					elements[doTranslateIndex(i)] = elements[doTranslateIndex(i + num)];
					elements[doTranslateIndex(i + num)] = null;
				} else {
					elements[doTranslateIndex(i)] = null;
				}
			}

			tail = increment(tail, -num);
		}
		size -= num;
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractList#set(int, java.lang.Object)
	 */
	@Override
	public E set(int index, final E element) {
		int pos = translateIndex(index);
		E ret = elements[pos];
		elements[pos] = element;
		modCount++;
		return ret;
	}

	/**
	 * Serializes this list
	 * @param s
	 * @throws java.io.IOException
	 */
	private void writeObject(final java.io.ObjectOutputStream s) throws java.io.IOException {
		int expectedModCount = modCount;
		// Write size
		s.defaultWriteObject();

		// Write inner array size
		s.writeInt(elements.length);
		// Writes all elements
		for (int i = 0; i < size; i++) {
			s.writeObject(elements[doTranslateIndex(i)]);
		}

		if (modCount != expectedModCount) {
			throw new ConcurrentModificationException();
		}

	}

	/**
	 * Reads serialized list
	 * @param s
	 * @throws java.io.IOException
	 * @throws ClassNotFoundException
	 */
	@SuppressWarnings("unchecked")
	private void readObject(final java.io.ObjectInputStream s) throws java.io.IOException, ClassNotFoundException {
		// Read size
		s.defaultReadObject();

		// Allocates array
		int arraySize = s.readInt();
		head = 0;
		tail = -1;
		elements = (E[]) new Object[arraySize];

		for (int i = 0; i < size; i++) {
			elements[++tail] = (E) s.readObject();
		}
	}
}
