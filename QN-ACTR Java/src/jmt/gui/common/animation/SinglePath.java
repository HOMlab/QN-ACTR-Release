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

package jmt.gui.common.animation;

/**
 * Created by IntelliJ IDEA.
 * User: orsotronIII
 * Represents a path the job must follow during the animation. This is a utility class
 * for job animation to obtain step by step the queue net elements the job must enter.
 * To define circular routes, ending element of the list provided within constructor call
 * must be adjacent (in queue net topology) to the starting element, otherwise job will
 * leap from an element to another during the animation.
 */
public class SinglePath implements JobPath {

	//Stations and edges this path goes through
	private JobContainer[] pathElems;

	//index of currently active container
	private int currentIndex = 0;

	/**creates a new instance of JobPath and initializes it with a list of elements of the
	 * queue net this path must go through, e.g. edges and stations.
	 * @param path: array of elements of the queue net (edges and stations) the job must
	 * pass by*/
	public SinglePath(JobContainer[] path) {
		pathElems = path;
	}

	/**returns a reference to the next element the job must be added to.
	 * @return the queue net element this job must be added to*/
	public JobContainer getNext() {
		currentIndex++;
		if (currentIndex >= pathElems.length) {
			currentIndex = currentIndex % pathElems.length;
		}
		return pathElems[currentIndex];
	}

	/**Tests wether current element is the last of the list, e.g. job has come to the end of
	 * his path. This can be useful if the job running on this path must run only once.*/
	public boolean isLast() {
		return currentIndex == pathElems.length - 1;
	}
}
