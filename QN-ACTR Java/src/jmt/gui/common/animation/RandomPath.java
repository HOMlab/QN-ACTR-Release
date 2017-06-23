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
 * User: OrsotronIII
 * Date: 1-mar-2005
 * Time: 16.00.00
 * This class implements methods to get a jobanimation to run by -and impredictible-
 * randomized paths in its queue net.
 */
public class RandomPath implements JobPath {

	//queuenet this path refers to.
	private QueueNetAnimation qnAni;
	//queuenet element the job is currently running into.
	private JobContainer currentElement;
	//queuenet element the job should start from.
	private JobContainer startingPoint;

	/**Creates a new instance for randomized path for specified queue net.
	 * @param qNet: queue net this path refers to.
	 * @param startingPoint: queue net element this path must start from.*/
	public RandomPath(QueueNetAnimation qNet, JobContainer startingPoint) {
		qnAni = qNet;
		this.startingPoint = startingPoint;
		currentElement = startingPoint;
	}

	/**Returns a reference to next element in path.
	 * @return JobContainer implementing class the job should be routed to.*/
	public JobContainer getNext() {
		JobContainer[] jc = qnAni.getSuccessors(currentElement);
		if (jc.length > 0) {
			currentElement = jc[(int) (Math.random() * jc.length)];
			return currentElement;
		} else {
			return startingPoint;
		}
	}

	/**Tells wether this path has come to end, e.g. there are no successor nodes. */
	public boolean isLast() {
		return false;
	}
}
