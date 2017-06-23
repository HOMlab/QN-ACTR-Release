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

package jmt.engine.QueueNet;

import java.util.ArrayList;
import java.util.ListIterator;

/** This class implements a list of job classes. Note that only classes of QueueNet
 * package can add or remove objects to/from the list.
 * @author Francesco Radaelli
 */
public class JobClassList {

	private ArrayList<JobClass> jobClasses;

	/** Creates a new instance of JobClassList object
	 */
	JobClassList() {
		jobClasses = new ArrayList<JobClass>();
	}

	/** Adds a new job class to the list.
	 * @param JobClass Reference to the JobClass to be added.
	 */
	void add(JobClass JobClass) {
		jobClasses.add(JobClass);
	}

	/** Removes a job class from the list.
	 * @param JobClass Reference to the JobClass to be removed.
	 */
	void remove(JobClass JobClass) {
		jobClasses.remove(JobClass);
	}

	/** Gets i-th job class in the list.
	 * @return Index-th job class in the list.
	 */
	public JobClass get(int Index) {
		return jobClasses.get(Index);
	}

	/**Gets, if it exists, the job class with the specified name.
	 *
	 * @param Name The name of the job class
	 * @return the job class. Null if it doesn't exist.
	 */
	public JobClass get(String Name) {
		ListIterator<JobClass> iterator = jobClasses.listIterator();
		JobClass jc;
		while (iterator.hasNext()) {
			jc = iterator.next();
			if (jc.getName().compareTo(Name) == 0) {
				return jc;
			}
		}
		return null;
	}

	/** Gets list size.
	 * @return Number of job classes in the list.
	 */
	public int size() {
		return jobClasses.size();
	}

	public ListIterator<JobClass> listIterator() {
		return jobClasses.listIterator();
	}

	int indexOf(JobClass JobClass) {
		return jobClasses.indexOf(JobClass);
	}
}
