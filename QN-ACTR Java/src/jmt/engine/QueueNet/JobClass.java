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

/**
 * This class implements the description of a job class.
 * @author Francesco Radaelli, Stefano Omini
 */
public class JobClass {

	//job class name
	private String name;
	//job class id
	private int Id;

	/* Closed class type */
	public static final int CLOSED_CLASS = 0;
	/* Open class type */
	public static final int OPEN_CLASS = 1;

	//class type
	private int type;
	//class priority
	private int priority;

	private String referenceNodeName;

	//----------------------CONSTRUCTORS---------------------------///

	/** Creates a new instance of JobClass
	 * @param Name Symbolic name of the job class.
	 * @throws jmt.common.exception.NetException
	 */
	public JobClass(String Name) throws jmt.common.exception.NetException {
		this.name = Name;
		this.priority = 0;
		this.referenceNodeName = null;
	}

	/** Creates a new instance of JobClass
	 * @param Name Symbolic name of the job class.
	 * @param priority Priority of this job class. Must be greater than 0.
	 * @param type Class type. See constants.
	 * @param refNode the reference node of this JobClass
	 * @throws jmt.common.exception.NetException
	 */
	public JobClass(String Name, int priority, int type, String refNode) throws jmt.common.exception.NetException {
		this.name = Name;
		this.type = type;
		this.referenceNodeName = refNode;

		if (priority < 0) {
			this.priority = 0;
		} else {
			this.priority = priority;
		}
	}

	//----------------------SETTER AND GETTER---------------------------///

	/**Sets the Id of this class
	 * @param Id
	 */
	void setId(int Id) {
		this.Id = Id;
	}

	/** Gets the id of this class.
	 * @return Value of property Id.
	 */
	public int getId() {
		return Id;
	}

	/** Gets the name of this class.
	 * @return Value of property Name.
	 */
	public String getName() {
		return name;
	}

	public int getPriority() {
		return priority;
	}

	public void setPriority(int priority) {
		this.priority = priority;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	public String getReferenceNodeName() {
		return referenceNodeName;
	}

	public void setReferenceNodeName(String referenceNodeName) {
		this.referenceNodeName = referenceNodeName;
	}
}
