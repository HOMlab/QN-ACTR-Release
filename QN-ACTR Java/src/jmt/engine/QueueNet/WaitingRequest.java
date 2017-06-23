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

/** This class implements a waiting request of a job to be processed by a
 section: it contains information about the job and the node/the section which
 sent the job. This class should be used to build a list of jobs sent by other
 nodes/sections which could not be processed immediately: a section can use this
 class to retrieve node/section to be acked when job could be processed.
 */
public class WaitingRequest extends JobInfo {

	private NetNode Node;

	private byte Section;

	/**
	 * Creates a new WaitingRequest object.
	 * @param Node Reference to the node which sent the job.
	 * @param Section Reference to the section which sent the job.
	 * @param Job Reference to the job sent.
	 */
	public WaitingRequest(NetNode Node, byte Section, Job Job) {
		super(Job);
		this.Node = Node;
		this.Section = Section;
	}

	/** Gets referenced node.
	 * @return Node property value.
	 */
	public NetNode getNode() {
		return Node;
	}

	/** Gets referenced section.
	 * @return Section property value.
	 */
	public byte getSection() {
		return Section;
	}
}