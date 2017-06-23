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

/*
 * inputSection.java
 *
 * Created on 21 ottobre 2002, 9.48
 */

package jmt.engine.NodeSections;

import jmt.engine.QueueNet.Job;
import jmt.engine.QueueNet.NodeSection;
import jmt.engine.simEngine.RemoveToken;

/**
 * This abstract class implements a generic input section of a NetNode.
 * @author Francesco Radaelli
 */
public abstract class InputSection extends PipeSection {

	/** Creates a new instance of inputSection.
	 */
	public InputSection() {
		super(NodeSection.INPUT);
	}

	/** Creates a new instance of inputSection
	 *  @param auto  Auto refresh of the jobsList attribute.
	 */
	public InputSection(boolean auto) {
		super(NodeSection.INPUT, auto, true);
	}

	/** Creates a new instance of inputSection
	 *  @param auto  Auto refresh of the jobsList attribute.
	 *  @param nodeAuto auto refresh the jobsList attribute at node level
	 */
	public InputSection(boolean auto, boolean nodeAuto) {
		super(NodeSection.INPUT, auto, nodeAuto);
	}

	/** Sends a job to the service section.
	 * @param Job Job to be sent.
	 * @param Delay Scheduling delay.
	 * @return a token to remove sent event
	 * @throws jmt.common.exception.NetException
	 */
	protected RemoveToken sendForward(Job Job, double Delay) throws jmt.common.exception.NetException {
		return send(Job, Delay, NodeSection.SERVICE);
	}

	/** Sends a message to the service section.
	 * @param Event Message tag.
	 * @param Data Data to be sent.
	 * @param Delay Scheduling delay.
	 * @return a token to remove sent event
	 * @throws jmt.common.exception.NetException
	 */
	protected RemoveToken sendForward(int Event, Object Data, double Delay) throws jmt.common.exception.NetException {
		return send(Event, Data, Delay, NodeSection.SERVICE);
	}
}
