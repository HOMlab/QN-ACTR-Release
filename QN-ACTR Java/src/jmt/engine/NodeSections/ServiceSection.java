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

package jmt.engine.NodeSections;

import jmt.engine.QueueNet.Job;
import jmt.engine.QueueNet.NodeSection;
import jmt.engine.simEngine.RemoveToken;

/**
 * This abstract class implements a generic service section of a NetNode.
 * @author Francesco Radaelli
 */
public abstract class ServiceSection extends PipeSection {

	/** Creates a new instance of serviceSection
	 */
	public ServiceSection() {
		super(NodeSection.SERVICE);
	}

	/** Creates a new instance of serviceSection
	 *  @param auto  auto refresh of the jobsList attribute.
	 */
	public ServiceSection(boolean auto) {
		super(NodeSection.SERVICE, auto, true);
	}

	/** Creates a new instance of serviceSection
	 *  @param auto  auto refresh of the jobsList attribute.
	 *  @param nodeAuto auto refresh the jobsList attribute at node level
	 */
	public ServiceSection(boolean auto, boolean nodeAuto) {
		super(NodeSection.SERVICE, auto, nodeAuto);
	}

	/** Sends a job to the output section.
	 * @param job job to be sent.
	 * @param delay Scheduling delay.
	 * @return a token to remove sent event
	 * @throws jmt.common.exception.NetException
	 */
	protected RemoveToken sendForward(Job job, double delay) throws jmt.common.exception.NetException {
		return send(job, delay, NodeSection.OUTPUT);
	}

	/** Sends a message to the output section.
	 * @param event Message tag.
	 * @param delay Scheduling delay.
	 * @return a token to remove sent event
	 * @throws jmt.common.exception.NetException
	 */
	protected RemoveToken sendForward(int event, Object data, double delay) throws jmt.common.exception.NetException {
		return send(event, data, delay, NodeSection.OUTPUT);
	}

	/** Sends a job to the input section.
	 * @param job job to be sent.
	 * @param delay Scheduling delay.
	 * @return a token to remove sent event
	 * @throws jmt.common.exception.NetException
	 */
	protected RemoveToken sendBackward(Job job, double delay) throws jmt.common.exception.NetException {
		return send(job, delay, NodeSection.INPUT);
	}

	/** Sends a message to the input section.
	 * @param event Message tag.
	 * @param delay Scheduling delay.
	 * @return a token to remove sent event
	 * @throws jmt.common.exception.NetException
	 */
	protected RemoveToken sendBackward(int event, Object data, double delay) throws jmt.common.exception.NetException {
		return send(event, data, delay, NodeSection.INPUT);
	}
}
