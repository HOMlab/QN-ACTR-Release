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

package jmt.engine.NodeSections;

import jmt.common.exception.NetException;
import jmt.engine.QueueNet.Job;
import jmt.engine.QueueNet.NetEvent;
import jmt.engine.QueueNet.NetMessage;
import jmt.engine.QueueNet.NetNode;

/**
 * This class implements a blocking router, i.e. a router which sends a job inside
 * a region with constraints (the "blocking region"). The destination node has been
 * already defined (the node each job has been redirected from), therefore no
 * routing strategy is required.
 * @author  Stefano Omini, Bertoli Marco
 */
public class BlockingRouter extends OutputSection {
	/** The number of ACKs we are waiting */
	private int waitingAcks = 0;

	/** Creates a new instance of Router which limitates the number of
	 * jobs entering in the region with constraints.
	 * No routing strategy is required: the destination node is already known
	 * when the job arrives to the input station.
	 */
	public BlockingRouter() {
		//FCR bug fix: Constructor modified from super() to spuer(true,false) which will enable
		//Jobs in Joblist at NodeSection to be automatically dropped where as for Node to be manually handled.
		super(true, false);
	}

	@Override
	protected int process(NetMessage message) throws NetException {
		switch (message.getEvent()) {
			case NetEvent.EVENT_JOB:
				// Sends the message to the real destination and wait for the ack
				Job job = message.getJob();
				//this is the real destination, i.e. the internal node that at first
				//had redirected the job to the input station
				NetNode realDestinationNode = job.getOriginalDestinationNode();
				send(job, 0.0, realDestinationNode);

				waitingAcks++;
				return MSG_PROCESSED;
			case NetEvent.EVENT_ACK:
				//this ack has been received from one of the output nodes (router was waiting for this ack)
				//sends an ack back to the service section to request another job to be routed
				if (waitingAcks > 0) {
					sendBackward(NetEvent.EVENT_ACK, message.getJob(), 0.0);
					waitingAcks--;
				} else {
					// Nobody was waiting this ACK.
					return MSG_NOT_PROCESSED;
				}
				return MSG_PROCESSED;
			default:
				return MSG_NOT_PROCESSED;
		}
	}
}
