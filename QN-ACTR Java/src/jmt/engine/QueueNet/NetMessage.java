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
 * This class implements a message which could be received for example by a
 * NetNode or by a NodeSection.
 * @author Francesco Radaelli
 */
public class NetMessage implements Cloneable {

	private int event;

	private byte sourceSection, destinationSection;

	private NetNode Source, Destination;

	private Object data;

	private double time;

	/** Creates a new instance of NetMessage setting all the field to default
	 * values.
	 */
	public NetMessage() {
		data = null;
		sourceSection = NodeSection.NO_ADDRESS;
		destinationSection = NodeSection.NO_ADDRESS;
		time = 0.0;
		Source = null;
		Destination = null;
	}

	/** Sets the event field of the NetMessage.
	 * @param event Value of property event.
	 */
	public void setEvent(int event) {
		this.event = event;
	}

	/** Gets the event field of the NetMessage.
	 * @return Value of property event.
	 */
	public int getEvent() {
		return event;
	}

	/** Sets the Source Section field of the NetMessage.
	 * @param sourceSection Value of property Source Section.
	 */
	public void setSourceSection(byte sourceSection) {
		this.sourceSection = sourceSection;
	}

	/** Gets the Source Section field of the NetMessage.
	 * @return Value of property Source Section.
	 */
	public byte getSourceSection() {
		return sourceSection;
	}

	/** Sets the Destination Section field of the NetMessage.
	 * @param destinationSection Value of property Destination Section.
	 */
	public void setDestinationSection(byte destinationSection) {
		this.destinationSection = destinationSection;
	}

	/** Gets the Destination Section field of the NetMessage.
	 * @return Value of property Destination Section.
	 */
	public byte getDestinationSection() {
		return destinationSection;
	}

	/** Sets the data field of the NetMessage.
	 * @param data Value of property data.
	 */
	public void setData(Object data) {
		this.data = data;
	}

	/** Gets the data field of the NetMessage.
	 * @return Value of property data.
	 */
	public Object getData() {
		return data;
	}

	/** Returns the job attached to message if the event is a JOB_EVENT or ACK_EVENT,
	 * otherwise returns null.
	 * @return The job of the NetMessage.
	 */
	public Job getJob() {
		if ((event == NetEvent.EVENT_JOB) || (event == NetEvent.EVENT_ACK)) {
			return (Job) data;
		} else {
			return null;
		}
	}

	/** Sets the time field of the NetMessage.
	 * @param time Value of property time.
	 */
	public void setTime(double time) {
		this.time = time;
	}

	/** Gets the time field of the NetMessage.
	 * @return Value of property time.
	 */
	public double getTime() {
		return time;
	}

	/** Sets the source field of the NetMessage.
	 * @param source Value of property source.
	 */
	public void setSource(NetNode source) {
		this.Source = source;
	}

	/** Gets the Source field of the NetMessage.
	 * @return Value of property Source.
	 */
	public NetNode getSource() {
		return Source;
	}

	/** Sets the destination field of the NetMessage.
	 * @param destination Value of property destination .
	 */
	public void setDestination(NetNode destination) {
		this.Destination = destination;
	}

	/** Gets the Destination field of the NetMessage.
	 * @return Value of property Destination .
	 */
	public NetNode getDestination() {
		return Destination;
	}

	public boolean sentBy(byte section, NetNode node) {
		return ((sourceSection == section) && (Source == node));
	}

	@Override
	public Object clone() {
		try {
			return super.clone();
		} catch (CloneNotSupportedException ex) {
			throw new RuntimeException(ex);
		}
	}
}
