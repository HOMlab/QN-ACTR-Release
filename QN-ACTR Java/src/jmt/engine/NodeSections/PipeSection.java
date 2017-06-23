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

import jmt.engine.QueueNet.NodeSection;

/**
 * This abstract class implements a generic pipe section of a NetNode.
 * @author Francesco Radaelli
 */
public abstract class PipeSection extends NodeSection {

	/** Creates a new instance of this PipeSection.
	 *  @param id    NodeSection identifier.
	 */
	public PipeSection(byte id) {
		super(id);
	}

	/** Creates a new instance of PipeSection
	 *  @param id    NodeSection identifier.
	 *  @param auto  auto refresh of the jobsList attribute.
	 *  @param nodeAuto auto refresh the jobsList attribute at node level
	 */
	public PipeSection(byte id, boolean auto, boolean nodeAuto) {
		super(id, auto, nodeAuto);
	}
}