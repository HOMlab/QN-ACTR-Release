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
 * Date: 17-feb-2005
 * Time: 10.18.23
 * This interface defines methods for the implementation of a queue net element that can accept
 * and render jobs
 */
public interface JobContainer {
	/**Adds a job to this jobcontainer. Once a job is added, it can be used for many purposes.
	 * For example, if the implementing class is a EdgeAnimation, the job is rendered and
	 * moved along the edge and then, when it gets to the end of the edge, routed to another
	 * edge or a station.
	 * @param jobAnimation: job to be added to this jobcontainer.*/
	public void addJob(JobAnimation jobAnimation);
}
