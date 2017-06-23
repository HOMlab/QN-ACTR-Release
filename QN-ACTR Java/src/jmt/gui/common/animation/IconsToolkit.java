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

import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;

/**
 * Created by IntelliJ IDEA.
 * User: OrsotronIII
 * Date: 17-feb-2005
 * Time: 16.25.59
 * This class provides constants for rendering of queue net elements.
 */
public abstract class IconsToolkit {

	/**Returns Image for rendering of jobs.
	 * @param bounds: customized dimensions of the job image to be returned.
	 * @return job image.*/
	public abstract Image getJobIcon(Rectangle bounds);

	/**Returns Image for rendering of stations.
	 * @param type: type of the station to be rendered, e.g. server, source...
	 * @param bounds: customized dimensions of the station image to be returned.
	 * @return station image.*/
	public abstract Image getStationIcon(String type, Rectangle bounds);

	/**Returns Image for rendering of Edges. an array of points must be specified to define:
	 * starting, ending point, and each point where the edge curves. This can be useful to
	 * represent non-straight edges.
	 * @param bounds: customized dimensions of the edge image to be returned.
	 * @param anglePoints: array of points of this edge.
	 * @return edge image.*/
	public abstract Image getEdgeIcon(Rectangle bounds, Point[] anglePoints);

	/**Returns Image for rendering of queue net background. The return image is a tyle that
	 * will be repeated to create the background for the queuenet animation panel.
	 * @param bounds: customized dimensions of the tile image to be returned.
	 * @return tile image.*/
	public abstract Image getBGTileIcon(Rectangle bounds);
}
