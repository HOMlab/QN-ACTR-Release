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

import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.image.ImageObserver;

/**
 * Created by IntelliJ IDEA.
 * User: orsotronIII
 * Date: 14-gen-2005
 * Time: 13.29.22
 * This Interface defines methods to implement a simple animation.
 * An {@link jmt.gui.common.animation.Animator} object could then provide support
 * by refreshing it periodically.
 */
public interface Animation {

	/** Called when an animation needs to be refreshed. This method should update image
	 * currently handled by the animation*/
	public void refresh();

	/** Initialization method for implementing class. The implementation of this method
	 * should prepare images for the animation, if this has not been done within cunstructor.*/
	public void init();

	/** Paints image currently handled in a rectangle defined within {@link this.setBounds()} method*/
	public void paint(Graphics g, ImageObserver io);

	/**returns background image for this animation.
	 * @return :Background image for this animation
	 */
	public Image getBGImage();

	/**sets background image for this animation.
	 * @param img: background image for this animation
	 */
	public void setBGImage(Image img);

	/** Sets bounding box for this animation. If this box is set, animation will update only
	 * this certain area of the Graphics object passed within {@link this.paint()} method.
	 * @param r: Bounding box for this animation.
	 */
	public void setBounds(Rectangle r);

	/** Returns bounding box for this animation, e.g. the rectangle within which the frames for this
	 *  animation will be painted
	 * @return : Bounding box for this animation.*/
	public Rectangle getBounds();
}
