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
package jmt.gui.common.resources;

import java.net.URL;

import jmt.framework.gui.image.ImageLoader;

/**
 * <p><b>Name:</b> ImageLoaderImpl</p> 
 * <p><b>Description:</b> 
 * An Implementation of the ImageLoader class that loads all images from this class package.
 * <br>Please put all images in the class package.
 * </p>
 * <p><b>Date:</b> 23/gen/07
 * <b>Time:</b> 15:56:11</p>
 * @author Bertoli Marco
 * @version 1.0
 */
public class ImageLoaderImpl extends ImageLoader {
	/* (non-Javadoc)
	 * @see jmt.framework.gui.image.ImageLoader#getImageURL(java.lang.String)
	 */
	@Override
	protected URL getImageURL(String resourceName) {
		return ImageLoaderImpl.class.getResource(resourceName);
	}

}
