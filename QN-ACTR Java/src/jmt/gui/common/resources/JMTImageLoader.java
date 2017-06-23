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

import java.awt.Dimension;
import java.awt.Image;

import javax.swing.ImageIcon;

import jmt.framework.gui.image.ImageLoader;

/**
 * <p><b>Name:</b> JMTImageLoader</p> 
 * <p><b>Description:</b> 
 * Loads and caches images to create icons. 
 * This class is designed to be accessed in a static manner.
 * </p>
 * <p><b>Date:</b> 23/gen/07
 * <b>Time:</b> 16:15:32</p>
 * @author Bertoli Marco
 * @version 1.2
 */
public class JMTImageLoader {
	protected static ImageLoader imageLoader = new ImageLoaderImpl();

	/**  Loads the image from this directory, please put all images in the class
	 * package.
	 *
	 * @param imageName string containing the image name
	 * @return the icon  of the image
	 *
	 */
	public static ImageIcon loadImage(String imageName) {
		return imageLoader.loadIcon(imageName);
	}

	/**  Loads the image from this directory, please put all images in the class
	 * package.
	 *
	 * @param imageName string containing the image name
	 * @param size specific size for the image to be returned
	 * @return the icon  of the image
	 *
	 */
	public static ImageIcon loadImage(String imageName, Dimension size) {
		return imageLoader.loadIcon(imageName, size);
	}

	/**  Loads the image from this directory, please put all images in the class
	 * package.
	 *
	 * @param imageName string containing the image name
	 * @return the image
	 */
	public static Image loadImageAwt(String imageName) {
		ImageIcon img = imageLoader.loadIcon(imageName);
		if (img != null) {
			return img.getImage();
		} else {
			return null;
		}
	}

	/**
	 * @return incapsulated ImageLoader object
	 */
	public static ImageLoader getImageLoader() {
		return imageLoader;
	}

	/** Loads the image from this directory, and apply a known modifier.
	 * Please put all images in the class
	 * package.
	 *
	 * @param imageName string containing the image name
	 * @param modifier known modifier method
	 * @return the icon image
	 * @see ImageLoader#loadIcon(String, String)
	 */
	public static ImageIcon loadImage(String imageName, String modifier) {
		return imageLoader.loadIcon(imageName, modifier);
	}
}
