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
package jmt.framework.gui.image;

import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.net.URL;
import java.util.HashMap;

import javax.swing.ImageIcon;

/**
 * <p><b>Name:</b> ImageLoader</p> 
 * <p><b>Description:</b> 
 * A class used to load images. Each subclass must implement the <code>getImageURL</code>
 * method to retrive URL of resource to be loaded. This class holds a cache of loaded images.
 * Cache size can be adjusted using the <code>maxCache</code> field.
 * </p>
 * <p><b>Date:</b> 23/gen/07
 * <b>Time:</b> 15:09:14</p>
 * @author Bertoli Marco
 * @version 1.0
 */
public abstract class ImageLoader {
	/** Modifier to be used for mirror images (rotated on y axis) */
	public static final String MODIFIER_MIRROR = "MIRROR";
	/** Modifier to be used for pressed button images */
	public static final String MODIFIER_PRESSED = "P";
	/** Modifier to be used for rollover button images */
	public static final String MODIFIER_ROLLOVER = "RO";
	/** Modifier to be used for selected (toggled) button images */
	public static final String MODIFIER_SELECTED = "S";
	/** Template used to derive modified toolbar images */
	private static final String TEMPLATE_IMAGE_NAME = "templateToolbarImage";

	/** Allowed images extensions */
	private static final String[] EXTENSIONS = new String[] { ".gif", ".png", ".jpg" };
	/** Maximum number of cached elements */
	protected int maxCache = 256;
	/** Internal caching data structure */
	protected HashMap<String, ImageIcon> iconCache = new HashMap<String, ImageIcon>();

	/**
	 * Returns the url of a given resource
	 * @param resourceName name of the resource to be retrived
	 * @return url of the resource or null if it was not found
	 */
	protected abstract URL getImageURL(String resourceName);

	/**
	 * Loads an icon with specified name and caches it.
	 * @param iconName name of the icon to be loaded. Extensions are automatically added, if needed.
	 * @return icon if found, null otherwise
	 */
	public ImageIcon loadIcon(String iconName) {
		return this.loadIcon(iconName, (String) null);
	}

	/**
	 * Loads an icon with specified name and specified modifier and caches it.
	 * @param iconName name of the icon to be loaded
	 * @param modifier modifier to be applied (can be null)
	 * @return icon if found, null otherwise
	 */
	public ImageIcon loadIcon(String iconName, String modifier) {
		if (iconName == null) {
			return null;
		}
		String derivedName = iconName;
		// Apply modifier to icon name if needed
		if (modifier != null) {
			derivedName = deriveIconName(iconName, modifier);
		}

		if (iconCache.containsKey(derivedName)) {
			return iconCache.get(derivedName);
		}

		URL url = getImageURL(derivedName);
		// If image is not found, try to add extensions
		for (int i = 0; i < EXTENSIONS.length && url == null; i++) {
			url = getImageURL(derivedName + EXTENSIONS[i]);
		}

		ImageIcon tmp = null;
		if (url != null) {
			tmp = new ImageIcon(url);
		} else if (modifier != null) {
			// Loads base icon without modifiers
			ImageIcon base = loadIcon(iconName);
			if (base != null) {
				// Apply known modifiers
				tmp = applyModifier(base, modifier);
			}
		}

		// Clears cache when it's bigger than maxCache. This is not needed but avoids memory leakages...
		if (iconCache.size() > maxCache) {
			iconCache.clear();
		}

		iconCache.put(derivedName, tmp);
		return tmp;
	}

	/**
	 * Loads an icon with specified name and caches it, then resizes it.
	 * @param iconName name of the icon to be loaded. Extensions are automatically added, if needed.
	 * @param size target dimension of image. a negative number means to mantain aspect ratio on that dimension
	 * @return icon if found, null otherwise
	 */
	public ImageIcon loadIcon(String iconName, Dimension size) {
		ImageIcon im = loadIcon(iconName);
		if (im != null) {
			Image scaled = im.getImage();
			scaled = scaled.getScaledInstance(size.width, size.height, Image.SCALE_SMOOTH);
			return new ImageIcon(scaled);
		} else {
			return im;
		}
	}

	/**
	 * Derives an icon name to apply suffixes before extension
	 * @param iconName name of the icon to be derived
	 * @param modifier suffix to be appended
	 * @return derived name
	 */
	public static String deriveIconName(String iconName, String modifier) {
		int dot = iconName.lastIndexOf('.');
		if (dot < 0) {
			return iconName + modifier;
		} else {
			return iconName.substring(0, dot) + modifier + iconName.substring(dot);
		}
	}

	// --- Methods to apply modifiers to loaded icons ---------------------------------------------------
	/**
	 * Apply known modifiers to given image
	 * @param base base image to be modified
	 * @param modifier modifier to be applied
	 * @return modified image
	 */
	protected ImageIcon applyModifier(ImageIcon base, String modifier) {
		if (modifier.equals(MODIFIER_MIRROR)) {
			return mirrorImage(base);
		} else if (modifier.equals(MODIFIER_ROLLOVER) || modifier.equals(MODIFIER_SELECTED)) {
			ImageIcon background = loadIcon(deriveIconName(TEMPLATE_IMAGE_NAME, modifier));
			if (background != null) {
				return mergeIcons(background, base, new Point(1, 1), new Point(background.getIconWidth() - 1, background.getIconHeight() - 1),
						new Point(1, 1));
			}
		} else if (modifier.equals(MODIFIER_PRESSED)) {
			ImageIcon background = loadIcon(deriveIconName(TEMPLATE_IMAGE_NAME, modifier));
			if (background != null) {
				return mergeIcons(background, base, new Point(1, 1), new Point(background.getIconWidth() - 2, background.getIconHeight() - 2),
						new Point(2, 2));
			}
		}
		// Modifier unsupported or template images not found.
		return base;
	}

	/**
	 * Merge two icons together. Output icon dimension is the same as background one.
	 * @param background background icon
	 * @param overlay overlay icon, needs to be transparent (will be moved and cropped)
	 * @param cropMin minimum crop point for overlay icon (0,0 for no crop)
	 * @param cropMax maximum crop point for overlay icon (width and height for no crop)
	 * @param move upper left corner where cropped overlay image should be positioned on background image
	 * @return merged icon
	 */
	protected ImageIcon mergeIcons(ImageIcon background, ImageIcon overlay, Point cropMin, Point cropMax, Point move) {
		BufferedImage out = new BufferedImage(background.getIconWidth(), background.getIconHeight(), BufferedImage.TYPE_INT_ARGB);
		Graphics2D g = out.createGraphics();
		g.drawImage(background.getImage(), 0, 0, null);
		g.drawImage(overlay.getImage(), move.x, move.y, move.x + cropMax.x - cropMin.x, move.y + cropMax.y - cropMin.y, cropMin.x, cropMin.y,
				cropMax.x, cropMax.y, null);
		return new ImageIcon(out);
	}

	/**
	 * Rotates image on y axis
	 * @param base image to be modified
	 * @return mirrored image
	 */
	protected ImageIcon mirrorImage(ImageIcon base) {
		BufferedImage out = new BufferedImage(base.getIconWidth(), base.getIconHeight(), BufferedImage.TYPE_INT_ARGB);
		Graphics2D g = out.createGraphics();
		g.drawImage(base.getImage(), base.getIconWidth(), 0, 0, base.getIconHeight(), 0, 0, base.getIconWidth(), base.getIconHeight(), null);
		return new ImageIcon(out);
	}
	// --------------------------------------------------------------------------------------------------
}
