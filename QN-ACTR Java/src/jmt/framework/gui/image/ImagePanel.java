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
import java.awt.Image;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.util.HashMap;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.SwingConstants;

/**
 * <p>Title: ImagePanel</p>
 * <p>Description: A graphical component used to swow a given IconImage. This panel is designed to
 * auto-resize the image basing on width assigned by layout managers, mantaining aspect ratio.
 * A maximum height can be specified too. Internal caching is used as long as width is not changed
 * to avoid to recalculate images scaling continously.</p>
 * 
 * @author Bertoli Marco
 *         Date: 1-lug-2005
 *         Time: 15.55.21
 */
public class ImagePanel extends JLabel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected ImageIcon image;
	protected int maxheight = Integer.MAX_VALUE;
	protected int currentWidth;
	/** Maximum number of cached elements */
	protected int maxCache = 256;
	protected HashMap<ImageIcon, ImageIcon> cache = new HashMap<ImageIcon, ImageIcon>();

	/**
	 * Construct a new ImagePanel
	 */
	public ImagePanel() {
		super();
		this.setHorizontalAlignment(SwingConstants.CENTER);
		this.addComponentListener(new ComponentAdapter() {
			@Override
			public void componentResized(ComponentEvent e) {
				if (image != null) {
					ImagePanel.this.resizeImage();
				}
			}
		});
	}

	/**
	 * Sets image to be shown on this panel
	 * @param image image to be shown in IconImage format
	 */
	public void setImage(ImageIcon image) {
		this.image = image;
		resizeImage();
	}

	/**
	 * Sets maximum height allowed for this component
	 * @param height maximum height allowed
	 */
	public void setMaximumHeight(int height) {
		this.maxheight = height;
		cache.clear();
		if (image != null) {
			resizeImage();
		}
	}

	/**
	 * Helper method used to resize input image and display it. If image was already resized to current size, 
	 * take the copy from local cache.
	 */
	protected void resizeImage() {
		// Resets cache if width has changed
		if (currentWidth != this.getWidth()) {
			currentWidth = this.getWidth();
			cache.clear();
		}

		// If this component size is not already defined, sets image without resizing
		Dimension d = this.getSize();
		if (d.width <= 0 || d.height <= 0) {
			this.setIcon(image);
			return;
		}

		// If cache contains image of the correct size, returns it.
		if (cache.containsKey(image)) {
			this.setIcon(cache.get(image));
			return;
		}

		// Calculates ratio factor
		Image tmp = image.getImage();
		float scale = (float) d.width / (float) image.getIconWidth();

		if (scale * image.getIconHeight() > maxheight) {
			scale = (float) maxheight / (float) image.getIconHeight();
		}

		// Resizes image
		tmp = tmp.getScaledInstance((int) (scale * image.getIconWidth()), (int) (scale * image.getIconHeight()), Image.SCALE_SMOOTH);
		ImageIcon resized = new ImageIcon(tmp);
		if (cache.size() > maxCache) {
			cache.clear();
		}
		cache.put(image, resized);
		this.setIcon(resized);
	}
}
