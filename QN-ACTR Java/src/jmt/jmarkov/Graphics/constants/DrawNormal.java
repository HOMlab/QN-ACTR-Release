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

/*
 * Created on 23-mar-2004 by Ernesto
 *
 */
package jmt.jmarkov.Graphics.constants;

import java.awt.BasicStroke;
import java.awt.Font;
import java.awt.Stroke;

/**
 * MMQueues
 * --------------------------------------
 * 23-mar-2004 - Graphics.constants/DrawSmall.java
 * 
 * @author Ernesto
 */
public class DrawNormal implements DrawConstrains {
	private static final double START_GAP = 5.0, ELEM_HEIGHT = 30.0, ELEM_WIDTH = 20.0, ELEMS_GAP = 5.0, PROC_RAD = 25.0, STAT_RAD = 15.0;
	private static final float STROKE_SIZE = 2.0f;

	private static final Font F = new Font("Monospaced", Font.BOLD, 12), FN = new Font("Verdana", Font.BOLD, 12), FS = new Font("Verdana", Font.BOLD,
			10), FB = new Font("Verdana", Font.BOLD, 14);

	private static final Stroke S = new BasicStroke(2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
	private static final Stroke BS = new BasicStroke(STROKE_SIZE + STROKE_SIZE / 2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);

	/* (non-Javadoc)
	 * @see Graphics.constants.DrawConstrains#getElementHeight()
	 */
	public double getElementHeight() {
		return ELEM_HEIGHT;
	}

	/* (non-Javadoc)
	 * @see Graphics.constants.DrawConstrains#getElementsGap()
	 */
	public double getElementsGap() {
		return ELEMS_GAP;
	}

	/* (non-Javadoc)
	 * @see Graphics.constants.DrawConstrains#getElementWidth()
	 */
	public double getElementWidth() {
		return ELEM_WIDTH;
	}

	/* (non-Javadoc)
	 * @see Graphics.constants.DrawConstrains#getFont()
	 */
	public Font getFont() {
		return F;
	}

	/* (non-Javadoc)
	 * @see Graphics.constants.DrawConstrains#getProcessorRadius()
	 */
	public double getProcessorRadius() {
		return PROC_RAD;
	}

	/* (non-Javadoc)
	 * @see Graphics.constants.DrawConstrains#getStartingGap()
	 */
	public double getStartingGap() {
		return START_GAP;
	}

	/* (non-Javadoc)
	 * @see Graphics.constants.DrawConstrains#getStatusRadius()
	 */
	public double getStatusRadius() {
		return STAT_RAD;
	}

	/* (non-Javadoc)
	 * @see Graphics.constants.DrawConstrains#getDrawStroke()
	 */
	public Stroke getDrawStroke() {
		return S;
	}

	/* (non-Javadoc)
	 * @see Graphics.constants.DrawConstrains#getBoldStroke()
	 */
	public Stroke getBoldStroke() {
		return BS;
	}

	/* (non-Javadoc)
	 * @see Graphics.constants.DrawConstrains#getNormalGUIFont()
	 */
	public Font getNormalGUIFont() {
		return FN;
	}

	/* (non-Javadoc)
	 * @see Graphics.constants.DrawConstrains#getSmallGUIFont()
	 */
	public Font getSmallGUIFont() {
		return FS;
	}

	/* (non-Javadoc)
	 * @see Graphics.constants.DrawConstrains#getBigGUIFont()
	 */
	public Font getBigGUIFont() {
		return FB;
	}
}
