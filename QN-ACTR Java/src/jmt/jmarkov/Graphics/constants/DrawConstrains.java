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

import java.awt.Font;
import java.awt.Stroke;

public interface DrawConstrains {

	/**
	 * 
	 * 	
	 * Returns the gap between the edges of the drawing from drawing itself
	 */
	double getStartingGap();

	/**
	 * 
	 * Returns the gap between two elements of the tail and between 
	 * the first element of the queue and the processor
	 */
	double getElementsGap();

	/**
	 * Returns the type of items
	 */
	Stroke getDrawStroke();

	Stroke getBoldStroke();

	//font
	/**
	 * returns the font for the area of panel
	 * 
	 */
	Font getFont();

	/**
	 * returns the font for the GUI
	 */
	Font getNormalGUIFont();

	/**
	 * returns the small font for the GUI
	 */
	Font getSmallGUIFont();

	/**
	 * returns the big font for the GUI
	 */
	Font getBigGUIFont();

	//queue 
	/**
	 * Returns the width of an element of the queue
	 */
	double getElementWidth();

	/**
	 * Returns the height of an element of the queue
	 * @return
	 */
	double getElementHeight();

	//processor
	/**
	 * Returns the radius of the processor
	 */
	double getProcessorRadius();

	//status
	/**
	 * * Returns the radius of the status
	 */
	double getStatusRadius();
}
