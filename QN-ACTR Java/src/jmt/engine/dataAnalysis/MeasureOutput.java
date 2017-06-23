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

package jmt.engine.dataAnalysis;

/**
 * A measure output object is used to print a measure value(s). It's an abstract class,
 * whose methods should be overridden to implement the way in which each kind of measure
 * should be written on the output stream.
 * @author Francesco Radaelli.
 */
public abstract class MeasureOutput {

	protected Measure measure;

	/** Creates a new instance of MeasureOutput class.
	 * @param measure Reference to the measure to be "outputted".
	 */
	public MeasureOutput(Measure measure) {
		this.measure = measure;
		measure.setOutput(this);
	}

	/** Overrides this method to implement the way in which each measure
	 * should be written on an output stream. This method is called for every
	 * sample.
	 */
	public abstract void write(double Sample, double Weight);

	/** Overrides this method to implement the way in which the final measure
	 * should be written on an output stream. This method is called at the end of
	 * a measure.
	 */
	public abstract void writeMeasure();
}
