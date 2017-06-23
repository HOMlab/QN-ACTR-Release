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

package jmt.engine.dataAnalysis.measureOutputs;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;

import jmt.engine.dataAnalysis.MeasureOutput;

/**
 * This class implements a csv output for measure class. <br>
 * File output: <br>
 * <table border="1"> <tr>
 * <td>Mean Value</td>
 * <td>Mean Value Lower Limit</td>
 * <td>Mean Value Upper Limit</td>
 * <td>Discarded Samples</td>
 * <td>Analyzed Samples</td>
 * <td>Max Samples</td>
 * </tr></table>
 @author Francesco Radaelli
 */
public class CSVMeasureOutput extends MeasureOutput {

	private Writer File;
	//private OutputStreamWriter File;

	private boolean StdOutput;

	/**
	 * Constructor of a CSVMeasureOutput object.
	 * @param Measure the measure to be sent in output
	 * @param Append true to write at the end of the file (of course
	 * it's useful only if a file is used)
	 * @param FileName the name of the file; if null the output will be sent
	 * on the standard output
	 * @throws java.io.IOException
	 */
	public CSVMeasureOutput(jmt.engine.dataAnalysis.Measure Measure, boolean Append, String FileName) throws IOException {
		super(Measure);
		if (FileName == null) {
			File = new OutputStreamWriter(System.out);
			StdOutput = true;
		} else {

			File = new BufferedWriter(new FileWriter(FileName + ".csv", Append));
			StdOutput = false;
		}
	}

	/**
	 * Constructor of a CSVMeasureOutput object, using a file.
	 * @param Measure the measure to be sent in output
	 * @param Append true to write at the end of the file (of course
	 * it's useful only if a file is used)
	 * @throws java.io.IOException
	 */
	public CSVMeasureOutput(jmt.engine.dataAnalysis.Measure Measure, boolean Append) throws IOException {
		super(Measure);
		File = new BufferedWriter(new FileWriter(Measure.getName() + ".csv", Append));
		StdOutput = false;
	}

	@Override
	public void write(double Sample, double Weight) {
		try {
			File.write(Sample + "; " + Weight + ";\n");
		} catch (IOException exc) {
		}
	}

	@Override
	public void writeMeasure() {
		try {

			if (measure.getSuccess() == true) {
				File.write("\nMeasure name; " + measure.getName() + ";\n" + "Measure value; " + measure.getMeanValue() + ";\n" + "Lower limit; "
						+ measure.getLowerLimit() + ";\n" + "Upper Limit; " + measure.getUpperLimit() + ";\n" + "Discarded samples; "
						+ measure.getDiscardedSamples() + ";\n" + "Analyzed samples; " + measure.getAnalyzedSamples() + ";\n" + "Max samples; "
						+ measure.getMaxSamples() + ";\n" + "Measure was successful; " + measure.getSuccess() + ";");
			} else {
				File.write("\nMeasure name; " + measure.getName() + ";\n" + "Extimated mean; " + measure.getExtimatedMeanValue() + ";\n"
						+ "Discarded samples; " + measure.getDiscardedSamples() + ";\n" + "Analyzed samples; " + measure.getAnalyzedSamples() + ";\n"
						+ "Max samples; " + measure.getMaxSamples() + ";\n" + "Measure was successful; " + measure.getSuccess() + ";");
			}

			if (!StdOutput) {
				File.close();
			}
		} catch (IOException exc) {
		}
	}
}
