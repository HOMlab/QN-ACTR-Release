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
 * Log.java
 *
 * Created on 21 ottobre 2002, 22.46
 */

package jmt.engine.dataAnalysis;

import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;

/**
 *  This class creates a file with a getLog of strings. Don't worry about
 *  performance, it uses a buffered writer. Remember to close the Log!
 *
 * @author  federico
 */
public class Log {
	private FileWriter fw;
	private BufferedWriter out;

	/** Creates a new instance of Log
	 *  @param  logName name of getLog file
	 */
	public Log(String logName) {
		try {
			fw = new FileWriter(logName);
			out = new BufferedWriter(fw);
		} catch (FileNotFoundException fe) {
			System.out.println("The creation of getLog file is impossible.");
		} catch (IOException ioe) {
			System.out.println("Error has occurred creating the getLog file");
		}
	}

	/** Creates a new instance of Log
	 *  @param  logName name of getLog file
	 *  @param  bufDim  dimension of buffer of chars
	 */
	public Log(String logName, int bufDim) {
		try {
			fw = new FileWriter(logName);
			out = new BufferedWriter(fw, bufDim);
		} catch (FileNotFoundException fe) {
			System.out.println("The creation of getLog file is impossible.");
		} catch (IOException ioe) {
			System.out.println("Error has occurred creting the getLog file");
		}
	}

	/** writes a a double in a new line: it is very useful to write logs in
	 *  matlab format. you can add comments with write(String str), if the text
	 *  is preceded by %.
	 * @param newSample the new sample to be written
	 */
	public void write(double newSample) {
		try {
			out.write(Double.toString(newSample) + "\n");
		} catch (IOException ioe) {
			System.out.println("Error has occurred writing of getLog file");
		}
	}

	/** writes a string in the getLog file
	 *  @param str the string
	 */
	public void write(String str) {
		try {
			out.write(str + "\n");
		} catch (IOException ioe) {
			System.out.println("Error has occurred writeing of getLog file");
		}
	}

	/** writes a comment in the getLog file with the standard Matlab format %
	 *  @param com the comment
	 */
	public void comment(String com) {
		try {
			out.write("%" + com + "\n");
		} catch (IOException ioe) {
			System.out.println("Error has occurred writeing of getLog file");
		}
	}

	/** it closes the getLog. Important you must close the getLog before closing the
	 *  application, otherwise the file of getLog will be void.
	 */
	public void close() {
		try {
			out.flush();
			out.close();
		} catch (IOException ioe) {
			System.out.println("Error has occurred closing the getLog file");
		}
	}

}
