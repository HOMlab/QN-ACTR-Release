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

package jmt.engine.random;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import jmt.common.exception.IncorrectDistributionParameterException;
import jmt.engine.dataAnalysis.Log;

/**

 * Manages the file containing numbers (previously generated).
 *
 * @author Federico Granata
 * Date: 28-lug-2003
 * Time: 9.40.51

 */
public class ReplayerPar extends AbstractParameter implements Parameter {
	private String fileName;
	private FileReader fr;
	private BufferedReader in;

	/**
	 * Creates a ReplayerPar
	 * @param fileName the file containing the previously generated numbers.
	 */
	public ReplayerPar(String fileName) {
		try {
			fr = new FileReader(fileName);
			in = new BufferedReader(fr);
			this.fileName = fileName;
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Returns the next number from the file.
	 *
	 */
	public double getNext() {
		String str = null;
		try {
			str = in.readLine();
		} catch (IOException e) {
			e.printStackTrace();
		}
		//if the EOF is not reached it continues to read from the file
		//else it restarts from the beginning of the file, it's cyclical
		if (str == null) {
			try {
				fr.close();
				fr = new FileReader(fileName);
				in = new BufferedReader(fr);
				str = in.readLine();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return Double.parseDouble(str);
	}

	/**
	 * Used only for test.
	 */
	public static boolean test(String fileName) {
		Log writer = new Log(fileName);
		Exponential e = new Exponential();
		ExponentialPar p = null;
		try {
			p = new ExponentialPar(1.0);
		} catch (IncorrectDistributionParameterException e1) {
			e1.printStackTrace();
		}
		for (int i = 0; i < 5; i++) {
			try {
				writer.write(e.nextRand(p));
			} catch (IncorrectDistributionParameterException e1) {
				e1.printStackTrace();
			}
		}
		writer.close();
		ReplayerPar reader = new ReplayerPar(fileName);
		for (int i = 0; i < 15; i++) {
			System.out.println(reader.getNext());
		}
		return false;
	}
}
