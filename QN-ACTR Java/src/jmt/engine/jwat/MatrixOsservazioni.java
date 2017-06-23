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
package jmt.engine.jwat;

import java.util.ArrayList;
import java.util.Arrays;

import jmt.engine.jwat.filters.FilterOnVariable;
import jmt.engine.jwat.input.VariableMapping;
import jmt.engine.jwat.workloadAnalysis.exceptions.TrasformException;
import jmt.gui.jwat.JWATConstants;

/**
 * This class stores all information about observations, variables and bivariate statistics
 * @author Brambilla Davide, Fumagalli Claudio
 * @version 1.1
 */
public class MatrixOsservazioni {
	/* Array of observations */
	private Observation[] VectOss;
	/* Array of variables */
	private VariableNumber[] VectVar;

	/**
	 * Constructor, instantiates a Matrix of observations passing an array of observations, an array of
	 * variable's names, an array of variable's types and an array of mapping class for each variable.
	 * @param Matrix Array of observations.
	 * @param Vname Varaible's names
	 * @param TipoVar Variable's types
	 * @param map array of mapping class for each variable
	 */
	public MatrixOsservazioni(Observation[] Matrix, String[] Vname, int[] TipoVar, VariableMapping[] map) throws OutOfMemoryError {
		/* Number of variables and observationss*/
		int VarDim = Vname.length;
		int OssDim = Matrix.length;
		VectOss = Matrix;
		/* Temporary array used for sorting by single variable */
		Observation[] VectOssTemp = new Observation[OssDim];
		for (int i = 0; i < OssDim; i++) {
			VectOssTemp[i] = VectOss[i];
		}
		/* Array of variables */
		VectVar = new VariableNumber[VarDim];
		/* Creation of each variable */
		for (int i = 0; i < VarDim; i++) {
			/* Sets up current varaible sorting information */
			VectOssTemp[0].setSorter(i);
			Arrays.sort(VectOssTemp);
			switch (TipoVar[i]) {
				case 0:
					VectVar[i] = new VariableNumber(VectOssTemp, Vname[i], i, JWATConstants.NUMERIC, null);
					break;
				case 1:
					VectVar[i] = new VariableString(VectOssTemp, Vname[i], i, JWATConstants.STRING, map[i]);
					break;
				case 2:
					VectVar[i] = new VariableDate(VectOssTemp, Vname[i], i, JWATConstants.DATE, map[i]);
					break;
				default:
			}

		}
		calcBivStats();
	}

	/**
	 * Returns the array of variables.
	 * @return Array of variables.
	 */
	public VariableNumber[] getVariables() {
		return VectVar;
	}

	/**
	 * Returns the number of variables.
	 * @return number of variables.
	 */
	public int getNumVariables() {
		return VectVar.length;
	}

	/**
	 * Returns the object of bivariate statistics.
	 * @return object of bivariate statistics.
	 */
	public StatBivariate getBivStatObj() {
		return listOfStatsBiv.get(listOfStatsBiv.size() - 1);
	}

	/**
	 * Returns the array of original observations.
	 * @return array of original observations.
	 */
	public Observation[] getListOss() {
		return VectOss;
	}

	/**
	 * Returns the array of variable's names.
	 * @return variable's names.
	 */
	public String[] getVariableNames() {
		String[] n = new String[VectVar.length];
		for (int i = 0; i < n.length; i++) {
			n[i] = VectVar[i].getName();
		}
		return n;
	}

	/**
	 * Returns the number of observations currently used.
	 * @return number of observations currently used.
	 */
	public int getNumOfObs() {
		if (VectVar != null && VectVar.length > 0) {
			return VectVar[0].Size();
		} else {
			return -1;
		}
	}

	/**
	 * Returns the number of observations.
	 * @return number of observations.
	 */
	public int getNumOfOriginalObs() {
		if (VectVar != null && VectVar.length > 0) {
			return VectOss.length;
		} else {
			return -1;
		}
	}

	private void calcBivStats() {
		listOfStatsBiv.add(new StatBivariate(VectVar));
	}

	private void undoBivStats(int size) {
		for (int i = 0; i < size; i++) {
			listOfStatsBiv.remove(listOfStatsBiv.size() - 1);
		}
	}

	//UPDATE 28/10/2006 + Spostamento operazioni di trasformazione da modelWorkloadAnalysis a MatrixOsservazioni
	//					+ Inserimento array delle statistiche bivariate per non effettuare il ricalcolo in caso di undo

	private ArrayList<StatBivariate> listOfStatsBiv = new ArrayList<StatBivariate>(); //<StatBivariate>

	public void applyTransformation(int varSel, short type) throws TrasformException {
		VectVar[varSel].doTransformation(type);
		calcBivStats();
	}

	public boolean undoTransformation(int varSel) {
		if (VectVar[varSel].undoLastTrasf()) {
			//E' stato necessario annullare il sampling
			for (int i = 0; i < VectVar.length; i++) {
				if (i != varSel) {
					VectVar[i].resetSampling();
				}
			}
			//Reset validity e ID osservazioni
			for (int i = 0; i < VectOss.length; i++) {
				VectOss[i].setValid(true);
				VectOss[i].setID(i + 1);
			}
			return true;
		}
		undoBivStats(listOfStatsBiv.size() - VectVar[varSel].getNumOfTransf() - 1);
		return false;
	}

	public void doSampling(int varSel, FilterOnVariable filter) {
		int size = VectVar[varSel].applySampling(filter);
		for (int i = 0; i < VectVar.length; i++) {
			if (i != varSel) {
				VectVar[i].updateOnSampling(size);
			}
		}
		calcBivStats();
	}

	public void undoSampling(int varSel) {
		VectVar[varSel].undoSampling();
		for (int i = 0; i < VectVar.length; i++) {
			if (i != varSel) {
				VectVar[i].resetSampling();
			}
		}
		undoBivStats(listOfStatsBiv.size() - VectVar[varSel].getNumOfTransf() - 1);
		//Reset validity e ID osservazioni
		for (int i = 0; i < VectOss.length; i++) {
			VectOss[i].setValid(true);
			VectOss[i].setID(i + 1);
		}

	}
}