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
package jmt.engine.jwat.workloadAnalysis.utils;

import java.util.Vector;

import jmt.engine.jwat.MatrixOsservazioni;
import jmt.engine.jwat.filters.FilterOnVariable;
import jmt.engine.jwat.workloadAnalysis.exceptions.TrasformException;
import jmt.gui.jwat.JWatModel;

public class ModelWorkloadAnalysis implements JWatModel {
	// Matrice delle osservazioni
	private MatrixOsservazioni matrix = null;
	// vector of the listener on set matrix
	private Vector<SetMatrixListener> listenerOnMatrixChange = null; //<SetMatrixListener> 
	// vector of the listener on change variable ( transformations )
	private Vector<ChangeVariableListener> listenerOnChangeVariable = null; //<ChangeVariableListener> 

	/**
	 * @param par
	 */
	public ModelWorkloadAnalysis() {
		listenerOnMatrixChange = new Vector<SetMatrixListener>();
		listenerOnChangeVariable = new Vector<ChangeVariableListener>();
	}

	/**
	 * @return
	 */
	public MatrixOsservazioni getMatrix() {
		return matrix;
	}

	/**
	 * @param matrix
	 */
	public void setMatrix(MatrixOsservazioni matrix) {
		this.matrix = matrix;
		fireNotifyOnSetMatrixObservation();
	}

	/**
	 *
	 *@param listener
	 */
	public void addOnSetMatrixObservationListener(SetMatrixListener listener) {
		listenerOnMatrixChange.add(listener);
	}

	/**
	 * 
	 * @param listener
	 */
	public void addOnChangeVariableValue(ChangeVariableListener listener) {
		if (!listenerOnChangeVariable.contains(listener)) {
			listenerOnChangeVariable.add(listener);
		}
	}

	/**
	 * 
	 * @param var
	 */
	public void setTransformation() {
		for (int i = 0; i < listenerOnChangeVariable.size(); i++) {
			listenerOnChangeVariable.get(i).onChangeVariableValues();
		}
	}

	/*
	 * Notify change on matrix observation to all registered listener 
	 */
	private void fireNotifyOnSetMatrixObservation() {
		for (int i = 0; i < listenerOnMatrixChange.size(); i++) {
			listenerOnMatrixChange.get(i).onSetMatrixObservation();
		}
	}

	private void fireNotifyOnResetMatrixObservation() {
		for (int i = 0; i < listenerOnMatrixChange.size(); i++) {
			listenerOnMatrixChange.get(i).onResetMatrixObservation();
		}
	}

	public void doTransformationOnVariable(int varSel, short type) throws TrasformException {
		matrix.applyTransformation(varSel, type);
		setTransformation();
	}

	public boolean undoTransformationOnVariable(int varSel) {
		boolean b = matrix.undoTransformation(varSel);
		setTransformation();
		return b;
	}

	public void doSamplingOnVariable(int varSel, FilterOnVariable filter) {
		matrix.doSampling(varSel, filter);
		setTransformation();
	}

	public void undoSamplingOnVariable(int varSel) {
		matrix.undoSampling(varSel);
		setTransformation();
	}

	//UPDATE 28/10/2006: +spostamento operazioni di trasformazione e sampling in matrixOsservazioni
	public void resetModel() {
		matrix = null;
		fireNotifyOnResetMatrixObservation();
	}
}
