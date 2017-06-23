package jmt.engine.jwat.fitting.utils;

import java.util.Arrays;
import java.util.Vector;

import jmt.engine.jwat.MatrixOsservazioni;
import jmt.engine.jwat.Observation;
import jmt.engine.jwat.workloadAnalysis.utils.ChangeVariableListener;
import jmt.engine.jwat.workloadAnalysis.utils.SetMatrixListener;
import jmt.gui.jwat.JWatModel;

public class ModelFitting implements JWatModel {

	// Matrice delle osservazioni
	private MatrixOsservazioni matrix = null;
	//array of observations (only numerical for fitting)
	private double[] obslist;
	//statistiscal values: mean, variance, coefficient of variation
	private double[] statValues;
	// vector of the listener on set matrix
	private Vector<SetMatrixListener> listenerOnMatrixChange = null; //<SetMatrixListener> 
	private Vector<ChangeVariableListener> listenerOnChangeVariable = null; //<ChangeVariableListener> 

	public ModelFitting() {
		listenerOnMatrixChange = new Vector<SetMatrixListener>();
		listenerOnChangeVariable = new Vector<ChangeVariableListener>();
	}

	public MatrixOsservazioni getMatrix() {
		// TODO Auto-generated method stub
		return matrix;
	}

	public void resetModel() {
		matrix = null;
		fireNotifyOnResetMatrixObservation();
	}

	public void setMatrix(MatrixOsservazioni matrix) {
		Observation[] obs;
		int i;
		this.matrix = matrix;

		//fill the list of double
		obs = matrix.getListOss();
		obslist = new double[obs.length];

		i = 0;
		for (Observation o : obs) {
			obslist[i] = o.getIndex(0);
			i++;
		}
		//order the observation list, because it is needed reordered by all fitting algorithm
		Arrays.sort(obslist);
		statValues = calculateStatisticalValues(obslist);
		//*****************************
		fireNotifyOnSetMatrixObservation();
	}

	public double[] getListObservations() {
		return obslist;
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

	/**
	 *
	 *@param listener
	 */
	public void addOnSetMatrixObservationListener(SetMatrixListener listener) {
		listenerOnMatrixChange.add(listener);
	}

	public void addOnChangeVariableValue(ChangeVariableListener listener) {
		if (!listenerOnChangeVariable.contains(listener)) {
			listenerOnChangeVariable.add(listener);
		}
	}

	public double getMean() {
		return statValues[0];
	}

	public double getVariance() {
		return statValues[1];
	}

	public double getCoeffVariation() {
		return statValues[2];
	}

	public double getMin() {
		return obslist[0];
	}

	public double getMax() {
		return obslist[obslist.length - 1];
	}

	public double getRange() {
		return obslist[obslist.length - 1] - obslist[0];
	}

	public double[] calculateStatisticalValues(double[] data) {
		double sum = 0;
		//double mean;
		//double variance;
		double[] result;

		result = new double[3];

		for (double element : data) {
			sum += element;
		}

		result[0] = sum / data.length;
		//System.out.println("Media:" + result[0]);
		result[1] = 0;

		for (double element : data) {
			result[1] += Math.pow(element - result[0], 2);
		}

		result[1] /= (data.length - 1);
		//System.out.println("Varianza: " + result[1]);
		result[2] = Math.sqrt(result[1]) / result[0];

		//System.out.println("Coefficient of variation: "+result[2]);

		return result;

	}
}
