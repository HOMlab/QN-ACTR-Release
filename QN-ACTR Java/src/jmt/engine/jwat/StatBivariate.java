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

/**
 * Classe che rappresenta le statistiche bivariate
 * @author Brambilla Davide, Fumagalli Claudio
 * @version 1.0
 */
public class StatBivariate {
	/**
	 * Costruttore della classe
	 * @param allVar array delle variabili
	 */
	public StatBivariate(VariableNumber[] allVar) {
		var = allVar;
		covarianza = new double[allVar.length][];
		covDone = new boolean[allVar.length][allVar.length + 1];
	}

	/**
	 * Restituisce il valore della covarianza(v1,v2)
	 * @param ind1 indice della prima variabile
	 * @param ind2 indice della seconda variabile
	 */
	public double getCovariance(int ind1, int ind2) {
		//Controllo se esiste il vettore delle covarianza per ind1
		if (!covDone[ind1][0]) {
			covarianza[ind1] = new double[var.length];
			covDone[ind1][0] = true;
		}
		//Controllo se la covarianza cercata e' gia calcolata
		if (!covDone[ind1][ind2 + 1]) {
			calcCovariance(ind1, ind2);
		}
		return covarianza[ind1][ind2];
	}

	//Calcola il valore della convarianza delle due variabili richieste
	private void calcCovariance(int ind1, int ind2) {
		double cov = 0;
		for (int i = 0; i < var[0].Size(); i++) {
			cov += (var[ind1].getValue(i) - var[ind1].getUniStats().getMean()) * (var[ind2].getValue(i) - var[ind2].getUniStats().getMean());
		}
		cov /= var[0].Size();
		covarianza[ind1][ind2] = cov / (var[ind1].getUniStats().getVariance() * var[ind2].getUniStats().getVariance());
		covDone[ind1][ind2 + 1] = true;
	}

	public void resetVar(int indVar) {
		int i;

		for (i = 0; i < var.length; i++) {
			covDone[indVar][i + 1] = false;
			covDone[i][indVar + 1] = false;
		}
	}

	/* Array delle variabili */
	private VariableNumber[] var;
	/*Covarainza*/
	private double[][] covarianza;
	//Il primo elemento di ogni riga contiene se esiste il vettore delle cov per la variabile
	private boolean[][] covDone;

}