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

package jmt.engine.jwat.workloadAnalysis.clustering.fuzzyKMean;

import jmt.engine.jwat.MatrixOsservazioni;
import jmt.engine.jwat.Observation;
import jmt.engine.jwat.TimeConsumingWorker;
import jmt.engine.jwat.VariableNumber;

public class FuzzyKMeanClusteringEngine {

	class SFKMClust {
		String centri; // contiene i centri dei cluster
		String log; // informazioni generali
		/** DA SISTEMARE **/
		String[] clus_log; /*= new String[37];	// log dei valori del Cluster = new String[MAXCLU]*/
	}

	public String m_strFKMLog;
	public SFKMClust m_arrClust[];
	public double clus_entropy[]; // entropie finali dei Cluster
	public int m_nMaxClust;
	private VariableNumber[] listOfVars; //Solo quelle selezionate

	private Observation[] obsSel;
	private int[] varS;

	private double[][] U = null;//0
	private double[][] newU = null;//1
	private double[][] centers = null;//2
	private double[][] mrow = null;//3
	private double[][] singularity = null;//4
	private double[][] distance = null;//5

	private int m_nNumNorm;
	private int m_nFLev;
	private int m_nIter;

	private FuzzyKMean fuzzy = null;
	private TimeConsumingWorker worker;

	public FuzzyKMeanClusteringEngine(FuzzyKMean f, TimeConsumingWorker worker) {
		this.worker = worker;
		fuzzy = f;
	}

	public void PrepFClustering(MatrixOsservazioni m, int indVarSel[], int nMaxClust, int nFLev, int nIter) {
		m_nNumNorm = indVarSel.length;
		m_nMaxClust = nMaxClust; // Num.massimo di clusters da considerare
		m_nFLev = nFLev; // Livello di fuzzyness
		m_nIter = nIter; // Valore di tollerabilità dell'errore
		clus_entropy = new double[m_nMaxClust - 1];

		obsSel = m.getVariables()[indVarSel[0]].getCurObs();
		varS = indVarSel;

		listOfVars = new VariableNumber[indVarSel.length];
		for (int i = 0; i < indVarSel.length; i++) {
			listOfVars[i] = m.getVariables()[indVarSel[i]];
		}
		m_arrClust = new SFKMClust[m_nMaxClust];
	}

	public void DoFClustering() {
		double entropy[] = new double[m_nMaxClust - 1];
		int currClust = 0;
		int nObs = listOfVars[0].getCurObs().length;

		//inizializzazione vettore entropia
		for (int temp = 0; temp < m_nMaxClust - 1; temp++) {
			entropy[temp] = 0.0;
		}

		for (int r = 0; r < m_nMaxClust; r++) {
			m_arrClust[r] = new SFKMClust();
			m_arrClust[r].clus_log = new String[r + 1];
			for (int j = 0; j < r; j++) {
				m_arrClust[r].clus_log[j] = "";
			}
			m_arrClust[r].log = "";
		}

		for (int cltemp = 2; cltemp < m_nMaxClust + 1; cltemp++) {

			initMatrix(0, cltemp, nObs);
			initMatrix(1, cltemp, nObs);

			//RIPETI PER IL NUMERO DI ITERAZIONI
			for (int kh = 0; kh < m_nIter; kh++) {
				if (worker.isCanceled()) {
					return;
				}
				worker.updateInfos(2 + m_nIter + ((cltemp - 2) * m_nIter), "Clustering " + (cltemp) + " Iteration " + (kh + 1), false);

				initMatrix(3, cltemp, 2);
				initMatrix(2, cltemp, m_nNumNorm);
				initMatrix(4, nObs, 1);
				initMatrix(5, cltemp, nObs);

				/*** OK ***/
				for (int i = 0; i < cltemp; i++) { //inizializza/calcola mrow
					mrow[i][1] = 0;
					for (int k = 0; k < nObs; k++) {
						mrow[i][1] = mrow[i][1] + Math.pow(U[i][k], m_nFLev);

					}
				}

				/*** OK ***/
				for (int i = 0; i < cltemp; i++) { //calcola i centri
					for (int j = 0; j < m_nNumNorm; j++) {
						centers[i][j] = 0;
						for (int k = 0; k < nObs; k++) {
							//centers[i][j]=centers[i][j] + Math.pow(U[i][k], (double)m_nFLev)*(Double.parseDouble(m_arrNorm[j].m_numArray.get(k).toString()));
							centers[i][j] = centers[i][j] + Math.pow(U[i][listOfVars[j].getObsID(k) - 1], m_nFLev) * listOfVars[j].getValue(k);
						}
						centers[i][j] = centers[i][j] / mrow[i][1];
					}
				}
				/*** OK ***/
				for (int k = 0; k < nObs; k++) {
					singularity[k][0] = -1;//era [1]
				}

				for (int i = 0; i < cltemp; i++) { //calcola distanza datapoint-centri
					for (int k = 0; k < nObs; k++) {
						distance[i][obsSel[k].getID() - 1] = 0;
						for (int j = 0; j < m_nNumNorm; j++) {
							//distance[i][k] = distance[i][k] + Math.pow(centers[i][j]-(Double.parseDouble(m_arrNorm[j].m_numArray.get(k).toString())),2.0);
							distance[i][obsSel[k].getID() - 1] = distance[i][obsSel[k].getID() - 1]
									+ Math.pow(centers[i][j] - obsSel[k].getIndex(varS[j]), 2.0);
						}
						distance[i][obsSel[k].getID() - 1] = Math.sqrt(distance[i][obsSel[k].getID() - 1]);
						if (distance[i][obsSel[k].getID() - 1] == 0) {
							singularity[obsSel[k].getID() - 1][0] = i;//era[1]
						}
					}
				}

				for (int i = 0; i < cltemp; i++) {
					for (int k = 0; k < nObs; k++) {
						if (singularity[obsSel[k].getID() - 1][0] >= 0) {//era[1]
							for (int j = 0; j < cltemp; j++) {
								if (j == singularity[obsSel[k].getID() - 1][0]) {
									newU[j][obsSel[k].getID() - 1] = 1;
								}
								newU[j][obsSel[k].getID() - 1] = 0;
							}
						} else {
							newU[i][obsSel[k].getID() - 1] = 0;
							for (int j = 0; j < cltemp; j++) {
								newU[i][obsSel[k].getID() - 1] = newU[i][obsSel[k].getID() - 1]
										+ Math.pow(distance[j][obsSel[k].getID() - 1], (-2 / (m_nFLev - 1)));
							}
							newU[i][obsSel[k].getID() - 1] = newU[i][obsSel[k].getID() - 1]
									* Math.pow(distance[i][obsSel[k].getID() - 1], (2 / (m_nFLev - 1)));
							newU[i][obsSel[k].getID() - 1] = 1 / newU[i][obsSel[k].getID() - 1];
						}
					}
				}

				U = newU;
			}

			fuzzy.setAssign(currClust++, newU);

			//calcolo delle entropie
			for (int i = 0; i < cltemp; i++) {
				for (int k = 0; k < nObs; k++) {
					if (U[i][k] != 0) {
						entropy[cltemp - 2] = entropy[cltemp - 2] + U[i][k] * Math.log(U[i][k]);
					}
				}
			}
			entropy[cltemp - 2] = entropy[cltemp - 2] / (-nObs);
			clus_entropy[cltemp - 2] = entropy[cltemp - 2];
		}
		fuzzy.setEntropy(clus_entropy);
	}

	private void initMatrix(int pos, int row, int col) {
		double[][] m = new double[0][0];
		switch (pos) {
			case 0:
				U = new double[row][col];
				//m = U;
				//inizializzazione come suggerito da E.N.Magenis
				for (int i = 0; i < row; i++) {
					for (int j = 0; j < col; j++) {
						if ((i == j) || ((i == row - 1) && (j > row - 1))) {
							U[i][j] = 1;
						} else {
							U[i][j] = 0;
						}
					}
				}
				break;
			case 1:
				newU = new double[row][col];
				//m = newU;
				break;
			case 2:
				centers = new double[row][col];
				//m = centers;
				break;
			case 3:
				mrow = new double[row][col];
				//m = mrow;
				break;
			case 4:
				singularity = new double[row][col];
				//m = singularity;
				break;
			case 5:
				distance = new double[row][col];
				//m = distance;
				break;
		}
	}
}
