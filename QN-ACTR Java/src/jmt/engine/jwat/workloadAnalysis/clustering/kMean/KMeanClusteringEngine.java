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

package jmt.engine.jwat.workloadAnalysis.clustering.kMean;

import java.text.DecimalFormat;
import java.text.NumberFormat;

import jmt.engine.jwat.MatrixOsservazioni;
import jmt.engine.jwat.Observation;
import jmt.engine.jwat.TimeConsumingWorker;
import jmt.engine.jwat.VariableNumber;

public class KMeanClusteringEngine {
	private TimeConsumingWorker worker;
	private KMean clustering;
	private MatrixOsservazioni m;
	private Observation[] obsVal;
	private VariableNumber[] listAllVar; //Puntatori a tutte le variabili
	private VariableNumber[] listSelVar; //Puntatori a tutte le variabili
	private int indexSelVar[];

	private int numMaxClust; //Num.massimo di cluster da considerare
	private int numMaxIter; //Num.massimo di iterazioni
	private int numTotVar; //Numero totale delle variabili
	private int numVarSel; //Numero totale delle variabili

	private boolean[] isSelectedVar; //Marca le variabili coinvolte nel Clustering all'interno del totale delle variabili

	public TempClusterStatistics[][] sum; //Contiene le statistiche dei vari cluster
	private double[] omsr; //Rapporto totale di varianza per partizione

	private int[] nn; //Usata per contenere il numero di oss.per

	public String m_strKMLog; //Log da aggiungere al session log
	//public Vector nclusArray;					//Vector contenente un vector per ogni cluster	//Array contenente il cluster attuale della variabile
	public short[][] nclust; //Matrice contenente cluster attuale osservazione, per ogni n-clustering

	NumberFormat Floatformatter = new DecimalFormat("###.###E0");

	public class TempClusterStatistics {
		public double Coord; //	1 coordinata
		public int numOs; //	2 # di osservazioni
		public double Media; //	3 media
		public double stdDv; //	4 std.deviation
		public double minOs; //	5 minimum value
		public double maxOs; //	6 maximum value
		public double Skewn; //	7 skewness
		public double Kurto; //	8 kurtosis
		public double Varnz; //	9 varianza
		public double SSDev; // 10 somma delle square deviations from centre of cluster

		public double Somma; //	1 somma dei valori
		public double SumQd; //	2 somma dei quadrati
		public double SumTz; //	3 somma delle terze potenze
		public double SumQt; //	4 somma delle quarte potenze

		public double VVdi5; //	5
		public double VVdi6; //	6
		public double VVdi7; //	7
	}

	public KMeanClusteringEngine(KMean clustering, TimeConsumingWorker worker) {
		this.worker = worker;
		this.clustering = clustering;
	}

	public boolean PrepClustering(MatrixOsservazioni m, int numeroMassimoCluster, int numeroMassimoIterazioni, int variabiliSelezionate[]) {
		int j = 0, i = 0;
		this.m = m;
		/* Inserite per TEST */
		numMaxClust = numeroMassimoCluster; // Num.massimo di clusters da considerare
		numMaxIter = numeroMassimoIterazioni; // Num. max di iterazioni
		/* Salvataggio del numero di variaibli totali */
		numTotVar = m.getNumVariables(); //Numero totale delle variabili
		numVarSel = variabiliSelezionate.length; //Numero totale delle variabili

		obsVal = m.getVariables()[0].getCurObs();
		/* Inizializzazione degli array variabili normalizzate e dei puntatori alle variabili */
		listAllVar = m.getVariables();
		listSelVar = new VariableNumber[numVarSel];
		indexSelVar = variabiliSelezionate;
		/* Settaggio delle variaibli che sono coinvolte nel clustering */

		isSelectedVar = new boolean[numTotVar];
		sum = new TempClusterStatistics[numTotVar][numMaxClust];
		omsr = new double[numMaxClust];
		nn = new int[numMaxClust];

		for (i = 0; i < numTotVar; i++) {
			isSelectedVar[i] = false;
		}

		for (i = 0; i < numTotVar; i++) {
			if (variabiliSelezionate[j] == i) {
				isSelectedVar[i] = true;
				listSelVar[j] = listAllVar[i];
				j++;
				if (j == numVarSel) {
					break;
				}
			}
		}
		InitData();
		return true;

	}

	private void InitData() {
		int i = 0;
		/* Inizializzazione del vettore delle statistiche delle variaibli */
		for (i = 0; i < numMaxClust; i++) {
			for (int j = 0; j < numVarSel; j++) {
				sum[j][i] = new TempClusterStatistics();
				sum[j][i].Coord = 0;
				sum[j][i].Kurto = 0;
				sum[j][i].maxOs = 0;
				sum[j][i].Media = listSelVar[j].getUniStats().getMinValue();
				sum[j][i].minOs = 0;
				sum[j][i].numOs = 0;
				sum[j][i].Skewn = 0;
				sum[j][i].SSDev = 0;
				sum[j][i].stdDv = 0;
				sum[j][i].Varnz = 0;

				sum[j][i].Somma = 0;
				sum[j][i].SumQd = 0;
				sum[j][i].SumTz = 0;
				sum[j][i].SumQt = 0;
				sum[j][i].VVdi5 = 0;
				sum[j][i].VVdi6 = 0;
				sum[j][i].VVdi7 = 0;
			}
			/* Inizializzazione del vettore che contiene il numero di punti per cluster */
			nn[i] = 0;
			/* Azzeramento dell'array del rapporto totale di varianza per partizione */
			omsr[i] = 0;
		}
	}

	public boolean DoClustering() {

		/* Indica il cluster di partenza */
		int endclust, startClust = 0;
		//nclusArray= new Vector();
		nclust = new short[numMaxClust][obsVal.length];
		/* Richiama la funzione che effettua l clustering */
		endclust = Build(startClust);
		if (endclust != -1) {
			clustering.setRatio(endclust + 1);
			return true;
		} else {
			return false;
		}
	}

	private int Build(int startClust) {
		//Costruisce effettivamente i vari cluster
		//aggiorna lo stato dei calcoli sulla Status Bar
		int km = 0;
		/* creazione variaibili di utilità ignota */
		int nclus = 0;
		double dclus = 0;
		int retVal = 0;
		int i = 0;
		/* Eseguo il ciclo di operazioni per il numero di cluster da considerare scelto nel form del clustering*/
		for (int kk = startClust; kk < numMaxClust; kk++) {
			/* Eseguo il ciclo di operazioni per il numero massimo di iterazioni scelte nel form del clustering */
			for (int nc = 0; nc < numMaxIter; nc++) {
				if (worker.isCanceled()) {
					return -1;
				}
				worker.updateInfos(2 + nc + (kk * numMaxIter), "Clustering " + (kk + 1) + " Iteration " + (nc + 1), false);
				boolean err = false;
				for (int kkk = 0; kkk <= kk; kkk++) {
					for (int j = 0; j < numVarSel; j++) {
						if ((nc == 0) || (sum[j][kkk].Coord != sum[j][kkk].Media)) {
							/* Controllo che la Coordinata ??? sia diversa dalla media del cluster j */
							err = true;
						}
					}
				}
				/* Il primo ciclo entra sempre nell'if poichè nc = 0 e err è settato a TRUE */
				if (err) {
					for (int kkk = 0; kkk <= kk; kkk++) {
						/* Azzero il numero di osservazioni per il cluster kkk */
						nn[kkk] = 0;
						/* Re Inizializzo la variabile sum che contiene le statistiche di ogni cluster */
						for (int j = 0; j < numVarSel; j++) {
							/* PERCHE' ???? */
							sum[j][kkk].Coord = sum[j][kkk].Media;
							/* Azzera i campi */
							sum[j][kkk].Somma = 0;
							sum[j][kkk].SumQd = 0;
							sum[j][kkk].SumTz = 0;
							sum[j][kkk].SumQt = 0;
							sum[j][kkk].VVdi5 = 0;
							sum[j][kkk].VVdi6 = 0;
							sum[j][kkk].VVdi7 = 0;
						}
					}
					/**************************************** kmeans ********************************************/
					/* Recupero il numero di osservazioni totali */
					int numOsserv = listSelVar[0].Size();
					/* Preparo la variaible per contenere il cluster attuale */
					//nclusArray.add(new Vector());
					//((Vector)nclusArray.get(kk)).setSize(numOsserv);
					for (int xx = 0; xx < numOsserv; xx++) {

						/* Operazioni per indicare nella barra la percentuale e il passo attuali dell'algoritmo di kmean */

						nclus = 0; /* Presumo siano il numero di cluster ??????????? */
						dclus = Double.POSITIVE_INFINITY; /* Indica la differenza iniziale da considerare tra i cluster ???????????????? */
						/*************************************************/
						/* kk è il numero corrente di cluster utilizzati
						  /*************************************************/
						for (int j = 0; j <= kk; j++) {
							/*
							 xp = 10^(-10)
							 dd = ????????
							 */
							double xp = Math.pow(10, -10);
							double dd = 0;
							/*
							 m_NumNorm Numero delle variaibli normalizzate
							 */
							for (int r = 0; r < numVarSel; r++) {
								dd += Math.pow(obsVal[xx].getIndex(indexSelVar[r]) - sum[r][j].Coord, 2);
								xp = xp + 1;
							}
							double temp = dd / xp;
							//double temp = dd/1;
							dd = Math.sqrt(temp);
							//System.err.println("Xp : " + xp + " temp : " + temp + " dclus : " + dclus + " dd : " + dd);
							if (dd <= dclus) {
								dclus = dd;
								nclus = j;
							}
						}

						nn[nclus]++;
						//((Vector)nclusArray.get(kk)).setElementAt(new Integer(nclus),obsVal[xx].getID()-1);
						nclust[kk][obsVal[xx].getID() - 1] = (short) nclus;
						//
						// computation of stat val
						//
						int tempNum = nn[nclus];

						if (tempNum == 0) {
							for (i = 0; i < numVarSel; i++) {
								sum[i][nclus].minOs = 0;
								sum[i][nclus].maxOs = 0;
								sum[i][nclus].Media = 0;
								sum[i][nclus].SSDev = 0;
							}
						} else {
							for (i = 0; i < numVarSel; i++) {
								double curVal = obsVal[xx].getIndex(indexSelVar[i]);
								if (tempNum == 1) {
									sum[i][nclus].Media = curVal;
									sum[i][nclus].SSDev = 0;
									sum[i][nclus].minOs = curVal;
									sum[i][nclus].maxOs = curVal;
								} else {
									if (sum[i][nclus].maxOs <= curVal) {
										sum[i][nclus].maxOs = curVal;
									}
									if (sum[i][nclus].minOs >= curVal) {
										sum[i][nclus].minOs = curVal;
									}
									sum[i][nclus].Media = sum[i][nclus].Media + (curVal - sum[i][nclus].Media) / tempNum;
									sum[i][nclus].SSDev = sum[i][nclus].SSDev + tempNum * Math.pow(curVal - sum[i][nclus].Media, 2) / (tempNum - 1);
								}

								sum[i][nclus].Somma += curVal;
								sum[i][nclus].SumQd += Math.pow(curVal, 2);
								sum[i][nclus].SumTz += Math.pow(curVal, 3);
								sum[i][nclus].SumQt += Math.pow(curVal, 4);
							}
						}
					}

					for (nclus = 0; nclus <= kk; nclus++) {

						int tempNum = nn[nclus];
						// stat2c
						for (i = 0; i < numVarSel; i++) {

							if (tempNum != 0) {
								sum[i][nclus].VVdi5 = Math.pow(sum[i][nclus].Media, 2);
								sum[i][nclus].VVdi6 = Math.pow(sum[i][nclus].Media, 3);
								sum[i][nclus].VVdi7 = Math.pow(sum[i][nclus].Media, 4);

								sum[i][nclus].stdDv = Math.sqrt(sum[i][nclus].SSDev / tempNum);

								if (!((tempNum == 1) || (sum[i][nclus].minOs == sum[i][nclus].maxOs))) {
									sum[i][nclus].Varnz = sum[i][nclus].SumQd - tempNum * sum[i][nclus].VVdi5 / (tempNum - 1);

									sum[i][nclus].Skewn = sum[i][nclus].SumTz;
									sum[i][nclus].Skewn -= 3 * sum[i][nclus].Media * sum[i][nclus].SumQd;
									sum[i][nclus].Skewn += 3 * sum[i][nclus].VVdi5 * sum[i][nclus].Somma;

									sum[i][nclus].Kurto = sum[i][nclus].SumQt;
									sum[i][nclus].Kurto -= 4 * sum[i][nclus].Media * sum[i][nclus].SumTz;
									sum[i][nclus].Kurto += 6 * sum[i][nclus].VVdi5 * sum[i][nclus].SumQd;
									sum[i][nclus].Kurto -= 4 * sum[i][nclus].VVdi6 * sum[i][nclus].Somma;

									double qsum9 = Math.pow(sum[i][nclus].Varnz, 2);
									double csum9 = Math.pow(sum[i][nclus].Varnz, 3 / 2);

									if (csum9 == 0) {
										sum[i][nclus].Skewn = 0;
										sum[i][nclus].Kurto = 0;
									} else {
										sum[i][nclus].Skewn /= tempNum;
										sum[i][nclus].Skewn -= sum[i][nclus].VVdi6 / csum9;

										if (qsum9 == 0) {
											sum[i][nclus].Kurto = 0;
										} else {
											sum[i][nclus].Kurto /= tempNum;
											sum[i][nclus].Kurto += sum[i][nclus].VVdi7;
											sum[i][nclus].Kurto /= qsum9;
											sum[i][nclus].Kurto -= 3;
										}
									}
								} else {
									sum[i][nclus].Varnz = 0;
									sum[i][nclus].stdDv = 0;
									sum[i][nclus].Skewn = 0;
									sum[i][nclus].Kurto = 0;
								}
							} else {
								sum[i][nclus].Varnz = 0;
								sum[i][nclus].stdDv = 0;
								sum[i][nclus].Skewn = 0;
								sum[i][nclus].Kurto = 0;
							}
						}

						for (int j = 0; j < numVarSel; j++) {
							sum[j][nclus].numOs = nn[nclus];

						}//j
					}// nclust
				} else {
					break; //esce dal for nc
				}// endif (err!=0)
			}// nc

			clustering.calcClusteringInfo(kk, sum,/*(Vector)nclusArray.get(kk),*/nclust[kk], m);

			if (kk != (numMaxClust - 1)) { //era venti
				double sm = 0;
				for (int j = 0; j < numVarSel; j++) {
					for (int kkk = 0; kkk <= kk; kkk++) {
						if (sum[j][kkk].stdDv >= sm) {
							sm = sum[j][kkk].stdDv;
							km = kkk;
						}
					}
				}

				int kn = kk + 1;
				for (int jj = 0; jj < numVarSel; jj++) {
					sum[jj][kn].numOs = 0;
					sum[jj][kn].Media = 0.;
					sum[jj][km].numOs = 0;
					sum[jj][km].Media = 0.;
				}
				// it compute the centroid of...
				for (int xx = 0; xx < listSelVar[0].Size(); xx++) {
					//if (Integer.parseInt(((Vector)nclusArray.get(kk)).get(obsVal[xx].getID()-1).toString()) == km) {
					if (nclust[kk][obsVal[xx].getID() - 1] == km) {
						for (int jj = 0; jj < numVarSel; jj++) {
							if (obsVal[xx].getIndex(indexSelVar[jj]) < sum[jj][km].Coord) {
								sum[jj][km].numOs++;
								sum[jj][km].Media += obsVal[xx].getIndex(indexSelVar[jj]);
							} else {
								sum[jj][kn].numOs++;
								sum[jj][kn].Media += obsVal[xx].getIndex(indexSelVar[jj]);
							}
						}
					}
				}

				for (int jj = 0; jj < numVarSel; jj++) {
					if (sum[jj][km].numOs != 0) {
						sum[jj][km].Media /= sum[jj][km].numOs;
					}

					if (sum[jj][kn].numOs != 0) {
						sum[jj][kn].Media /= sum[jj][kn].numOs;
					}
				}
			}

			retVal = kk;
		}// kk
		return retVal;
	}
}
