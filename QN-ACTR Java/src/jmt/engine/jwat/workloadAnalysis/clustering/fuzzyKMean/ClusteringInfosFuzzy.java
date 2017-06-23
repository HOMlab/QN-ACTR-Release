package jmt.engine.jwat.workloadAnalysis.clustering.fuzzyKMean;

import jmt.engine.jwat.MatrixOsservazioni;
import jmt.engine.jwat.Observation;
import jmt.engine.jwat.workloadAnalysis.clustering.ClusteringInfos;

public class ClusteringInfosFuzzy implements ClusteringInfos {

	public ClusterInfoFuzzy[] infoCluster;
	private double[][] assignment = null;
	private double err;
	private int numCluster;
	private short assignedToClust[];

	public int[] numElem;
	public double[] percent; // percentuale sul totale

	public String centri; // contiene i centri dei cluster
	public String log; // informazioni generali

	public ClusteringInfosFuzzy(double[][] ass, int nCluster) {
		assignment = ass;
		numCluster = nCluster;
		err = -1;
		percent = new double[nCluster + 1];
		assignedToClust = new short[assignment[0].length];
		infoCluster = new ClusterInfoFuzzy[nCluster];
	}

	public int getNumClusters() {
		return numCluster;
	}

	public ClusterInfoFuzzy getCluster(int index) {
		if (err != -1 && index < numCluster) {
			return infoCluster[index];
		}
		return null;
	}

	public short[] getAssignment() {
		return assignedToClust;
	}

	public void setError(MatrixOsservazioni m, double err) {
		if (this.err == err) {
			return;
		}
		this.err = err;
		int i, j;
		boolean assigned;
		numElem = new int[numCluster + 1];
		//Creazione InfoCluster
		for (i = 0; i < assignment[0].length; i++) {
			assigned = false;
			for (j = 0; j < assignment.length; j++) {
				if (assignment[j][i] >= err) {
					numElem[j]++;
					if (!assigned) {
						assignedToClust[i] = (short) (j + 2);
					} else {
						assignedToClust[i] = 1;
					}
					assigned = true;
				}
			}
			if (!assigned) {
				assignedToClust[i] = 0;
				numElem[numCluster]++;
			}
		}
		for (i = 0; i < numCluster + 1; i++) {
			percent[i] = (double) numElem[i] / assignment[0].length;
		}
		InitInfo(m.getNumVariables());
		DoStat(m);
	}

	public double getError() {
		return err;
	}

	private void InitInfo(int nVar) {
		for (int i = 0; i < infoCluster.length; i++) {
			infoCluster[i] = new ClusterInfoFuzzy(nVar);
		}
	}

	private void DoStat(MatrixOsservazioni m) {
		int nclus;

		Observation[] currOss = m.getListOss(); //Valore corrente di ogni variabile
		for (int count = 0; count < m.getNumOfObs(); count++) {

			nclus = assignedToClust[count];
			if (nclus < 2) {
				continue;
			}
			nclus -= 2;
			infoCluster[nclus].numOss++;

			//Calcola somma, somma quadr. etc.

			for (int i = 0; i < m.getNumVariables(); i++) {
				if (currOss[count].getIndex(i) != 0) {
					infoCluster[nclus].statClust[i].iNotZr++;
				}
				if (infoCluster[nclus].numOss == 1) {
					infoCluster[nclus].statClust[i].dMaxOs = currOss[count].getIndex(i);
					infoCluster[nclus].statClust[i].dMinOs = currOss[count].getIndex(i);
				} else {
					if (infoCluster[nclus].statClust[i].dMaxOs < currOss[count].getIndex(i)) {
						infoCluster[nclus].statClust[i].dMaxOs = currOss[count].getIndex(i);
					}
					if (infoCluster[nclus].statClust[i].dMinOs > currOss[count].getIndex(i)) {
						infoCluster[nclus].statClust[i].dMinOs = currOss[count].getIndex(i);
					}
				}
				infoCluster[nclus].statClust[i].dSomma += currOss[count].getIndex(i);
				infoCluster[nclus].statClust[i].dSQuad += Math.pow(currOss[count].getIndex(i), 2);
				infoCluster[nclus].statClust[i].dSTerz += Math.pow(currOss[count].getIndex(i), 3);
				infoCluster[nclus].statClust[i].dSQuar += Math.pow(currOss[count].getIndex(i), 4);

			}
		}

		double somma_per_var;
		//Calcola percentuale della variabile usata nel cluster
		for (int i = 0; i < m.getNumVariables(); i++) {
			somma_per_var = 0;
			for (int l = 0; l < numCluster; l++) {
				somma_per_var += infoCluster[l].statClust[i].dSomma;
			}

			for (int l = 0; l < numCluster; l++) {

				infoCluster[l].percVar[i] = infoCluster[l].statClust[i].dSomma / somma_per_var;
			}
		}

		//Porting di stat2cl
		//Calcola media, varianza, etc,etc

		for (int l = 0; l < numCluster; l++) {

			for (int i = 0; i < m.getNumVariables(); i++) {
				infoCluster[l].statClust[i].dRange = infoCluster[l].statClust[i].dMaxOs - infoCluster[l].statClust[i].dMinOs;
				infoCluster[l].statClust[i].dMedia = infoCluster[l].statClust[i].dSomma / infoCluster[l].numOss;

				if (infoCluster[l].numOss != 1) {
					infoCluster[l].statClust[i].dPerc5 = Math.pow(infoCluster[l].statClust[i].dMedia, 2);
					infoCluster[l].statClust[i].dPerc6 = Math.pow(infoCluster[l].statClust[i].dMedia, 3);
					infoCluster[l].statClust[i].dPerc7 = Math.pow(infoCluster[l].statClust[i].dMedia, 4);

					infoCluster[l].statClust[i].dVarnz = (infoCluster[l].statClust[i].dSQuad - infoCluster[l].numOss
							* infoCluster[l].statClust[i].dPerc5)
							/ (infoCluster[l].numOss - 1);

					if (infoCluster[l].statClust[i].dVarnz != 0) {
						infoCluster[l].statClust[i].dStdDv = Math.sqrt(infoCluster[l].statClust[i].dVarnz);
						infoCluster[l].statClust[i].dStdEr = infoCluster[l].statClust[i].dStdDv / Math.sqrt(infoCluster[l].numOss);

						infoCluster[l].statClust[i].dSkewn = infoCluster[l].statClust[i].dSTerz - 3 * infoCluster[l].statClust[i].dMedia
								* infoCluster[l].statClust[i].dSQuad + 3 * infoCluster[l].statClust[i].dPerc5 * infoCluster[l].statClust[i].dSomma;
						infoCluster[l].statClust[i].dSkewn /= infoCluster[l].numOss;
						infoCluster[l].statClust[i].dSkewn -= infoCluster[l].statClust[i].dPerc6;
						infoCluster[l].statClust[i].dSkewn /= Math.pow(infoCluster[l].statClust[i].dVarnz, 1.5);

						infoCluster[l].statClust[i].dKurto = infoCluster[l].statClust[i].dSQuar - 4 * infoCluster[l].statClust[i].dMedia
								* infoCluster[l].statClust[i].dSTerz + 6 * infoCluster[l].statClust[i].dPerc5 * infoCluster[l].statClust[i].dSQuad
								- 4 * infoCluster[l].statClust[i].dPerc6 * infoCluster[l].statClust[i].dSomma;
						infoCluster[l].statClust[i].dKurto /= infoCluster[l].numOss;
						infoCluster[l].statClust[i].dKurto += infoCluster[l].statClust[i].dPerc7;
						infoCluster[l].statClust[i].dKurto /= Math.pow(infoCluster[l].statClust[i].dVarnz, 2);
						infoCluster[l].statClust[i].dKurto -= 3;
					}
				}
			}
		}
	}

}
