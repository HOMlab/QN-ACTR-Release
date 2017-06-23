package jmt.engine.jwat.workloadAnalysis.clustering.kMean;

import jmt.engine.jwat.MatrixOsservazioni;
import jmt.engine.jwat.Observation;
import jmt.engine.jwat.workloadAnalysis.clustering.ClusteringInfos;
import jmt.engine.jwat.workloadAnalysis.clustering.kMean.KMeanClusteringEngine.TempClusterStatistics;

public class ClusteringInfosKMean implements ClusteringInfos {

	public ClusterInfoKMean[] infoCluster;

	public int isGoodCluster; // -1 non presente; 0 non ottimo; 1 ottimo
	public double omsr; // overall
	public double ratio; // ratio

	public int[] numElem; // numero di elementi di ogni singolo cluster
	public double[] percent; // percentuale sul totale

	public int numCluster;

	public double passw;

	public ClusteringInfosKMean(int numCluster, int nvars) {
		ratio = -1;
		isGoodCluster = 0;
		numElem = new int[numCluster + 1];
		infoCluster = new ClusterInfoKMean[numCluster + 1];
		for (int i = 0; i < numCluster + 1; i++) {
			infoCluster[i] = new ClusterInfoKMean(nvars);
		}
		percent = new double[numCluster + 1];
		this.numCluster = numCluster;
	}

	public void Output(int[] varSel, TempClusterStatistics[][] sum, short[] clusAssign, MatrixOsservazioni m, double oldPassw) //Deve valere assw precedentemente calcolato 
	{

		passw = oldPassw;

		double ssb; // varianza fra le classi sulla variabile j
		double ssw; // varianza interna sulla variabile j
		double dfw = 0; // grado di libertà interna fra le classi
		double assw = 0;// errore totale nella partizione

		double sd, sc, r[];
		r = new double[varSel.length];
		for (int i = 0; i < varSel.length; i++) {
			r[i] = 0;
		}

		double th = Math.pow(10, -10);
		double dfb = numCluster + 1;

		for (int j = 0; j < varSel.length; j++) {

			sd = 0;
			sc = 0;
			ssb = 0;
			ssw = 0;
			for (int k = 0; k <= numCluster; k++) {
				sd += (sum[j][k].Media * sum[j][k].numOs);
				ssb += (Math.pow(sum[j][k].Media, 2) * sum[j][k].numOs);
				ssw += sum[j][k].SSDev;
				sc += sum[j][k].numOs;
			}

			dfw = sc - dfb - 2;
			if (sc == 0.) {
				sc = th;
			}
			if (dfw == 0.) {
				dfw = th;
			}
			if (dfb == 0.) {
				dfb = th;
			}
			assw += ssw;
			ssb -= (Math.pow(sd, 2) / sc);
			ssb /= dfb;
			ssw /= dfw;
			if (ssw == 0.) {
				ssw = th;
			}

			ratio = 0; // variance ratio for variable j
			if (ssw != 0) { // per me non serve...
				ratio = (r[j] / ssw - 1) * (1 + dfw) + 1;
				r[j] = ssw;

			}
		}

		if (assw == 0) {
			omsr = 0;
		} else {
			omsr = ((passw) / assw - 1) * dfw;
		}

		passw = assw;

		/* Determina per ogni cluster il numero dei punti che cadono in ciascun cluster */
		for (int xx = 0; xx < m.getNumOfObs(); xx++) {
			numElem[clusAssign[xx]]++;
		}
		for (int xx = 0; xx < numCluster + 1; xx++) {
			percent[xx] = (double) numElem[xx] / (double) m.getNumOfObs();
		}

	}

	public void DOStat(int[] varSel, short[] clusAssign, MatrixOsservazioni m) {
		boolean[] bValidVar = new boolean[m.getNumVariables()];

		int df = 0;
		for (int sk = 0; sk < bValidVar.length; sk++) {
			if (varSel[df] == sk) {
				bValidVar[sk] = true;
				df++;
				if (df == varSel.length) {
					break;
				}
			} else {
				bValidVar[sk] = false;
			}
		}
		DoStat(m, varSel.length, clusAssign, bValidVar);
	}

	private void DoStat(MatrixOsservazioni m, int nNumUsed, short[] arrNClus, boolean[] bValidVar) {

		double absoluteVal[][] = new double[arrNClus.length][m.getNumVariables()];

		int nclus;
		//Observation[] currOss=m.getListOss();		//Valore corrente di ogni variabile
		Observation[] currOss = m.getVariables()[0].getCurObs();
		for (int count = 0; count < m.getNumOfObs(); count++) {

			//nclus = Integer.parseInt(arrNClus.get(count).toString());
			nclus = arrNClus[currOss[count].getID() - 1];
			infoCluster[nclus].numOss++;

			//Porting di stat1cl
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
				absoluteVal[nclus][i] += Math.abs(currOss[count].getIndex(i));
				infoCluster[nclus].statClust[i].dSQuad += Math.pow(currOss[count].getIndex(i), 2);
				infoCluster[nclus].statClust[i].dSTerz += Math.pow(currOss[count].getIndex(i), 3);
				infoCluster[nclus].statClust[i].dSQuar += Math.pow(currOss[count].getIndex(i), 4);

			}
		}

		double somma_per_var;
		double abs_somma_per_var;
		//Calcola percentuale della variabile usata nel cluster
		for (int i = 0; i < m.getNumVariables(); i++) {
			somma_per_var = 0;
			abs_somma_per_var = 0;
			for (int l = 0; l <= numCluster; l++) {
				somma_per_var += infoCluster[l].statClust[i].dSomma;
				abs_somma_per_var += absoluteVal[l][i];
			}

			for (int l = 0; l <= numCluster; l++) {
				infoCluster[l].percVar[i] = absoluteVal[l][i] / abs_somma_per_var;
				//infoCluster[l].percVar[i] = infoCluster[l].statClust[i].dSomma / somma_per_var;
			}
		}

		//Porting di stat2cl
		//Calcola media, varianza, etc,etc
		double dPerc5, dPerc6, dPerc7;

		for (int l = 0; l <= numCluster; l++) {

			for (int i = 0; i < m.getNumVariables(); i++) {
				infoCluster[l].statClust[i].dRange = infoCluster[l].statClust[i].dMaxOs - infoCluster[l].statClust[i].dMinOs;
				infoCluster[l].statClust[i].dMedia = infoCluster[l].statClust[i].dSomma / infoCluster[l].numOss;

				if (infoCluster[l].numOss != 1) {
					dPerc5 = Math.pow(infoCluster[l].statClust[i].dMedia, 2);
					dPerc6 = Math.pow(infoCluster[l].statClust[i].dMedia, 3);
					dPerc7 = Math.pow(infoCluster[l].statClust[i].dMedia, 4);

					infoCluster[l].statClust[i].dVarnz = (infoCluster[l].statClust[i].dSQuad - infoCluster[l].numOss * dPerc5)
							/ (infoCluster[l].numOss - 1);

					if (infoCluster[l].statClust[i].dVarnz != 0) {
						infoCluster[l].statClust[i].dStdDv = Math.sqrt(infoCluster[l].statClust[i].dVarnz);
						infoCluster[l].statClust[i].dStdEr = infoCluster[l].statClust[i].dStdDv / Math.sqrt(infoCluster[l].numOss);

						infoCluster[l].statClust[i].dSkewn = infoCluster[l].statClust[i].dSTerz - 3 * infoCluster[l].statClust[i].dMedia
								* infoCluster[l].statClust[i].dSQuad + 3 * dPerc5 * infoCluster[l].statClust[i].dSomma;
						infoCluster[l].statClust[i].dSkewn /= infoCluster[l].numOss;
						infoCluster[l].statClust[i].dSkewn -= dPerc6;
						infoCluster[l].statClust[i].dSkewn /= Math.pow(infoCluster[l].statClust[i].dVarnz, 1.5);

						infoCluster[l].statClust[i].dKurto = infoCluster[l].statClust[i].dSQuar - 4 * infoCluster[l].statClust[i].dMedia
								* infoCluster[l].statClust[i].dSTerz + 6 * dPerc5 * infoCluster[l].statClust[i].dSQuad - 4 * dPerc6
								* infoCluster[l].statClust[i].dSomma;
						infoCluster[l].statClust[i].dKurto /= infoCluster[l].numOss;
						infoCluster[l].statClust[i].dKurto += dPerc7;
						infoCluster[l].statClust[i].dKurto /= Math.pow(infoCluster[l].statClust[i].dVarnz, 2);
						infoCluster[l].statClust[i].dKurto -= 3;
					}
				}
			}
		}
	}
}
