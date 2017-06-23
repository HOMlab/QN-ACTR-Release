package jmt.engine.jwat.workloadAnalysis.clustering.kMean;

import jmt.engine.jwat.MatrixOsservazioni;
import jmt.engine.jwat.workloadAnalysis.clustering.Clustering;
import jmt.engine.jwat.workloadAnalysis.clustering.ClusteringInfos;
import jmt.engine.jwat.workloadAnalysis.clustering.kMean.KMeanClusteringEngine.TempClusterStatistics;
import jmt.gui.jwat.JWATConstants;

public class KMean implements Clustering, JWATConstants {

	private ClusteringInfosKMean[] results;
	private short[][] clustAssign = null;
	//private Vector clusterAssignemnt=null; //Vector di Vector
	private int[] varSel;

	public KMean(int numClust, int[] varSel) {
		this.varSel = varSel;
		results = new ClusteringInfosKMean[numClust];
		clustAssign = new short[numClust][];
	}

	public KMean(ClusteringInfosKMean[] res, int[] varSel, short clustAssign[][]) {
		this.varSel = varSel;
		results = res;
		this.clustAssign = clustAssign;
	}

	public String getName() {
		return "k-Means";
	}

	public int getNumCluster() {
		return results.length;
	}

	public ClusteringInfos getClusteringInfos(int numCluster) {
		return results[numCluster];
	}

	public void calcClusteringInfo(int numCluster, TempClusterStatistics[][] sum, short[] cAss, MatrixOsservazioni m) {
		//clusterAssignemnt.add(clusAssign);
		clustAssign[numCluster] = cAss;
		results[numCluster] = new ClusteringInfosKMean(numCluster, m.getNumVariables());
		if (numCluster != 0) {
			results[numCluster].Output(varSel, sum, cAss, m, results[numCluster - 1].passw);
		} else {
			results[numCluster].Output(varSel, sum, cAss, m, 0);
		}
	}

	public void setRatio(int endClust) {
		if (endClust > 2) {
			//Mostra riassunto sui cluster
			/* Calcola un indice che mostri la validità di un clustering */
			for (int i = 1; i < (endClust - 1); i++) {
				if (results[i + 1].omsr != 0) {
					results[i].ratio = results[i].omsr / results[i + 1].omsr;
				}
			}
			//Calcola se esistono cluster validi
			for (int i = 1; i < (endClust - 1); i++) {
				if (results[i].ratio >= 1.5) {
					if (results[i].omsr > 10) {
						results[i].isGoodCluster = 1;
					}
				}
			}
			results[endClust - 1].ratio = 0;
		}
	}

	/*
	public Vector getClusteringAssignment()
	{
		return clusterAssignemnt;
	}
	*/
	/*
	public void setClusteringAssignment(Vector clusterAssign)
	{
		clusterAssignemnt=clusterAssign;
	}
	*/
	public int getClusteringType() {
		return KMEANS;
	}

	//UPDATE 02/11/2006: + matrice degli assegnamenti
	//					 + getter della matrice degli assegnamenti
	//					 + getter delle variabili clusterizzate
	public short[][] getAsseg() {
		return clustAssign;
	}

	public int[] getVarClust() {
		return varSel;
	}

	public ClusteringInfosKMean[] getResults() {
		return results;
	}
}
