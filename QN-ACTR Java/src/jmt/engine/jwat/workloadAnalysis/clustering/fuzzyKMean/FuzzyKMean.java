package jmt.engine.jwat.workloadAnalysis.clustering.fuzzyKMean;

import jmt.engine.jwat.workloadAnalysis.clustering.Clustering;
import jmt.engine.jwat.workloadAnalysis.clustering.ClusteringInfos;

public class FuzzyKMean implements Clustering {

	private ClusteringInfosFuzzy[] results;
	private int maxClust;
	private int[] varSel;
	private double[][][] assign;
	private double[] entropy;

	public FuzzyKMean(int numClust, int[] varSel) {
		maxClust = numClust;
		this.varSel = varSel;
		assign = new double[numClust - 1][][];
		results = new ClusteringInfosFuzzy[numClust - 1];
	}

	public String getName() {
		return "Fuzzy k-Means";
	}

	public int getClusteringType() {
		return 1;
	}

	public int getNumCluster() {
		return results.length;
	}

	public void setAssign(int pos, double[][] ass) {
		assign[pos] = ass;
		results[pos] = new ClusteringInfosFuzzy(ass, pos + 2);
	}

	public void setEntropy(double[] ent) {
		entropy = ent;
	}

	public ClusteringInfos getClusteringInfos(int numCluster) {
		return results[numCluster];
	}

	public double[][] getAssignment(int clust) {
		return assign[clust];
	}

	public int[] getVarClust() {
		return varSel;
	}

	public double[] getEntropy() {
		return entropy;
	}
}
