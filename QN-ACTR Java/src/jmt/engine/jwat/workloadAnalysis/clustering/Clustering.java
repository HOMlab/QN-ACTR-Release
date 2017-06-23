package jmt.engine.jwat.workloadAnalysis.clustering;

public interface Clustering {
	public int getNumCluster();

	public String getName();

	public ClusteringInfos getClusteringInfos(int numCluster);

	public int getClusteringType(); //UPDATE 03/11/06

	public int[] getVarClust();
}
