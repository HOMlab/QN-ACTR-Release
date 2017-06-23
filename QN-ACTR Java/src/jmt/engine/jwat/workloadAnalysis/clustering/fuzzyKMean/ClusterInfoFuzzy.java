package jmt.engine.jwat.workloadAnalysis.clustering.fuzzyKMean;

public class ClusterInfoFuzzy {

	public ClusterInfoFuzzy(int numVars) {
		statClust = new SFCluStat[numVars];
		for (int i = 0; i < numVars; i++) {
			statClust[i] = new SFCluStat();
		}
		percVar = new double[numVars];
	}

	public class SFCluStat {

		public int iNotZr; //Numero delle Osservazioni diverse da Zero
		public double dMedia; //Media
		public double dStdEr; //Standard Error
		public double dStdDv; //Standard Deviation
		public double dVarnz; //Varianza
		public double dKurto; //Kurtosis
		public double dSkewn; //Skewness
		public double dRange; //Range   (Max-Min)
		public double dMinOs; //Valore Minimo
		public double dMaxOs; //Valore Massimo

		public double dSomma; //Somma dei valori per ogni variabile	v[1]
		public double dSQuad; //Somma dei quadrati					v[2]
		public double dSTerz; //Somma delle terze potenze				v[3]
		public double dSQuar; //Somma delle quarte potenze			v[4]
		public double dPerc5; //Usato per i percentili				v[5]
		public double dPerc6; //Usato per i percentili				v[6]
		public double dPerc7; //Usato per i percentili				v[7]
	}

	public String clus_log; // log dei valori del Cluster
	public double[] percVar;// percentuale di ogni variabile sul totale della variabile
	public SFCluStat[] statClust;
	public int numOss;
}
