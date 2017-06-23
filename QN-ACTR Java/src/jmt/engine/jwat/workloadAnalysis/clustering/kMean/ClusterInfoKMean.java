package jmt.engine.jwat.workloadAnalysis.clustering.kMean;

public class ClusterInfoKMean {

	public ClusterInfoKMean(int numVars) {
		statClust = new SCluStat[numVars];
		for (int i = 0; i < numVars; i++) {
			statClust[i] = new SCluStat();
		}
		percVar = new double[numVars];
	}

	public class SCluStat {

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

		//Temp variables
		public double dSomma; //Somma dei valori per ogni variabile	v[1]
		public double dSQuad; //Somma dei quadrati					v[2]
		public double dSTerz; //Somma delle terze potenze				v[3]
		public double dSQuar; //Somma delle quarte potenze			v[4]
	}

	public double[] percVar;// percentuale di ogni variabile sul totale della variabile
	public SCluStat[] statClust;
	public int numOss;

}
