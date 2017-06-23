package jmt.engine.jwat.trafficAnalysis;

import jmt.engine.jwat.VariableNumber;
import jmt.gui.jwat.trafficAnalysis.panels.InputPanel;

/**
 * <p>Title: BurstEngine</p>
 * <p>Description: The engine to calculate the burstiness factors
 * 
 * @author Marco Rosini
 *         Date: 27-07-2006
 *         Time: 11.01.29
 */
public class BurstEngine {

	private VariableNumber v;

	private double T;
	//arrival rate
	private double lambda;
	//epoch number
	private int n;

	private double EpochDuration[];

	private double b;

	//parameter a
	private double a;

	private double resultB[];

	private double resultA[];

	private double ArrPlus[];
	private double ArrPlusCount[];
	private double lambdaplus[];
	private double ArrMinusCount[];
	private double lambda_k[];

	/**
	 * Set burstiness factor a
	 * @param a is the burstiness factor a
	 * @param i is the number of epochs
	 */
	private void setA(double a, int i) {
		resultA[i - 1] = a;
	}

	/**
	 * Set burstiness factor b
	 * @param a is the burstiness factor b
	 * @param i is the number of epochs
	 */
	private void setB(double b, int i) {
		resultB[i - 1] = b;
	}

	/**
	 * Get burstiness factor a
	 */
	public double[] getA() {
		return resultA;
	}

	/**
	 * Get burstiness factor a
	 */
	public double[] getB() {
		return resultB;
	}

	public BurstEngine(VariableNumber v, int n, InputPanel IP) {
		resultA = new double[n];
		resultB = new double[n];
		ArrPlus = new double[n];
		ArrPlusCount = new double[n];
		ArrMinusCount = new double[n];
		lambdaplus = new double[n];
		lambda_k = new double[n];
		this.v = v;
		this.T = (long) v.getValue(v.Size() - 1) - (long) v.getValue(0);
		lambda = v.Size() / (T / 1000);
		this.n = n;
		EpochDuration = new double[n];
		calculate();
	}

	/**
	 * Called by the constructor to start the engine and calculate the values for all the epochs n
	 */
	private void calculate() {
		// <--- Inserire finestra di uodate ?
		epochDuration(T, n);
		for (int i = 1; i <= n; i++) {
			startEng(i);
		}
	}

	/**
	 * Calculate the duration of the epochs
	 * @param T is the time interval during which the requests of the http log arrive at the web server
	 * @param n is the number oh epochs
	 */
	private void epochDuration(double T, int n) {
		for (int i = 1; i <= n; i++) {
			EpochDuration[i - 1] = T / i;
		}
	}

	/**
	 * The engine
	 * @paraam k is the number of epochs
	 */
	private void startEng(int k) {
		int ArrPlusCount = 0, ArrPlus = 0;
		int ArrMinusCount = 0, ArrMinus = 0;
		double limit;
		double lambda_k[] = new double[k];
		int Arr[] = new int[v.Size()];
		int j = 0;
		for (int i = 0; i < k; i++) {
			limit = ((long) v.getValue(0)) + (EpochDuration[k - 1] * (i + 1));
			for (Arr[i] = 0; (j < v.Size() && ((long) v.getValue(j) <= limit)); j++) {
				Arr[i]++;
			}
			lambda_k[i] = (k * Arr[i]) / (T / 1000);
			//setLambda_k(lambda_k[i],k);
			if (lambda_k[i] > lambda) {
				ArrPlus++;
				ArrPlusCount += Arr[i];
			}
			if (lambda_k[i] <= lambda) {
				ArrMinus++;
				ArrMinusCount += Arr[i];
			}
		}

		b = (double) (ArrPlus) / k;
		setB(b, k);
		setArrPlus(ArrPlus, k);
		setArrPlusCount(ArrPlusCount, k);
		setArrMinusCount(ArrMinusCount, k);
		setLambdaPlus(ArrPlusCount, b, k);
		if (b != 0) {
			a = (ArrPlusCount) / (b * v.Size());
		} else {
			a = 0;
		}
		setA(a, k);

	}

	/**
	 * Set the number of the http request that arrive in epoch k
	 * @param ArrPlus is the number of the http request that arrive in epoch k
	 * @param k is the number of epochs
	 */
	private void setArrPlus(int ArrPlus, int k) {
		// TODO Auto-generated method stub
		this.ArrPlus[k - 1] = ArrPlus;
	}

	/**
	 * Get the number of the http request that arrive in epoch k
	 */
	public double[] getArrPlus() {
		return ArrPlus;
	}

	/**
	 * Set the total number of requests that arrive in epohs in which the epoch arrival rate exceeds the average arrival rate observed in the log
	 * @param ArrPlusCount is the total number of requests that arrive in epohs in which the epoch arrival rate exceeds the average arrival rate observed in the log
	 * @param k is the epoch number
	 */
	private void setArrPlusCount(int ArrPlusCount, int k) {
		// TODO Auto-generated method stub
		this.ArrPlusCount[k - 1] = ArrPlusCount;
	}

	/**
	 * Get the total number of requests that arrive in epohs in which the epoch arrival rate exceeds the average arrival rate observed in the log
	 */
	public double[] getArrPlusCount() {
		return ArrPlusCount;
	}

	/**
	 * Set the above-average arrival rate
	 * @param ArrPlusCount is the total number of requests that arrive in epohs in which the epoch arrival rate exceeds the average arrival rate observed in the log
	 * @param b is the burstiness factor
	 * @param k is the epoch number
	 */
	private void setLambdaPlus(double ArrPlusCount, double b, int k) {
		// TODO Auto-generated method stub
		this.lambdaplus[k - 1] = ArrPlusCount / (b * (T / 1000));
	}

	/**
	 * Get the above-average arrival rate
	 */
	public double[] getLambdaPlus() {
		return lambdaplus;
	}

	/**
	 * Set the total number of requests that arrive in epohs in which the epoch arrival rate does not exceeds the average arrival rate observed in the log
	 * @param ArrPlusCount is the total number of requests that arrive in epohs in which the epoch arrival rate does not exceeds the average arrival rate observed in the log
	 * @param k is the epoch number
	 */
	private void setArrMinusCount(int ArrMinusCount, int k) {
		// TODO Auto-generated method stub
		this.ArrMinusCount[k - 1] = ArrMinusCount;
	}

	/**
	 * Get the total number of requests that arrive in epohs in which the epoch arrival rate does not exceeds the average arrival rate observed in the log
	 */
	public double[] getArrMinusCount() {
		return ArrMinusCount;
	}

	/**
	 * Get the arrival rate
	 */
	public double[] getLambda() {
		double l[] = new double[1];
		l[0] = lambda;
		return l;
	}

	/** TEMPORANEO **/
	public double[] getData(String s) {
		if (s == "ArrPlus") {
			return getArrPlus();
		}
		if (s == "b") {
			return getB();
		}
		if (s == "ArrPlusCount") {
			return getArrPlusCount();
		}
		if (s == "lambdaPlus") {
			return getLambdaPlus();
		}
		if (s == "ArrMinusCount") {
			return getArrMinusCount();
		}
		if (s == "lambda") {
			return getLambda();
		} else {
			return getA();
		}
	}

	public int getEpochRange() {
		return n;
	}
}
