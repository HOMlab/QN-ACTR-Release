/**
 * 
 */
package jmt.jmarkov.Queues;

import jmt.jmarkov.Queues.Exceptions.NonErgodicException;

/**
 * @author arifc
 *
 */
public class MMNdLogic extends MM1Logic {

	private int numberOfServer;
	private int max;

	/**
	 * @param lambda
	 * @param s
	 */
	public MMNdLogic(double lambda, double s, int aNumberOfServer, int amax) {
		super(lambda, s);
		numberOfServer = aNumberOfServer;
		this.max = amax;
	}

	private double p() {
		return lambda * s / numberOfServer;
	}

	@Override
	public double getStatusProbability(int status) throws NonErgodicException {
		double result;
		if (status <= numberOfServer) {
			result = pzero() * Math.pow(numberOfServer * p(), status) / factorial(status);
		} else if (status <= numberOfServer + max) {
			result = pzero() * Math.pow(numberOfServer, numberOfServer) * Math.pow(p(), status) / factorial(numberOfServer);
		} else {
			result = 0.0;
		}
		return result;
	}

	private double pzero() throws NonErgodicException { // pzero = (u+v) to the -1
		return 1 / (u() + v());
	}

	private double u() throws NonErgodicException { //it is not utilization it is: sum from j=0 to m-1 ( (mp) to the j / fact(j))
		double result = 0;
		double mp = p() * numberOfServer;
		double mpToN = 1;// for to the 0
		double factorialN = 1;// for 0

		//base case 0:
		result += mpToN / factorialN;

		for (int n = 1; n < numberOfServer; n++) {
			mpToN *= mp;
			factorialN *= n;
			result += mpToN / factorialN;
		}
		return result;
	}

	private double v() throws NonErgodicException {// m to m / m! * ((p)to the m) * (1- (p to the k+1)) /(1-p) 
		double p = p();
		return Math.pow(numberOfServer, numberOfServer) / factorial(numberOfServer) * (Math.pow(p, numberOfServer)) * (1 - Math.pow(p, (max + 1)))
				/ (1 - p);

	}

	//	private double Pq() throws NonErgodicException{//The probability Pq that upon an arrival all servers are busy and the customer has to wait is
	//		return v()/(u()+v()); //??
	//	}

	@Override
	public double mediaJobs() throws NonErgodicException { //sum from j=0 to m+k : j*pi(j)
		//in another words: pi0 * ( (sum j=1 to m-1 : j * (mp)to the j / j! ) +  ( m to the m / m! * (sum j=m to m+k : j * p to the j) ) )  
		double result1 = 0;
		double result2 = 0;
		double p = p();
		double mp = numberOfServer * p;
		double factj = 1;

		for (int j = 1; j < numberOfServer; j++) {
			factj *= j;
			result1 += j * Math.pow(mp, j) / factj;
		}

		for (int j = numberOfServer; j <= numberOfServer + max; j++) {
			result2 += j * Math.pow(p, j);
		}
		result2 *= Math.pow(numberOfServer, numberOfServer) / factorial(numberOfServer);

		return pzero() * (result1 + result2);
	}

	/**
	 * Calculate the utilizaion of the server
	 * with respect to parameters lambda ,s and queue length and number of cpu 
	 * 
	 * @return utilization
	 * 
	 * @exception NonErgodicException queue is not ergodic (U > 1 o U < 0)
	 */
	@Override
	public double utilization() throws NonErgodicException {
		return numberOfServer * p() * (1 - getStatusProbability(numberOfServer + max));
	}

	/* (non-Javadoc)
	 * @see Queues.QueueLogic#throughput()
	 */
	@Override
	public double throughput() throws NonErgodicException {
		return lambda * (1 - getStatusProbability(numberOfServer + max));
	}

	/* (non-Javadoc)
	 * @see Queues.QueueLogic#responseTime()
	 */
	@Override
	public double responseTime() throws NonErgodicException {
		return mediaJobs() / throughput();
	}

	@Override
	public int getNumberServer() {
		return numberOfServer;
	}

	private double factorial(int n) {
		double result = 1;
		for (int i = 1; i <= n; i++) {
			result *= i;
		}

		return result;
	}

	/* (non-Javadoc)
	 * @see Queues.QueueLogic#getMaxStates()
	 */
	@Override
	public int getMaxStates() {
		return max;
	}

	@Override
	public void setMaxStates(int max) {
		this.max = max;
	}

}
