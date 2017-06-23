package jmt.engine.jwat.workloadAnalysis.utils;

/**
 * 
 * @author Brambilla Davide matr 667986, Fumagalli Claudio 667971
 */
public interface SetMatrixListener {
	/**
	 * This event is fired everytime the observation matrix is reset
	 */
	public void onSetMatrixObservation();

	public void onResetMatrixObservation();
}
