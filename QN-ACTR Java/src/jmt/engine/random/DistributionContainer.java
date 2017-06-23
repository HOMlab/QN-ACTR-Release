package jmt.engine.random;

/**
 * <p>Title:Distribution Container</p>
 * <p>Description: Datastructure that stores a nested distribution</p>
 * @author Peter Parapatics
 *			Date: 17-dec-1007
 */
public class DistributionContainer {
	protected Distribution distribution;
	protected Parameter parameter;

	/**
	 * Constructs a Distribution Container with a given distribution and a parameter
	 * @param distribution the distribution to be stored
	 * @param parameter the parameter of the distribution
	 */
	public DistributionContainer(Distribution distribution, Parameter parameter) {
		this.distribution = distribution;
		this.parameter = parameter;
	}

	/**
	 * Get method for the distribution
	 * @return the distribution
	 */
	public Distribution getDistribution() {
		return distribution;
	}

	/**
	 * Get method for the distribution parameter
	 * @return the parameter belonging to the distribution
	 */
	public Parameter getParameter() {
		return parameter;
	}

}
