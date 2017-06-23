package jmt.engine.random;

/**
 * 
 * This is the parameter that should be passed to the Burst distribution.
 * 
 * <br>
 * <br>
 * Copyright (c) 2003 <br>
 * Politecnico di Milano - dipartimento di Elettronica e Informazione
 * 
 * @author Federico Dal Castello
 * 
 */
public class BurstPar extends AbstractParameter implements Parameter {

	/** parameter of value distribution A */
	private Parameter valueParameterA;

	/** parameter of value distribution B */
	private Parameter valueParameterB;

	/** value distribution A */
	private Distribution valueDistributionA;

	/** value distribution A */
	private Distribution valueDistributionB;

	/**
	 * It creates a new burst parameter. It accepts two <tt>DistributionContainer</tt>
	 * containing the value distribution A and its parameter, and the value
	 * distribution B and its parameter, respectively.
	 * 
	 * @param contA the <tt>DistributionContainer</tt> containing the
	 * value distribution A and its parameter
	 * @param contB the <tt>DistributionContainer</tt> containing the
	 * value distribution B and its parameter
	 */
	public BurstPar(DistributionContainer contA, DistributionContainer contB) {
		this.valueDistributionA = contA.getDistribution();
		this.valueParameterA = contA.getParameter();
		this.valueDistributionB = contB.getDistribution();
		this.valueParameterB = contB.getParameter();
	}

	/**
	 * Returns the value distribution A. 
	 * 
	 * @return the value distribution A
	 */
	public Distribution getValueDistributionA() {
		return valueDistributionA;
	}

	/**
	 * Returns the parameter of value distribution A. 
	 * 
	 * @return the parameter of value distribution A
	 */
	public Parameter getValueParameterA() {
		return valueParameterA;
	}

	/**
	 * Returns the value distribution B.
	 * 
	 * @return the value distribution B
	 */
	public Distribution getValueDistributionB() {
		return valueDistributionB;
	}

	/**
	 * Returns the parameter of value distribution B. 
	 * 
	 * @return the parameter of value distribution B
	 */
	public Parameter getValueParameterB() {
		return valueParameterB;
	}

	/**
	 * Sets the the value distribution A.
	 * 
	 * @param distributionA the value distribution A
	 */
	public void setValueDistributionA(Distribution distributionA) {
		this.valueDistributionA = distributionA;
	}

	/**
	 * Sets the the parameter of value distribution A.
	 * 
	 * @param parameterA the parameter of value distribution A
	 */
	public void setValueParameterA(Parameter parameterA) {
		this.valueParameterA = parameterA;
	}

	/**
	 * Sets the the value distribution B.
	 * 
	 * @param distributionA the value distribution B
	 */
	public void setValueDistributionB(Distribution distributionB) {
		this.valueDistributionB = distributionB;
	}

	/**
	 * Sets the the parameter of value distribution B.
	 * 
	 * @param parameterA the parameter of value distribution B
	 */
	public void setValueParameterB(Parameter parameterB) {
		this.valueParameterB = parameterB;
	}

} // end BurstPar
