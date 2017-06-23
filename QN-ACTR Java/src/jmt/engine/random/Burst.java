package jmt.engine.random;

import jmt.common.exception.IncorrectDistributionParameterException;
import jmt.common.exception.NetException;
import jmt.engine.QueueNet.NetEvent;
import jmt.engine.QueueNet.NetMessage;
import jmt.engine.QueueNet.NetNode;
import jmt.engine.random.engine.RandomEngine;
import jmt.engine.simEngine.SimSystem;

/**
 * 
 * This is the Burst distribution.
 * 
 * <br>
 * <br>
 * Copyright (c) 2003 <br>
 * Politecnico di Milano - dipartimento di Elettronica e Informazione
 * 
 * @author Federico Dal Castello
 * 
 */
public class Burst extends NetNode implements Distribution {

	/** static counter to ensure that two distribution will not have the same name */
	private static int count = 0;
	/** if <tt>true</tt> the next EVENT_DISTRIBUTION_CHANGE message will be discarded */
	boolean discardNextMessage = false;

	/** 
	 * Probability of having events of type A. The probability of having events
	 * of type B is (1 - probability).
	 */
	private Double probability;
	/** length distribution A */
	private Distribution lenghtDistrA;
	/** length distribution B */
	private Distribution lenghtDistrB;
	/** parameter of length distribution A */
	private Parameter lengthParamA;
	/** parameter of length distribution B */
	private Parameter lengthParamB;
	/**
	 * If <tt>true</tt>, the behavior of the interval change is set to Round-Robin;
	 * if <tt>false</tt> the interval type is changed according to the probability parameter.
	 */
	private Boolean isRoundRobin;

	/** absolute system time of the interval end */
	private double intervalEnd;

	/** the distribution selected and active at this time */
	private Distribution currentLengthDistr;
	/** the parameter of the distribution selected and active at this time */
	private Parameter currentLengthPar;

	/** Represents the random generator of uniformal distributed 32 bits numbers */
	private RandomEngine engine;

	/**
	 * Creates a new Burst distribution with the specified probability of having
	 * events of type A. <tt>distrContA</tt> is the DistributionContainer containing
	 * the length distribution A and the related parameters, while <tt>distrContB</tt>
	 * is the same of <tt>distrContA</tt> but for distribution B.
	 * 
	 * @param probability the probability of having events of type A
	 * @param distrContA the DistributionContainer containing the length
	 * distribution A and the related parameter
	 * @param distrContB the DistributionContainer containing the length
	 * distribution A and the related parameter
	 * @param isRoundRobin if <tt>true</tt>, the behavior of the interval change is
	 * set to Round-Robin. That is, there is a pure alternation of the interval types,
	 * starting with interval A: A-B-A-B-A-B-A-B... In this case, the
	 * probability parameter is ignored. If <tt>false</tt>, the interval type is
	 * changed according to the probability parameter
	 * @throws IncorrectDistributionParameterException if the probability
	 * is not a value comprised between 0 and 1
	 */
	public Burst(Double probability, DistributionContainer distrContA, DistributionContainer distrContB, Boolean isRoundRobin)
			throws IncorrectDistributionParameterException {

		super("@@JSIM:RESERVED@@ Burst Distribution " + count++);

		//TODO also check if the length distributions parameter are correct? 
		if (!check(probability)) {
			throw new IncorrectDistributionParameterException("The probability must be comprised between 0 and 1.");
		}

		engine = RandomEngine.makeDefault();
		this.probability = probability;
		this.lenghtDistrA = distrContA.getDistribution();
		this.lengthParamA = distrContA.getParameter();
		this.lenghtDistrB = distrContB.getDistribution();
		this.lengthParamB = distrContB.getParameter();
		this.isRoundRobin = isRoundRobin;

		// sets the starting distribution and the starting interval end
		intervalEnd = changeInterval();
		sendMe(intervalEnd, NetEvent.EVENT_DISTRIBUTION_CHANGE);
	}

	/**
	 * Creates a new non-Round-Robin Burst distribution with the specified probability
	 * of having events of type A. <tt>distrContA</tt> is the DistributionContainer
	 * containing the length distribution A and the related parameters, while
	 * <tt>distrContB</tt> is the same of <tt>distrContA</tt> but for distribution B.
	 * 
	 * @param probability the probability of having events of type A
	 * @param distrContA the DistributionContainer containing the length
	 * distribution A and the related parameter
	 * @param distrContB the DistributionContainer containing the length
	 * distribution A and the related parameter
	 * @throws IncorrectDistributionParameterException if the probability
	 * is not a value comprised between 0 and 1
	 */
	public Burst(Double probability, DistributionContainer distrContA, DistributionContainer distrContB)
			throws IncorrectDistributionParameterException {
		this(probability, distrContA, distrContB, new Boolean(false));
	}

	/**
	 * Verifies if the parameters are correct. For the burst distribution
	 * the parameters are correct if the value of the probability is comprised
	 * between 0 and 1.
	 *
	 * @return <tt>true</tt> if the probability is correct; <tt>false</tt> otherwise
	 *
	 */
	public boolean check(Double probability) {
		double prob = probability.doubleValue();

		if (prob < 0 || prob > 1) {
			return false;
		} else {
			return true;
		}
	}

	/** 
	 * Changes the interval and returns the new interval length.
	 * 
	 * @return the new interval length of the selected length distribution
	 * @throws IncorrectDistributionParameterException if the length
	 * distributions parameter are not correct
	 */
	private double changeInterval() throws IncorrectDistributionParameterException {

		if (isRoundRobin.booleanValue()) {
			// alternates the interval distributions, starting with distribution A
			if ((currentLengthDistr == null) || // the length distribution has not been initialized yet
					currentLengthDistr.equals(lenghtDistrB)) {
				// sets it to length distribution A
				currentLengthDistr = lenghtDistrA;
				currentLengthPar = lengthParamA;
			} else {
				// sets it to length distribution B
				currentLengthDistr = lenghtDistrB;
				currentLengthPar = lengthParamB;
			}
		} else {
			// changes the interval distributions according to the probability parameter
			if (engine.raw() < probability.doubleValue()) {
				currentLengthDistr = lenghtDistrA;
				currentLengthPar = lengthParamA;
			} else {
				currentLengthDistr = lenghtDistrB;
				currentLengthPar = lengthParamB;
			}
		}

		return currentLengthDistr.nextRand(currentLengthPar);
	}

	/**
	 * @return always 0 because in this type of distribution it has no sense
	 * to request the cdf of the distribution.
	 * 
	 */
	public double cdf(double x, Parameter p) throws IncorrectDistributionParameterException {
		return 0.0;
	}

	/**
	 * @return always 0 because in this type of distribution it has no sense
	 * to request the pdf of the distribution.
	 * 
	 */
	public double pdf(double x, Parameter p) throws IncorrectDistributionParameterException {
		return 0.0;
	}

	/**
	 * @return always 0 because in this type of distribution it has no sense
	 * to request the mean of the distribution.
	 * 
	 */
	public double theorMean(Parameter p) throws IncorrectDistributionParameterException {
		return 0.0;
	}

	/**
	 * @return always 0 because in this type of distribution it has no sense
	 * to request the variance of the distribution.
	 * 
	 */
	public double theorVariance(Parameter p) throws IncorrectDistributionParameterException {
		return 0.0;
	}

	/**
	 * Returns the new random number.
	 * This method is used to obtain from the distribution the next number distributed
	 * according to the distribution parameter and the actual interval (A or B).
	 *
	 * @param p parameter of the burst distribution
	 * @throws IncorrectDistributionParameterException if the value distributions
	 * parameter are not correct
	 * @return double with the next random number of this distribution, according
	 * to the actual interval (A or B)
	 */
	public double nextRand(Parameter p) throws IncorrectDistributionParameterException {

		// we suppose that the next message will not be discarded
		discardNextMessage = false;

		// the sum of the "jumped" intervals remaining times; 0 if the first event is in the current interval
		double offset = 0;
		// value returned by the current value distribution
		double value = getCurrentValueDistrNextRand(p);
		// remaining time of the current interval
		double remainingTime = intervalEnd - SimSystem.getClock();
		if (remainingTime < 0) { // for debug purpose
			throw new RuntimeException("remainingTime < 0");
		}

		/*
		 * While the current value is larger than the remaining time of the current interval:
		 * - the next arriving message must be discarded because it will be set a new interval
		 * - the remaining time of the "jumped" interval has to be added to the offset
		 * - change the interval and save the returned interval length as the new remaining time
		 * - compute the new absolute interval end by adding the remaining time and the offset (remainingTime + offset)
		 * - obtain a new value from the current value distribution (the value distribution of the NEW interval)
		 */
		while (value > remainingTime) {
			// The interval is surpassed: we have to discard the next
			// EVENT_DISTRIBUTION_CHANGE message
			discardNextMessage = true;

			offset += remainingTime;
			remainingTime = changeInterval();
			intervalEnd = remainingTime + offset + SimSystem.getClock();

			value = getCurrentValueDistrNextRand(p);
		}

		if (discardNextMessage == true) {
			// send the new EVENT_DISTRIBUTION_CHANGE message
			sendMe(offset + remainingTime, NetEvent.EVENT_DISTRIBUTION_CHANGE);
		}

		// sums the random value returned by the value distribution to the offset
		return offset + value;
	}

	/** 
	 * Returns the current value distribution new random number.
	 * 
	 * @param p parameter of the burst distribution
	 * @return the current value distribution new random number
	 * @throws IncorrectDistributionParameterException if the value distributions
	 * parameter are not correct
	 */
	private double getCurrentValueDistrNextRand(Parameter p) throws IncorrectDistributionParameterException {

		BurstPar par = (BurstPar) p;
		Distribution currentValueDistr;
		Parameter currentValPar;

		if (currentLengthDistr.equals(lenghtDistrA)) {
			// the current distribution is distribution A
			currentValueDistr = par.getValueDistributionA();
			currentValPar = par.getValueParameterA();
		} else {
			// the current distribution is distribution B
			currentValueDistr = par.getValueDistributionB();
			currentValPar = par.getValueParameterB();
		}

		return currentValueDistr.nextRand(currentValPar);
	}

	/**
	 * Send an event to this entity with no data, specifying the delivery delay
	 * and the event type.
	 * 
	 * @param delay how long from the current simulation time the event should be sent
	 * @param eventType an user-defined number representing the type of event
	 */
	protected void sendMe(double delay, int eventType) {
		simSchedule(this.getId(), delay, eventType);
	}

	/**
	 * Message dispatcher for Burst distribution. When an EVENT_DISTRIBUTION_CHANGE
	 * message arrives, it chooses the new interval type based on the probability
	 * and sends to itself a new EVENT_DISTRIBUTION_CHANGE message with a delay
	 * generated by the chosen interval. Otherwise it discards the message, if
	 * it is set to discard.
	 */
	@Override
	protected void dispatch(NetMessage message) throws NetException {

		int eventType = message.getEvent();

		/*
		 * If we have to discard the next message: do nothing, that is return
		 * without sending the new EVENT_DISTRIBUTION_CHANGE message.
		 * 
		 * The second condition is necessary to overcome the simulator behavior,
		 * which processes method calls before messages: if there is a surpass
		 * of an interval length at the simulation time 0, it is set to discard
		 * the next message, but since messages are processed after method calls,
		 * it will imply to discard the message arriving at the instant 0
		 * (automatically triggered by an EVENT_START event), which has no sense.
		 * We want to discard messages only if they are not arrived at instant 0.
		 * 
		 */
		if (discardNextMessage == true && Double.compare(SimSystem.getClock(), 0.0) != 0) {
			discardNextMessage = false;
			return;
		}

		try {

			if (eventType == NetEvent.EVENT_DISTRIBUTION_CHANGE) {
				/* 
				 * Change the interval and return the new interval length,
				 * which will be added to the absolute system time of the 
				 * interval end, and auto send an EVENT_DISTRIBUTION_CHANGE
				 * message to indicate the next distribution change
				 */
				double delay = changeInterval();
				intervalEnd += delay;
				sendMe(delay, NetEvent.EVENT_DISTRIBUTION_CHANGE);

			} else if (eventType == NetEvent.EVENT_START) {
				// do nothing, because the switch of distributions is activated
				// in the constructor
			}

		} catch (IncorrectDistributionParameterException idpe) {
			// re-thrown as NetException to not modify the method declaration
			throw new NetException("Incorrect distribution parameter: " + idpe.getMessage());
		}

	}

}
