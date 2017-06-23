package jmt.gui.common.distributions;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Locale;

import javax.swing.ImageIcon;

import jmt.gui.common.resources.JMTImageLoader;

/**
 * <p>Title: Burst Distribution</p>
 * <p>Description: Burst distribution data structure</p>
 * 
 * @author Peter Parapatics
 *         Date: 12-dic-2007
 *         
 */
public class Burst extends Distribution {

	/**
	 * constructs a new Burst distribution
	 *
	 */
	public Burst() {
		super("Burst (General)", "jmt.engine.random.Burst", "jmt.engine.random.BurstPar", "Burst definition");
		hasC = false;
		hasMean = false;
		isNestable = false;
		//Add a value checker to check if every distribution has been specified
		this.setValueChecker(new ValueChecker() {

			public boolean checkValue(Object value) {
				Burst b = (Burst) value;

				for (int i = 0; i < 6; i++) {
					if (b.getParameter(i).getValue() == null) {
						return false;
					}
				}
				return true;
			}

		});
	}

	/**
	 * Set illustrating figure in distribution panel
	 * user to understand meaning of parameters.
	 * @return illustrating figure
	 */
	@Override
	protected ImageIcon setImage() {
		ImageIcon icon = JMTImageLoader.loadImage("Burst");

		return icon;
	}

	/**
	 * Used to set parameters of this distribution.
	 * @return distribution parameters
	 */
	@Override
	protected Parameter[] setParameters() {
		// Creates parameter array
		Parameter[] parameters = new Parameter[6];

		//Set the probability parameter
		//This parameter has to be added directly to the Distribution in the XML
		parameters[0] = new Parameter("Probability", "Probability", Double.class, new Double(0.5), true);

		//set the interval length distribution of interval A
		//This parameter has to be added directly to the Distribution in the XML
		parameters[1] = new Parameter("Length-Distribution_IntervalA", "Interval A - Length Distribution", Distribution.class, null, true);

		//Set the value distribution of interval A
		parameters[2] = new Parameter("Value-Distribution_IntervalA", "Interval A - Value Distribution", Distribution.class, null);

		//set the interval length distribution of interval B
		//This parameter has to be added directly to the Distribution in the XML
		parameters[3] = new Parameter("Length-Distribution_IntervalB", "Interval B - Length Distribution", Distribution.class, null, true);

		//Set the value distribution of interval B
		parameters[4] = new Parameter("Value-Distribution_IntervalB", "Interval B - Value Distribution", Distribution.class, null);

		//Set the round-robin parameter
		//This parameter has to be added directly to the Distribution in the XML
		parameters[5] = new Parameter("Round-Robin", "Round-Robin", Boolean.class, new Boolean(false), true);

		return parameters;
	}

	/**
	 * Returns precondition that parameters' values must satisfy for this distribution to be valid
	 * @return Message describing distribution's preconditions
	 */
	@Override
	public String getPrecondition() {
		return "All values and interval-length distributions have to be defined";
	}

	@Override
	public boolean isNestable() {
		return isNestable;
	}

	/**
	 * Returns this distribution's short description
	 * @return distribution's short description
	 */
	@Override
	public String toString() {
		DecimalFormat df = new DecimalFormat("#.############");
		df.setDecimalFormatSymbols(new DecimalFormatSymbols(Locale.ENGLISH));
		Double probability = (Double) parameters[0].getValue();

		//		double revProp = 1-probability.doubleValue();
		return "burst" + "(" + df.format(probability) + "," + parameters[2].getValue() + "," + parameters[4].getValue() + ")";
	}

}
