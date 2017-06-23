/**    
  * Copyright (C) 2006, Laboratorio di Valutazione delle Prestazioni - Politecnico di Milano

  * This program is free software; you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation; either version 2 of the License, or
  * (at your option) any later version.

  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.

  * You should have received a copy of the GNU General Public License
  * along with this program; if not, write to the Free Software
  * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
  */

package jmt.gui.common.distributions;

import java.lang.reflect.Field;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.ArrayList;
import java.util.Locale;

import javax.swing.ImageIcon;

import jmt.gui.common.serviceStrategies.ServiceStrategy;

/**
 * <p>Title: Distribution</p>
 * <p>Description: This abstract class provides a generic pattern used to specify a
 * distribution.</p>
 * 
 * @author Bertoli Marco
 *         Date: 25-giu-2005
 *         Time: 14.47.12
 */
public abstract class Distribution implements ServiceStrategy {
	protected static Distribution[] all = null; // Used to store all distributions
	protected static Distribution[] allWithMean = null; // Used to store all distributions with Mean value adjustable
	protected static Distribution[] nestableDistributions = null; // Used to store nestable distributions
	protected String classpath;
	protected String name;
	protected String parameterClasspath;
	protected Parameter[] parameters;
	protected String description;
	protected ImageIcon image;
	protected ValueChecker checker;
	protected boolean hasMean, hasC; // Used to provide input parameters with the tuple (mean, C)
	protected double c, mean;
	protected boolean isNestable;

	/**
	 * Constructs a new Distribution object. Initialize all internal objects calling abstract
	 * methods setParameterNames(), setParameterClasses(), setParameterValues().
	 * @param name Name of the distribution
	 * @param classpath engine classpath for this distribution
	 * @param parameterClasspath ngine's classpath for this distribution's parameters
	 * @param description description of this distribution
	 */
	public Distribution(String name, String classpath, String parameterClasspath, String description) {
		this.classpath = classpath;
		this.name = name;
		this.parameterClasspath = parameterClasspath;
		this.description = description;
		this.parameters = setParameters();
		this.image = setImage();
		updateCM();
	}

	/**
	 * Gets engine's classpath for this distribution
	 * @return classpath
	 */
	public String getClassPath() {
		return classpath;
	}

	/**
	 * Get engine's classpath for this distribution's parameters (This is needed as each
	 * distribution in the engine has a Distribution object and a Parameter object which
	 * are distinct classes)
	 * @return parameter's classpath
	 */
	public String getParameterClassPath() {
		return parameterClasspath;
	}

	/**
	 * Gets this distribution's name
	 * @return distribution's name
	 */
	public String getName() {
		return name;
	}

	/**
	 * Get's this distribution description
	 * @return distribution's description
	 */
	public String getDescription() {
		return description;
	}

	/**
	 * Returns number of parameters for this distribution
	 * @return number of parameters
	 */
	public int getNumberOfParameters() {
		return parameters.length;
	}

	/**
	 * Get a parameter data structure of this distribution, given its index
	 * @param num index of requested parameter
	 * @return requested parameter
	 * @throws java.lang.ArrayIndexOutOfBoundsException if num is not inside bounds
	 */
	public Parameter getParameter(int num) {
		return parameters[num];
	}

	/**
	 * Gets a parameter data structure of this distribution, given its name
	 * @param name name of parameter to be retrived
	 * @return requested parameter or null if a parameter with specified name does not exist
	 */
	public Parameter getParameter(String name) {
		for (Parameter parameter : parameters) {
			if (parameter.getName().equals(name)) {
				return parameter;
			}
		}
		return null;
	}

	/**
	 * Gets explicative image of this distribution used, together with description, to help the
	 * user to understand meaning of parameters.
	 * @return explicative image
	 */
	public ImageIcon getImage() {
		return image;
	}

	/**
	 * Checks if all parameters' values are correct for this distribution type
	 * @return true if no check has to be performed or parameters are correct, false otherwise.
	 */
	public boolean checkValue() {
		if (checker != null) {
			return checker.checkValue(this);
		} else {
			return true;
		}
	}

	/**
	 * Returns precondition that parameters' values must satisfy for this distribution to be valid
	 * Every Distribution that provides a checker must provide a valid precondition message too.
	 * @return Message describing distribution's preconditions
	 */
	public String getPrecondition() {
		return "none";
	}

	/**
	 * Returns a deep copy of this Distribution
	 * @return a clone of this distribution
	 */
	@Override
	public Distribution clone() {
		// Gets subtype of this class to instantiate new object through reflection
		Class<? extends Distribution> instance = this.getClass();
		Distribution tmp = null;
		try {
			tmp = instance.newInstance();
			// Newly created instance will have default parameters. Now will clone parameters
			// of this and sets them as parameters of new instance.
			Parameter[] parameters = new Parameter[this.parameters.length];
			for (int i = 0; i < this.parameters.length; i++) {
				parameters[i] = (Parameter) this.parameters[i].clone();
			}
			tmp.parameters = parameters;
			tmp.mean = mean;
			tmp.c = c;
		} catch (IllegalAccessException e) {
			System.err.println("Error: Cannot clone Distribution object: cannot access to correct class");
			e.printStackTrace();
		} catch (InstantiationException e) {
			System.err.println("Error: Cannot clone Distribution object: instantiation problem during reflection");
			e.printStackTrace();
		}
		return tmp;
	}

	/**
	 * Returns if this distribution can be initialized providing variation coefficient C
	 * @return true iff this distribution can be initialized using variation coefficient C
	 */
	public boolean hasC() {
		return hasC;
	}

	/**
	 * Returns if this distribution can be initialized providing its mean value
	 * @return true iff this distribution can be initialized using its mean value
	 */
	public boolean hasMean() {
		return hasMean;
	}

	/**
	 * Returns distribution variation coefficient C only if <code>hasC()</code> is true
	 * @return variation coefficient C
	 */
	public double getC() {
		return c;
	}

	/**
	 * Returns distribution mean only if <code>hasMean()</code> is true
	 * @return distribution Mean
	 */
	public double getMean() {
		return mean;
	}

	/**
	 * Sets a value checker for this entire distribution (checks intra-parameters preconditions)
	 * @param checker checker to be set to this distribution
	 */
	protected void setValueChecker(ValueChecker checker) {
		this.checker = checker;
	}

	/**
	 * returns whether a distribution can be used as nested distribution inside another distribution
	 * @return true if it can be nested. false otherwise
	 */
	public boolean isNestable() {
		return isNestable;
	}

	/**
	 * Helper method used to formats given number into string according to default rules.
	 * @param d bouble to be converted
	 * @return string representation of given number
	 */
	protected String FormatNumber(double d) {
		DecimalFormat nf = new DecimalFormat();
		// Puts '.' as decimal separator.
		nf.setDecimalFormatSymbols(new DecimalFormatSymbols(Locale.ENGLISH));
		String ret;
		// If module of number is greater than 1e3 or lesser than 1e-3 uses exponential notation
		if (Math.abs(d) >= 1e-3 && Math.abs(d) <= 1e3 || d == 0) {
			nf.applyPattern("#.###");
			ret = nf.format(d);
			if (ret.length() > 7) {
				ret = ret.substring(0, 6);
			}
		} else {
			nf.applyPattern("0.00E00");
			ret = nf.format(d);
		}
		return ret;
	}

	/**
	 * Return an array with an istance of every allowed Distribution. Uses internal
	 * caching to search for distributions only the first time that this method is called.
	 * @return an array with an istance of every allowed Distribution
	 */
	public static Distribution[] findAll() {
		if (all != null) {
			return all;
		}
		String path = "jmt.gui.common.distributions.";

		Field[] fields = jmt.gui.jmodel.JMODELConstants.class.getFields();
		ArrayList<Distribution> tmp = new ArrayList<Distribution>();
		try {
			for (Field field : fields) {
				if (field.getName().startsWith("DISTRIBUTION_")) {
					tmp.add((Distribution) Class.forName(path + (String) field.get(null)).newInstance());
				}
			}
		} catch (IllegalAccessException ex) {
			System.err.println("A security manager has blocked reflection");
			ex.printStackTrace();
		} catch (ClassNotFoundException e) {
			System.err.println("Reflection Error: cannot access to distribution's implementation");
			e.printStackTrace();
		} catch (InstantiationException e) {
			System.err.println("Reflection Error: Cannot instantiate a distribution");
			e.printStackTrace();
		}
		Distribution[] ret = new Distribution[tmp.size()];
		for (int i = 0; i < tmp.size(); i++) {
			ret[i] = tmp.get(i);
		}
		all = ret;
		return ret;
	}

	/**
	 * Return an array with an istance of every Distribution that allows mean setting. Uses internal
	 * caching to search for distributions only the first time that this method is called.
	 * @return an array with an istance of every Distribution in which mean value can be specified
	 */
	public static Distribution[] findAllWithMean() {
		if (allWithMean != null) {
			return allWithMean;
		}
		Distribution[] all = findAll();
		Distribution[] tmp = new Distribution[all.length];
		int n = 0;
		for (Distribution element : all) {
			if (element.hasMean()) {
				tmp[n++] = element;
			}
		}
		// Now removes empty elements into tmp array
		allWithMean = new Distribution[n];
		for (int i = 0; i < n; i++) {
			allWithMean[i] = tmp[i];
		}
		return allWithMean;
	}

	/**
	 * Return an array with an istance of every nestable Distribution. Uses internal
	 * caching to search for distributions only the first time that this method is called.
	 * @return an array with an istance of every nestable Distribution
	 */
	public static Distribution[] findNestableDistributions() {
		if (nestableDistributions != null) {
			return nestableDistributions;
		}
		Distribution[] all = findAll();
		Distribution[] tmp = new Distribution[all.length];
		int n = 0;
		for (Distribution element : all) {
			if (element.isNestable()) {
				tmp[n++] = element;
			}
		}
		// Now removes empty elements into tmp array
		nestableDistributions = new Distribution[n];
		for (int i = 0; i < n; i++) {
			nestableDistributions[i] = tmp[i];
		}
		return nestableDistributions;
	}

	/**
	 * Tells if this distribution is equivalent to an other one
	 * @param o other distribution
	 * @return true if equals, false otherwise
	 */
	@Override
	public boolean equals(Object o) {
		if (!(o instanceof Distribution)) {
			return false;
		}
		Distribution d = (Distribution) o;
		// Check if class is the same
		if (!getClass().equals(d.getClass())) {
			return false;
		}
		// Check if all parameters are equals
		for (int i = 0; i < getNumberOfParameters(); i++) {
			if (!getParameter(i).getValue().equals(d.getParameter(i).getValue())) {
				return false;
			}
		}
		return true;
	}

	// ----- Abastract Methods that must be implemented by all distributions ------------------------------

	/**
	 * Used to set parameters of this distribution.
	 * @return distribution parameters
	 */
	protected abstract Parameter[] setParameters();

	/**
	 * Sets explicative image of this distribution used, together with description, to help the
	 * user to understand meaning of parameters.
	 * @return explicative image
	 */
	protected abstract ImageIcon setImage();

	/**
	 * Returns this distribution's short description
	 * @return distribution's short description
	 */
	@Override
	public abstract String toString();

	// ----- Methods that must be implemented ONLY if hasC or hasMean are true ----------------------------
	/**
	 * Sets the variation coefficient C for this distribution
	 * @param value variation coefficient C value
	 */
	public void setC(double value) {
		c = value;
	}

	/**
	 * Sets the mean for this distribution
	 * @param value mean value
	 */
	public void setMean(double value) {
		mean = value;
	}

	/**
	 * This method has to be called whenever a parameter changes and <code>hasC</code> or
	 * <code>hasMean</code> are true
	 */
	public void updateCM() {
	}

	// ----- Inner class used to store parameters ---------------------------------------------------------

	/**
	 * This class describes a parameter. It can be instantiated only by Distribution and its subclassses
	 * but will be accessed with public methods
	 */
	public class Parameter implements Cloneable {
		protected Class<?> valueClass;
		protected Object value;
		protected String name;
		protected String description;
		protected ValueChecker checker;
		protected boolean directParameter;

		/**
		 * Construct a new Parameter object. If a custom check on input values is needed, a call
		 * to <code>setValueChecker(ValueChecker checker)</code> method must be performed.
		 * @param name name of this parameter
		 * @param description brief description of this parameter usage
		 * @param valueClass Class type of value of this parameter
		 * @param defaultValue initial value for this parameter
		 * <code>getParameterClasspath()</code> for more details.
		 */
		public Parameter(String name, String description, Class<?> valueClass, Object defaultValue) {
			this(name, description, valueClass, defaultValue, false);
		}

		/**
		 * 
		 * Construct a new Parameter object. If a custom check on input values is needed, a call
		 * to <code>setValueChecker(ValueChecker checker)</code> method must be performed.
		 * @param name name of this parameter
		 * @param description brief description of this parameter usage
		 * @param valueClass Class type of value of this parameter
		 * @param defaultValue initial value for this parameter
		 * @param directParameter indicates if this parameter will be added to the distribution of the parameter object when translating it to XML
		 * <code>getParameterClasspath()</code> for more details.
		 */
		public Parameter(String name, String description, Class<?> valueClass, Object defaultValue, boolean directParameter) {
			this.name = name;
			this.description = description;
			this.valueClass = valueClass;
			this.value = defaultValue;
			checker = null;
			this.directParameter = directParameter;
		}

		/**
		 * Sets a ValueChecker to check if a parameter's value is in the correct
		 * range. If no checks are required don't call this method
		 * 
		 * @param checker
		 *            Instance of ValueChecker Interface
		 */
		public void setValueChecker(ValueChecker checker) {
			this.checker = checker;
		}

		/**
		 * Sets value for this parameter. Returns if parameter could be assigned correctly
		 * @param value value to be assigned to this parameter
		 * @return If value.class is different from valueClass, or parameter value is invalid,
		 * returns false, otherwise true
		 */
		public boolean setValue(Object value) {
			// if instance is not correct returns false
			if (valueClass.isInstance(value)) {
				// If checker exists and its check is not satisfied returns false
				if (checker != null) {
					if (!checker.checkValue(value)) {
						return false;
					}
				}
				// Otherwise sets value and returns true
				this.value = value;
				return true;
			}
			return false;
		}

		/**
		 * Sets value for this parameter. If parameter class is String sets it, otherwise
		 * try to parse string if parameter is of a known numeric value. Returns if parameter
		 * could be assigned correctly.
		 * @param value to be assigned to this parameter or to be parsed
		 * @return If value.class is different from valueClass, or parameter value is invalid,
		 * or is not parsable, returns false, otherwise true
		 */
		public boolean setValue(String value) {
			// If parameter is of the correct type, sets it and return
			if (setValue((Object) value)) {
				return true;
			}

			// Otherwise if it's a known number, try to decode it
			Object objval = null;
			try {
				if (valueClass.equals(Integer.class)) {
					objval = Integer.decode(value);
				} else if (valueClass.equals(Long.class)) {
					objval = Long.decode(value);
				} else if (valueClass.equals(Short.class)) {
					objval = Short.decode(value);
				} else if (valueClass.equals(Byte.class)) {
					objval = Byte.decode(value);
				} else if (valueClass.equals(Float.class)) {
					objval = new Float(Float.parseFloat(value));
				} else if (valueClass.equals(Double.class)) {
					objval = new Double(Double.parseDouble(value));
				} else if (valueClass.equals(Boolean.class)) {
					objval = new Boolean(value);
				} else {
					return false;
				}
				return setValue(objval);
			} catch (NumberFormatException e) {
				return false;
			}
		}

		/**
		 * Gets this parameter's value
		 * @return this parameter's value
		 */
		public Object getValue() {
			return value;
		}

		/**
		 * Gets this parameter value's class, ie the class of which parameter value is instance of.
		 * @return parameter value's class
		 */
		public Class<?> getValueClass() {
			return valueClass;
		}

		/**
		 * Gets a brief description of this parameter, used on distributionpanel
		 * @return parameter's description
		 */
		public String getDescription() {
			return description;
		}

		/**
		 * Gets the name of this parameter
		 * @return this parameter's name
		 */
		public String getName() {
			return name;
		}

		/**
		 * Returns if this parameter has to be added directly to the distribution when translating it into XML
		 * @return true if the parameter has to be added to the distribution. false otherwise
		 */
		public boolean isDirectParameter() {
			return directParameter;
		}

		/**
		 * Returns a shallow copy of this Parameter. Note that as nothing can be infered about
		 * all values clonability, that parameters are only referenced and not cloned. This should
		 * not be a problem as every value should be an immutable type.
		 * @return a copy of this parameter
		 */
		@Override
		public Object clone() {
			Parameter tmp = new Parameter(this.name, this.description, this.valueClass, this.value, this.directParameter);
			tmp.setValueChecker(this.checker);
			return tmp;
		}
	}

	/**
	 * This interface is used to specify a custom check method on parameter's value (for example
	 * value's range).
	 */
	protected interface ValueChecker {
		/**
		 * Checks if value of this object is correct. Note that when this method is called,
		 * parameter's value is granted to be instance of the correct class.
		 * @param value value to be checked
		 * @return true iff value is correct for this distribution parameter
		 */
		public boolean checkValue(Object value);
	}
}
