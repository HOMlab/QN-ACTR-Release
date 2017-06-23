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

package jmt.gui.common.serviceStrategies;

import java.text.DecimalFormat;
import java.util.NoSuchElementException;
import java.util.TreeMap;

import jmt.engine.math.parser.EvaluationException;
import jmt.engine.math.parser.ParseError;
import jmt.engine.math.parser.Parser;
import jmt.gui.common.Defaults;
import jmt.gui.common.distributions.Distribution;

/**
 * <p>Title: Load Dependent Station Service Strategy</p>
 * <p>Description: This object holds informations for a load dependent service strategy.
 * The strategy is organized in subsequent ranges (starting from 1 to infinity),
 * and each of them can specify a distribution, its c (if any, fixed) and its mean value
 * as a function of load into the station.</p>
 * 
 * @author Bertoli Marco
 *         Date: 11-ott-2005
 *         Time: 12.25.52
 */
public class LDStrategy implements ServiceStrategy {
	protected static final String VAR = "n"; // Dipendent variable
	protected static DecimalFormat formatter = new DecimalFormat("#.####");
	protected TreeMap<Object, Range> ranges;

	/**
	 * Return engine classpacth for LoadDependent strategy
	 * @return
	 */
	public static String getEngineClassPath() {
		return "jmt.engine.NetStrategies.ServiceStrategies.LoadDependentStrategy";
	}

	/**
	 * Builds a new Load Dependent Service Time Strategy with default values
	 */
	public LDStrategy() {
		ranges = new TreeMap<Object, Range>();
		// Adds first range
		ranges.put(new Integer(1), new Range(1));
	}

	/**
	 * Adds a new range to this strategy. 'From' field is initialized
	 * with greatest created range by now plus one (so if last range has from = 7,
	 * this will have from = 8)
	 * @return search's key for newly created range
	 */
	public synchronized Object addRange() {
		int from = ((Integer) ranges.lastKey()).intValue() + 1;
		Range newRange = new Range(from);
		Object key = new Integer(from);
		ranges.put(key, newRange);
		return key;
	}

	/**
	 * Changes starting number of jobs for a given range, only if it does not overlap
	 * with another range.
	 * @param key search's key for given range
	 * @param startingInterval starting number of jobs for this range to be set
	 * @return new search's key for this range
	 */
	public synchronized Object setRangeFrom(Object key, int startingInterval) {
		if (ranges.containsKey(new Integer(startingInterval))) {
			return key;
		}
		// Does not allow to change first key from value.
		if (ranges.firstKey() == key) {
			return key;
		}
		Range tmp = ranges.remove(key);
		tmp.from = startingInterval;
		Object newkey = new Integer(startingInterval);
		ranges.put(newkey, tmp);
		return newkey;
	}

	/**
	 * Sets distribution for this range, preserving old value of c, if applayable.
	 * @param key search's key for this range
	 * @param distribution distribution to set in this range
	 */
	public synchronized void setRangeDistribution(Object key, Distribution distribution) {
		Range tmp = ranges.get(key);
		Distribution tmpdist = distribution.clone();
		// Preserve stored value of c, if applayable
		if (tmp.distribution.hasC() && distribution.hasC()) {
			double c = tmp.distribution.getC();
			tmpdist.setC(c);
		}
		tmpdist.setMean(tmp.distribution.getMean());
		// If old mean expression is not valid for new distribution, overwrites it
		if (tmpdist.getMean() - tmp.distribution.getMean() > 1e-5) {
			tmp.meanExpression = formatter.format(tmpdist.getMean());
		}
		tmp.distribution = tmpdist;
	}

	/**
	 * Sets distribution for this range, don't preserver old values or C, nor of mean
	 * @param key search's key for this range
	 * @param distribution distribution to set in this range
	 */
	public synchronized void setRangeDistributionNoCheck(Object key, Distribution distribution) {
		Range tmp = ranges.get(key);
		tmp.distribution = distribution.clone();
	}

	/**
	 * Sets expression to calculate mean value of provided range only if it can be parsed
	 * without errors and its value in 'from' point is assignable to current distribution
	 * @param key search key for provided range
	 * @param meanExpression expression to be evaluated to find mean value
	 */
	public synchronized void setRangeDistributionMean(Object key, String meanExpression) {
		Range tmp = ranges.get(key);
		// Calculates min value using parser and sets it into distribution
		Parser parser = new Parser(meanExpression, true);

		try {
			parser.setVariable(VAR, tmp.from);
			tmp.distribution.setMean(parser.getValue());
			// Allow range only if its value in 'from' point is assignable to
			// current distribution
			if (tmp.distribution.getMean() - parser.getValue() < 1e-5) {
				tmp.meanExpression = parser.getExpression();
			}
		} catch (ParseError e) {
			// Do nothing
		} catch (EvaluationException e) {
			// Do nothing
		}
	}

	/**
	 * Sets expression to calculate mean value, performing no checks
	 * @param key search key for provided range
	 * @param meanExpression expression to be evaluated to find mean value
	 */
	public synchronized void setRangeDistributionMeanNoCheck(Object key, String meanExpression) {
		Range tmp = ranges.get(key);
		tmp.meanExpression = meanExpression;

	}

	/**
	 * Sets variation coefficient 'c' for the distribution of a given range
	 * @param key search key for provided range
	 * @param c variation coefficient
	 */
	public synchronized void setRangeDistributionC(Object key, double c) {
		Range tmp = ranges.get(key);
		if (tmp.distribution.hasC()) {
			tmp.distribution.setC(c);
		}
	}

	/**
	 * Gets starting number of jobs for this range
	 * @param key search key for provided range
	 * @return starting number of jobs for this range
	 */
	public synchronized int getRangeFrom(Object key) {
		Range tmp = ranges.get(key);
		return tmp.from;
	}

	/**
	 * Gets maximum number of jobs for this range
	 * @param key search key for provided range
	 * @return maximum number of jobs for this range. -1 means Infinity.
	 */
	public synchronized int getRangeTo(Object key) {
		Integer nextKey = new Integer(((Integer) key).intValue() + 1);
		try {
			Range next = ranges.get(ranges.tailMap(nextKey).firstKey());
			return next.from - 1;
		} catch (NoSuchElementException ex) {
			return -1;
		}
	}

	/**
	 * Returns the distribution of a given range
	 * @param key search key for given range
	 * @return its distribution
	 */
	public synchronized Distribution getRangeDistribution(Object key) {
		Range tmp = ranges.get(key);
		return tmp.distribution;
	}

	/**
	 * Returns the expression to be evaluated to find the mean of the distribution
	 * of a given range
	 * @param key search key for given range
	 * @return expression to find mean value
	 */
	public synchronized String getRangeDistributionMean(Object key) {
		Range tmp = ranges.get(key);
		return tmp.meanExpression;
	}

	/**
	 * Returns number of defined ranges
	 * @return number of defined ranges
	 */
	public synchronized int getRangeNumber() {
		return ranges.size();
	}

	/**
	 * Returns an array with all available ranges keys
	 * @return all available ranges keys, in ordered form
	 */
	public synchronized Object[] getAllRanges() {
		return ranges.keySet().toArray();
	}

	/**
	 * Removes a Range from this data structure
	 * @param key search's key for range to be removed
	 */
	public synchronized void deleteRange(Object key) {
		// Does not allow delete of first range
		if (ranges.firstKey() != key) {
			ranges.remove(key);
		}
	}

	/**
	 * Returns a String description of this object
	 * @return a String description of this object
	 */
	@Override
	public String toString() {
		return "Load Dependent Service Strategy";
	}

	/**
	 * Clones this strategy in a new Object
	 * @return a clone of current service strategy
	 */
	@Override
	public LDStrategy clone() {
		LDStrategy tmp = new LDStrategy();
		// Sets first range
		Object[] rangesKeys = ranges.keySet().toArray();
		for (Object rangesKey : rangesKeys) {
			Range oldRange = ranges.get(rangesKey);
			Range newRange = new Range(oldRange.from);
			newRange.distribution = oldRange.distribution.clone();
			newRange.meanExpression = oldRange.meanExpression;
			tmp.ranges.put(rangesKey, newRange);
		}
		return tmp;
	}

	/**
	 * Returns mean value of service time distribution for given number of jobs.
	 * @param jobs queue lenght of the station
	 * @return  mean value of service time distribution. On insuccess returns -1
	 */
	public double getMeanValue(int jobs) {
		// Finds key of right range
		Object key = ranges.headMap(new Integer(jobs + 1)).lastKey();
		Range range = ranges.get(key);

		Parser parser = new Parser(range.meanExpression, true);

		try {
			parser.setVariable(VAR, jobs);
			return parser.getValue();
		} catch (ParseError e) {
			// Do nothing
		} catch (EvaluationException e) {
			// Do nothing
		}
		return -1;
	}

	/**
	 * Checks if the given Load Dependent Strategy equals <code>this</code>,
	 * where the equivalence is iff:
	 * <br>1) have the same number of ranges</br>
	 * <br>2) have the same distribution in each range</br>
	 * <br>3) have the same mean value in each range</br>
	 * <br>4) have the same 'from' and 'to' values in each range</br>
	 * @param ld the given LD strategy
	 * @return  true if ld equals <code>this</code>
	 *
	 * Author: Francesco D'Aquino
	 */
	public boolean isEquivalent(LDStrategy ld) {
		boolean equivalent = true;
		Object[] ldRanges = ld.getAllRanges();
		Object[] thisRanges = this.getAllRanges();
		if (this.getRangeNumber() == ld.getRangeNumber()) {
			for (int i = 0; i < ld.getRangeNumber(); i++) {
				if (ld.getRangeDistribution(ldRanges[i]).getName().equals(getRangeDistribution(thisRanges[i]).getName())) {
					if (ld.getRangeDistributionMean(ldRanges[i]).equals(getRangeDistributionMean(thisRanges[i]))) {
						if ((ld.getRangeFrom(ldRanges[i]) != getRangeFrom(thisRanges[i]))
								|| (ld.getRangeTo(ldRanges[i]) != getRangeTo(thisRanges[i]))) {
							equivalent = false;
							break;
						}
					} else {
						equivalent = false;
						break;
					}
				} else {
					equivalent = false;
					break;
				}
			}
		} else {
			equivalent = false;
		}
		return equivalent;
	}

	/**
	 * Inner class used to store definition of a Range
	 */
	protected class Range implements Comparable {
		public int from;
		public Distribution distribution;
		public String meanExpression;

		/**
		 * Builds a new Range, starting interval from specified queue length. Distribution
		 * is set as Default service time distribution, if it allows mean, otherwise to the
		 * first distribution with mean available.
		 * @param from starting index for this range
		 */
		public Range(int from) {
			this.from = from;
			distribution = (Distribution) Defaults.getAsNewInstance("stationServiceStrategy");
			if (!distribution.hasMean()) {
				distribution = Distribution.findAllWithMean()[0];
			}
			meanExpression = formatter.format(distribution.getMean());
		}

		/**
		 * Used to store this object in a SortedSet.
		 * @param o object to compare with
		 * @return a negative integer, zero, or a positive integer as this object
		 * is less than, equal to, or greater than the specified object.
		 */
		public int compareTo(Object o) {
			Range tmp = (Range) o;
			return from - tmp.from;
		}
	}
}
