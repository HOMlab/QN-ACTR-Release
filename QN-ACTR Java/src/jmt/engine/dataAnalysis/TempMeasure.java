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

package jmt.engine.dataAnalysis;

import jmt.engine.QueueNet.SimConstants;

/**
 * @author Stefano Omini
 * @author Bertoli Marco (Added some features)
 */
public class TempMeasure {

	//measure
	private Measure measure;

	//name of the measure
	private String name;

	//the node of the queue network on which this measure is computed
	private String nodeName;
	//the job class this measure refers to
	private String jobClass;
	//the measure type
	private int measureType;
	// type of node (station or region)
	private String nodeType;

	//temp mean
	private double tempMean;

	//number of analyzed samples
	private int nsamples;
	//number of discarded samples
	private int discarded;

	// becomes true only when the analyzer has finished its computation
	private boolean finished;
	// true only if the analyzer has successfully finished
	// (i.e. if the confidence interval has been computed)
	private boolean success;
	//true if no samples have been received
	private boolean noSamplesTest;

	// --- Bertoli Marco
	// alpha, precision
	private double alpha, precision;
	private double upperBound, lowerBound;

	// --- end

	/**
	 * Creates a TempMeasure object, which can be used to refresh the actual value
	 * of mean during a simulation.
	 * @param measure the Measure object to be checked
	 */
	public TempMeasure(Measure measure) {
		this.measure = measure;

		//when created, reads these data from Measure object
		//later only dynamic values will be refreshed
		name = measure.getName();
		// This is a blocking region measure
		if (measure.getNodeName() != null && !measure.getNodeName().equals("")
				&& measure.getNetwork().getNode(measure.getNodeName()).isBlockingRegionInputStation()) {
			nodeName = measure.getNetwork().getNode(measure.getNodeName()).getBlockingRegionInputStation().getName();
			nodeType = SimConstants.NODE_TYPE_REGION;
		} else {
			nodeName = measure.getNodeName();
			nodeType = SimConstants.NODE_TYPE_STATION;
		}
		jobClass = measure.getJobClassName();
		measureType = measure.getMeasureType();
		alpha = measure.getAnalyzer().getAlfa();
		precision = measure.getAnalyzer().getPrecision();

		//Initialize
		nsamples = 0;
		discarded = 0;

		tempMean = 0;
		upperBound = 0;
		lowerBound = 0;
		finished = false;
		success = false;
		noSamplesTest = false;
	}

	public void refreshMeasure() {
		if (finished) {
			//no need to refresh measure
			return;
		} else {
			//refresh
			nsamples = measure.getAnalyzedSamples();
			discarded = measure.getDiscardedSamples();
			tempMean = measure.getExtimatedMeanValue();
			upperBound = measure.getUpperLimit();
			lowerBound = measure.getLowerLimit();
			finished = measure.hasFinished();
			if (finished) {
				success = measure.getSuccess();
				noSamplesTest = measure.receivedNoSamples();
			}

		}
	}

	//***************GET METHODS*******************/    

	public String getName() {
		return name;
	}

	public String getNodeName() {
		return nodeName;
	}

	public String getJobClass() {
		return jobClass;
	}

	public int getMeasureType() {
		return measureType;
	}

	public double getTempMean() {
		return tempMean;
	}

	public int getNsamples() {
		return nsamples;
	}

	public int getDiscarded() {
		return discarded;
	}

	public boolean isSuccessful() {
		return success;
	}

	public boolean isFinished() {
		return finished;
	}

	public boolean receivedNoSamples() {
		return noSamplesTest;
	}

	public boolean abort() {
		return measure.abortMeasure();
	}

	public double getAlpha() {
		return alpha;
	}

	public double getPrecision() {
		return precision;
	}

	public double getUpperBound() {
		return upperBound;
	}

	public double getLowerBound() {
		return lowerBound;
	}

	public String getNodeType() {
		return nodeType;
	}

}
