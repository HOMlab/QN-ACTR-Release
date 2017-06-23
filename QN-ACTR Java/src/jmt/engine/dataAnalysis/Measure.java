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

import java.util.Iterator;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;

import jmt.common.exception.NetException;
import jmt.engine.NodeSections.BlockingQueue;
import jmt.engine.QueueNet.JobClass;
import jmt.engine.QueueNet.NetNode;
import jmt.engine.QueueNet.NodeSection;
import jmt.engine.QueueNet.QueueNetwork;
import jmt.engine.QueueNet.SimConstants;
import jmt.engine.log.JSimLogger;
import jmt.engine.math.SampleMean;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * This class implements a measure object.
 *
 * @author Federico Granata (modified by Stefano Omini) (modified by Bertoli Marco)
 * @author Bertoli Marco (new Data Analyzer, bugfixed scaling)
 */
public class Measure {

	/**This is the object which receives the collected samples, computes the mean value
	and determines when the simulation must be stopped (confidence interval reached or
	too many analyzed samples*/
	protected DynamicDataAnalyzer analyzer;

	/** name of the measure*/
	private String name;

	/** becomes true only when the analyzer has finished its computation*/
	private boolean finish;

	/** True, if each measure should be sent on output */
	private boolean verbose;

	/**true if new data is added*/
	private boolean changed;

	/**Number of original samples: remember that these samples are divided in batches
	and each effective sample is the mean updated every pointSize (i.e. the size of the batch)
	original samples */
	private int counter = 0;

	/**String into which new data are copied, in order to be inserted in a Document*/
	private String newValues;

	/**when the number of original samples (i.e. counter) is equal to fireEventSize,
	counter is reset and "changed" is set to true*/
	private static int fireEventSize = 1000;

	/**size of the batch used to reduce the memory space
	for example, mean value is updated every pointSize samples instead of
	updating it for each new sample*/
	private static int pointSize = 10;

	private int size_samples = fireEventSize % pointSize;

	//OLD
	//private double[] samples = new double[fireEventSize / pointSize];
	/** the array of samples: each element is the mean computed after pointSize original samples */
	private double[] samples = new double[size_samples];

	/** mean of all samples */
	private SampleMean mean = new SampleMean();

	/** max precision percentage reached */
	private double maxPrecisionPercentage;

	/** A measure output object is used to print a measure values */
	private MeasureOutput output;

	/**
	 * DOM (Document Object Model) used to contain everything regards this measure
	 * (name, mean, precision, ...) */
	private Document data;

	//NEW
	//@author Stefano Omini
	private JSimLogger logger = JSimLogger.getLogger(JSimLogger.STD_LOGGER);
	//end NEW

	//NEW
	//@author Stefano Omini

	//these informations were already contained in the SimMeasure class, but were no longer
	//available in the Measure object, for this reason they have been replicated in the Measure
	//object. The info are stored with the measureTarget(...) method.

	//the node of the queue network on which this measure is computed
	private String nodeName;
	//the job class this measure refers to
	private String jobClassName;
	//the measure type
	private int measureType;

	//the queue network this measure belongs to
	private QueueNetwork network;

	//end NEW

	//NEW
	//@author Stefano Omini

	//true if no samples have been received
	//in this case the measure should be forced to end, otherwise
	//the simulation wouldn't stop
	private boolean noSamplesTest = false;

	//true if measure has been aborted
	private boolean aborted = false;

	//end NEW

	/**
	 * deadState is used to check if a measure is in a dead state (so doesn't receive
	 * any more samples). A measure is marked as dead when it reaches maxDeadState.
	 */
	private int deadState = 0;
	private int maxDeadState = 2;

	//-------SCALE FACTOR FOR RESIDENCE TIME MEASURES------------//

	//NEW
	//@author Stefano Omini

	// due to its architecture, jSIM correctly computes the
	// residence time for a single visit,
	// not the total residence time for all visits
	// to correct this measure is therefore needed a scale factor
	// (the visit ratio, i.e. the mean number of accesses at a resource)

	// the other types of measures do not need any scaling, so
	// the scale factor remains equal to 1

	//true if the measure has been scaled, false otherwise
	private boolean scaled = false;

	//the scale factor
	double scaleFactor = 1.0;

	//end NEW

	//-------end SCALE FACTOR FOR RESIDENCE TIME MEASURES------------//

	/** Creates a new instance of measure class.
	 * @param Name name of the measure.
	 * @param alfa    the quantile required for the confidence interval
	 * @param precision   indicator of maximum amplitude of confidence interval
	 *                      (precision = maxamplitude / mean)
	 * @param maxSamples  maximum number of data to be analyzed
	 * @param Verbose   True, if each measure should be sent on output
	 * @param quantiles the quantiles to be computed (null, if no quantiles should be computed)
	 */

	public Measure(String Name, double alfa, double precision, int maxSamples, boolean Verbose, double[] quantiles) {
		this.name = Name;
		//DEK (Federico Granata)
		if (quantiles != null && quantiles.length > 0) {
			//quantile calculation is requested too
			analyzer = new QuantileDataAnalyzer(alfa, precision, maxSamples, quantiles);
		} else {
			//analyzer = new DynamicDataAnalyzerImpl(alfa, precision, maxSamples);
			analyzer = new NewDynamicDataAnalyzer(alfa, precision, maxSamples);
		}

		finish = false;
		maxPrecisionPercentage = 0.0;
		this.verbose = Verbose;
		createDOM();
	}

	//NEW
	//@author Stefano Omini

	/**
	 * Sets the station, the class, the type of this Measure
	 * @param node the node on which the measure is computed
	 * @param jClass the job class this measure refers to
	 * @param mType the type of measure (see constants defined in clas Simulation)
	 */
	public void measureTarget(String node, String jClass, int mType) {
		nodeName = node;
		jobClassName = jClass;
		measureType = mType;
	}

	/**
	 *  Gets the node this measure refers to
	 */
	public String getNodeName() {
		return nodeName;
	}

	/**
	 *  Gets the job class this measure refers to
	 */
	public String getJobClassName() {
		return jobClassName;
	}

	/**
	 *  Gets the measure type (see constants defined in class Simulation)
	 */
	public int getMeasureType() {
		return measureType;
	}

	/**
	 * Gets the QueueNetwork this measure belongs to
	 * @return the QueueNetwork this measure belongs to
	 */
	public QueueNetwork getNetwork() {
		return network;
	}

	/**
	 * Sets the QueueNetwork this measure belongs to
	 * @param network the QueueNetwork this measure belongs to
	 */
	public void setNetwork(QueueNetwork network) {
		this.network = network;
	}

	//end NEW

	public boolean getVerbose() {
		return verbose;
	}

	/**
	 * Gets output object.
	 * @return output object.
	 */
	public MeasureOutput getOutput() {
		return output;
	}

	/**
	 * Sets output object.
	 * @param Output output object.
	 */
	void setOutput(MeasureOutput Output) {
		this.output = Output;
	}

	/**
	 * Returns true if the analysis measure is successful
	 * @return true if the analysis measure respects all users requests
	 */
	public boolean getSuccess() {
		return analyzer.getSuccess();
	}

	/** Gets measure value.
	 * @return measure value.
	 */
	public double getMeanValue() {
		// OLD
		// return analyzer.getMean();

		//NEW
		//@author Stefano Omini
		if (!noSamplesTest) {
			return analyzer.getMean() * scaleFactor;
		} else {
			return 0.0;
			//end NEW
		}
	}

	//NEW
	//@author Stefano Omini

	/** Gets measure value: if the confidence requirements have not been
	 * reached, it is returned the value extimated up to that moment.
	 * @return measure value.
	 */
	public double getExtimatedMeanValue() {
		// OLD
		// return analyzer.extimatedMean();

		//NEW
		//@author Stefano Omini
		if (!noSamplesTest) {
			return analyzer.extimatedMean() * scaleFactor;
		} else {
			return 0.0;
			//end NEW
		}
	}

	//end NEW

	/** Gets lower limit.
	 * @return Lower limit.
	 */
	public double getLowerLimit() {

		//NEW
		//@author Stefano Omini
		if (analyzer.isZero()) {
			return 0.0;
		}
		//end NEW

		double lower = analyzer.getMean() - analyzer.getConfInt();

		// NEW Bertoli
		if (analyzer.getConfInt() == 0) {
			return 0;
			//end NEW
		}

		if (lower > 0) {
			// OLD
			// return lower;

			//NEW
			//@author Stefano Omini
			return lower * scaleFactor;
			//end NEW

		} else {
			return 0;
		}
	}

	/** Gets upper limit.
	 * @return Lower limit.
	 */
	public double getUpperLimit() {
		//NEW
		//@author Stefano Omini
		if (analyzer.isZero()) {
			return analyzer.getNullMeasure_upperLimit();
		}
		//end NEW
		double upper = analyzer.getMean() + analyzer.getConfInt();

		// NEW Bertoli
		if (analyzer.getConfInt() == 0) {
			return 0;
			//end NEW
		}

		// OLD
		// return upper;

		//NEW
		//@author Stefano Omini
		return upper * scaleFactor;
		//end NEW
	}

	/** Returns number of analyzed samples.
	 * @return Samples analyzed.
	 */
	public int getAnalyzedSamples() {
		return analyzer.getSamples();
	}

	/** Returns number of discarded samples.
	 * @return Samples analyzed.
	 */
	public int getDiscardedSamples() {
		return analyzer.getDiscarded();
	}

	/** Returns the maximum number of samples that can be analyzed.
	 * @return Samples analyzed.
	 */
	public int getMaxSamples() {
		return analyzer.getMaxData();
	}

	/** Updates data. This method should be called to add a new sample to the
	 * collected data.
	 * @param sample sample to be added.
	 * @param weight sample weight.
	 * @return True if the computation of this measure has been finished, false otherwise.
	 */

	public synchronized boolean update(double sample, double weight) {

		//NEW
		//@author Stefano Omini

		/*
		The old version has this problem:
		Even if the confidence requirements have been reached, measure is updated until ALL
		confidence intervals have been computed. The MeasureOutput results, on the other hand,
		are no more updated after confidence has been reached for the first time.

		Two possible solutions:
		1. do not update measure after reaching confidence interval
		2. continue updating, but periodically refresh results in MeasureOutput

		At the moment solution 1 is preferred because it saves time and resources of
		the simulation.

		The following if-block is used to skip measure updating phase when the computation
		has already reached the confidence interval.
		*/

		if (finish) {
			//confidence interval computation has been already finished
			//it's not useful to continue updating this measure
			return true;
		}

		//end NEW

		// Resets measure dead state
		deadState = 0;

		mean.putNewSample(sample * weight);

		//the sample is added, but the value of mean is updated only if counter
		//is a multiple of pointSize (x % y is equal to the rest of the division x/y)

		//OLD
		//if (counter % pointSize == 0 || counter != 0)
		//NEW
		//In the old way, mean was updated for each new sample (because counter was >0 !!)
		//instead of waiting for every point size samples
		//@author Stefano Omini
		if (counter % pointSize == 0 && (counter % pointSize) < size_samples && counter >= 0) {
			samples[counter / pointSize] = mean.getMean();
		}
		counter++;

		if (analyzer.addSample(sample, weight)) {
			//data analysis finished

			if (!finish) {
				//simulation not finished yet
				finish = true;

				if (output != null) {
					//writes the final measure
					output.writeMeasure();
				}
			}
			//simulation already finished

			if (logger.isDebugEnabled()) {
				boolean log_success = analyzer.getSuccess();
				double log_mean = getExtimatedMeanValue();
				logger.debug("Measure " + name + " finished. Mean value: " + log_mean + " Success = " + log_success);
			}

			return true;

		} else if (verbose) {
			//data analysis not finished yet

			if (output != null) {
				//writes the new sample
				output.write(sample, weight);
			}

		}
		//NEW
		//@author Stefano Omini
		if (measureType == SimConstants.RESIDENCE_TIME) {
			scaleMeasureWithVisitRatio();
		}

		//end NEW
		if (counter == fireEventSize) {
			counter = 0;
			changed = true;

		}
		return false;
	}

	/**Gets the name of the measure.
	 * @return name property value.
	 */
	public String getName() {
		return name;
	}

	//NEW
	//@author Stefano Omini
	public DynamicDataAnalyzer getAnalyzer() {
		return analyzer;
	}

	//end NEW

	/** Returns analyzed samples percentage.
	 * @return analyzed samples percentage.
	 */
	public double getSamplesAnalyzedPercentage() {
		return (double) analyzer.getSamples() / analyzer.getMaxData();
	}

	/** Returns max precision percentage reached.
	 * @return Max precision percentage reached.
	 */
	public double getMaxPrecisionPercentage() {
		double p;
		p = analyzer.getPrecision() * analyzer.getMean() / analyzer.getConfInt();
		if (maxPrecisionPercentage < p) {
			maxPrecisionPercentage = p;
		}
		return maxPrecisionPercentage;
	}

	/**
	 * Has the simulation already finished?
	 * @return true, if the simulation has already finished; false otherwise
	 */
	public boolean hasFinished() {
		return finish;
	}

	//DEK (Federico Granata)
	/**
	 * gets all requested quantiles
	 *
	 * @return vector of quatiles, null if quantiles computation wasn't requested
	 * while creating the Measure object. See constructor's parameters.
	 */
	public double[] getQuantileResults() {
		double[] results;
		if (analyzer instanceof QuantileDataAnalyzer) {
			results = ((QuantileDataAnalyzer) analyzer).getQuantiles();
		} else {
			results = null;
		}
		return results;
	}

	//DEK (Federico Granata)
	/**
	 * Checks whether the data is changed
	 * @return true iff data is changed
	 */
	public boolean isChanged() {
		return changed;
	}

	/**
	 * Gets the new analyzed data. Data are divided in batches. Every batch is
	 * the sample mean of a group of data weighted.
	 * @return data array
	 */
	public final double[] getData() {
		return samples;
	}

	/**
	 * gets the new data that have been generated; returns null if there aren't
	 * new data.
	 */
	public Document getNewData() {
		if (!changed) {
			Element root = data.getDocumentElement();
			root.setAttribute("name", name);
			return data;
		}
		changed = false;
		Element root = data.getDocumentElement();
		root.setAttribute("name", name);
		root.setAttribute("meanValue", Double.toString(analyzer.getMean()));
		root.setAttribute("upperBound", Double.toString(getUpperLimit()));
		root.setAttribute("lowerBound", Double.toString(getLowerLimit()));
		root.setAttribute("progress", Double.toString(getSamplesAnalyzedPercentage()));

		root.setAttribute("finished", Boolean.toString(finish));
		root.setAttribute("discarded", Integer.toString(getDiscardedSamples() / pointSize));

		newValues = "";
		for (double sample : samples) {
			newValues += Double.toString(sample) + "/n";
		}
		root.setAttribute("data", newValues);
		root.setAttribute("numData", Integer.toString(samples.length));
		return data;
	}

	/**
	 * Creates a DOM (Document Object Model) <code>Document<code>.
	 */
	private void createDOM() {
		try {
			DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
			DocumentBuilder builder = factory.newDocumentBuilder();

			//data is a Document
			data = builder.newDocument();

			Element root = data.createElement("measure");

			root.setAttribute("name", name);
			root.setAttribute("meanValue", "null");
			root.setAttribute("upperBound", "null");
			root.setAttribute("lowerBound", "null");
			root.setAttribute("progress", Double.toString(getSamplesAnalyzedPercentage()));
			root.setAttribute("data", "null");
			root.setAttribute("finished", "false");
			root.setAttribute("discarded", "0");
			root.setAttribute("precision", Double.toString(analyzer.getPrecision()));
			root.setAttribute("maxSamples", Double.toString(getMaxSamples()));

			data.appendChild(root);

		} catch (FactoryConfigurationError factoryConfigurationError) {
			factoryConfigurationError.printStackTrace();
		} catch (ParserConfigurationException e) {
			e.printStackTrace();
		}

	}

	//NEW
	//@author Stefano Omini

	/**
	 * jSIM architecture does not allow to correctly compute residence time for
	 * all visits.
	 * for this reason a scaling operation is needed.
	 * the scale factor is the mean number of accesses to the resource (i.e.
	 * the visit ratio = number of accesses to this station / number of
	 * accesses to the reference station)
	 */

	private void scaleMeasureWithVisitRatio() {
		//retrieves JobClass object using its name
		JobClass jobClass = network.getJobClass(jobClassName);
		double visitRatio = 1.0;
		//current measure node
		NetNode thisNode = network.getNode(nodeName);

		// If measure is class specific
		if (jobClass != null) {
			//reference node for this job class
			String referenceNodeName = jobClass.getReferenceNodeName();
			NetNode referenceNode = network.getNode(referenceNodeName);
			int visitsReferenceNode, visitsThisNode;
			try {
				//visits to the reference node
				NodeSection refOutputSection = referenceNode.getSection(NodeSection.OUTPUT);
				visitsReferenceNode = refOutputSection.getIntSectionProperty(NodeSection.PROPERTY_ID_ARRIVED_JOBS, jobClass);

				//visits to this node node
				NodeSection thisNodeInputSection = thisNode.getSection(NodeSection.INPUT);

				visitsThisNode = thisNodeInputSection.getIntSectionProperty(NodeSection.PROPERTY_ID_ARRIVED_JOBS, jobClass);

				// the reference source generates a certain number of jobs
				// - some of them have reached this station
				// - other have been dropped before enetering this station or
				// elsewhere in the network
				int droppedJobs = network.getDroppedJobs(jobClass);

				if (thisNodeInputSection instanceof BlockingQueue) {

					// if the input section is a BlockingQueue,
					// remove (from the total number of visits)
					// the jobs dropped by the BlockingQueue itself,
					// because they have been counted
					// in arrived jobs
					visitsThisNode -= ((BlockingQueue) thisNode.getSection(NodeSection.INPUT)).getDroppedJobPerClass(jobClass.getId());

				}

				//visit ratio
				//
				// we must consider only not dropped jobs:
				// --> visit ratio = visits to this node / (visits to ref node - dropped jobs)
				visitRatio = (double) (visitsThisNode) / (visitsReferenceNode - droppedJobs);

				//System.out.println("refNode: " + Integer.toString(visitsReferenceNode));
				//System.out.println("thisNode: " + Integer.toString(visitsThisNode));
				//System.out.println("dropped: " + droppedJobs);
				//System.out.println("visit ratio: " + Double.toString(visitRatio));

			} catch (NetException ne) {
				visitRatio = 1;
				logger.error("Error in computing visit ratio.");
				ne.printStackTrace();
			}
		}
		// Measure is class-independent --> Bertoli Marco
		else {
			try {
				Iterator it = network.getJobClasses().listIterator();
				double ratioSum = 0.0, weight = 0.0;
				while (it.hasNext()) {
					JobClass jobC = (JobClass) it.next();
					//reference node for this job class
					String referenceNodeName = jobC.getReferenceNodeName();
					NetNode referenceNode = network.getNode(referenceNodeName);
					int visitsReferenceNode, visitsThisNode;
					//visits to the reference node
					NodeSection refOutputSection = referenceNode.getSection(NodeSection.OUTPUT);
					visitsReferenceNode = refOutputSection.getIntSectionProperty(NodeSection.PROPERTY_ID_ARRIVED_JOBS, jobC);

					//visits to this node node
					NodeSection thisNodeInputSection = thisNode.getSection(NodeSection.INPUT);

					visitsThisNode = thisNodeInputSection.getIntSectionProperty(NodeSection.PROPERTY_ID_ARRIVED_JOBS, jobC);

					int droppedJobs = network.getDroppedJobs(jobC);

					if (thisNodeInputSection instanceof BlockingQueue) {
						visitsThisNode -= ((BlockingQueue) thisNode.getSection(NodeSection.INPUT)).getDroppedJobPerClass(jobC.getId());

					}
					double currVisitRatio = (double) (visitsThisNode) / (visitsReferenceNode - droppedJobs);

					// Uses new calculated visit ratio to compute a weighted visit ratio
					// This is done only if currVisitRatio value is valid
					if (!Double.isNaN(currVisitRatio) && !Double.isInfinite(currVisitRatio)) {
						ratioSum += currVisitRatio * visitsThisNode;
						weight += visitsThisNode;
						visitRatio = ratioSum / weight;
					}
				}
			} catch (NetException ne) {
				visitRatio = 1;
				logger.error("Error in computing visit ratio.");
				logger.error(ne);
			}
		}

		//correggere mean upper lower ecc....

		//sets scaled = true (to avoid other scaling operations)
		scaled = true;
		scaleFactor = visitRatio;

	}

	//******************ABORT**********************//

	public synchronized boolean abortMeasure() {

		if (finish) {
			//measure already finished
			//nothing to do
			return false;
		} else {
			//abort measure
			aborted = true;
			finish = true;
			//stops analyzer
			stopMeasure();

		}

		return true;

	}

	// --- Dead Measures Test - Bertoli Marco --------------------------------------------
	/**
	 * This method will check if a measure is dead i.e. no samples are received
	 * for a long period (so the measure will probably be zero). This is needed to
	 * abort measures in stations that are not reachable after initial transient
	 * @return true iff measure has been marked as dead and was stopped
	 */
	public boolean testDeadMeasure() {
		if (deadState > maxDeadState) {
			stop_NoSamples();
			return true;
		}
		deadState++;
		return false;
	}

	// -----------------------------------------------------------------------------------

	//*****************NO SAMPLES TEST*******************//

	//NEW
	//@author Stefano Omini

	public boolean receivedNoSamples() {
		return noSamplesTest;
	}

	public void stop_NoSamples() {
		finish = true;
		noSamplesTest = true;

		//stop measure with success
		stopMeasure(true);
	}

	protected void stopMeasure(boolean success) {
		//stops measure
		analyzer.stopMeasure(success);

		//writes measure output on log
		if (logger.isDebugEnabled()) {

			double log_mean = getExtimatedMeanValue();
			boolean log_success = analyzer.getSuccess();

			logger.debug("Measure " + name + " finished. Mean value: " + log_mean + " Success = " + log_success);
		}

	}

	protected void stopMeasure() {
		boolean success = analyzer.getSuccess();
		//stops measure
		analyzer.stopMeasure(success);

		//writes measure output on log
		if (logger.isDebugEnabled()) {

			double log_mean = getExtimatedMeanValue();
			boolean log_success = analyzer.getSuccess();

			logger.debug("Measure " + name + " finished. Mean value: " + log_mean + " Success = " + log_success);
		}
	}

	public boolean hasBeenAborted() {
		return aborted;
	}

	//end NEW

	//NEW
	//@author Stefano Omini
	public void setSimParameters(SimParameters param) {
		analyzer.setParameters(param);
		maxDeadState = param.getDeadMeasureMaxChecks();
	}
	//end NEW

}