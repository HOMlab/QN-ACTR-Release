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

package jmt.engine.simDispatcher;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import jmt.common.exception.LoadException;
import jmt.common.exception.NetException;
import jmt.engine.QueueNet.NetSystem;
import jmt.engine.QueueNet.QueueNetwork;
import jmt.engine.dataAnalysis.Measure;
import jmt.engine.dataAnalysis.TempMeasure;
import jmt.engine.log.JSimLogger;
import jmt.engine.simEngine.SimLoader;
import jmt.engine.simEngine.Simulation;
import jmt.framework.xml.XMLUtils;
import jmt.gui.common.definitions.CommonModel;
import jmt.gui.common.definitions.ModelConverter;
import jmt.gui.common.definitions.SimulationDefinition;
import jmt.gui.common.xml.XMLConstantNames;
import jmt.gui.common.xml.XMLWriter;
import jmt.gui.exact.ExactConstants;
import jmt.gui.exact.ExactModel;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * <p><b>Name:</b> Dispatcher_jMVAschema</p> 
 * <p><b>Description:</b> Receives the absolute path of a xml file which describes the model using the
 * JMTmodel.xsd schema.
 * This file is converted into another xml file in order to be passed to the
 * simulator. The simulation results, at the end, are converted and
 * are inserted in the original file.
 * </p>
 * <p><b>Date:</b> 13-lug-2006
 * <b>Time:</b> 11.15.47</p>
 * @author Stefano Omini, Bertoli Marco
 * @version 2.0
 */
public class Dispatcher_jMVAschema {
	private static final String SEPARATOR = System.getProperty("file.separator");

	//xml files containing model definition, simmodel definition and sim results
	File modelDefinition;
	File simModelDefinition;
	File simResults;

	//path of the xml files
	String modelDefinitionPath;
	String simModelDefinitionPath;
	String simResultsPath;

	//if true, deletes the intermediate temp files created to solve the model with jSIM.
	//Only the initial passed file (containing the results)
	boolean deleteIntermediateFiles = false;

	//true if the simulation results have been already obtained, false otherwise
	boolean resultsObtained = false;

	//if true, the simulation seed
	private boolean automaticSeed = true;
	//if true, the simulation seed
	private long simulationSeed;

	private static JSimLogger logger = JSimLogger.getLogger(JSimLogger.STD_LOGGER);

	//used to compute the progress of the simulation
	private boolean simStarted = false;
	private boolean simFinished = false;
	private QueueNetwork net;

	// Used to tell if transformer input and output must be validated
	private boolean validate = true;

	//used to store temp measures info
	private TempMeasure[] tempMeasures = null;

	/**
	 * This constructor receives the absolute path of the xml file containing
	 * the model to be solved.
	 * The model must be described using the JMTmodel.xsd schema
	 *
	 * @param absolutePath absolute path of the xml file containing the
	 * model to be solved
	 */
	public Dispatcher_jMVAschema(String absolutePath) {
		modelDefinitionPath = absolutePath;
		modelDefinition = new File(absolutePath);
	}

	/**
	 * This constructor receives the absolute path of the xml file containing
	 * the model to be solved.
	 * The model must be described using the JMTmodel.xsd schema
	 *
	 * @param model the xml file containing the
	 * model to be solved
	 */
	public Dispatcher_jMVAschema(File model) {
		modelDefinition = model;
		modelDefinitionPath = model.getAbsolutePath();

	}

	/**
	 * By invoking this method, simulation will generate a random simulation seed
	 */
	public void automaticSimulationSeed() {
		automaticSeed = true;
	}

	/**
	 * Specifies the seed to be used in the simulation.
	 * @param seed the simulation seed
	 */
	public void setSimulationSeed(long seed) {
		simulationSeed = seed;
		automaticSeed = false;
	}

	/**
	 * Determine whether the intermediate files must be deleted or not.
	 * @param deleteIntermediateFiles if true, intermediate files will be deleted
	 */
	public void setDeleteIntermediateFiles(boolean deleteIntermediateFiles) {
		this.deleteIntermediateFiles = deleteIntermediateFiles;
	}

	public boolean solveModel() {

		boolean transfSuccess = false;

		/*********************DEFINITION MODEL*********************/

		if (!modelDefinition.exists()) {
			//the passed file does not exist
			logger.error("The model file " + modelDefinitionPath + " does not exist...");
			return false;
		} else {
			logger.debug("Model definition path: " + modelDefinitionPath);
		}

		/*********************SIM DEFINITION MODEL*********************/

		//first of all transforms the JMTmodel into SIMmodeldefinition
		ModelTransformer md = new ModelTransformer();
		md.setValidate(validate);

		//save the file in the same directory of model file, with the same name preceeded by "sim_"

		String modelDefinitionParent = modelDefinition.getParent();

		if (modelDefinitionParent.endsWith(SEPARATOR)) {
			simModelDefinitionPath = modelDefinitionParent + "sim_" + modelDefinition.getName();
		} else {
			simModelDefinitionPath = modelDefinitionParent + SEPARATOR + "sim_" + modelDefinition.getName();
		}

		transfSuccess = md.MVAtoSIM_parallel(modelDefinitionPath, simModelDefinitionPath);

		if (!transfSuccess) {
			logger.error("MVA to SIM transformation failed for model file " + modelDefinitionPath);
			return false;
		} else {
			logger.debug("jSIM model definiton path: " + simModelDefinitionPath);
		}

		/*********************SIMULATION LOADING AND RUNNING*********************/

		//sim model successfully created
		//now prepare simulation
		try {
			SimLoader simLoader = new SimLoader(simModelDefinitionPath);
			Simulation sim = simLoader.getSim();

			//sets in the Simulation object the path of the original (i.e. MVA)
			//xml model definition
			sim.setXmlModelDefPath(modelDefinitionPath);

			if (!automaticSeed) {
				//if automaticSeed == false, then a user defined seed is present
				sim.setRandomEngineSeed(simulationSeed);
				sim.initialize();

				logger.debug("jSIM correctly initialized with simulation seed = " + simulationSeed);
			} else {
				sim.initialize();
				logger.debug("jSIM correctly initialized");
			}

			//Avoid saturation
			if (sim.getAllRegions() == null) {
				//if there are no regions...
				boolean sufficientProcessingCapacity = checkProcessingCapacity();

				if (!sufficientProcessingCapacity) {
					logger.warn("Current workload causes system saturation. Simulation will not be started.");
					return false;
				}
			}

			simStarted = true;
			net = sim.getNetwork();

			long start, stop;
			double elapsed;

			//Start time
			start = System.currentTimeMillis();

			sim.run();

			//stop time
			stop = System.currentTimeMillis();
			elapsed = ((stop - start)) / 1000.0;

			simFinished = true;

			logger.info("Model " + modelDefinitionPath + " solved by jSIM in " + Double.toString(elapsed) + " seconds");

		} catch (IOException e) {
			e.printStackTrace();
			logger.error(e.getMessage());
			return false;
		} catch (LoadException e) {
			e.printStackTrace();
			logger.error(e.getMessage());
			return false;
		} catch (Exception e) {
			e.printStackTrace();
			logger.error(e.getMessage());
			return false;
		}

		simModelDefinition = new File(simModelDefinitionPath);

		/*********************COPYING SIMULATION RESULTS*********************/

		//the results file has been obtained
		//the file has been saved in the same directory of model file, with the same name preceeded by "res_sim_"

		if (modelDefinitionParent.endsWith(SEPARATOR)) {
			simResultsPath = modelDefinition.getParent() + "res_" + simModelDefinition.getName();
		} else {
			simResultsPath = modelDefinition.getParent() + SEPARATOR + "res_" + simModelDefinition.getName();
		}

		simResults = new File(simResultsPath);

		logger.debug("jSIM results path: " + simResultsPath);

		//transforms the SIMmodeloutput into JMTmodel

		//resets the transformer
		md = new ModelTransformer();
		md.setValidate(validate);
		//create temp file to contain model definition and results
		File temp = null;
		String tempPath = null;

		try {
			temp = File.createTempFile("~jmt_model", ".xml", modelDefinition.getParentFile());
			tempPath = temp.getAbsolutePath();
		} catch (IOException e) {
			e.printStackTrace();
			logger.error(e.getMessage());
			logger.error("Error in creating temp file for model " + modelDefinitionPath);

			return false;
		}

		transfSuccess = md.OUTtoMVAScaling(simResultsPath, tempPath);

		if (!transfSuccess) {
			logger.error("OUT to MVA transformation failed model " + modelDefinitionPath);
			return false;
		} else {
			//if successful, rename to model definition file, after deleting the old version
			if (modelDefinition.exists()) {
				modelDefinition.delete();
			}
			//copy the model with results (rename doesn't work!)

			//if (!temp.renameTo(modelDefinition)){
			if (!copyFile(temp, modelDefinition)) {
				logger.error("could not copy results into model " + modelDefinitionPath);
			} else {
				resultsObtained = true;
				logger.debug("jSIM results copied into model " + modelDefinitionPath);
			}

			//delete temp file
			if (!temp.delete()) {
				temp.deleteOnExit();
				logger.warn("Could not delete a temp file. Deletion will be retried on JVM exit.");
			}

		}

		/*********************DELETE INTERMEDIATE FILES*********************/

		//if true delete intermediate files
		if (deleteIntermediateFiles) {

			//false if one or more files cannot be deleted
			boolean successfulDelete = true;

			if (simModelDefinition.exists()) {
				if (!simModelDefinition.delete()) {
					successfulDelete = false;
					//try in another way
					simModelDefinition.deleteOnExit();
				}
			}
			if (simResults.exists()) {
				if (!simResults.delete()) {
					successfulDelete = false;
					//try in another way
					simResults.deleteOnExit();
				}
			}

			if (successfulDelete) {
				logger.debug("Intermediate files deleted for model " + modelDefinitionPath);
			} else {
				logger.warn("Could not delete some intermediate files. They will be deleted on JVM exit.");
			}
		}

		return true;
	}

	/**
	 * Transforms model using new Java class to perform transformation instead of old bugged XSLT transformer.
	 * Avoid to measure elements with null service demand.
	 * @param inputModel input model in exact solver format
	 * @param outputModel output model in simulator format
	 * @return open ExactModel or null if an error occurred
	 */
	public ExactModel getTransformedModel(String inputModel, String outputModel) {
		/*********************DEFINITION MODEL*********************/
		File input = new File(inputModel);
		if (!input.exists()) {
			//the passed file does not exist
			logger.error("The model file " + inputModel + " does not exist...");
			return null;
		} else {
			logger.debug("Model definition path: " + input.getAbsolutePath());
		}

		/*********************SIM DEFINITION MODEL*********************/

		//Loads ExactModel and transforms it to CommonModel
		ExactModel exact = new ExactModel();
		XMLUtils xmlUtils = new XMLUtils();
		Document inputDoc;
		try {
			inputDoc = xmlUtils.loadXML(input);
			exact.loadDocument(inputDoc);
		} catch (SAXException e1) {
			logger.error("Cannot parse correctly input JMVA model: " + e1.getMessage());
			return null;
		} catch (IOException e1) {
			logger.error("I/O error opening input JMVA model: " + e1.getMessage());
			return null;
		}
		logger.debug("Input model opened correctly...");
		CommonModel simModel = new CommonModel();
		List<String> warnings = ModelConverter.convertJMVAtoJSIM(exact, simModel);
		// Last element of warning is transform matrix
		for (int i = 0; i < warnings.size(); i++) {
			logger.warn("Warning during conversion: " + warnings.get(i));
		}
		// Removes all measures from classes with null service demands
		removeNullMeasures(exact, simModel);
		logger.debug("Removed null measures from simulator model");

		Document modelDocument = XMLWriter.getDocument(simModel, outputModel);
		logger.debug("Converted JMVA xml to JSIM one");
		// Adds blocking region informations to model document
		NodeList blockingList = inputDoc.getElementsByTagName(XMLConstantNames.XML_E_REGION);
		// If model has one blocking region only, adds router to it (otherwise a job will exit from
		// region when it's not requested
		if (blockingList.getLength() == 1) {
			Element region = (Element) blockingList.item(0);
			Element router = inputDoc.createElement(XMLConstantNames.XML_E_REGIONNODE);
			router.setAttribute(XMLConstantNames.XML_A_REGIONNODE_NAME, "Router");
			region.insertBefore(router, region.getFirstChild());
		}
		Element root = modelDocument.getDocumentElement();
		NodeList preload = root.getElementsByTagName(XMLConstantNames.XML_E_PRELOAD);
		NodeList measures = root.getElementsByTagName(XMLConstantNames.XML_E_MEASURE);
		Node lastMeasure = measures.getLength() > 0 ? measures.item(measures.getLength() - 1) : null;
		for (int i = 0; i < blockingList.getLength(); i++) {
			Element imported = (Element) modelDocument.importNode(blockingList.item(i), true);
			String name = imported.getAttribute(XMLConstantNames.XML_A_REGION_NAME);
			if (preload.getLength() > 0) {
				root.insertBefore(imported, preload.item(0));
			} else {
				root.appendChild(imported);
			}
			// Adds blocking region measures
			Element dropRate = (Element) lastMeasure.cloneNode(true);
			dropRate.setAttribute(XMLConstantNames.XML_A_MEASURE_NODETYPE, XMLConstantNames.NODETYPE_REGION);
			dropRate.setAttribute(XMLConstantNames.XML_A_MEASURE_STATION, name);
			dropRate.setAttribute(XMLConstantNames.XML_A_MEASURE_TYPE, SimulationDefinition.MEASURE_DR);
			// For each class add drop rate measure
			for (int j = 0; j < exact.getClasses(); j++) {
				Element cloned = (Element) dropRate.cloneNode(true);
				cloned.setAttribute(XMLConstantNames.XML_A_MEASURE_CLASS, exact.getClassNames()[j]);
				cloned.setAttribute(XMLConstantNames.XML_A_MEASURE_NAME, exact.getClassNames()[j] + "_" + name + "_Drop Rate");
				root.insertBefore(cloned, lastMeasure);
				lastMeasure = cloned;
			}
		}
		logger.debug("Added finite capacity regions");

		XMLWriter.writeXML(outputModel, modelDocument);

		logger.debug("JSIM model definition wrote to disk");

		return exact;
	}

	/**
	 * Solves model using new Java class to perform transformation instead of old bugged XSLT transformer.
	 * Avoid to measure elements with null service demand.
	 * @return true if model was solved correctly, false otherwise.
	 */
	public boolean solveModelNewTransform() {
		boolean transfSuccess = false;
		//get file name: same directory of model file, with the same name preceeded by "sim_"
		String modelDefinitionParent = modelDefinition.getParent();

		if (modelDefinitionParent.endsWith(SEPARATOR)) {
			simModelDefinitionPath = modelDefinitionParent + "sim_" + modelDefinition.getName();
		} else {
			simModelDefinitionPath = modelDefinitionParent + SEPARATOR + "sim_" + modelDefinition.getName();
		}

		ExactModel inputModel = getTransformedModel(modelDefinitionPath, simModelDefinitionPath);
		if (inputModel == null) {
			return false;
		}

		/*********************SIMULATION LOADING AND RUNNING*********************/

		//sim model successfully created
		//now prepare simulation
		try {
			SimLoader simLoader = new SimLoader(simModelDefinitionPath);
			Simulation sim = simLoader.getSim();
			//sim.MONITOR_BLOCKING_REGION = true;            
			//sets in the Simulation object the path of the original (i.e. MVA)
			//xml model definition
			sim.setXmlModelDefPath(modelDefinitionPath);

			if (!automaticSeed) {
				//if automaticSeed == false, then a user defined seed is present
				sim.setRandomEngineSeed(simulationSeed);
				sim.initialize();

				logger.debug("jSIM correctly initialized with simulation seed = " + simulationSeed);
			} else {
				sim.initialize();
				logger.debug("jSIM correctly initialized");
			}

			//Avoid saturation
			if (sim.getAllRegions() == null) {
				//if there are no regions...
				boolean sufficientProcessingCapacity = inputModel.checkSaturation() == ExactModel.NO_SATURATION;

				if (!sufficientProcessingCapacity) {
					logger.warn("Current workload causes system saturation. Simulation will not be started.");
					return false;
				}
			}

			simStarted = true;
			net = sim.getNetwork();

			long start, stop;
			double elapsed;

			//Start time
			start = System.currentTimeMillis();

			sim.run();

			//stop time
			stop = System.currentTimeMillis();
			elapsed = ((stop - start)) / 1000.0;

			simFinished = true;

			logger.info("Model " + modelDefinitionPath + " solved by jSIM in " + Double.toString(elapsed) + " seconds");

		} catch (IOException e) {
			e.printStackTrace();
			logger.error(e.getMessage());
			return false;
		} catch (LoadException e) {
			e.printStackTrace();
			logger.error(e.getMessage());
			return false;
		} catch (Exception e) {
			e.printStackTrace();
			logger.error(e.getMessage());
			return false;
		}

		simModelDefinition = new File(simModelDefinitionPath);

		/*********************COPYING SIMULATION RESULTS*********************/

		//the results file has been obtained
		//the file has been saved in the same directory of model file, with the same name preceeded by "res_sim_"

		if (modelDefinitionParent.endsWith(SEPARATOR)) {
			simResultsPath = modelDefinition.getParent() + "res_" + simModelDefinition.getName();
		} else {
			simResultsPath = modelDefinition.getParent() + SEPARATOR + "res_" + simModelDefinition.getName();
		}

		simResults = new File(simResultsPath);

		logger.debug("jSIM results path: " + simResultsPath);

		//transforms the SIMmodeloutput into JMTmodel

		//setup transformer
		ModelTransformer md = new ModelTransformer();
		md.setValidate(validate);
		//create temp file to contain model definition and results
		File temp = null;
		String tempPath = null;

		try {
			temp = File.createTempFile("~jmt_model", ".xml", modelDefinition.getParentFile());
			tempPath = temp.getAbsolutePath();
		} catch (IOException e) {
			e.printStackTrace();
			logger.error(e.getMessage());
			logger.error("Error in creating temp file for model " + modelDefinitionPath);

			return false;
		}

		transfSuccess = md.OUTtoMVA(simResultsPath, tempPath);

		if (!transfSuccess) {
			logger.error("OUT to MVA transformation failed model " + modelDefinitionPath);
			return false;
		} else {
			//if successful, rename to model definition file, after deleting the old version
			if (modelDefinition.exists()) {
				modelDefinition.delete();
			}

			// Put in place all measures from classes with null service demands and adjusts throughputs
			Document outDocument;
			XMLUtils xmlUtils = new XMLUtils();
			try {
				outDocument = xmlUtils.loadXML(temp);
				addNullMeasuresCorrectThroughputs(inputModel, outDocument);
			} catch (SAXException e2) {
				logger.error("Cannot parse correctly output JMVA model: " + e2.getMessage());
				return false;
			} catch (IOException e1) {
				logger.error("I/O error opening output JMVA model: " + e1.getMessage());
				return false;
			}
			logger.debug("Added null measures to final results");

			try {
				xmlUtils.saveXML(outDocument, modelDefinition);
			} catch (Exception e3) {
				logger.error("could not copy results into model " + modelDefinitionPath);
				return false;
			}

			resultsObtained = true;
			logger.debug("jSIM results copied into model " + modelDefinitionPath);

			//delete temp file
			if (!temp.delete()) {
				temp.deleteOnExit();
				logger.warn("Could not delete a temp file. Deletion will be retried on JVM exit.");
			}

		}

		/*********************DELETE INTERMEDIATE FILES*********************/

		//if true delete intermediate files
		if (deleteIntermediateFiles) {

			//false if one or more files cannot be deleted
			boolean successfulDelete = true;

			if (simModelDefinition.exists()) {
				if (!simModelDefinition.delete()) {
					successfulDelete = false;
					//try in another way
					simModelDefinition.deleteOnExit();
				}
			}
			if (simResults.exists()) {
				if (!simResults.delete()) {
					successfulDelete = false;
					//try in another way
					simResults.deleteOnExit();
				}
			}

			if (successfulDelete) {
				logger.debug("Intermediate files deleted for model " + modelDefinitionPath);
			} else {
				logger.warn("Could not delete some intermediate files. They will be deleted on JVM exit.");
			}
		}

		return true;
	}

	/**
	 * allows to know the state of simulation
	 * @return the percentage of progress (= number of computed confidence intervals /
	 * total number of required confidence intervals)
	 */
	public double checkSimProgress() {
		if (!simStarted) {
			//not started yet
			return 0.0;
		}
		if (simFinished) {
			//already finished
			return 1.0;
		}
		try {
			//compute progress

			if (pauseSim()) {
				double progr = NetSystem.checkProgress(net);
				restartSim();
				return progr;
			} else {
				return 0;
			}

		} catch (NetException e) {
			return 0.0;
		}

	}

	/**
	 * Pauses the simulation, refreshes temp measures and then restarts simulation
	 */
	public void refreshTempMeasures() {

		//simulation not started yet
		if (!simStarted) {
			return;
		}

		//pauses the computation
		if (pauseSim()) {

			//at the first execution, measures must be retrieved
			if (tempMeasures == null) {
				if (net == null) {
					return;
				} else {
					//gets the measures defined for this queue network
					LinkedList measures = net.getMeasures();

					//creates the array of temp measures
					tempMeasures = new TempMeasure[measures.size()];

					for (int m = 0; m < measures.size(); m++) {
						tempMeasures[m] = new TempMeasure((Measure) measures.get(m));
						tempMeasures[m].refreshMeasure();
					}
				}
			} else {
				//temp measures have already been retrieved
				//only refresh temp values
				for (TempMeasure tempMeasure : tempMeasures) {
					tempMeasure.refreshMeasure();
				}
			}

			//restart computation
			restartSim();

		}
		return;
	}

	/**
	 * gets the temp measures
	 * @return the temp measure
	 */
	public TempMeasure[] getTempMeasures() {
		return tempMeasures;
	}

	public boolean abortAllMeasures() {
		if (!simStarted) {
			return false;
		}

		if (simFinished) {
			return true;
		}

		if (pauseSim()) {
			refreshTempMeasures();

			logger.debug("All remaining measures will be aborted");
			for (int m = 0; m < tempMeasures.length; m++) {
				abortMeasure(m);
			}

			return true;

		} else {
			return false;
		}
	}

	public boolean abortMeasure(int index) {
		boolean success = false;

		if (pauseSim()) {
			refreshTempMeasures();

			if ((0 <= index) && (index <= tempMeasures.length)) {
				success = tempMeasures[index].abort();
			} else {
				success = false;
			}
			restartSim();
		}

		return success;
	}

	/**
	 * Prints on System.out the temp values of measures that haven't finished yet
	 */
	public void printTempMeasures() {

		if (pauseSim()) {
			if (tempMeasures != null) {
				for (TempMeasure temp : tempMeasures) {
					if (temp.isFinished()) {
						continue;
					} else {
						System.out.println(temp.getName() + ": " + Double.toString(temp.getTempMean()) + " " + temp.getNsamples());
					}
				}
			}
			restartSim();
		}
		return;

	}

	/**
	 * Pauses the simulation
	 * @return true if the operation was successful, false otherwise (for example
	 * simulation not started or already finished)
	 */
	private boolean pauseSim() {
		return NetSystem.pause();
	}

	/**
	 * Resumes the simulation
	 * @return true if the operation was successful, false otherwise (for example
	 * simulation not started or already finished)
	 */
	private boolean restartSim() {
		return NetSystem.restartFromPause();
	}

	//------------------------TEST-----------------------------------//

	public static void testSimulationTime() {

		int N = 5;
		long[] duration = new long[N];
		long tot = 0;

		for (int i = 0; i < N; i++) {

			String path = "D:\\JMTtest\\prova" + i + ".xml";

			Dispatcher_jMVAschema disp = new Dispatcher_jMVAschema(path);
			long start, stop, elapsed;

			start = System.currentTimeMillis();

			if (disp.solveModel()) {
				stop = System.currentTimeMillis();
				elapsed = stop - start;
				//System.out.println("Duration execution "+i+": ");
				System.out.println(Long.toString(elapsed));

				duration[i] = elapsed;
				tot += elapsed;

			} else {
				System.out.println("Error!!");
			}
		}

		long mean = tot / N;
		System.out.println("Mean: ");
		System.out.println(Long.toString(mean));

	}

	public static void testSolution() {
		//Dispatcher disp = new Dispatcher("D:\\test_2open.xml");
		Dispatcher_jMVAschema disp = new Dispatcher_jMVAschema("D:\\JMTtest\\solverstep0.xml");
		//Dispatcher disp = new Dispatcher("D:\\JMTtest\\solverstep10.xml");
		//Dispatcher disp = new Dispatcher("D:\\JMTtest\\prova.xml");
		//Dispatcher disp = new Dispatcher("D:\\JMTtest\\test_base.xml");

		//Dispatcher disp = new Dispatcher("D:\\JMTtest\\model_with_blocking.xml");
		//Dispatcher disp = new Dispatcher("D:\\JMTtest\\model_with_blocking_2.xml");
		//Dispatcher disp = new Dispatcher("D:\\JMTtest\\model_with_blocking_open_drop.xml");
		//Dispatcher disp = new Dispatcher("D:\\JMTtest\\preload.xml");
		disp.setSimulationSeed(23000);

		long start, stop, elapsed;
		start = System.currentTimeMillis();

		if (disp.solveModel()) {
			stop = System.currentTimeMillis();
			elapsed = stop - start;
			//System.out.println("OK - Simulation time: ");
			//System.out.println(Long.toString(elapsed));
		} else {
			System.out.println("Error!!");
		}

	}

	private boolean copyFile(File input, File output_file) {

		FileInputStream inputStream = null;
		FileOutputStream outputStream_file1 = null;
		int fileLenght = 0;

		try {
			inputStream = new FileInputStream(input.getAbsolutePath());
		} catch (IOException e) {
			e.printStackTrace();
			logger.error("Error while reading input file " + input.getAbsolutePath());
			return false;
		}

		// try to open the new files, to be written with FileOutputStream.

		try {
			outputStream_file1 = new FileOutputStream(output_file);
		} catch (IOException e) {
			e.printStackTrace();
			logger.error("Error while creating the copy of input file " + input.getAbsolutePath());
			return false;
		}

		try {
			//number of available bytes
			fileLenght = inputStream.available();
		} catch (IOException e) {
			e.printStackTrace();
			logger.error("Error while computing the length of input file " + input.getAbsolutePath());
			return false;
		}

		// Reads the bytes from input stream and copies them into the output stream
		// using FileInputStream.read() and FileOutputStream.write().

		try {
			int byte_read;
			for (int i = 0; i < fileLenght; i++) {
				//reads one byte
				byte_read = inputStream.read();
				//copies one byte
				outputStream_file1.write(byte_read);
			}
		} catch (IOException e) {
			e.printStackTrace();
			logger.error("Error while copying input file " + input.getAbsolutePath());
			return false;
		}

		// The file has been copied, now closes the stream...

		try {
			// closes InputStream
			inputStream.close();
			// closes OutputStream
			outputStream_file1.close();
		} catch (IOException e) {
			e.printStackTrace();
			logger.error("Error while closing the copy of input file " + input.getAbsolutePath());
			return false;
		}

		// OK, file has been copied
		return true;
	}

	/**
	 * A system is said to have sufficient capacity to process a given load
	 * <tt>lambda</tt> if no service center is saturated as a result of the combined loads
	 * of all the open classes.
	 * <br>
	 * WARNING: This method should be called before solving the system.
	 * @return true if sufficient capacity exists for the given workload, false otherwise
	 */
	public boolean checkProcessingCapacity() {
		ExactModel exact = new ExactModel();
		XMLUtils xmlUtils = new XMLUtils();
		Document inputDoc;
		try {
			inputDoc = xmlUtils.loadXML(modelDefinition);
			exact.loadDocument(inputDoc);
		} catch (SAXException e1) {
			logger.error("Cannot parse correctly input JMVA model: " + e1.getMessage());
			return false;
		} catch (IOException e1) {
			logger.error("I/O error opening input JMVA model: " + e1.getMessage());
			return false;
		}
		return exact.checkSaturation() == ExactModel.NO_SATURATION;
	}

	/**
	 * Sets if parser must validate 
	 * @param validate true if parser must validate, false otherwise
	 */
	public void setValidate(boolean validate) {
		this.validate = validate;
	}

	/**
	 * Removes all null measures from output model. A null measure is a measure from a class
	 * with 0 service demand at a station. Removed measures are stored to be later restored.
	 * @param input input JMVA model
	 * @param output output JSIM model
	 */
	protected void removeNullMeasures(ExactModel input, CommonModel output) {
		// Finds a mapping between indices in ExactModel data structure and in CommonModel one
		Object[] classKeys = new Object[input.getClasses()];
		Object[] stationKeys = new Object[input.getStations()];
		for (int i = 0; i < input.getClasses(); i++) {
			classKeys[i] = output.getClassByName(input.getClassNames()[i]);
		}
		for (int i = 0; i < input.getStations(); i++) {
			stationKeys[i] = output.getStationByName(input.getStationNames()[i]);
		}

		// Search for null elements and remove them
		for (int i = 0; i < input.getStations(); i++) {
			for (int j = 0; j < input.getClasses(); j++) {
				if (input.getVisits()[i][j] == 0
						|| (input.getServiceTimes()[i][j][0] == 0 && input.getStationTypes()[i] != ExactConstants.STATION_LD)) {
					// Removes all measures from station i for class j
					Iterator it = output.getMeasureKeys().iterator();
					while (it.hasNext()) {
						Object key = it.next();
						if (output.getMeasureClass(key) == classKeys[j] && output.getMeasureStation(key) == stationKeys[i]) {
							it.remove();
						}
					}
				}
			}
		}
	}

	/**
	 * Adds all removed null measures to output document
	 * @param input input exactmodel
	 * @param output output document
	 */
	protected void addNullMeasuresCorrectThroughputs(ExactModel input, Document output) {
		Element solutions = (Element) output.getElementsByTagName("solutions").item(0);
		// Search for null elements adds them
		for (int i = 0; i < input.getStations(); i++) {
			// for all stations
			NodeList stations = solutions.getElementsByTagName("stationresults");
			Element station = null;
			for (int k = 0; k < stations.getLength(); k++) {
				station = (Element) stations.item(k);
				if (station.getAttribute("station").equals(input.getStationNames()[i])) {
					break;
				}
			}
			for (int j = 0; j < input.getClasses(); j++) {
				// for all classes
				NodeList classes = station.getElementsByTagName("classresults");
				Element customerclass = null;
				for (int k = 0; k < stations.getLength(); k++) {
					customerclass = (Element) classes.item(k);
					if (customerclass.getAttribute("customerclass").equals(input.getClassNames()[j])) {
						break;
					}
				}
				if (input.getVisits()[i][j] == 0
						|| (input.getServiceTimes()[i][j][0] == 0 && input.getStationTypes()[i] != ExactConstants.STATION_LD)) {
					// Create a null measure for each measure type
					for (String element : ExactConstants.INDICES_TYPES) {
						Element measure = output.createElement("measure");
						measure.setAttribute("measureType", element);
						measure.setAttribute("meanValue", "0.0");
						measure.setAttribute("successful", "true");
						measure.setAttribute("lowerLimit", "0.0");
						measure.setAttribute("upperLimit", "0.0");
						customerclass.appendChild(measure);
					}
				}
			}
		}
	}

	public static void main(String[] args) {
		final Dispatcher_jMVAschema disp = new Dispatcher_jMVAschema("c:\\test\\test.xml");
		disp.setValidate(false);
		//disp.getTransformedModel("c:\\test\\test.xml", "c:\\test\\test1.xml");
		new Thread() {
			@Override
			public void run() {
				try {
					sleep(60 * 1000);
				} catch (InterruptedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				disp.abortAllMeasures();
			}
		}.start();
		disp.solveModelNewTransform();
	}
}
