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

package jmt.engine.testSystem;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;

import javax.xml.parsers.DocumentBuilderFactory;

import jmt.analytical.CommandLineSolver;
import jmt.common.exception.LoadException;
import jmt.common.xml.resources.XSDSchemaLoader;
import jmt.engine.simDispatcher.Dispatcher_jMVAschema;

import org.apache.log4j.Logger;
import org.apache.xerces.parsers.DOMParser;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;

/**
 * Solves the same model with mva and simulator, then checks
 * whether the results are correct.
 * The result of test are written on the log file of BatchTest class
 *
 * @author Stefano
 * @version 4-mar-2005 11.02.23
 */
public class ResultsCheck {

	public static final boolean DEBUG = false;

	private String model_path;
	private String jmva_model_path;
	private String jsim_model_path;

	private File model_file;
	private File jmva_model_file;
	private File jsim_model_file;

	//retrieves the reference to the BatchTest logger
	private Logger logger = Logger.getLogger(BatchTest.class);

	//if true, more info are written to log (for example, precision, maxsamples, ecc..)
	//if false, only basic info are written
	private boolean extendedInfo;

	//if true jsim will always use a automatic seed for simulation
	private boolean automaticSeed = true;
	//user defined seed for simulation
	private long userDefinedSeed;
	//number of simulation runs for this test
	private int simulationRunsCounter = 1;

	//if true jsim will delete the intermediate files used for resolution
	private boolean deleteIntermediateSimFiles = false;

	/**
	 * Creates a ResultCheck object.
	 * @param filePath the model to be tested
	 * @param extendedInfo if true, more test simulation info will be logged
	 */
	public ResultsCheck(String filePath, boolean extendedInfo) {

		model_path = filePath;
		model_file = new File(filePath);
		this.extendedInfo = extendedInfo;
		automaticSeed = true;
	}

	/**
	 * Creates a ResultCheck object.
	 * @param filePath the model to be tested
	 * @param extendedInfo if true, more test simulation info will be logged
	 * @param simSeed the simulationon seed to be used (use this feature only for test)
	 */
	public ResultsCheck(String filePath, boolean extendedInfo, long simSeed) {

		model_path = filePath;
		model_file = new File(filePath);
		this.extendedInfo = extendedInfo;
		automaticSeed = false;
		userDefinedSeed = simSeed;

	}

	public void checkRes(int runs) {
		simulationRunsCounter = 0;

		for (int i = 1; i <= runs; i++) {
			simulationRunsCounter++;
			if (!checkRes()) {
				logger.error("Simulation run number " + i + "failed for model file" + model_path);
			}
		}
	}

	/**
	 * Solves the model with jMVA and jSIM, then checks if all simulation
	 * confidence intervals contain the exact solution.
	 *
	 */

	public boolean checkRes() {

		//verifies if the passed file exists
		if (!model_file.exists()) {
			//the passed file does not exist
			logger.error("The model file " + model_path + "does not exist.");
			return false;
		}

		//creates two copy of the same file, which will be passed to the risolution engines

		jmva_model_path = model_file.getParent() + "\\jmva_" + model_file.getName();
		jsim_model_path = model_file.getParent() + "\\jsim_" + model_file.getName();

		//creates empty files
		jmva_model_file = new File(jmva_model_path);
		jsim_model_file = new File(jsim_model_path);

		if (!copyFile(model_file, jmva_model_file, jsim_model_file)) {
			//error during copy
			logger.error("Cannot create copies of the model file " + model_path);

		};

		if (!solve_jmva(jmva_model_file)) {
			logger.error("The model " + jmva_model_path + " could not be solved using jMVA.");
			return false;
		} else {
			logger.debug(jmva_model_path + ": jMVA solution has been completed.");
		}

		if (!solve_jsim(jsim_model_file)) {
			logger.error("The model " + jsim_model_path + " could not be solved using jSIM.");
			return false;
		} else {
			logger.debug(jsim_model_path + ": jSIM solution has been completed.");
		}

		//both solutions are available

		//retrieves a DOM representation from xml files
		Document jmva_document, jsim_document;
		try {
			jmva_document = load(new FileInputStream(jmva_model_file));
			jsim_document = load(new FileInputStream(jsim_model_file));
		} catch (FileNotFoundException e) {
			e.printStackTrace();
			return false;
		}

		createTestReport(jmva_document, jsim_document);

		return true;

	}

	/**
	 * Creates two copies of the given input File.
	 * @param input the File to be copied
	 * @param output_file1 the first copy
	 * @param output_file2 the second copy
	 * @return true if the copy files have been successfully created, false otherwise
	 */

	private boolean copyFile(File input, File output_file1, File output_file2) {

		FileInputStream inputStream = null;
		FileOutputStream outputStream_file1 = null;
		FileOutputStream outputStream_file2 = null;
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
			outputStream_file1 = new FileOutputStream(output_file1);
			outputStream_file2 = new FileOutputStream(output_file2);
		} catch (IOException e) {
			e.printStackTrace();
			logger.error("Error while creating the copies of input file " + input.getAbsolutePath());
			return false;
		}

		try {
			//number of available bytes
			fileLenght = inputStream.available(); // Mi informo sul num. bytes.
		} catch (IOException e) {
			e.printStackTrace();
			logger.error("Error while computing the length of input file " + input.getAbsolutePath());
			return false;
		}

		// Reads the bytes from input stream and copies them into the two output streams
		// using FileInputStream.read() and FileOutPutStream.write().

		try {
			int byte_read;
			for (int i = 0; i < fileLenght; i++) {
				//reads one byte
				byte_read = inputStream.read();
				//copies one byte
				outputStream_file1.write(byte_read);
				outputStream_file2.write(byte_read);
			}
		} catch (IOException e) {
			e.printStackTrace();
			logger.error("Error while copying input file " + input.getAbsolutePath());
			return false;
		}

		// The files have been copied, now closes the streams...

		try {
			// closes InputStream
			inputStream.close();
			// closes OutputStream
			outputStream_file1.close();
			outputStream_file2.close();
		} catch (IOException e) {
			e.printStackTrace();
			logger.error("Error while closing the copies of input file " + input.getAbsolutePath());
			return false;
		}

		// OK, file have been copied
		return true;
	}

	/**
	 * Solves the given model (xml file) using the jmva solver
	 * @param model the xml File containing the model description
	 * @return true if the model has been correctly solved
	 */
	private boolean solve_jmva(File model) {
		CommandLineSolver solver = new CommandLineSolver();

		long start, stop, elapsed;
		start = System.currentTimeMillis();
		boolean success = solver.solve(model);
		stop = System.currentTimeMillis();
		elapsed = stop - start;

		logger.debug("Model " + model.getAbsolutePath() + " solved in " + elapsed + " milliseconds.");

		return success;
	}

	/**
	 * Solves the given model (xml file) using the jsim solver
	 * @param model the xml File containing the model description
	 * @return true if the model has been correctly solved
	 */
	private boolean solve_jsim(File model) {

		Dispatcher_jMVAschema solver = new Dispatcher_jMVAschema(model);
		solver.setDeleteIntermediateFiles(deleteIntermediateSimFiles);
		if (!automaticSeed) {
			solver.setSimulationSeed(userDefinedSeed);
		}
		boolean success = solver.solveModel();
		return success;
	}

	/**
	 * Compares the solutions of jMVA and jSIM and writes on log.
	 * The wealth of information depends on the value of the "extendedInfo" property.
	 * This method can be called only after solving the model in both ways
	 * @return an XML file containing the report of solutions check
	 */
	private File createTestReport(Document jmva_doc, Document jsim_doc) {

		//data structures which will contain the results

		//computes the number of measures
		int measuresNumber = jsim_doc.getElementsByTagName("measure").getLength();

		logger.debug(measuresNumber + " found in jSIM results.");

		//list of all station results object
		NodeList statRes = jsim_doc.getElementsByTagName("stationresults");
		int statResLength = statRes.getLength();

		//loop over all station results
		for (int s = 0; s < statResLength; s++) {
			//current station element
			Element currStation = (Element) statRes.item(s);
			String currStationName = currStation.getAttribute("station");

			//list of all class results for the current station
			NodeList classRes = currStation.getElementsByTagName("classresults");
			int classResLength = classRes.getLength();

			//loop over all class results
			for (int c = 0; c < classResLength; c++) {
				//current class element
				Element currClass = (Element) classRes.item(c);
				String currClassName = currClass.getAttribute("customerclass");

				//list of all measures
				NodeList measRes = currClass.getElementsByTagName("measure");
				int measResLength = measRes.getLength();

				//loop over all measures
				for (int m = 0; m < measResLength; m++) {
					Element currMeasure = (Element) measRes.item(m);
					String currMeasureName = currMeasure.getAttribute("measureType");

					//measure data
					boolean success = currMeasure.getAttribute("successful").equalsIgnoreCase("true");

					double mean, lower, upper, exact;
					int max, analyzed, discarded;
					double alfa, precision;
					String msg, extendedMsg = "";

					boolean correctConfidenceInterval;

					String modelName = model_file.getName();
					int simulationRun = simulationRunsCounter;

					exact = findExactValue(jmva_doc, currStationName, currClassName, currMeasureName);

					if (success) {
						//confidence interval was successfully computed
						//exact value may be contained or not contained in this interval

						lower = Double.parseDouble(currMeasure.getAttribute("lowerLimit"));
						mean = Double.parseDouble(currMeasure.getAttribute("meanValue"));
						upper = Double.parseDouble(currMeasure.getAttribute("upperLimit"));

						if (extendedInfo) {
							max = Integer.parseInt(currMeasure.getAttribute("maxSamples"));
							analyzed = Integer.parseInt(currMeasure.getAttribute("analyzedSamples"));
							discarded = Integer.parseInt(currMeasure.getAttribute("discardedSamples"));
							alfa = Double.parseDouble(currMeasure.getAttribute("alfa"));
							precision = Double.parseDouble(currMeasure.getAttribute("precision"));

							extendedMsg = alfa + ";" + precision + ";" + analyzed + ";" + discarded + ";" + max + ";";
						}

						//check whether the exact value is contained into the confidence interval

						if ((exact >= lower) && (exact <= upper)) {
							//is contained
							correctConfidenceInterval = true;

							msg = modelName + ";" + simulationRun + ";" + currStationName + ";" + currClassName + ";" + currMeasureName + ";" + exact
									+ ";" + lower + ";" + mean + ";" + upper + ";" + "CORRECT" + ";";
							if (extendedInfo) {
								msg = msg + extendedMsg;
							}
							logger.info(msg);

						} else {
							//is not contained
							correctConfidenceInterval = false;

							msg = modelName + ";" + simulationRun + ";" + currStationName + ";" + currClassName + ";" + currMeasureName + ";" + exact
									+ ";" + lower + ";" + mean + ";" + upper + ";" + "NOT CORRECT" + ";";
							if (extendedInfo) {
								msg = msg + extendedMsg;
							}

							logger.warn(msg);
						}

					} else {
						//confidence interval not successfully computed
						correctConfidenceInterval = false;

						lower = Double.parseDouble(currMeasure.getAttribute("lowerLimit"));
						mean = Double.parseDouble(currMeasure.getAttribute("meanValue"));
						upper = Double.parseDouble(currMeasure.getAttribute("upperLimit"));

						if (extendedInfo) {
							max = Integer.parseInt(currMeasure.getAttribute("maxSamples"));
							analyzed = Integer.parseInt(currMeasure.getAttribute("analyzedSamples"));
							discarded = Integer.parseInt(currMeasure.getAttribute("discardedSamples"));
							alfa = Double.parseDouble(currMeasure.getAttribute("alfa"));
							precision = Double.parseDouble(currMeasure.getAttribute("precision"));

							extendedMsg = alfa + ";" + precision + ";" + analyzed + ";" + discarded + ";" + max + ";";
						}

						msg = modelName + ";" + simulationRun + ";" + currStationName + ";" + currClassName + ";" + currMeasureName + ";" + exact
								+ ";" + lower + ";" + mean + ";" + upper + ";" + "NOT COMPUTED" + ";";
						if (extendedInfo) {
							msg = msg + extendedMsg;
						}

						logger.warn(msg);

					}
				}

			}

		}

		return null;
	}

	/**
	 * Returns the exact value of the measure requested by the user
	 * @param jmvaDocument Document containing the mva solutions
	 * @param stat the station measure refers to
	 * @param cls the class measure refers to
	 * @param type the measure type
	 * @return the exact value of the measure, NaN if the requested measure does not exist
	 */
	private double findExactValue(Document jmvaDocument, String stat, String cls, String type) {
		//retrieves station results
		NodeList statRes = jmvaDocument.getElementsByTagName("stationresults");
		int statResLength = statRes.getLength();

		for (int i = 0; i < statResLength; i++) {
			//current station
			Element currentStat = (Element) statRes.item(i);

			if (currentStat.getAttribute("station").equalsIgnoreCase(stat)) {
				//it's the correct station

				//retrieves class results of this station
				NodeList classRes = currentStat.getElementsByTagName("classresults");
				int classResLength = classRes.getLength();

				for (int j = 0; j < classResLength; j++) {
					//current class
					Element currentCls = (Element) classRes.item(j);

					if (currentCls.getAttribute("customerclass").equalsIgnoreCase(cls)) {
						//it's the correct class

						//retrieves the measures of this (station, class)
						NodeList measures = currentCls.getElementsByTagName("measure");
						int measureLength = measures.getLength();

						for (int k = 0; k < measureLength; k++) {
							//current measure
							Element currentMeas = (Element) measures.item(k);

							if (currentMeas.getAttribute("measureType").equalsIgnoreCase(type)) {
								//it's the requested measure

								double exactValue = Double.parseDouble(currentMeas.getAttribute("meanValue"));
								return exactValue;

							}

							//else iterates to next measure
						}
					}
					//else iterates to next class
				}
			}
			//else iterates to next station
		}

		//this point should never been reached!!!

		return Double.NaN;
	}

	/**
	 * Load an xml file and returns the corresponding DOM object
	 * @param is Input Stream
	 * @return the DOM representation of the file
	 * @throws FileNotFoundException
	 */
	private Document load(InputStream is) throws FileNotFoundException {
		if (is == null) {
			throw new FileNotFoundException("File not Found");
		}
		InputSource inputSource = new InputSource(is);
		//create a parser
		DOMParser parser = new DOMParser();
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setValidating(true);
		factory.setNamespaceAware(true);

		Document document = null;

		try {
			// turn on schema validation ( note need to set both sax and dom validation )
			parser.setFeature("http://xml.org/sax/features/validation", true);
			parser.setFeature("http://apache.org/xml/features/validation/schema", true);
			parser.setFeature("http://apache.org/xml/features/validation/schema-full-checking", true);

			//NEW
			//TODO: setto lo schema xsd con cui fare il parsing
			String externalSchemaLocation = XSDSchemaLoader.loadSchema(XSDSchemaLoader.JMVA_MODEL_DEFINITION);
			parser.setProperty("http://apache.org/xml/properties/schema/external-noNamespaceSchemaLocation", externalSchemaLocation);
			//end NEW

			try {
				//document parsing
				parser.parse(inputSource);
			} catch (FileNotFoundException e) {
				throw new LoadException("Problems parsing", e);
			}

			//get the w3c document
			document = parser.getDocument();

		} catch (SAXNotRecognizedException e) {
			e.printStackTrace();
		} catch (SAXNotSupportedException e) {
			e.printStackTrace();
		} catch (SAXException sxe) {
			// Error generated during parsing
			Exception x = sxe;
			if (sxe.getException() != null) {
				x = sxe.getException();
			}
			x.printStackTrace();

		} catch (IOException e) {
			e.printStackTrace();
		} catch (Exception e) {
			e.printStackTrace();
		}

		return document;
	}

}
