/**    
  * Copyright (C) 2009, Laboratorio di Valutazione delle Prestazioni - Politecnico di Milano

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

/**
 * 2013. Modified for QN-Java project
 * 
 * 
 * [2013-06-24] Add QN-ACTR simulation unit to JMT simulation object, at the end of initialize(). See places with // QN-Java marks. 
 * 
 */

package jmt.engine.simEngine;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.JOptionPane;

import jmt.common.exception.LoadException;
import jmt.common.exception.NetException;
import jmt.engine.NodeSections.BlockingQueue;
import jmt.engine.NodeSections.BlockingRouter;
import jmt.engine.NodeSections.InputSection;
import jmt.engine.NodeSections.OutputSection;
import jmt.engine.NodeSections.Queue;
import jmt.engine.NodeSections.RandomSource;
import jmt.engine.NodeSections.Router;
import jmt.engine.NodeSections.ServiceSection;
import jmt.engine.NodeSections.ServiceTunnel;
import jmt.engine.NodeSections.Terminal;
import jmt.engine.QueueNet.BlockingRegion;
import jmt.engine.QueueNet.GlobalJobInfoList;
import jmt.engine.QueueNet.JobClass;
import jmt.engine.QueueNet.NetNode;
import jmt.engine.QueueNet.NetSystem;
import jmt.engine.QueueNet.NodeSection;
import jmt.engine.QueueNet.QueueNetwork;
import jmt.engine.QueueNet.SimConstants;
import jmt.engine.dataAnalysis.Measure;
import jmt.engine.dataAnalysis.MeasureOutput;
import jmt.engine.dataAnalysis.SimParameters;
import jmt.engine.dataAnalysis.XMLSimulationOutput;
import jmt.engine.dataAnalysis.measureOutputs.CSVMeasureOutput;
import jmt.engine.log.JSimLogger;
import jmt.engine.random.engine.RandomEngine;

import org.w3c.dom.Document;

import qnactr.sim.GlobalUtilities;
import qnactr.sim.QnactrSimulation; //QN-Java

/**
 * This class creates a new Simulation. It provides an easy way to initialize
 * the jmt engine.
 *
 * @author Federico Granata, Stefano Omini, Bertoli Marco
 * 
 * Modified by Ashanka (May 2010): 
 * Patch: Multi-Sink Perf. Index 
 * Description: Added new Performance index for the capturing the 
 * 				1. global response time (ResponseTime per Sink)
 *              2. global throughput (Throughput per Sink)
 *              each sink per class. 
 */
public class Simulation {
	/** Output file handler */
	File outputFile;

	//---------------------- SIMULATION COMPONENTS --------------------------//
  
	QnactrSimulation[] qnactrSims; //QN-Java
	
	//name of the simulation
	private String name;
	// queueing network associated to this simulation
	private QueueNetwork network;

	//classes of the system
	private JobClass[] classes = null;
	//service centers of the system
	private List<SimNode> nodes = new ArrayList<SimNode>();
	//Burst distributions to register to the event handler
	private List<NetNode> distrNetNodes = new ArrayList<NetNode>();
	//connections
	private List<Connection> connections = new ArrayList<Connection>();
	//measures requested by the simulation
	private List<SimMeasure> measures = new ArrayList<SimMeasure>();
	// blocking region measures requested by the simulator
	private List<SimMeasure> regionMeasures = new ArrayList<SimMeasure>();
	//blocking regions
	private List<BlockingRegion> regions = new ArrayList<BlockingRegion>();

	//used to run the simulation only if the queue network has been initialized
	private boolean initialized = false;

	//NEW
	//@author Stefano Omini
	//logger of the simulation
	private JSimLogger logger = JSimLogger.getLogger(JSimLogger.STD_LOGGER);
	//private JSimLogger logger = JSimLogger.getRootLogger();
	//end NEW

	// Global JobInfoList - Bertoli Marco
	private GlobalJobInfoList globalInfoList;

	//---------------------- end SIMULATION COMPONENTS --------------------------//

	//---------------------- SIMULATION TIMER --------------------------//

	//NEW
	//@author Stefano Omini

	//true if an upper bound has been set to the simulation time
	private boolean timeLimited = false;
	//the max simulation time
	private long maxSimulationTime = 0;
	//the timer which controls max simulation time
	//true if the simulation has finished
	private boolean finished = false;

	//end NEW

	//---------------------- end SIMULATION TIMER --------------------------//

	//---------------------- SIMULATION SEED --------------------------//

	//NEW
	//@author Stefano Omini

	//seed of the simulation
	private long seed = -1;
	//reference to the RandomEngine used by the simulation
	private RandomEngine randomEng = null;

	//end NEW

	//---------------------- end SIMULATION SEED --------------------------//

	//NEW
	//@author Stefano Omini
	private SimParameters simParameters = null;
	//end NEW

	//---------------------- XML FILES PATH --------------------------//
	//NEW
	//@author Stefano Omini

	//path of the xml file containing the mva model description
	//this attribute has been setted only if a SimLoader has been used to
	//create this Simulation object
	String xmlModelDefPath;

	//path of the xml file containing the simulation model description
	//this attribute has been setted only if a SimLoader has been used to
	//create this Simulation object
	String xmlSimModelDefPath;

	//end NEW

	//---------------------- end XML FILES PATH --------------------------//

	//---------------------- PRELOAD ------------------------------------//

	//if true preload jobs
	private boolean preloadEnabled = false;
	//the names of the stations to be preloaded
	private String[] preload_stationNames;
	//the matrix of initial populations
	private int[][] preload_initialPopulations;

	//---------------------- end PRELOAD -------------------------------//

	//-------------------------CONSTRUCTORS-------------------------------//

	//NEW
	//@author Stefano Omini

	/**
	 * Creates a new Simulation object.
	 * @param seed simulation seed (if -1, an automatic seed will be generated)
	 * @param simName Name of simulation
	 * @param maxSimulationTime The max duration (in milliseconds) of the simulation
	 * @throws IOException
	 *
	 */
	public Simulation(long seed, String simName, long maxSimulationTime) throws IOException {

		//name of the simulation
		name = simName;

		//Initializes NetSystem
		NetSystem.initialize();

		//sets the maximum duration of the simulation run
		this.timeLimited = true;
		this.maxSimulationTime = maxSimulationTime;

		//sets seed (if -1 --> automatic seed)
		if (seed != -1) {
			//sets the seed
			setRandomEngineSeed(seed);
		}
	}

	//NEW
	//@author Stefano Omini
	/**
	 * Creates a new Simulation object.
	 * @param seed simulation seed (if -1, an automatic seed will be generated)
	 * @param simName Name of simulation
	 * @throws IOException
	 */
	public Simulation(long seed, String simName) throws IOException {
		//name of the simulation
		name = simName;

		//Initializes NetSystem
		NetSystem.initialize();

		//sets seed (if -1 --> automatic seed)
		if (seed != -1) {
			//sets the seed
			setRandomEngineSeed(seed);
		}
	}

	//end NEW

	//-------------------------end CONSTRUCTORS-------------------------------//

	//--------------METHODS TO ADD SIMULATION COMPONENTS----------------------//

	// These methods are used while class SimLoader loads the model from an xml file:
	// all the simulation components are at first put into vectors,
	// then the method initialize() uses these vectors to setup the Simulation object.

	/**
	 * Adds all Job Classes to the simulation.	 *
	 * @param classes the job classes of simulation
	 */
	public void addClasses(JobClass[] classes) {
		this.classes = classes;
	}

	/**
	 * Adds a node to the simulation model.
	 * @param name name of the node
	 * @param inSec input section of the node
	 * @param serSec service section of the node
	 * @param outSec output section of the node
	 */
	public void addNode(String name, InputSection inSec, ServiceSection serSec, OutputSection outSec) {
		// nodes.add(new SimNode(name, inSec, serSec, outSec)); // before QN-Java modification
	  
	  //QN-Java modification TODO
	  SimNode aSimNode = new SimNode(name, inSec, serSec, outSec);
	  aSimNode.getNode().setSimJMT(this);
	  //JOptionPane.showMessageDialog(null, "Simulation has added a node with name: " +  aSimNode.getNode().getName() , "Simulation.java", JOptionPane.INFORMATION_MESSAGE);
	  
	  nodes.add(aSimNode); // before QN-Java modification
	}

	/**
	 * Connects two nodes of the simulation: if a node hasn't been inserted in the model,
	 * then return a LoadException.
	 * @param start the sourcce node
	 * @param end the target node
	 * @throws LoadException one of the nodes hasn't been inserted in te model
	 */
	public void addConnection(String start, String end) throws LoadException {
		if (isNode(start) && isNode(end)) {
			connections.add(new Connection(start, end));
		} else {
			throw new LoadException("Trying to connect nodes that haven't been inserted yet.");
		}
	}

	/**
	 * Adds to the system a new measure to be computed.
	 * @param measureType type of measure (see the constants specified in this class).
	 * @param nodeName name of the node to be measured.
	 * @param measure
	 * @param jClass
	 * @throws LoadException
	 */
	public void addMeasure(int measureType, String nodeName, Measure measure, String jClass) throws LoadException {
		addMeasure(measureType, nodeName, measure, jClass, null);
	}

	/**
	 * Adds to the system a new measure to be computed.
	 * @param measureType type of measure (see the constants specified in this class).
	 * @param nodeName name of the node to be measured.
	 * @param measure
	 * @param jClass
	 * @param nodeType type of node. This can be "station" or null for normal measures and "region" for blocking region related ones
	 * @throws LoadException
	 */
	public void addMeasure(int measureType, String nodeName, Measure measure, String jClass, String nodeType) throws LoadException {
		//sets all the parameters shared by all Measure object
		// (i.e. number and size of batches, ...) 
		if (simParameters != null) {
			measure.setSimParameters(simParameters);
		}
		if (SimConstants.NODE_TYPE_REGION.equalsIgnoreCase(nodeType)) {
			// Region measures
			regionMeasures.add(new SimMeasure(measureType, nodeName, measure, jClass));
		} else {
			// Normal measures
			measures.add(new SimMeasure(measureType, nodeName, measure, jClass));
		}
	}

	//NEW
	//@author Stefano Omini

	/**
	 * Creates a blocking region.
	 *
	 * @param name The name of the blocking region
	 * @param maxCapacity the max capacity of the region
	 * @param maxCapacityPerClass the max capacity for each class
	 * @param classWeights the weight of each class (tokens per job request)
	 * @param drop for each class, specifies if jobs should be dropped or not when the region is blocked
	 * @param stations the array of the names of the stations contained in the region
	 *
	 * @throws LoadException if some errors occur during the region creation
	 */
	public void addRegion(String name, double maxCapacity, double[] maxCapacityPerClass, double[] classWeights, boolean[] drop, String[] stations)
			throws LoadException {
		if (true) {
			//TODO: andrebbero aggiunti controlli (nome non univoco, jobs<0, ecc...)
			regions.add(new BlockingRegion(name, maxCapacity, maxCapacityPerClass, classWeights, drop, this, stations));
		} else {
			throw new LoadException("Exception while creating a blocking region.");
		}
	}

	//end NEW

	//--------------end METHODS TO ADD SIMULATION COMPONENTS---------------//

	//------------------INITIALIZATION AND RUN------------------------------//

	/**
	 * Intilalizes all the components of this Simulation.
	 * This method assures that all the operations are
	 * executed in the correct order.
	 *
	 */
	public void initialize() throws NetException {

		// creates network
		network = new QueueNetwork("jSIM simulation: " + name);
		// adds network to NetSystem
		NetSystem.addNetwork(network);

		//add all job classes to QueueNetwork
		for (JobClass classe : classes) {
			network.addJobClass(classe);
		}

		try {
			//creates all nodes
			NetNode[] netNodes = new NetNode[nodes.size()];
			for (int i = 0; i < nodes.size(); i++) {
				netNodes[i] = (nodes.get(i)).getNode();
			}

			//add connections
			for (int i = 0; i < connections.size(); i++) {
				int nodePosition1 = findNodePosition(connections.get(i).getStart());
				int nodePosition2 = findNodePosition(connections.get(i).getEnd());
				netNodes[nodePosition1].connect(netNodes[nodePosition2]);
			}

			//add all nodes to QueueNetwork
			for (int i = 0; i < nodes.size(); i++) {
				SimNode simNode = (nodes.get(i));
				if (simNode.isReference()) {
					//reference nodes are the nodes (random source, terminal, ..)
					//which create jobs: these nodes must receive the start event
					network.addReferenceNode(simNode.getNode());
				} else {
					network.addNode(simNode.getNode());
				}
			}

			//add distribution net nodes to QueueNetwork
			for (int i = 0; i < distrNetNodes.size(); i++) {
				network.addNode(distrNetNodes.get(i));
			}

			//add all nodes sections
			for (int i = 0; i < nodes.size(); i++) {
				SimNode n = nodes.get(i);
				if (n.getInput() != null) {
					n.getNode().addSection(n.getInput());
				}
				if (n.getService() != null) {
					n.getNode().addSection(n.getService());
				}
				if (n.getOutput() != null) {
					n.getNode().addSection(n.getOutput());
				}
				n.getNode().initializeSections();
			}

			//NEW
			//@author Stefano Omini

			//add blocking regions
			BlockingRegion br;
			for (int i = 0; i < regions.size(); i++) {
				br = regions.get(i);

				String regionName = br.getName();
				String inputStationName = regionName + "_inputStation";

				InputSection is = new BlockingQueue(br);
				ServiceSection ss = new ServiceTunnel();
				OutputSection os = new BlockingRouter();

				SimNode inputStation = new SimNode(inputStationName, is, ss, os);
				inputStation.getNode().setBlockingRegionInputStation(br);

				//adds the input station of the blocking region
				nodes.add(inputStation);

				network.addNode(inputStation.getNode());

				//auto-connect the node, to avoid problems in job info lists refreshing
				//(otherwise a node with no connections presents problems)
				inputStation.getNode().connect(inputStation.getNode());

				NetNode inputSt = inputStation.getNode();
				if (inputStation.getInput() != null) {
					inputStation.getNode().addSection(inputStation.getInput());
				}
				if (inputStation.getService() != null) {
					inputStation.getNode().addSection(inputStation.getService());
				}
				if (inputStation.getOutput() != null) {
					inputStation.getNode().addSection(inputStation.getOutput());
				}
				inputStation.getNode().initializeSections();

				//sets the input station of the blocking region
				br.setInputStation(inputSt);

				//sets blocking region behaviour for inner nodes
				String[] regNodes = br.getRegionNodeNames();
				for (String regNode : regNodes) {
					NetNode innerNode = br.getRegionNode(regNode);

					//at the moment inner stations must have a Queue-type input section
					//and a Router-type output section
					//other not compliant sections will cause a NetException

					//nodes which receive jobs from the outside must
					//have redirecting queue behaviour turned on
					NodeSection input = innerNode.getSection(NodeSection.INPUT);
					if (input instanceof Queue) {
						((Queue) input).redirectionTurnON(br);
					} else {
						throw new NetException("Error in creating blocking region: " + "inner station " + innerNode.getName()
								+ " has a not compliant input section.");
					}

					//nodes which sends jobs outside the region must have border router behaviour
					NodeSection output = innerNode.getSection(NodeSection.OUTPUT);
					if (output instanceof Router) {
						((Router) output).borderRouterTurnON(br);
					} else {
						throw new NetException("Error in creating blocking region: " + "inner station " + innerNode.getName()
								+ " has a not compliant output section.");
					}
				}
				if (regionMeasures.size() > 0) {
					//adds measures in blocking region input station
					for (Iterator<SimMeasure> it = regionMeasures.iterator(); it.hasNext();) {
						SimMeasure measure = it.next();
						if (measure.getNodeName().equals(regionName)) {
							addMeasure(measure.getMeasureType(), inputStationName, measure.getMeasure(), measure.getjClass());
							it.remove();
						}
					}
				}
			}
			//end NEW

			//NEW
			//@author Stefano Omini
			//refresh nodes list, after input stations has been added
			netNodes = new NetNode[nodes.size()];
			for (int i = 0; i < nodes.size(); i++) {
				netNodes[i] = (nodes.get(i)).getNode();
			}
			//end NEW

			//add measures
			SimMeasure ms;

			for (int i = 0; i < measures.size(); i++) {
				ms = measures.get(i);
				boolean verbose = ms.getMeasure().getVerbose();

				if (verbose) {
					//if true, for each Measure sets the corresponding MeasureOutput
					//a measure output can be a file (xml, txt, ...) which contains
					//samples and final report of that measure

					//MeasureOutput output = new SimpleMeasureOutput(ms.getMeasure(), false, ms.getMeasure().getName() + ".log");
					MeasureOutput output = new CSVMeasureOutput(ms.getMeasure(), false);
				}

				network.addMeasure(ms.getMeasure());
			}

			//intialize measures
			for (int i = 0; i < measures.size(); i++) {
				ms = measures.get(i);

				//NEW
				//@author Stefano Omini
				JobClass jClass;
				if (ms.getjClass() != "") {
					//measure relative to a specified class
					String[] jName = { ms.getjClass() };
					int[] position = findClassPosition(jName);
					jClass = classes[position[0]];
				} else {
					//aggregated measure
					jClass = null;
				}
				//end NEW

				// If measure is not global
				if (ms.getNodeName() != null && !ms.getNodeName().equals("")) {
					//measures are computed by sampling specific
					int nodePosition = findNodePosition(ms.getNodeName());

					switch (ms.getMeasureType()) {
						case SimConstants.QUEUE_LENGTH:
							//measures the number of jobs in the global node (=mean number of jobs present in node joblist)
							netNodes[nodePosition].analyze(SimConstants.LIST_NUMBER_OF_JOBS, jClass, ms.getMeasure());
							break;
						case SimConstants.UTILIZATION:
							//measures the utilization of the service section (=mean number of jobs present in service section joblist)
							netNodes[nodePosition].getSection(NodeSection.SERVICE).analyze(SimConstants.LIST_NUMBER_OF_JOBS, jClass, ms.getMeasure());
							break;
						case SimConstants.THROUGHPUT:
							//measures the throughput of the service section (= throughput of service section joblist)
							netNodes[nodePosition].getSection(NodeSection.SERVICE).analyze(SimConstants.LIST_THROUGHPUT, jClass, ms.getMeasure());
							break;
						case SimConstants.RESPONSE_TIME:
							//measures the resp time for all visits
							netNodes[nodePosition].analyze(SimConstants.LIST_RESPONSE_TIME, jClass, ms.getMeasure());
							break;
						case SimConstants.RESIDENCE_TIME:
							//measures the response time on the global node (= mean residence time in node joblist)
							netNodes[nodePosition].analyze(SimConstants.LIST_RESIDENCE_TIME, jClass, ms.getMeasure());
							break;
						case SimConstants.QUEUE_TIME:
							//measures the queue time spent in the input setion (= mean residence time in input section joblist)
							netNodes[nodePosition].getSection(NodeSection.INPUT).analyze(SimConstants.LIST_RESIDENCE_TIME, jClass, ms.getMeasure());
							break;
						case SimConstants.DROP_RATE:
							// measures the drop rate of an input section
							netNodes[nodePosition].getSection(NodeSection.INPUT).analyze(SimConstants.LIST_DROP_RATE, jClass, ms.getMeasure());
							break;
							
						case SimConstants.RESPONSE_TIME_PER_SINK:
							// measures the drop rate of an input section
							netNodes[nodePosition].analyze(SimConstants.RESPONSE_TIME_PER_SINK, jClass, ms.getMeasure());
							break;
							
						case SimConstants.THROUGHPUT_PER_SINK:
							// measures the drop rate of an input section
							netNodes[nodePosition].analyze(SimConstants.THROUGHPUT_PER_SINK, jClass, ms.getMeasure());
							break;
					}
				}
				// Global measures (new by Bertoli Marco)
				else {
					switch (ms.getMeasureType()) {
						case SimConstants.SYSTEM_RESPONSE_TIME:
							network.getJobInfoList().analyzeResponseTime(jClass, ms.getMeasure());
							break;
						case SimConstants.SYSTEM_THROUGHPUT:
							network.getJobInfoList().analyzeThroughput(jClass, ms.getMeasure());
							break;
						case SimConstants.SYSTEM_JOB_NUMBER:
							network.getJobInfoList().analyzeJobNumber(jClass, ms.getMeasure());
							break;
						case SimConstants.SYSTEM_DROP_RATE:
							network.getJobInfoList().analyzeDropRate(jClass, ms.getMeasure());
							break;
						//Added by ASHANKA START
						//Added System Power performance index as a part of the global indeces.
						case SimConstants.SYSTEM_POWER:
							network.getJobInfoList().analyzeSystemPower(jClass, ms.getMeasure());
							break;
						//Added by ASHANKA STOP
					}
				}
			}

			//Preload
			//NEW
			//@author Stefano Omini
			if (preloadEnabled) {
				//preload enabled: uses the info loaded by SimLoader
				for (int s = 0; s < preload_stationNames.length; s++) {
					//preload, in the specified station, the specified populations
					preload_station(preload_stationNames[s], preload_initialPopulations[s]);
					
					//JOptionPane.showMessageDialog(null, "preload_station\npreload_stationNames[s]:" + preload_stationNames[s] + "\npreload_initialPopulations[s]: " + preload_initialPopulations[s].toString(), "Simulation.java", JOptionPane.INFORMATION_MESSAGE); //CAO 
					
				}
			}
			//end NEW

			//CAO
			//JOptionPane.showMessageDialog(null, "preloadEnabled: " + preloadEnabled, "Simulation.java", JOptionPane.INFORMATION_MESSAGE); //CAO
			
			
			//String showString = "all nodes(servers):\n" + nodes.toString() + "\n"; 
	    //JOptionPane.showMessageDialog(null, showString, "Simulation.java", JOptionPane.INFORMATION_MESSAGE); //CAO 
			
			
		} catch (jmt.common.exception.NetException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} catch (Exception e) {
			e.printStackTrace();
			return;
		}

		initialized = true;

		logger.debug("Simulation has been initialized");
		
		//JOptionPane.showMessageDialog(null, "Simulation has been initialized with name: " +  this.name + ". Num of regions: " + regions.size(), "Simulation.java", JOptionPane.INFORMATION_MESSAGE);
		//QN-Java add QN-ACTR simulation
		int numOfOperators = regions.size(); // defined by number of regions appear in a JMT QN-Net file
		qnactrSims = new QnactrSimulation[numOfOperators];
		
		QnactrSimulation.globalVarInitialized = false;
		for (int i = 0; i < numOfOperators; i ++) {
		  //JOptionPane.showMessageDialog(null, "Simulation has been initialized with i: " +  i + ". Num of regions: " + regions.size(), "Simulation.java", JOptionPane.INFORMATION_MESSAGE);
		  qnactrSims[i] = new QnactrSimulation(i + 1, this);
		}
		
		SimSystem.qnactrSims = qnactrSims;
		
	}

	/**
	 * Runs this simulation. If it hasn't been initialized, then throws an Exception.
	 * Controls the timer for max simulation time (if it has been defined).
	 * @throws Exception Simulaton not initialized
	 */
	public void run() throws Exception {
		if (initialized) {
			//runs the simulation
			NetSystem.start();
			finished = true;

		  //JOptionPane.showMessageDialog(null, "run()", "Simulation.java", JOptionPane.INFORMATION_MESSAGE); //CAO
			
			
			//simulation has finished
			//results are put into a xml file
			XMLSimulationOutput output = new XMLSimulationOutput(this);
			outputFile = output.writeAllMeasures();

			//end NEW

			NetSystem.terminate();
			
			//QN-Java
			GlobalUtilities.setMainWindowStatusLabelText("Clock (s): "); 
			for(QnactrSimulation QnSim : qnactrSims){
			  QnSim.funs.ProgramUtilitiesFun__SimulationFinalizationCode();

			  double simulationUsedRealTimeDuration = (QnactrSimulation.simEndRealClockTime - QnactrSimulation.simStartRealClockTime) / 1000.0; //second
			  double actualSimulationSpeedFactor = simulationUsedRealTimeDuration / QnactrSimulation.simEndSimulationClockTime ;
			  System.out.println("Simulation clock run duration: " + QnactrSimulation.simEndSimulationClockTime);
			  //System.out.println("Real-world clock run duration: " + simulationUsedRealTimeDuration + ", " + GlobalUtilities.formatDoubleToString(actualSimulationSpeedFactor * 100.0, 3) + "% of simulation duration.");
			  System.out.println("Real-world clock run duration: " + simulationUsedRealTimeDuration + ", " + GlobalUtilities.formatDoubleToString(1 / actualSimulationSpeedFactor , 3) + "x times of simulation duration.");
			  
			  
			  QnSim.files.closeAllTxtFileWriters();
			}
			
		} else {
			throw new NetException("Simulation has not been initialized");
		}
	}

	/**
	 * Aborts simulation.
	 * @throws jmt.common.exception.NetException
	 */
	public void abort() throws jmt.common.exception.NetException {
		logger.info("Aborting simulation...");
		NetSystem.terminate();
		
	}

	/**
	 * Returns output file handler
	 * @return output file handler or null if a problem occurred
	 */
	public File getOutputFile() {
		return outputFile;
	}

	//------------------end INITIALIZATION AND RUN------------------------------//

	//-------------------------SUPPORT METHODS---------------------------------//

	//check if exists a node with the given name
	private boolean isNode(String name) {
		for (int i = 0; i < nodes.size(); i++) {
			String netNode = (nodes.get(i)).getNode().getName();
			if (netNode.equals(name)) {
				return true;
			}
		}
		return false;
	}

	//finds the position of the classes from the giving names
	private int[] findClassPosition(String[] names) {
		int[] classPos = new int[names.length];
		for (int i = 0; i < names.length; i++) {
			String name = names[i];
			for (int j = 0; j < classes.length; j++) {
				JobClass aClass = classes[j];
				if (name.equals(aClass.getName())) {
					classPos[i] = j;
				}
			}
		}
		return classPos;
	}

	//finds the node
	private int findNodePosition(String name) throws LoadException {
		for (int i = 0; i < nodes.size(); i++) {
			if ((nodes.get(i)).getNode().getName().equals(name)) {
				return i;
			}
		}
		throw new LoadException("node not found");
	}

	/**
	 * Gets all the measures inserted so far.
	 *
	 */
	public Document[] getAllMeasures() {
		Document[] tempMeasures = new Document[measures.size()];
		for (int i = 0; i < measures.size(); i++) {
			SimMeasure simMeasure = measures.get(i);
			tempMeasures[i] = simMeasure.getMeasure().getNewData();
		}
		return tempMeasures;
	}

	/*
	public NetLog getLog() {
	return log;
	}
	*/

	/**
	 * Preload jobs in a queue
	 */
	public void preload_station(String stationName, int[] jobs) {

		//find the node
		NetNode node = NetSystem.getNode(stationName);

		if (node != null) {
			try {
				//retrieves the input section of the node
				NodeSection section = node.getSection(NodeSection.INPUT);
				if (section instanceof Queue) {
					//preload jobs for each class
					((Queue) section).preloadJobs(jobs);
				}
			} catch (NetException e) {
				return;
			}
		}

	}

	//-------------------------end SUPPORT METHODS---------------------------------//

	//------------------- GETTER AND SETTER ----------------------------------//

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	//NEW
	//@author Stefano Omini

	public String getXmlModelDefPath() {
		return xmlModelDefPath;
	}

	public void setXmlModelDefPath(String xmlModelDefPath) {
		this.xmlModelDefPath = xmlModelDefPath;
	}

	public String getXmlSimModelDefPath() {
		return xmlSimModelDefPath;
	}

	public void setXmlSimModelDefPath(String xmlSimModelDefPath) {
		this.xmlSimModelDefPath = xmlSimModelDefPath;
	}

	//end NEW

	//NEW
	//@author Stefano Omini
	public void setRandomEngineSeed(long seed) {
		this.seed = seed;

		randomEng = RandomEngine.makeDefault();
		RandomEngine.setSeed(seed);

	}

	//end NEW

	/**
	 *  Sets the max simulation length
	 */
	public void setMaxSimulationTime(long maxSimulationTime) {
		this.maxSimulationTime = maxSimulationTime;
		this.timeLimited = true;
	}

	/**
	 * Tells whether the simulation has a timer, which sets an upper limit to the
	 * simulation time
	 * @return true if the simulation has an upper bound for the simulation time
	 */
	public boolean isTimeLimited() {
		return timeLimited;
	}

	/**
	 * Returns true if the simulation has finished
	 * @return true if the simulation has finished
	 */
	public boolean hasFinished() {
		return finished;
	}

	//end NEW

	//TODO: servono per blocking
	//NEW
	//@author Stefano Omini
	public QueueNetwork getNetwork() {
		return network;
	}

	/**
	 * Returns an array with all the blocking regions defined inside the model
	 * @return null if no region has been defined
	 */
	public BlockingRegion[] getAllRegions() {
		if (regions.size() == 0) {
			return null;
		}
		BlockingRegion[] regions_vector = new BlockingRegion[regions.size()];

		for (int br = 0; br < regions.size(); br++) {
			regions_vector[br] = regions.get(br);
		}
		return regions_vector;
	}

	/**
	 * Returns the array of classes
	 * @return the array of classes
	 */
	public JobClass[] getClasses() {
		return classes;
	}

	//end NEW

	//NEW
	//@author Stefano Omini
	public void setSimParameters(SimParameters simParameters) {
		this.simParameters = simParameters;
	}

	//end NEW

	//NEW
	//@author Stefano Omini

	public void setPreloadEnabled(boolean preloadEnabled) {
		this.preloadEnabled = preloadEnabled;
	}

	public void setPreload_stationNames(String[] preload_stationNames) {
		this.preload_stationNames = preload_stationNames;
	}

	public void setPreload_initialPopulations(int[][] preload_initialPopulations) {
		this.preload_initialPopulations = preload_initialPopulations;
	}

	//end NEW

	public void addDistrNetNode(NetNode netNode) {
		distrNetNodes.add(netNode);
	}

	//------------------- end GETTER AND SETTER ----------------------------------//

	//-------------------------SUPPORT CLASSES-------------------------------//

	/**
	 * This is a connection between two nodes of the simulation.
	 */
	class Connection {
		private String start, end;

		/**
		 * Creates a node connection between two stations of the model
		 * @param startNode start node
		 * @param endNode end node
		 */
		public Connection(String startNode, String endNode) {
			start = startNode;
			end = endNode;
		}

		public String getEnd() {
			return end;
		}

		public String getStart() {
			return start;
		}
	}

	/**
	 * Creates a measure to be estimated on the simulaton model.
	 */
	class SimMeasure {
		private String jClass;
		private Measure measure;
		private String nodeName;
		//see constants in class Simulation
		private int measureType;

		/**
		 * Creates a measure to be computed during simulation
		 * @param measureType measure type (see constants in class Simulation)
		 * @param nodeName node to which this measure refers
		 * @param measure Measure object which will control this measure
		 * @param jClass class to which this measure refers
		 *
		 * @throws LoadException
		 */
		public SimMeasure(int measureType, String nodeName, Measure measure, String jClass) throws LoadException {

			this.jClass = jClass;
			this.measureType = measureType;
			this.measure = measure;
			this.measure.measureTarget(nodeName, jClass, measureType);
			this.nodeName = nodeName;
			if (this.nodeName == null) {
				throw new LoadException("Trying to add a measure to a not-existent node.");
			}
		}

		public Measure getMeasure() {
			return measure;
		}

		public int getMeasureType() {
			return measureType;
		}

		public String getNodeName() {
			return nodeName;
		}

		public String getjClass() {
			return jClass;
		}
	}

	/**
	 * Represents a service center in the simulation
	 */
	public class SimNode { // QN-Java changed to public
		private NetNode node;
		//node sections
		private InputSection input;
		private ServiceSection service;
		private OutputSection output;
		//true if the node has been created with all its sections,
		//false otherwise
		private boolean nodeInit;

		//reference nodes are nodes used to compute job throughput or to create jobs
		//(and therefore must receive the START event of simulation)
		private boolean reference = false;

		/**
		 * Creates a SimNode object
		 * @param name node name
		 * @param inSec input section of the node
		 * @param serSec service section of the node
		 * @param outSec output section of the node
		 */
		public SimNode(String name, InputSection inSec, ServiceSection serSec, OutputSection outSec) {
			this.node = new NetNode(name);
			this.input = inSec;
			this.service = serSec;
			this.output = outSec;
			//
			nodeInit = true;

			if (simParameters != null) {
				this.node.setSimParameters(simParameters);
			}

			//OLD
			//if (inSec instanceof RandomSource) {
			//NEW
			//@author Stefano Omini
			if ((inSec instanceof RandomSource) || (inSec instanceof Terminal)) {
				reference = true;
			}
		}

		/**
		 * @deprecated
		 * @param nodeName
		 */
		@Deprecated
		public SimNode(String nodeName) {
			this.node = new NetNode(nodeName);
			input = null;
			service = null;
			output = null;
			nodeInit = false;
		}

		public InputSection getInput() {
			return input;
		}

		public NetNode getNode() {
			return node;
		}

		public OutputSection getOutput() {
			return output;
		}

		public ServiceSection getService() {
			return service;
		}

		public void initialize() {
			if (!this.nodeInit) {
			}
		}

		public boolean isReference() {
			return reference;
		}

		
	  /**
	   * QN-Java
	   * @return  a string representation of the object.
	   */
	  @Override
	  public String toString() {
	    String str = new String();
	    str += "\nServer name: " + node.getName() + "\n";
	    return str;
	  }
	  
		
	}
	
	//QN-Java
	public QnactrSimulation[] getQnactrSims(){
	  return qnactrSims;
	}
	
	public List<SimNode> getSimNodes (){
	  return nodes;
	}
	
}
