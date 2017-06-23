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

/**
 * 2013. Modified for QN-Java project
 * 
 * [2013-07-13] change from routing to fork, i.e., rather than just pick one output Node, each connected output Node may receive a copied Job/Entity
 *              
 * 
 */

package jmt.engine.NetStrategies.RoutingStrategies;

import qnactr.objectDesigner.Entity;
import qnactr.objectDesigner.Enums;
import qnactr.sim.GlobalUtilities;
import jmt.common.exception.IncorrectDistributionParameterException;
import jmt.common.exception.LoadException;
import jmt.engine.NetStrategies.RoutingStrategy;
import jmt.engine.QueueNet.JobClass;
import jmt.engine.QueueNet.NetNode;
import jmt.engine.QueueNet.NetSystem;
import jmt.engine.QueueNet.NodeList;
import jmt.engine.random.Empirical;
import jmt.engine.random.EmpiricalEntry;
import jmt.engine.random.EmpiricalPar;
import jmt.engine.random.engine.RandomEngine;
import jmt.engine.simEngine.SimSystem;

/**
 * The node, into which job must be routed, is chosen using an empirical strategy.
 * An empirical strategy is based on an empirical distribution, that is a
 * distribution constructed from the data provided by the user.
 *
 * @author Federico Granata, Stefano Omini, Bertoli Marco
 */

public class EmpiricalStrategy extends RoutingStrategy {
	private int CLOSED_CLASS = JobClass.CLOSED_CLASS;
	// Used to terminate the loop to avoid to send a job of closed class into a sink
	// By the way a check is implemented at gui level, so this condition will never apply.
	private final int maxSinkTry = 4096;
	//the empirical distribution
	private Empirical distribution;
	//the parameter of the empirical distribution
	private EmpiricalPar param;
	//the array with the routing probabilities (one for each output node)
	private double probabilities[];
	//the NetNode objects corresponding to the output nodes
	private NetNode[] nodes;
	//the names of the output nodes
	private String[] nodeNames;

	/**
	 * Creates an empirical strategy by adding nodes and naming them with the specified names.
	 * @param distribution The empirical distribution
	 * @param param The parameters of the empirical distribution
	 * @throws LoadException
	 */
	public EmpiricalStrategy(Empirical distribution, EmpiricalPar param) throws LoadException {
		this.distribution = distribution;
		this.param = param;
		//gets the values contained in the empirical parameter (in this case these
		//objects are the names of the output nodes)
		Object[] names = param.getValues();

		nodeNames = new String[names.length];

		//OLD
		//nodes = new NetNode[names.length];

		//NEW
		//@author Stefano Omini
		for (int i = 0; i < names.length; i++) {
			if (names[i] instanceof String) {
				//sets the name of the i-th output node
				nodeNames[i] = (String) names[i];

				//OLD
				//this reserach of the NetNode gives an error if the owner node of this
				//strategy is inserted before the output nodes referred to in the strategy
				//For this reason, at this point only nodeNames information are stored.
				//The NetNode objects will be found later by the findNodes() method.

				//sets the NetNode reference to the i-th output node (found
				//using its name)
				//nodes[i] = NetSystem.getNode(nodeNames[i]);
				//end OLD

			} else {
				throw new LoadException("Name of the node is not a String");
			}
		}

	}

	/**
	 * Creates an empirical strategy using the passed data.
	 * @param entries Entries used to construct the empirical distribution (each entry
	 * contains a node name and the corresponding routing probability).
	 * @throws LoadException
	 * @throws IncorrectDistributionParameterException
	 */
	public EmpiricalStrategy(EmpiricalEntry[] entries) throws LoadException, IncorrectDistributionParameterException {

		probabilities = new double[entries.length];

		//OLD
		//nodes = new NetNode[entries.length];

		//NEW
		//@author Stefano Omini
		nodeNames = new String[entries.length];
		//end NEW

		for (int i = 0; i < entries.length; i++) {
			EmpiricalEntry entry = entries[i];
			if (entry.getValue() instanceof String) {
				//gets the value contained in the empirical entry (in this case this
				//object is the name of the output node)
				String nodeName = (String) entry.getValue();

				//sets the name of the node; the corresponding NetNode object will be found later
				nodeNames[i] = nodeName;

				//sets the corresponding routing probability
				probabilities[i] = entry.getProbability();
				
//				System.out.println("EmpiricalStrategy.Java nodeNames[i] + " + nodeNames[i] + " probabilities[i]: " + probabilities[i]);
				
			} else {
				throw new LoadException("Name of the node is not a String");
			}
		}

		//uses the obtained probabilities to create the empirical distribution
		//and its parameter
		distribution = new Empirical();
		param = new EmpiricalPar(probabilities);
	}


	
	/**
	 * It controls whether the distribution is correct or not.
	 * For the empirical distribution, the parameter is correct if the sum of the
	 * routing probabilities are greater than zero and they sum to 1.0.
	 */
	@Override
	public boolean check() {
		return param.check();
	}

	/**
	 * Gets the output node, into which the job must be routed, using an
	 * empirical strategy.
	 * @param nodeList the list of output nodes
	 * @param jobClass class ofcurrent job to be routed
	 * @return The selected node.
	 */
	@Override
	public NetNode getOutNode(NodeList nodeList, JobClass jobClass) {
		try {

			if (nodes == null) {
				//it's the first execution of this method: find the NetNode objects
				//corresponding to the nodeNames
				findNodes();
			}
			//the empirical distribution returns the position of the chosen output node
			int nodePos = (int) distribution.nextRand(param);

			NetNode node = this.nodes[nodePos];

			// Controls if a closed class job is put into a sink - Bertoli Marco
			if (jobClass.getType() == CLOSED_CLASS) {
				// when reaches maxSinkTry, aborts this strategy returning null
				int tries = 0;
				while (node.isSink()) {
					if (tries++ >= maxSinkTry) {
						return null;
					}
					// Try to find a node different from a sink
					nodePos = (int) distribution.nextRand(param);
					node = this.nodes[nodePos];
				}
			}

			if (nodeList.contains(node)) {
				//the chosen node must be contained in the list of the output nodes
				return node;
			}
		} catch (IncorrectDistributionParameterException e) {
			e.printStackTrace();
		}
		return null;
	}

	//NEW
	//@author Stefano Omini
	/**
	 * Finds the NetNode objects corresponding to the node names received by constructor.
	 * @return true if all names actually correspond to nodes of the model, false otherwise;
	 */
	private boolean findNodes() {
		nodes = new NetNode[nodeNames.length];

		for (int i = 0; i < nodeNames.length; i++) {
			nodes[i] = NetSystem.getNode(nodeNames[i]);
			if (nodes[i] == null) {
				//the passed name does not correspond to a node of the model
				return false;
			}
		}
		return true;
	}
	//end NEW
	
	//QN-Java
	public NodeList getOutNodes(NodeList nodeList, JobClass jobClass, String fromNodeRawName, Entity qnactrEntity) {
	  
	  String fromLocalServerName =  GlobalUtilities.getServerOperatorNamesFromRawName(fromNodeRawName)[0] ;
	  Enums.ServerName fromLocalServerNameEnum =  Enums.ServerName.valueOf(GlobalUtilities.stringLowNoSpace(fromLocalServerName));
	  int indexHMI = GlobalUtilities.getHMIIndex(fromNodeRawName); // 0 for global. 1 for HMI 1, i.e., qnactrSims[0]
	  
	  NodeList returnList = new NodeList();
	  if (nodes == null) {
      //it's the first execution of this method: find the NetNode objects
      //corresponding to the nodeNames
      findNodes();
    }
    
    //for each output link, check if to add the Node in the return list
    for(int i = 0; i < nodes.length ; i ++){
      NetNode node = this.nodes[i];
      boolean pickThisNode;
      
      
      // double prob = probabilities[i]; //completely using GUI setting
      
      String toLocalServerName =  GlobalUtilities.getServerOperatorNamesFromRawName(node.getName())[0] ;
      Enums.ServerName toLocalServerNameEnum =  Enums.ServerName.valueOf(GlobalUtilities.stringLowNoSpace(toLocalServerName));
      
      
      double prob;
      if(indexHMI == 0)prob = probabilities[i];
      else prob =  SimSystem.qnactrSims[indexHMI - 1].pathLogics.getPathProbability(fromLocalServerNameEnum, toLocalServerNameEnum, qnactrEntity, probabilities[i]);
          
      if(prob == 1.0)pickThisNode = true;
      else if (prob == 0.0) pickThisNode = false;
      else{
        double rand = GlobalUtilities.randomDouble(0.0, 1.0);
        if(rand < prob) pickThisNode = true;
        else pickThisNode = false;
      }
              
      if (pickThisNode && nodeList.contains(node)) {
        //the chosen node must be contained in the list of the output nodes
        
        // Controls if a closed class job is put into a sink - Bertoli Marco
        if (jobClass.getType() == CLOSED_CLASS) {
          // when reaches maxSinkTry, aborts this strategy returning null
          int tries = 0;
          while (node.isSink()) {
            if (tries++ >= maxSinkTry) {
              return null;
            }
          }
        }
        
        
        returnList.add(node);
      }
      
    }
    return returnList;
	}
}