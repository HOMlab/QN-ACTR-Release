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
 */

package jmt.engine.NodeSections;

import java.util.ListIterator;

import javax.swing.JOptionPane;

import qnactr.objectDesigner.Enums;
import qnactr.sim.GlobalUtilities;

import jmt.common.exception.LoadException;
import jmt.engine.NetStrategies.RoutingStrategy;
import jmt.engine.NetStrategies.RoutingStrategies.EmpiricalStrategy;
import jmt.engine.QueueNet.BlockingRegion;
import jmt.engine.QueueNet.Job;
import jmt.engine.QueueNet.JobClass;
import jmt.engine.QueueNet.NetEvent;
import jmt.engine.QueueNet.NetMessage;
import jmt.engine.QueueNet.NetNode;
import jmt.engine.QueueNet.NodeList;
import jmt.engine.QueueNet.NodeSection;
import jmt.engine.simEngine.SimSystem;

/**
 * This class implements a router, which routes the jobs according to the specified
 * routing strategies (one for each job class).
 * <br><br>
 * The class has different constructors to create a generic router or a blocking
 * region border queue, that is the router of a node which is inside a blocking
 * region and which sends jobs also to nodes outside the region.
 * When a job leaves the blocking region, the region input station must receive
 * a message, in order to serve the blocked jobs.
 * <br>
 * However it's also possible to create a generic router and then to turn on/off the
 * "border router" behaviour using the <tt>borderRouterTurnON(..)</tt> and
 * <tt>borderRouterTurnOFF()</tt> methods.
 *
 * @author Francesco Radaelli, Stefano Omini
 * @author Bertoli Marco - Fixed lockup issues with closed class and sinks 13/11/2005
 * @author Bertoli Marco - Fixed bug with multiserver stations
 * 
 * Modified by Ashanka (Oct 2009) for FCR Bug fix: Events are created with job instead of null for EVENT_JOB_OUT_OF_REGION
 */
public class Router extends OutputSection {

  //QN-Java
  boolean routerDebugPopupFlag = false;
  
	/** Property Identifier: Routing strategy.*/
	public static final int PROPERTY_ID_ROUTING_STRATEGY = 0x0101;
	private RoutingStrategy routingStrategies[];

	/*----------------BLOCKING REGION PROPERTIES---------------------*/
	//@author Stefano Omini
	/*
	these properties are used if this router is the border router of
	a blocking region (i.e. it is connected also to nodes outside the
	blocking region)
	in fact, if the router sends a job outside the region, a message
	must be sent to the input station of that region, to decrease the
	number of jobs inside the region
	*/

	/**true if this router is the border router of a blocking region*/
	private boolean borderRouter;
	/** the blocking region this router belongs to */
	private BlockingRegion myBlockingRegion;
	/** the region input station of the blocking region */
	private NetNode regionInputStation;

	/*---------------------------------------------------------------*/

	/** Creates a new instance of Router.
	 * @param routingStrategies Routing strategies, one for each class.
	 */
	public Router(RoutingStrategy routingStrategies[]) {
		super();
		this.routingStrategies = routingStrategies;

		borderRouter = false;
		myBlockingRegion = null;
		regionInputStation = null;
	}

	/** Creates a new instance of blocking region border Router.
	 * @param routingStrategies Routing strategies, one for each class.
	 */
	public Router(RoutingStrategy routingStrategies[], BlockingRegion blockReg) {
		super();
		this.routingStrategies = routingStrategies;

		borderRouter = true;
		myBlockingRegion = blockReg;
		regionInputStation = myBlockingRegion.getInputStation();
	}

	/**
	 * Tells whether this router is a border router of a blocking region.
	 * @return true if this router is a border router of a blocking region.
	 */
	public boolean isBorderRouter() {
		return borderRouter;
	}

	/**
	 * Turns on the "borderRouter" behaviour.
	 * @param region the blocking region to which the owner node
	 * of this router belongs
	 */
	public void borderRouterTurnON(BlockingRegion region) {
		//sets blocking region properties
		borderRouter = true;
		myBlockingRegion = region;
		regionInputStation = myBlockingRegion.getInputStation();
		return;
	}

	/**
	 * Turns off the "borderRouter" behaviour.
	 */
	public void borderRouterTurnOFF() {
		//sets blocking region properties
		borderRouter = false;
		myBlockingRegion = null;
		regionInputStation = null;
		return;
	}

	@Override
	public Object getObject(int id, JobClass jobClass) throws jmt.common.exception.NetException {
		switch (id) {
			case PROPERTY_ID_ROUTING_STRATEGY:
				return routingStrategies[jobClass.getId()];
			default:
				return super.getObject(id);
		}
	}

	@Override
	protected int process(NetMessage message) throws jmt.common.exception.NetException{

		switch (message.getEvent()) {

			case NetEvent.EVENT_JOB:
			  
			  Job job = message.getJob();
			  
			  //EVENT_JOB
			  //if the router is not busy, an output node is chosen using
			  //the routing strategy and a message containing the job is sent to it.
			  
			  JobClass jobClass = job.getJobClass();
			  
			  //check if the routingStrategies[] is probability (fork)
			  if(!routingStrategies[jobClass.getId()].getClass().getName().equals("jmt.engine.NetStrategies.RoutingStrategies.EmpiricalStrategy")){
			    //JMT original:				
			    //choose the outNode using the corresponding routing strategy
			    NetNode outNode;
			    
			    outNode = routingStrategies[jobClass.getId()].getOutNode(getOwnerNode().getOutputNodes(), jobClass);
			    
			    // Bertoli Marco: sanity checks with closed classes and sinks were moved inside
			    // routing strategies
			    
			    if (outNode == null) {
			      
			      job.qnactrEntity.updateCurrentPlace(Enums.EntityPlaceHeader.endedin, null, Enums.NodeSection.output); //QN-Java
			      if(routerDebugPopupFlag)JOptionPane.showMessageDialog(null, "Router not by probability Did not send anything because outNode == null \n + clock: " + SimSystem.clock() + "\n NodeName: " + this.getOwnerNode().getName() , "Router.java process" , JOptionPane.INFORMATION_MESSAGE); //QN-Java
			      
			      return MSG_NOT_PROCESSED;
			    }
			    
			    //send the job to all nodes identified by the strategy
			    send(job, 0.0, outNode);
			    
			    job.qnactrEntity.updateCurrentPlace(Enums.EntityPlaceHeader.goingto, GlobalUtilities.fromGlobalNodeNameToEnumServerName(outNode.getName()), Enums.NodeSection.queue); //QN-Java
			    
			    //Border router behaviour (used in case of blocking region)
			    if (isBorderRouter()) {
			      //the owner node of this router is inside the region: if the outNode is outside
			      //the region, it means that one job has left the blocking region so the region
			      //input station (its blocking router) must receive a particular message
			      if (!myBlockingRegion.belongsToRegion(outNode)) {
			        
			        //the first time finds the input station
			        if (regionInputStation == null) {
			          regionInputStation = myBlockingRegion.getInputStation();
			        }
			        
			        myBlockingRegion.decreaseOccupation(jobClass);
			        
			        //send(NetEvent.EVENT_JOB_OUT_OF_REGION, null, 0.0, NodeSection.INPUT, regionInputStation);
			        send(NetEvent.EVENT_JOB_OUT_OF_REGION, job, 0.0, NodeSection.INPUT, regionInputStation);
			        //Since now for blocking regions the job dropping is handles manually at node level 
			        //hence need to create events with Jobs ..Modified for FCR Bug Fix
			        
			        
			      }
			    }
			    
			    if(routerDebugPopupFlag)JOptionPane.showMessageDialog(null, "Router not by probability sent \n + clock: " + SimSystem.clock() + "\n NodeName: " + this.getOwnerNode().getName() , "Router.java process" , JOptionPane.INFORMATION_MESSAGE); //QN-Java
			    
			  }
			  else{
			    //QN-Java
			    //System.out.println("Router.Java QN-Java EmpiricalStrategy jobClass: " + jobClass.getName() + ". getOwnerNode(): " + getOwnerNode().getName());
			    
			    //choose the outNode using the corresponding routing strategy
			    NodeList outNodes = null;
			    
			    outNodes = ((EmpiricalStrategy)routingStrategies[jobClass.getId()]).getOutNodes(getOwnerNode().getOutputNodes(), jobClass , this.getOwnerNode().getName(), job.qnactrEntity);
			    if (outNodes == null || outNodes.size() == 0) {
//			      System.out.println("Router.Java  MSG_NOT_PROCESSED");
			      
			      job.qnactrEntity.Trash = true;
			      job.qnactrEntity.updateCurrentPlace(Enums.EntityPlaceHeader.endedin, null, Enums.NodeSection.output); //QN-Java
			      if(routerDebugPopupFlag)JOptionPane.showMessageDialog(null, "Router by QN-Java probability Not sent because outNodes == null || outNodes.size() == 0\n + clock: " + SimSystem.clock() + "\n NodeName: " + this.getOwnerNode().getName() + ". Entity Tag: " + job.qnactrEntity.Tag , "Router.java process" , JOptionPane.INFORMATION_MESSAGE); //QN-Java
			      return MSG_NOT_PROCESSED;
			    }
			    
			    ListIterator<NetNode> itr_nodes = outNodes.listIterator();
			    boolean firstNode = true;
			    while(itr_nodes.hasNext()){
			      NetNode outNode = itr_nodes.next();
			      Job copiedJob = null;
			      //send the job to all nodes identified by the strategy
			      if (firstNode) {
			        send(job, 0.0, outNode);
			        job.qnactrEntity.updateCurrentPlace(Enums.EntityPlaceHeader.goingto, GlobalUtilities.fromGlobalNodeNameToEnumServerName(outNode.getName()), Enums.NodeSection.queue); //QN-Java
			      }
			      else{
			        //copy job and entity
			        copiedJob = new Job(job.getJobClass());
			        //copiedJob.qnactrEntity = job.qnactrEntity.cloneWithNewTag(); //this will create a new Enity, which is not desired
			        job.qnactrEntity.copyPropertiesOtherThanTagTo(copiedJob.qnactrEntity);
			        send(copiedJob, 0.0, outNode);
			        copiedJob.qnactrEntity.updateCurrentPlace(Enums.EntityPlaceHeader.goingto, GlobalUtilities.fromGlobalNodeNameToEnumServerName(outNode.getName()), Enums.NodeSection.queue); //QN-Java
			        copiedJob.qnactrEntity.To = GlobalUtilities.getServerOperatorNamesFromRawName( outNode.getName() )[0];
			      }
			      
			      
//			      System.out.println("Router.Java  outNode: " + outNode.getName() + ". Entity.Tag: " + job.qnactrEntity.Tag);
			      
			      
			      //Border router behaviour (used in case of blocking region)
			      if (isBorderRouter()) {
			        //the owner node of this router is inside the region: if the outNode is outside
			        //the region, it means that one job has left the blocking region so the region
			        //input station (its blocking router) must receive a particular message
			        if (!myBlockingRegion.belongsToRegion(outNode)) {
			          
			          //the first time finds the input station
			          if (regionInputStation == null) {
			            regionInputStation = myBlockingRegion.getInputStation();
			          }
			          
			          myBlockingRegion.decreaseOccupation(jobClass);
			          
			          //send(NetEvent.EVENT_JOB_OUT_OF_REGION, null, 0.0, NodeSection.INPUT, regionInputStation);
			          
			          if (firstNode) send(NetEvent.EVENT_JOB_OUT_OF_REGION, job, 0.0, NodeSection.INPUT, regionInputStation);
			          else send(NetEvent.EVENT_JOB_OUT_OF_REGION, copiedJob, 0.0, NodeSection.INPUT, regionInputStation);
			          
			          //Since now for blocking regions the job dropping is handles manually at node level 
			          //hence need to create events with Jobs ..Modified for FCR Bug Fix
			        }
			      }
			      
			      if(routerDebugPopupFlag){
			        if (firstNode)   JOptionPane.showMessageDialog(null, "Router by QN-Java probability sent \n + clock: " + SimSystem.clock() + "\n NodeName: " + this.getOwnerNode().getName() + ". Entity Tag: " + job.qnactrEntity.Tag , "Router.java process" , JOptionPane.INFORMATION_MESSAGE); //QN-Java
			        else             JOptionPane.showMessageDialog(null, "Router by QN-Java probability sent \n + clock: " + SimSystem.clock() + "\n NodeName: " + this.getOwnerNode().getName() + ". Entity Tag: " + copiedJob.qnactrEntity.Tag , "Router.java process" , JOptionPane.INFORMATION_MESSAGE); //QN-Java
			      }
			      if (firstNode) firstNode = false;
            
			    } //end of while
			    
			  }
			  
			  return MSG_PROCESSED;

			case NetEvent.EVENT_ACK:
				//EVENT_ACK
				//
				//An ack is sent back to the service section.
				//

				sendBackward(NetEvent.EVENT_ACK, message.getJob(), 0.0);
				break;

			default:
				return MSG_NOT_PROCESSED;
		}
		return MSG_PROCESSED;
	}
}
