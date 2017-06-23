/**
 * The simjava package is a support library for discrete
 * event simulations.
 */

/**
 * 2013. Modified for QN-Java project
 * 
 * [2013-06-30] add reference to qnactrSim
 *              add Txt File close in runStop()
 *              
 * [2013-06-28] add update of clock display on GUI 
 * 
 */



package jmt.engine.simEngine;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.JOptionPane;

import qnactr.sim.GlobalUtilities;
import qnactr.sim.QnactrSimulation;

import jmt.engine.QueueNet.NetNode;

/**
 * This is the system class which manages the simulation. All
 * the members of this class are static, so there is no need to
 * create an instance of this class.
 */
public class SimSystem {

	private static boolean DEBUG = false;

	// Private data members
	static private List<SimEntity> entities; // The current entity list

	static private EventQueue future; // The future event queue

	static private EventQueue deferred; // The deferred event queue

	static private double clock; // Holds the current global simulation time
	static private boolean running; // Tells whether the run() member been called yet
	static private NumberFormat nf;
	
	//QN-Java
	static public QnactrSimulation qnactrSims[]; 

	//
	// Public library interface
	//

	// Initializes system
	/** Initializes the system, this function works as a
	 * constructor, and should be called at the start of any simulation
	 * program. It comes in several flavours depending on what context
	 * the simulation is running.<P>
	 * This is the simplest, and should be used for standalone simulations
	 * It sets up trace output to a file in the current directory called
	 * `tracefile'
	 */
	static public void initialize() {

		entities = new ArrayList<SimEntity>();

		// future = new ListEventQueue();
		// future = new CircularEventQueue();
		// future = new SuperEventQueue();
		future = new HybridEventQueue();

		deferred = new ListEventQueue();

		clock = 0.0;
		running = false;

		// Set the default number format
		nf = NumberFormat.getInstance();
		nf.setMaximumFractionDigits(4);
		nf.setMinimumFractionDigits(2);
		
	}

	/** Returns the number format used for generating
	 * times in trace lines
	 */
	static public NumberFormat getNumberFormat() {
		return nf;
	}

	// The two standard predicates
	/** A standard predicate that matches any event. */
	static public SimAnyP SIM_ANY = new SimAnyP();
	/** A standard predicate that does not match any events. */
	static public SimNoneP SIM_NONE = new SimNoneP();

	// Public access methods

	/** Get the current simulation time.
	 * @return The simulation time
	 */
	static public double clock() {
		return clock;
	}

	/** A different name for <tt>SimSystem.clock()</tt>.
	 * @return The current simulation time
	 */
	static public double getClock() {
		return clock;
	}

	/** Gets the current number of entities in the simulation
	 * @return A count of entities
	 */
	static public int getNumEntities() {
		return entities.size();
	}

	/** Finds an entity by its id number.
	 * @param id The entity's unique id number
	 * @return A reference to the entity, or null if it could not be found
	 */
	static final public SimEntity getEntity(int id) {
		return entities.get(id);
	}

	/** Finds an entity by its name.
	 * @param name The entity's name
	 * @return A reference to the entity, or null if it could not be found
	 */
	static public SimEntity getEntity(String name) {
		SimEntity ent, found = null;

		for (Iterator<SimEntity> it = entities.iterator(); it.hasNext();) {
			ent = it.next();
			if (name.compareTo(ent.getName()) == 0) {
				found = ent;
			}
		}
		if (found == null) {
			System.out.println("SimSystem: could not find entity " + name);
		}
		return found;
	}

	/** Finds out an entity unique id number from its name.
	 * @param name The entity's name
	 * @return The entity's unique id number, or -1 if it could not be found
	 */
	static public int getEntityId(String name) {
		return entities.indexOf(getEntity(name));
	}

	/** Adds a new entity to the simulation.
	 * This is now done automatically in the SimEntity constructor,
	 * so there is no need to call this explicitly.
	 * @param e A reference to the new entity
	 */
	static public void add(SimEntity e) {
		SimEvent evt;
		if (running()) {
			/* Post an event to make this entity */
			evt = new SimEvent(SimEvent.CREATE, clock, e.getId(), 0, 0, e);
			future.add(evt);
		} else {
			if (e.getId() == -1) {

				//-1 is the default value written by the constructor.
				// Now a new ID is set

				// Only add once!
				e.setId(entities.size());
				entities.add(e);
			}
		}
	}

	/** Adds a new entity to the simulation, when the simulation is running.
	 * Note this is an internal method and should not be called from
	 * user simulations. Use <tt>add()</tt> instead to add entities
	 * on the fly.
	 * @param e A reference to the new entity
	 */
	static synchronized void addEntityDynamically(SimEntity e) {
		e.setId(entities.size());
		entities.add(e);
		e.start();
	}

	/**
	 * Starts the simulation running, by calling the start() method of each entity.
	 * Of course this should be called after all the entities have been setup and added,
	 * and their ports linked.
	 */
	static public void runStart() {
		SimEntity ent;
		running = true;
		// Start all the entities' threads
		if (DEBUG) {
			System.out.println("SimSystem: Starting entities");
		}
		for (Iterator<SimEntity> it = entities.iterator(); it.hasNext();) {
			ent = it.next();
			ent.start();
		}

	}

	/**
	 * Runs one tick of the simulation: the system looks for events in the future queue.
	 * @return <tt>false</tt> if there are no more future events to be processed
	 */
	static public boolean runTick() throws jmt.common.exception.NetException {
		// If there are more future events then deals with them
		SimEvent event;
		
		//String showString = "runTick()\nclock: " + SimSystem.clock() + ".\nfuture.size(): " + future.size() + "\n" + future.toString(); //CAO
		//JOptionPane.showMessageDialog(null, showString, "SimSystem.java", JOptionPane.INFORMATION_MESSAGE); //CAO 
		
		//JOptionPane.showMessageDialog(null, "getNumEntities: " + getNumEntities(), "SimSystem.java", JOptionPane.INFORMATION_MESSAGE); //CAO
		//JOptionPane.showMessageDialog(null, "getEntity: " + ((NetNode)getEntity("s2")).getIntNodeProperty(NetNode.PROPERTY_ID_ARRIVED_JOBS), "SimSystem.java", JOptionPane.INFORMATION_MESSAGE); //CAO
		
		//abort(); // CAO
		
		
		
		if (future.size() > 0) {
		  
		  
		  
			event = future.pop();

			//JOptionPane.showMessageDialog(null, "runTick() + clock: " + SimSystem.clock() + ". event: " + event.scheduledBy() , "SimSystem.java", JOptionPane.INFORMATION_MESSAGE); //CAO
			
			processEvent(event);
			double now = event.eventTime();
			//double nowRealTime1 = System.currentTimeMillis();
			
			// Checks if next events are at same time...
			boolean trymore = (future.size() > 0);
			while (trymore) {
				event = future.peek();
				if (event.eventTime() == now) {
					processEvent(future.pop());
					trymore = (future.size() > 0);
				} else {
					trymore = false;
				}
			}
			
			
			
			//QN-Java
			//System.out.println(clock() == now); //always true 
      //String roundedClock = GlobalUtilities.formatDoubleToString(clock(), 3);
      //String roundedClock = Double.toString(GlobalUtilities.round(clock(), 3, BigDecimal.ROUND_HALF_UP));
      //GlobalUtilities.setMainWindowStatusLabelText("Clock (s): " + roundedClock);
			
			//at here all events at the clock "now" has been processed. consume real world time from last shown clock to now.
			if(future.size() > 0){
        double currentSimClockTime = now;
        if(QnactrSimulation.simSpeedFactor > 0.0){
          if(currentSimClockTime < 1.79E308){ //if the next event is not the ending events
            
//            double simClockStep = currentSimClockTime - QnactrSimulation.simLastShownClock;
//            double realClockStep = simClockStep * QnactrSimulation.simSpeedFactor;
//            double nowRealTime2 = System.currentTimeMillis();
//            double alreadyPassedTime = (nowRealTime2 - nowRealTime1) / 1000.0; //in second, in case the computation or pause used some time
//            double alreadyPassedTime = (nowRealTime2 - QnactrSimulation.simLastUpdatedRealSystemClock) / 1000.0; //in second, in case the computation or pause used some time
//            double timeToSleep = realClockStep - alreadyPassedTime;
            
            double currentSimClock = currentSimClockTime;
            double currentRealTimePassed =  (System.currentTimeMillis() - QnactrSimulation.simStartRealClockTime) / 1000.0;
            double timeToSleep = currentSimClock / QnactrSimulation.simSpeedFactor - currentRealTimePassed; 
            
            
            
//            System.out.println("SimSystem.java timeToSleep: " + timeToSleep);
            if(timeToSleep <= 0) {
//              System.out.println("SimSystem.java Warning! timeToSleep < 0, timeToSleep: " + timeToSleep);
              timeToSleep = 0.0;
              QnactrSimulation.simLastShownClock = currentSimClockTime;
              GlobalUtilities.setMainWindowStatusLabelText("Clock (s): " + GlobalUtilities.formatDoubleToString(QnactrSimulation.simLastShownClock, 3));
            }
            else{
              QnactrSimulation.simLastShownClock = currentRealTimePassed * QnactrSimulation.simSpeedFactor;
            }
            
            double sleepCycle = QnactrSimulation.simSleepCycle ; // second
            while ( timeToSleep > 0){
              double microSleep;
              if(timeToSleep >= sleepCycle){
                microSleep = sleepCycle;              
              }
              else {
                microSleep = timeToSleep;             
              }
              timeToSleep -= microSleep;
              
              try
              {
                Thread.sleep((long) (microSleep * 1000.0));
              } catch (InterruptedException e)
              {
                // TODO Auto-generated catch block
                e.printStackTrace();
              }
              
              QnactrSimulation.simLastShownClock += microSleep * QnactrSimulation.simSpeedFactor;
              GlobalUtilities.setMainWindowStatusLabelText("Clock (s): " + GlobalUtilities.formatDoubleToString(QnactrSimulation.simLastShownClock, 3));
            }
            
            
            //JOptionPane.showMessageDialog(null, "runTick() + clock: " + SimSystem.clock() + ". nextEvent.eventTime(): " + nextEvent.eventTime() , "SimSystem.java", JOptionPane.INFORMATION_MESSAGE); //CAO
            QnactrSimulation.simEndSimulationClockTime = currentSimClockTime;
            QnactrSimulation.simEndRealClockTime = System.currentTimeMillis();
//            double simulationUsedRealTimeDuration = (QnactrSimulation.simEndRealClockTime - QnactrSimulation.simStartRealClockTime) / 1000.0; //second
//            System.out.println("SimSystem.java at sim clock: " + currentSimClockTime + " Real-world clock - sim clock: " + (simulationUsedRealTimeDuration - QnactrSimulation.simEndSimulationClockTime) );
            
          }   
        }
        else{ //as fast as possible, no sleep
          GlobalUtilities.setMainWindowStatusLabelText("Clock (s): " + GlobalUtilities.formatDoubleToString(SimSystem.clock(), 3));
          if(currentSimClockTime < 1.79E308){
            QnactrSimulation.simEndSimulationClockTime = currentSimClockTime;
            QnactrSimulation.simEndRealClockTime = System.currentTimeMillis();
          }
        }       
      }
			
			//QN-Java old backup
//			if(future.size() > 0){
//			  SimEvent nextEvent = future.peek();
//			  double nextEventSimClockTime = nextEvent.eventTime();
//			  if(nextEventSimClockTime < 1.79E308){ //if the next event is not the ending events
//			    double simClockNew = clock();
//			    double simClockStep = nextEventSimClockTime - now;
//			    double realClockStep = simClockStep * QnactrSimulation.simSpeedFactor;
//			    double nowRealTime2 = System.currentTimeMillis();
//			    double alreadyPassedTime = (nowRealTime2 - nowRealTime1) / 1000.0; //in second, in case the computation or pause used some time
//			    double timeToSleep = realClockStep - alreadyPassedTime;
//			    if(timeToSleep < 0) timeToSleep = 0.0;
//			    
//			    double sleepCycle = QnactrSimulation.simSleepCycle ; // second
//			    while ( timeToSleep > 0){
//			      double microSleep;
//			      if(timeToSleep >= sleepCycle){
//			        microSleep = sleepCycle;			        
//			      }
//			      else {
//			        microSleep = timeToSleep;			        
//			      }
//			      timeToSleep -= microSleep;
//			      
//  			    try
//            {
//              Thread.sleep((long) (microSleep * 1000.0));
//            } catch (InterruptedException e)
//            {
//              // TODO Auto-generated catch block
//              e.printStackTrace();
//            }
//  			    
//  			    simClockNew += microSleep / QnactrSimulation.simSpeedFactor;
//  			    GlobalUtilities.setMainWindowStatusLabelText("Clock (s): " + GlobalUtilities.formatDoubleToString(simClockNew, 3));
//			    }
//			    //JOptionPane.showMessageDialog(null, "runTick() + clock: " + SimSystem.clock() + ". nextEvent.eventTime(): " + nextEvent.eventTime() , "SimSystem.java", JOptionPane.INFORMATION_MESSAGE); //CAO
//			    
//			  }
//			}
			
			
		} else {
			running = false;
		}
		return running;
	}

	/** Stops the simulation, by calling the poison() method of each SimEntity.
	 */
	static public void runStop() {
		SimEntity ent;
		// Attempt to kill all the entity threads
		for (Iterator<SimEntity> it = entities.iterator(); it.hasNext();) {
			ent = it.next();
			ent.poison();
		}
		if (DEBUG) {
			System.out.println("Exiting SimSystem.run()");
		}
		
		//moved to Simulation.java
//		//QN-Java
//		GlobalUtilities.setMainWindowStatusLabelText("Clock (s): "); 
//    for (int i = 0; i < qnactrSims.length; i ++) {
//      qnactrSims[i].files.closeAllTxtFileWriters();
//    }
	}

	//
	// Package level methods
	//

	static boolean running() {
		return running;
	}

	// Entity service methods

	// Called by an entity just before it becomes non-RUNNABLE
	//	static void paused() {
	//		onestopped.v();
	//	}

	static synchronized RemoveToken hold(int src, double delay) {
		SimEvent e = new SimEvent(SimEvent.HOLD_DONE, clock + delay, src);
		future.add(e);
		return new RemoveToken(e);
	}

	static synchronized RemoveToken send(int src, int dest, double delay, int tag, Object data) {
		SimEvent e = new SimEvent(SimEvent.SEND, clock + delay, src, dest, tag, data); // Shi Cao note: data Object is of Class Job
		future.add(e);
		return new RemoveToken(e);
	}

	/**
	 * Given a remove token, this method will remove a future or deferred simulation event
	 * @param token the remove token
	 * @return true if the event was found and removed, false otherwise.
	 */
	static public synchronized boolean remove(RemoveToken token) { //QN-Java, change to public
		if (token.isDeferred()) {
			return deferred.remove(token.getEvent());
		} else {
			return future.remove(token.getEvent());
		}
	}

	static synchronized void wait(int src) {

	}

	static synchronized int waiting(int d, SimPredicate p) {
		int w = 0;
		SimEvent event;
		//DEK (Federico Granata) 25-11-2003
		for (Object element : deferred) {
			event = (SimEvent) element;
			if (event.getDest() == d) {
				if (p.match(event)) {
					w++;
				}
			}
		}
		return w;

		/* old
		for (e = deferred.elements(); e.hasMoreElements();) {
			ev = (SimEvent) e.nextElement();
			if (ev.getDest() == d) {
				//	trcout.println("t: in waiting, - event from "+ev.getSrc()+" to "+
				//	       ev.getDest() + " type:"+ev.type()+" time:"+ev.eventTime());

				if (p.match(ev))
					w++;
			}
		}
		return w;*/
		//end DEK (Federico Granata) 25-11-2003
	}

	static synchronized void select(int src, SimPredicate p) {
		SimEvent ev = null;
		boolean found = false;

		// retrieve + remove event with dest == src
		for (Iterator it = deferred.iterator(); it.hasNext() && !found;) {
			ev = (SimEvent) it.next();
			if (ev.getDest() == src) {
				if (p.match(ev)) {
					deferred.remove(ev);
					found = true;
				}
			}
		}
		if (found) {
			entities.get(src).setEvbuf((SimEvent) ev.clone());

		} else {
			entities.get(src).setEvbuf(null);

		}
	}

	static synchronized void cancel(int src, SimPredicate p) {
		SimEvent ev = null;
		boolean found = false;

		// retrieves + remove event with dest == src
		for (Iterator it = future.iterator(); it.hasNext() && !found;) {
			ev = (SimEvent) it.next();
			if (ev.getSrc() == src) {
				if (p.match(ev)) {
					it.remove();
					found = true;
				}
			}
		}
		if (found) {
			entities.get(src).setEvbuf((SimEvent) ev.clone());

		} else {
			entities.get(src).setEvbuf(null);

		}
	}

	static RemoveToken putback(SimEvent ev) {
		deferred.add(ev);
		return new RemoveToken(ev, true);
	}

	//
	// Private internal methods
	//

	static private void processEvent(SimEvent e) throws jmt.common.exception.NetException {
		int dest, src;
		SimEntity destEnt;

		//System.out.println("SimSystem: Processing event");
		// Update the system's clock
		if (e.eventTime() < clock) {
			throw new jmt.common.exception.NetException("SimSystem: Error - past event detected! \n" + "Time: " + clock + ", event time: "
					+ e.eventTime() + ", event type: " + e.getType() + future);
		}
		clock = e.eventTime();

		// Ok now process it
		switch (e.getType()) {

			case (SimEvent.SEND):
				// Checks for matching wait
				dest = e.getDest();
				if (dest < 0) {
					throw new jmt.common.exception.NetException("SimSystem: Error - attempt to send to a null entity");
				} else {
					destEnt = entities.get(dest);
					if (destEnt.getState() == SimEntity.WAITING) {
						SimPredicate p = destEnt.getWaitingPred();

						if (p == null) {
							//the entity was waiting for a generic predicate
							destEnt.setEvbuf((SimEvent) e.clone());
							destEnt.setState(SimEntity.RUNNABLE);
							try {
								destEnt.execute();
							} catch (jmt.common.exception.NetException e1) {
								abort();
								throw e1;
							}
						} else {
							//the entity was waiting for events with a specified predicate
							//this event matches with such predicate??
							if (destEnt.getWaitingPred().match(e)) {
								p = null;
								destEnt.setEvbuf((SimEvent) e.clone());
								destEnt.setState(SimEntity.RUNNABLE);
								try {
									destEnt.execute();
								} catch (jmt.common.exception.NetException e1) {
									abort();
									throw e1;
								}
							} else {
								//the event doesn't match with the predicate, so it's put in the deferred queue
								destEnt.simPutback(e);
							}
						}
					} else {
						//if the entity is not WAITING the event is put in the deferred queue
						deferred.add(e);
					}
				}
				break;

			case (SimEvent.ENULL):
				throw new jmt.common.exception.NetException("SimSystem: Error - event has a null type");

			case (SimEvent.CREATE):
				SimEntity newe = (SimEntity) e.getData();
				addEntityDynamically(newe);
				break;

			case (SimEvent.HOLD_DONE):
				src = e.getSrc();
				if (src < 0) {
					throw new jmt.common.exception.NetException("SimSystem: Error - NULL entity holding");
				} else {
					entities.get(src).setState(SimEntity.RUNNABLE);
					entities.get(src).restart();
				}
				break;
		}
	}

	/**
	 * Aborts the simulation!
	 *
	 */
	static public void abort() {
		running = false;
		if (DEBUG) {
			System.out.println("Simulation Aborted");
		}

	}
	
	//QN-Java
	static public EventQueue getFutureQueue(){
	  return future;
	}
	
}
