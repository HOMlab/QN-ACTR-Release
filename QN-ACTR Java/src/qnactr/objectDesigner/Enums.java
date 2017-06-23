package qnactr.objectDesigner;

import javax.swing.JOptionPane;

import jmt.engine.simEngine.SimSystem;

public class Enums {
	
	public static enum ServerName {
	  none, 
	  audiodisplay, audiomodule, aurallocationbuffer, auralbuffer, declarativemodule, execution, goalbuffer2, goalbuffer,
		imaginalbuffer, imaginarymodule, intentionalmodule, manualbuffer, matchingandselection, motorexecution, motorinitiation,
		motormodule, motorpreparation, retrievalbuffer, speechexecution, speechinitiation, speechmodule, speechpreparation, 
		temporalbuffer, temporalmodule, triggerbuffer, triggerbufferloop,visionmodule, visionmoduletrigger, visionmoduletriggerloop,
		visuallocationbuffer, visualbuffer, visualdisplay, vocalbuffer,
		
		testbuffer, testmodule, firsttriggerentity, prescheduledevents, timertriggeringfeedbackordisplay, entitydirectcastdelay, 
		visualandaudiodisplayschedule, controltodisplay, dynamicevents, controlvoicekey, controlmotorreleasequeue, delayedfunctioncallnoreturnvalue, 
		controlmotor, world3dcyclicrefresh, othertasksdummy, toexit, exit,
		
		recurrentevent
	}
	
	public static enum ServiceStage {
		Release, Beginning, 
		Ending, Timing, Route
	}
	
	public static enum EntityPlaceHeader{
	  none, 
	  goingto, in, left, abortedfrom, endedin
	}
	
	public static enum NodeSection {
	  none, 
	  queue, server, output
	}

	
}
