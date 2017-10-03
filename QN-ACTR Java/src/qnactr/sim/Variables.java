/**
 * 2013 QN-Java project file
 * 
 */

package qnactr.sim;

import qnactr.GUI.TaskVisualization2D;
import qnactr.objectDesigner.*;
import java.util.*;

/**
 * for each QnactrSimulation object representing an HMI unit,
 * here are the variables/objects/modules
 * The word "module" is better to be limited to QN-ACTR mental modules.
 * 
 * @author shicao
 *
 */
public class Variables {
  public Variables(QnactrSimulation Sim){
    sim = Sim;
    
	// added by Yelly
	// variables concerning merging into DM
	DeclarativeModule__Merge_Chunk_Into_DM_do_no_merge.add("visual-location-world3d-driving");		//these are near and far points, assume that they are not stored in DM.
	  
	DeclarativeModule__Merge_Chunk_Into_DM_do_no_rename.add("world3d-driving-criticalelement-vehicle");
	DeclarativeModule__Merge_Chunk_Into_DM_do_no_rename.add("world3d-driving-criticalelement-sign");
	DeclarativeModule__Merge_Chunk_Into_DM_do_no_rename.add("world3d-driving-speed");
	DeclarativeModule__Merge_Chunk_Into_DM_do_no_rename.add("world3d-driving-criticalelement-vehicle-img");
	DeclarativeModule__Merge_Chunk_Into_DM_do_no_rename.add("world3d-driving-criticalelement-sign-img");
	DeclarativeModule__Merge_Chunk_Into_DM_do_no_rename.add("world3d-driving-speed-img");
	DeclarativeModule__Merge_Chunk_Into_DM_do_no_rename.add("visual-location-world3d-driving-criticalelement");
	DeclarativeModule__Merge_Chunk_Into_DM_do_no_rename.add("visual-location-world3d-driving-speed");
  }
  
  QnactrSimulation sim;  
  
//TODO // public IAnimatorRuntime Animator;
	public Animator_Module animatorModule = new Animator_Module();
//TODO // public MAAD.Animator3D.Animator3D Animator3D;
	public Animator3D_Module animator3DModule = new Animator3D_Module();
	public Audio_Display audioDisplay = new Audio_Display();
	public Audio_Module audioModule = new Audio_Module();
	public Aural_Buffer auralBuffer = new Aural_Buffer();
	public Aural_Location_Buffer auralLocationBuffer = new Aural_Location_Buffer();
	public Central_Parameters_Module centralParametersModule = new Central_Parameters_Module();
	public double clock;
	public LinkedList<String> correctResponseToEachVisualText = new LinkedList<String>();
	public Declarative_Module declarativeModule = new Declarative_Module();
	public Device_Module deviceModule = new Device_Module();
	public String displayAndResponseDurationRandomization;
	public double displayItemDelay;
	 //TODO // public MAAD.Simulator.Utilities.Distributions Distributions;
	// public Entity entity;
	//public int entityNumber = 1;
	public Goal_Buffer goalBuffer = new Goal_Buffer();
	public Imaginal_Buffer imaginalBuffer = new Imaginal_Buffer();
	public Imaginary_Module imaginaryModule = new Imaginary_Module();
	public Intentional_Module intentionalModule = new Intentional_Module();
	public Manual_Buffer manualBuffer = new Manual_Buffer();
	public boolean messageOn;
	 //TODO // public MAAD.Simulator.Model<Entity> Model;
	public Motor_Module motorModule = new Motor_Module();
	public Network_Details_Visualization_Module networkDetailsVisualizationModule = new Network_Details_Visualization_Module();
	public Printing_Module printingModule = new Printing_Module();
	public Production_Compilation_Module productionCompilationModule = new Production_Compilation_Module();
	public Production_Module productionModule = new Production_Module();
	
	
	public LinkedList<String> programGlobalVar__All_Server_Node_ID_List = new LinkedList<String>();
	public LinkedList<String> programGlobalVar__Animator3D_Comment_IDs = new LinkedList<String>();
	public Hashtable programGlobalVar__Block_Variable_Common_RandomMethod_Last_Seed=new Hashtable();
	public int programGlobalVar__Color_String_To_Color_Type;
	public LinkedList<LinkedList<String>> programGlobalVar__DeclarativeModule_Add_DM_Input_List = new LinkedList<LinkedList<String>> ();
	public LinkedList<Integer> programGlobalVar__DeclarativeModule_Beginning_To_Timing_And_Ending_MatchedChunkID = new LinkedList<Integer> ();
	public Chunk programGlobalVar__DeclarativeModule_Beginning_To_Timing_And_Ending_RetrievedChunk = new Chunk();
	public LinkedList<LinkedList<String>> programGlobalVar__DeclarativeModule_Sdp_Input= new LinkedList<LinkedList<String>> ();
	public LinkedList<String> programGlobalVar__DeclarativeModule_Set_All_Base_Levels_Input= new LinkedList<String> ();
	public LinkedList<Three_Tuple> programGlobalVar__DeclarativeModule_Set_Base_Levels_Input = new LinkedList<Three_Tuple>();
	public LinkedList<Three_Tuple> programGlobalVar__DeclarativeModule_Set_Similarities_Input = new LinkedList<Three_Tuple>();
	public Hashtable<String, Integer> programGlobalVar__Event_Priority_Table=new Hashtable<String, Integer>();
	public String programGlobalVar__IntentionalModule_Initial_Goal_Focus_Chunk_Name = "";
	public String programGlobalVar__IntentionalModule_Initial_Goal_2_Focus_Chunk_Name = "";
	public boolean programGlobalVar__MotorModule_Start_Hand_At_Mouse;
	public Hashtable<String, Integer> programGlobalVar__New_Name_Simple_Count = new Hashtable<String, Integer>();
  public LinkedList<LinkedList<String>> programGlobalVar__ProductionCompilationModule_Equivalent_Var_Var_Constant_List = new LinkedList<LinkedList<String>>();
  public LinkedList<LinkedList<String>> programGlobalVar__ProductionModule_P_Rules_Definition_Input = new LinkedList<LinkedList<String>>();
  public LinkedList<LinkedList<String>> programGlobalVar__ProductionModule_Spp_Input = new LinkedList<LinkedList<String>>();


  /**
   * //need to be from [1, 2147483562] [1, M1-1]
used for customized random sequencies
used for common random number method to compare with ACT-R, 
there the same random number generator is needed, which use Discrete event simulation textbook random number generator.
ProgramUtilitiesFun__Combined_Linear_Congruential_Generators

set by sgp :seed ( num1 num2 ) as ACT-R, but ACT-R use the Mersenne Twister algorithm (see ACT-R reference-manual), QN-ACTR use Combined_Linear_Congruential_Generators Example BCNN 7.5 (following L'Ecuyer, 1988) p258
   */
  public int programGlobalVar__Rand1_Seed1 = 1;
  
  /**
   * //from [1, M2-1] //int M2=2147483399;
used for customized random sequencies
used for common random number method to compare with ACT-R, 
there the same random number generator is needed, which use Discrete event simulation textbook random number generator.
ProgramUtilitiesFun__Combined_Linear_Congruential_Generators

set by sgp :seed ( num1 num2 ) as ACT-R, but ACT-R use the Mersenne Twister algorithm (see ACT-R reference-manual), QN-ACTR use Combined_Linear_Congruential_Generators Example BCNN 7.5 (following L'Ecuyer, 1988) p258
   */
  public int programGlobalVar__Rand1_Seed2 = 1;
  
  /**
   * used for ACT-R noise
used for common random number method to compare with ACT-R, 
there the same random number generator is needed, which use Discrete event simulation textbook random number generator.
ProgramUtilitiesFun__Combined_Linear_Congruential_Generators

1059 as used in shi-random, a random file for ACT-R
   */
  public int programGlobalVar__Rand2_Seed1 = 10;
  
  /**
   * used for ACT-R noise 
used for common random number method to compare with ACT-R, 
there the same random number generator is needed, which use Discrete event simulation textbook random number generator.
ProgramUtilitiesFun__Combined_Linear_Congruential_Generators

571054 as used in shi-random, a random file for ACT-R
   */
  public int programGlobalVar__Rand2_Seed2 = 571054;
  
	public Retrieval_Buffer retrievalBuffer = new Retrieval_Buffer();
	public Speech_Module speechModule = new Speech_Module();
//  public MAAD.Simulator.Utilities.IRuntimeTask Task;
	
	public double spaceDrivingVar__Position_Acc_Constant = 1.0; // m/ss
	public double spaceDrivingVar__Rotation_Acc_Constant = 2.0; // deg/ss
	
	/**
	 * Ob 1-3, X Y Z, in cm. 
	 * Ob 4-6, Pitch, Yaw, Roll
	 */
	public Six_Tuple spaceDrivingVar__Self_Initiatial_Location_And_Rotation= new Six_Tuple();
	public Six_Tuple spaceDrivingVar__Target_Initiatial_Location_And_Rotation= new Six_Tuple();
	public Task_Template taskTemplate = new Task_Template();
  public Temporal_Buffer temporalBuffer = new Temporal_Buffer();
  public Temporal_Module temporalModule = new Temporal_Module();
  public Utility_Module utilityModule = new Utility_Module();
  public Hashtable<Double,Double> utilization__Audio_Module_Changes_In_A_Second = new Hashtable<Double,Double>();
  public double utilization__Audio_Module_In_A_Second;
  public double utilization__Clock_Minus_1;
  public double utilization__Cognitive_SubNetwork_In_A_Second;
  public Hashtable<Double,Double> utilization__Declarative_Module_Changes_In_A_Second = new Hashtable<Double,Double>();
  public double utilization__Declarative_Module_In_A_Second;
  public Hashtable<Double,Double> utilization__Imaginary_Module_Changes_In_A_Second = new Hashtable<Double,Double>();
  public double utilization__Imaginary_Module_In_A_Second;
  public Hashtable<Double,Double> utilization__Motor_Module_Changes_In_A_Second = new Hashtable<Double,Double>();
  public double utilization__Motor_Module_In_A_Second;
  public double utilization__Motor_SubNetwork_In_A_Second;
  public double utilization__Perceptual_SubNetwork_In_A_Second;
  public Hashtable<Double,Double> utilization__Production_Module_Changes_In_A_Second = new Hashtable<Double,Double>();
  public double utilization__Production_Module_In_A_Second;
  public Hashtable<Double,Double> utilization__Speech_Module_Changes_In_A_Second = new Hashtable<Double,Double>();
  public double utilization__Speech_Module_In_A_Second;
  public Hashtable<Double,Double> utilization__Vision_Module_Changes_In_A_Second = new Hashtable<Double,Double>();
  public double utilization__Vision_Module_In_A_Second;
  public double varCopy__DriverCar_Pitch;
  public double varCopy__DriverCar_Roll;
  public double varCopy__DriverCar_Speed;
  public double varCopy__DriverCar_Yaw;
  public int visicon_History_Count;
  public Vision_Module visionModule = new Vision_Module();
  public Visual_Buffer visualBuffer = new Visual_Buffer();
  public Visual_Display visualDisplay = new Visual_Display();
  public Visual_Location_Buffer visualLocationBuffer = new Visual_Location_Buffer();
  public Vocal_Buffer vocalBuffer = new Vocal_Buffer();
  public World3D_Template world3DTemplate = new World3D_Template();
  public int visualization__DM_Chunk_Number = 0;
  public String visualization__Visual_Display = "";
  public String visualization__Audio_Display = "";
  public String visualization__Vision_Module_Last_Attended_Visicon_Name = "";
  public Network_Details_Visualization_Module network_Details_Visualization_Module = new Network_Details_Visualization_Module();
  public String visualization__Audicon = "";
  public String visualization__Aural_Buffer_Contents = "";
  public String visualization__Aural_Location_Buffer_Contents = "";
  public String visualization__Goal_Buffer_2_Contents = "";
  public String visualization__Goal_Buffer_Contents = "";
  public String visualization__Imaginal_Buffer_Contents = "";
  public String visualization__Retrieval_Buffer_Contents = "";
  public String visualization__Speech_Details = "";
  public String visualization__Temporal_Buffer_Contents = "";
  public String visualization__Visicon = "";
  public String visualization__Visual_Buffer_Contents = "";
  public String visualization__Visual_Location_Buffer_Contents = "";
  public String programGlobalVar__Use_Predefined_Model_Setup = "";
  public ArrayList<String> programGlobalVar__High_Level_Process_Module_Name_List = new ArrayList<String>();
  public LinkedList<String> programGlobalVar__ProductionModule_Currently_Processing_Rules_Name_List = new LinkedList<String> ();
  /**
   * fragment_length sets the length of a fragment. A segment is further divided into fragments. For a straight road, it is set as the length of the segment. For a curve road, it is set at ? (meter) by default. Fragments will be used for drawing lines in Animator3D visualization. In the case that the last fragment is smaller than the length set here, the actual length is used for that fragment. 
   */
  public double programGlobalVar__World3D_Curve_Fragment_Length = 10.0;
  public Hashtable programGlobalVar__Hashtable_Experiment_Specific_Data = new Hashtable();
  public String visualization__Day_Block_Trial = "";
  public String visualization__Block_Variables = "";
  public String visualization__Last_Executed = "";
  public int programGlobalVar__ProgrammingChecker_Declarative_Module_And_Trigger_Buffer_Retrieve_A_Chunk_Order;
  public int programGlobalVar__Trigger_Buffer_To_Declarative_Module_Rand2_Seed1;
  public int programGlobalVar__Trigger_Buffer_To_Declarative_Module_Rand2_Seed2;
  public String visualization__Last_Match = "";
  public String visualization__Motor_Details = "";
  public int sampleModelVar__transcript_typing_char_num_in_word;
  public String programGlobalVar__Who_Drive = "";

	 //TODO // public MAAD.Simulator.Utilities.IRuntimeTask Task;
	
	//TODO: add other variables
  
  // added by Yelly
  // output qn-answer
  public boolean qn_answer = false;
  
  // added by Yelly
  // variables concerning merge_into_DM
  // to improve performance
  public List<String> DeclarativeModule__Merge_Chunk_Into_DM_do_no_merge = new ArrayList<String> ();
  public List<String> DeclarativeModule__Merge_Chunk_Into_DM_do_no_rename = new ArrayList<String> ();
	
  public TaskVisualization2D taskVisualization2D = QnactrSimulation.taskVisualization2D;
  public boolean programGlobalVar__MotorModule_Start_Right_Hand_At_Touch_Screen = false;
  public boolean programGlobalVar__MotorModule_Start_Left_Hand_At_Touch_Screen = false;
	
  public Variables(){
		
	displayAndResponseDurationRandomization = "fixed_order";
	displayItemDelay =  0.0;

	messageOn = false;
	programGlobalVar__Color_String_To_Color_Type = 0;
		
	utilization__Audio_Module_In_A_Second = 0.0F;
	utilization__Clock_Minus_1 = 0.0F;
	utilization__Cognitive_SubNetwork_In_A_Second=0.0F;
	utilization__Declarative_Module_In_A_Second = 0.0F;
	utilization__Imaginary_Module_In_A_Second = 0.0F;
	utilization__Motor_Module_In_A_Second=0.0F;
	utilization__Motor_SubNetwork_In_A_Second=0.0F;
	utilization__Perceptual_SubNetwork_In_A_Second=0.0F;
	utilization__Production_Module_In_A_Second=0.0F;
	utilization__Speech_Module_In_A_Second=0.0F;
	utilization__Vision_Module_In_A_Second = 0.0F;
	varCopy__DriverCar_Pitch = 0.0F;
	varCopy__DriverCar_Roll = 0.0F;
	varCopy__DriverCar_Speed = 0.0F;
	varCopy__DriverCar_Yaw = 0.0F;
	visicon_History_Count=0;
    
  }
}
