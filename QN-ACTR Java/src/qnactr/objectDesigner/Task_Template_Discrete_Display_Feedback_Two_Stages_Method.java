package qnactr.objectDesigner;

import java.util.*;

public class Task_Template_Discrete_Display_Feedback_Two_Stages_Method {
	public int Add_Number_Of_Blocks_Per_Day;
	public int Add_Number_Of_Days;
	public String Add_Number_Of_Trials_Per_Block;
	
	
	/**
	 * Hashtable of <string, LinkedList <string> >
< string is :block_variable_name,  LinkedList < string >  is the list of values  >
	 */
	public Hashtable<String, LinkedList <String> > Block_Variable=new Hashtable<String, LinkedList <String> >();
	
	/**
	 * Hashtable of <string, string >
< day_block_name,   content >

e.g.,  < 1__2__:block_variable_1 ,   1.0 >
a content for each day each block each variable name.

	 */
	public Hashtable<String,String> Block_Variable_Content_By_Day_Block_VName=new Hashtable<String,String>();
	public Hashtable<String,String> Block_Variable_Randomization=new Hashtable<String,String>();
	public LinkedList<String> Block_Variable_Randomization_Ordered_Name=new LinkedList<String>();
	public LinkedList<String> Display_And_Response_Duration=new LinkedList<String>();
	public String Display_And_Response_Duration_Randomization;
	public LinkedList<String> Dynamic_Item_Names=new LinkedList<String>();
	public LinkedList<String> Dynamic_Trigger_Names=new LinkedList<String>();
	public LinkedList<String> Feedback_Duration=new LinkedList<String>();
	public String Feedback_Duration_Randomization;
	
	/**
	 * the methods to generate items for display on each trial.

when more than one item, response correctness matching will match the first response with item-0:correct_response_to_each_visual_text, 


each hashtable is a method to generate one item for all the trials needed.

For "display_item_visual_text" TYPE.
Then followed by multiple () parentheses, each specifying the method to generate displayed items to fill the trials.
Within a pair of parentheses,
The first parameter is the type of the display item (feedback information included in it). Each template needs to have at least one display item (specify them if it is empty).
(:item_type   (string), like "display_item_visual_text"
:visual_text, (string list in parentheses), if all the texts are the same, put only 1 text in here. (if want to show texts like 1 + 2 = ?, whether to use 5 texts or 1 text with spaces in it, depends on whether the user wants the model's eye to have 5 fixations or only 1 fixation) ("") by default, means nothing to display
:correct_response_to_each_visual_text, (string list in parentheses), the number of items must match the number of text. E.g., space, 1, q, for the complete names, see DeviceModuleFun__Initialize_Device_Interface_Hashtables in ACTR-QN. ("") by default, means nothing to display
:feedback_to_each_visual_text   (string list in parentheses) ("") by default, means nothing to display
:text_randomization, string, with_replacement or without_replacement (if a list runs to the end before trials end, restart a new list) or fixed_order (by default, if a list runs to the end before trials end, restart a new list)  [:visual_text, :correct_response_to_each_visual_text, and :feedback_to_each_visual_text, are controlled by the same randomization method]
:display_item_screen_location_x, (a list of string for positive integers)    (0 at the left edge) if all the locations are the same, put only one number in here. 
:display_item_screen_location_x_randomization, string, with_replacement or without_replacement (if a list runs to the end before trials end, restart a new list) or fixed_order (by default, if a list runs to the end before trials end, restart a new list)
:display_item_screen_location_y, (a list of string for positive integers numbers)     (0 at the top edge) if all the locations are the same, put only one number in here. 
:display_item_screen_location_y_randomization, string, with_replacement or without_replacement (if a list runs to the end before trials end, restart a new list) or fixed_order (by default, if a list runs to the end before trials end, restart a new list)
:display_item_color, (string in list), for a string e.g., black ( (black) by default), if all the colors are the same, put only one color name in here. 
:display_item_color_randomization, string, with_replacement or without_replacement (if a list runs to the end before trials end, restart a new list) or fixed_order (by default, if a list runs to the end before trials end, restart a new list)
:display_item_delay, (a list of string for double numbers), (0) by default.
:display_item_delay_randomization, string, with_replacement or without_replacement (if a list runs to the end before trials end, restart a new list) or fixed_order (by default, if a list runs to the end before trials end, restart a new list)
:display_item_duration, (a list of string for double numbers), -1(or <0 in general) means infinite duration.
:display_item_duration_randomization, string, with_replacement or without_replacement (if a list runs to the end before trials end, restart a new list) or fixed_order (by default, if a list runs to the end before trials end, restart a new list)
:feedback_item_screen_location_x, (a list of string for positive integers)    (0 at the left edge) if all the locations are the same, put only one number in here. 
:feedback_item_screen_location_x_randomization, string, with_replacement or without_replacement (if a list runs to the end before trials end, restart a new list) or fixed_order (by default, if a list runs to the end before trials end, restart a new list)
:feedback_item_screen_location_y, (a list of string for positive integers numbers)     (0 at the top edge) if all the locations are the same, put only one number in here. 
:feedback_item_screen_location_y_randomization, string, with_replacement or without_replacement (if a list runs to the end before trials end, restart a new list) or fixed_order (by default, if a list runs to the end before trials end, restart a new list)
:feedback_item_color, (string in list), for a string e.g., black ( (black) by default), if all the colors are the same, put only one color name in here. 
:feedback_item_color_randomization, string, with_replacement or without_replacement (if a list runs to the end before trials end, restart a new list) or fixed_order (by default, if a list runs to the end before trials end, restart a new list)
:feedback_item_delay, (a list of string for double numbers), (0) by default
:feedback_item_delay_randomization, string, with_replacement or without_replacement (if a list runs to the end before trials end, restart a new list) or fixed_order (by default, if a list runs to the end before trials end, restart a new list)
:feedback_item_duration, (a list of string for double numbers), -1(or <0 in general) means infinite duration.
:feedback_item_duration_randomization, string, with_replacement or without_replacement (if a list runs to the end before trials end, restart a new list) or fixed_order (by default, if a list runs to the end before trials end, restart a new list)
//ToDo, user should be able to add new attributes and random method for the attributes.
)


	 */
	public LinkedList<Hashtable> Generating_Method_For_Each_Item_Over_Trials=new LinkedList<Hashtable>();
	public String How_Many_Display_Items_To_Use_In_A_Trial;
	public String Number_Of_Responses_Per_Trial;
	
	public Task_Template_Discrete_Display_Feedback_Two_Stages_Method(){
		Add_Number_Of_Blocks_Per_Day=0;
		Add_Number_Of_Days=0;
		Add_Number_Of_Trials_Per_Block="1";
		Display_And_Response_Duration_Randomization="fixed_order";
		Feedback_Duration_Randomization="fixed_order";
		How_Many_Display_Items_To_Use_In_A_Trial="all";
		Number_Of_Responses_Per_Trial="-1";
	}
}
