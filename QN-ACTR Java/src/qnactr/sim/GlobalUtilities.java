/**
 * 2013 QN-Java project file
 * 
 */

package qnactr.sim;

import java.awt.Color;
import java.io.File;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Hashtable;
import java.util.Random;

import javax.swing.JOptionPane;

import qnactr.objectDesigner.Enums;

import jmt.engine.QueueNet.NetNode;
import jmt.gui.jmodel.mainGui.MainWindow;

/**
 *  
 * here are the methods at the global level of QN-Java project, e.g., across all HMIs  
 * @author shicao
 *
 */
public class GlobalUtilities
{
  
  public static MainWindow mainWin; //assigned value in MainWindow object instantiation
  static Random randGenerator = new Random();
  
  

  
  public static String formatDoubleToString(double val, int precision) {
    
      if ( precision < 0 || precision > 9){
        System.out.println("Error! GlobalUtilities.formatDoubleToString has precision out of range. precision: " + precision);
        return null;
      }
      int POW10[] = {1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000 };
    
      StringBuilder sb = new StringBuilder();
      if (val < 0) {
          sb.append('-');
          val = -val;
      }
      int exp = POW10[precision];
      long lval = (long)(val * exp + 0.5);
      sb.append(lval / exp).append('.');
      long fval = lval % exp;
      for (int p = precision - 1; p > 0 && fval < POW10[p]; p--) {
          sb.append('0');
      }
      sb.append(fval);
      return sb.toString();
  }
  
  public static Double round(double val, int precision) {
    return Double.valueOf(formatDoubleToString(val, precision)) ;
  }
  
  
  public static Enums.ServerName fromGlobalNodeNameToEnumServerName (String globalNodeName){
    String serverName = getServerOperatorNamesFromRawName(globalNodeName)[0];
    
    //System.out.println("fromGlobalNodeNameToEnumServerName has serverName: " + globalNodeName);
    //System.out.println("fromGlobalNodeNameToEnumServerName has serverName: " + serverName);
    
    return Enums.ServerName.valueOf(  stringLowNoSpace(serverName)  ) ;
  }
  
  
  /**
   * split rawName by "_" [2013-06-24]
   * @param rawName, name of station name in JMT, e.g., "Visual Module_1".
   * @return String[], [0] = Server Name, e.g., Visual Module; [1] = Operator ID, e.g., 1.
   */
  public static String[] getServerOperatorNamesFromRawName (String rawName){
    
    if(rawName == null || rawName.length() < 3 || !rawName.contains("_")){
      JOptionPane.showMessageDialog(null, "getServerOperatorNamesFromRawName has illegal rawName: " +  rawName, "GlobalUtilities.java", JOptionPane.ERROR_MESSAGE);
      return null;
    }
    
    String[] returnStringArray = rawName.split("_");
    
    //JOptionPane.showMessageDialog(null, "getServerOperatorNamesFromRawName name1: " +  returnStringArray[0] + "; name2: " +  returnStringArray[1] , "GlobalUtilities.java", JOptionPane.INFORMATION_MESSAGE);
    
    return returnStringArray;
  }
  
  public static int getHMIIndex(String rawNodeName){
    if(rawNodeName == null){
      JOptionPane.showMessageDialog(null, "getHMIIndex has illegal rawName: " +  rawNodeName, "GlobalUtilities.java", JOptionPane.ERROR_MESSAGE);
      return -1;
    }
    else if(rawNodeName.length() < 3 || !rawNodeName.contains("_")){
      // not belong to any HMI
      return 0;
    }
    else{
      String[] stringArray = rawNodeName.split("_");
      int index = Integer.valueOf(stringArray[1]);
      return index;
    }
    
  }
  
  public static URI getQNJProjectURI(){
    
    ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
    URL url = classLoader.getResource("");
    File file;
    try
    {
      file = new File(url.toURI());
      return file.getParentFile().toURI();
      
    } catch (URISyntaxException e)
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
      return null;
    }
    
  }
  
  
  public static URI getQNFrameworksFolderURI(){
    URI qnjProjectURI = getQNJProjectURI();
    String stringURI = qnjProjectURI.toString();
    stringURI += "QN%20workspace/QN%20frameworks/";
    return URI.create(stringURI);
 }
 
  public static void popUpMessage(String text){
    JOptionPane.showMessageDialog(null, text, "QN-Java message", JOptionPane.INFORMATION_MESSAGE);
  }
  
  /**
   * return an integer within [minInclusive, maxInclusive]
   * @param minInclusive
   * @param maxInclusive
   * @return
   */
 public static int randomInteger(int minInclusive, int maxInclusive){
   int returnInt = minInclusive + randGenerator.nextInt(maxInclusive + 1);
   //  System.out.println( "randomInteger: " + returnInt );
   return returnInt;
 }
  
 /**
  * return a double within [minInclusive, maxExclusive)
  * @param minInclusive
  * @param maxExclusive
  * @return
  */
 public static double randomDouble(double minInclusive, double maxExclusive){
   double returnDouble = minInclusive + (maxExclusive - minInclusive) * randGenerator.nextDouble();
   
   //System.out.println("randomDouble: " +  returnDouble );
   
   return returnDouble;   
 }
 
  public static void setMainWindowStatusLabelText(String newText){
    mainWin.statusLabel.setText(newText);
  }
  
  public static String stringLowNoSpace(String input){
    return input.toLowerCase().replaceAll("\\s","");
  }
  
  public static Color colorStringToColorType(String colorString){
    try
    {
      Field field = Class.forName("java.awt.Color").getField(colorString.toLowerCase());
      return (Color)field.get(null);
    } catch (Exception e) {
      System.out.println("ERROR! GlobalUtilities.colorStringToColorType has invalid colorString: " + colorString);
      return null;
    }
  }
  

  

}
