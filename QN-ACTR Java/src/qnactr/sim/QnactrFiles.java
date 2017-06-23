/**
 * 2013 QN-Java project file
 * output files such as .txt .xls
 * 
 */


package qnactr.sim;


import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.net.URLDecoder;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;

import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.apache.poi.ss.util.CellReference;





public class QnactrFiles
{
  QnactrSimulation sim;  
  public String rootPath;
  String hmiFolder;
  
  //String FILE_NAME_Defaults_ACTR_Default;
  //String FILE_NAME_Defaults_compilation_check_criteria;
  //String FILE_NAME_Defaults_ExperimentSpecifics_DrivingComprehension;
  //String FILE_NAME_Defaults_ModelParameters;
  
//  String FILE_NAME_Results_Experiment_Trial_Parameter_List ;
//  String FILE_NAME_Results_results_eye_movement ;
//  String FILE_NAME_Results_results_foot;
//  String FILE_NAME_Results_results_human_drive ;
//  String FILE_NAME_Results_results_response ;
//  String FILE_NAME_Results_trace ;
  
  
  
  final Charset ENCODING = StandardCharsets.UTF_8;
  
  QnactrTxtFile Defaults_ACTR_Default;
  QnactrTxtFile Results_Experiment_Trial_Parameter_List;
  QnactrTxtFile Results_results_eye_movement;
  QnactrTxtFile Results_results_foot;
  QnactrTxtFile Results_results_human_drive;
  QnactrTxtFile Results_results_response;
  QnactrTxtFile Results_trace;
  QnactrTxtFile Results_mental_workload;
  QnactrTxtFile QN_ACTR_Model_Initialization;
  
  QnactrExcelFile excelTest;
  
  QnactrExcelFile Defaults_compilation_check_criteria;
  QnactrExcelFile Defaults_ExperimentSpecifics_DrivingComprehension;
  QnactrExcelFile Defaults_ModelParameters;
  
  public QnactrFiles(QnactrSimulation Sim) {
    sim = Sim;
    
    //update FILE_NAMEs based on sim ID
    hmiFolder = GlobalUtilities.getQNJProjectURI().toString() + "QN%20workspace/HMI_" + (sim.ID + 1) + "/";
    
    Defaults_ACTR_Default = new QnactrTxtFile(hmiFolder + "Defaults/" + "ACTR_Default.txt");
    
    //test
    //excelTest = new QnactrExcelFile(hmiFolder + "Defaults/" + "test.xls");
    
    Defaults_compilation_check_criteria = new QnactrExcelFile(hmiFolder + "Defaults/" + "compilation_check_criteria.xls");
    Defaults_ExperimentSpecifics_DrivingComprehension = new QnactrExcelFile(hmiFolder + "Defaults/" + "ExperimentSpecifics_DrivingComprehension.xls");
    Defaults_ModelParameters = new QnactrExcelFile(hmiFolder + "Defaults/" + "ModelParameters.xls");
    
    Results_Experiment_Trial_Parameter_List = new QnactrTxtFile(hmiFolder + "Results/" + "Experiment_Trial_Parameter_List.txt");
    Results_results_eye_movement = new QnactrTxtFile(hmiFolder + "Results/" + "results_eye_movement.txt");
    Results_results_foot = new QnactrTxtFile(hmiFolder + "Results/" + "results_foot.txt");
    Results_results_human_drive = new QnactrTxtFile(hmiFolder + "Results/" + "results_human_drive.txt");
    Results_results_response = new QnactrTxtFile(hmiFolder + "Results/" + "results_response.txt");
    Results_trace = new QnactrTxtFile(hmiFolder + "Results/" + "trace.txt");
    Results_mental_workload = new QnactrTxtFile(hmiFolder + "Results/" + "results_mental_workload.txt");
    QN_ACTR_Model_Initialization = new QnactrTxtFile(hmiFolder + "QN_ACTR_Model_Initialization.txt");
        
    
    
    //System.out.println(FILE_NAME_Defaults_ModelParameters + " " + FILE_NAME_Results_trace);
    
  }
  
  
  
  /**
   * Text file
   * @author shicao
   *
   */
  public class QnactrTxtFile {
    public String fileName;
    public BufferedReader reader;
    public BufferedWriter writer;
    
    public QnactrTxtFile(String aFileName) {
      fileName = aFileName;
      Path path = Paths.get(URI.create(aFileName));
      try
      {
        reader = Files.newBufferedReader(path, ENCODING);
      } catch (IOException e)
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
      
      
      try
      {
        writer = Files.newBufferedWriter(path, ENCODING, StandardOpenOption.WRITE, StandardOpenOption.APPEND);
      } catch (IOException e)
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }
    
    public void clear() {
      
      try
      {
        Path path = Paths.get(URI.create(fileName));
        writer = Files.newBufferedWriter(path, ENCODING); 
        writer = Files.newBufferedWriter(path, ENCODING, StandardOpenOption.WRITE, StandardOpenOption.APPEND); // clear and reset the writer to append mode
      } catch (IOException e)
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
      
    }
    
    

    public void writeLines(List<String> aLines) {
      
      for(String line : aLines){
        try
        {
          writer.write(line);
          writer.newLine();
        } catch (IOException e)
        {
          // TODO Auto-generated catch block
          e.printStackTrace();
        }
        
      }
      
      try
      {
        writer.flush();
      } catch (IOException e)
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }
    
    public void writeLine(String line)
    {
      try
      {
        writer.write(line);
        writer.newLine();
        writer.flush();
      } catch (IOException e)
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
      
    }
    
  }


  public void closeAllTxtFileWriters()
  {
    try
    {
      Defaults_ACTR_Default.writer.close();
      Results_Experiment_Trial_Parameter_List.writer.close();
      Results_results_eye_movement.writer.close();
      Results_results_foot.writer.close();
      Results_results_human_drive.writer.close();
      Results_results_response.writer.close();
      Results_trace.writer.close();
      Results_mental_workload.writer.close();
    } catch (IOException e)
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    
  }
  
  
  
//  void readTextFile(String aFileName) throws IOException {
//    Path path = Paths.get(aFileName);
//    try (BufferedReader reader = Files.newBufferedReader(path, ENCODING)){
//      String line = null;
//      while ((line = reader.readLine()) != null) {
//        //process each line in some way
//        //log(line);
//      }      
//    }
//  }
  
  
  
  
  /**
   * Excel .xls file
   * @author shicao
   *
   */
  public class QnactrExcelFile {
    public String fileName;
    public Workbook wb;
    
    public QnactrExcelFile(String aFileName) {
      fileName = aFileName;
      try
      {
        wb = WorkbookFactory.create(new File (URI.create(aFileName)) );
      } catch (InvalidFormatException e)
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      } catch (IOException e)
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
      
      
    }
    
    

//  try
//  {
//    Workbook wb = WorkbookFactory.create(new File (URI.create(aFileName)) );
//    
//    Sheet sheet = wb.getSheet("s1");
//    
//    CellReference cellRef = new CellReference(0,0);
//    System.out.print(cellRef.formatAsString());
//    
//    
//    Row row = sheet.getRow(cellRef.getRow());
//    Cell cell = row.getCell(cellRef.getCol());
//    
//    System.out.println(cell.getStringCellValue());
//    
//    //sheet.
//    
//    
//  } catch (InvalidFormatException | IOException e)
//  {
//    // TODO Auto-generated catch block
//    e.printStackTrace();
//  }
    
    
    
  }
  
  

  
  
}
