See \QN docs folder for QN-ACTR manual.


Quick start:

1. Download and install JDK: (Java Development Kit). http://www.oracle.com/technetwork/java/javase/downloads/index.html

2. Download an Integrated Development Environments (IDE) for Java, e.g., Eclipse (Eclipse IDE for Java Developers)

3. Unzip QN-ACTR Java [version number].zip.

4. Open Eclispe. From File -> New -> Project ... -> Java -> Java Project From Existing Ant Buildfile
   Browse and select the build.xml from the QN-ACTR Java folder. Finish.
   You should see the project in your project explorer or navigator window.

5. Specify a model in QN_ACTR_Model_Initialization.txt under \QN workspace\HMI_1. A model setup assistant is provided (\QN workspace\QN-ACTR Model Setup Assistant\MSA_QN-ACTR.exe). If creating a model using this assistant, copy created QN_ACTR_Model_Initialization.txt to \QN workspace\HMI_1.

6. To start simulation, run jmt.gui.jmodel.mainGui.MainWindow.java under src from the project explorer or navigator.
   
7. If need to repeat the simulation immediately, click green play button on the panel to start simulation (or Solve -> Simulate).

8. Close or stop the java application.

FAQ:

If Eclipse shows "syntax error", need "source level is 1.5 or greater," follow these steps:
1. Go to your project's Properties
2. On the Properties dialog choose the Java Compiler node in the left tree.
3. Finally set the Compiler compliance level to 1.5 or more. (e.g., 1.7)
4. Rebuild the project.





