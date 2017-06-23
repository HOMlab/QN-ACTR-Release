package qnactr.test;

//First we import packages that I use for Java3D
import java.awt.Frame;
import java.applet.Applet;
import java.awt.*;
import java.awt.event.*;
import com.sun.j3d.utils.applet.MainFrame;
import com.sun.j3d.utils.geometry.*;
import com.sun.j3d.utils.universe.*;
import javax.media.j3d.*;
import javax.vecmath.*;

public class lesson01 extends Applet { // notice'lesson01', which is also the name of the file : lesson01.java
  SimpleUniverse simpleU; // this is the SimpleUniverse Class that is used for Java3D
  
  public lesson01 (){  // this constructor is sometimes needed, even when empty as in here    
  }    
  
  public void init() { 
    // this function will be called by both applications and applets
    //this is usually the first function to write        
    setLayout(new BorderLayout()); // standard Java code for BorderLayout
    
    // Canvas3D is where all the action will be taking place, don't worry, after adding it
    // to your layout, you don't have to touch it.      
    Canvas3D c = new Canvas3D(SimpleUniverse.getPreferredConfiguration()); 
    
    
    // add Canvas3D to center of BorderLayout
    add("Center", c);    
    
    simpleU= new SimpleUniverse(c); // setup the SimpleUniverse, attach the Canvas3D
    
    
    //This is very important, the SceneGraph (where all the action takes place) is created
    //by calling a function which here is called 'createSceneGraph'.
    //The function is not necessary, you can put all your code here, but it is a 
    //standard in Java3D to create your SceneGraph contents in the function 'createSceneGraph'
    
    BranchGroup scene = createSceneGraph(); 
    
    //set the ViewingPlatform (where the User is) to nominal, more on this in the next lesson
    //simpleU.getViewingPlatform().setNominalViewingTransform();
    
    
    // ViewingPlatform
    
    
    ViewingPlatform vp = simpleU.getViewingPlatform();
    
    //vp.getViewers()[0].getView().setFieldOfView(Math.PI / 180 * 45);
    vp.getViewers()[0].getView().setFrontClipDistance(0.1); // z positive
    vp.getViewers()[0].getView().setBackClipDistance(10000); // z negative
    
    TransformGroup View_TransformGroup = vp.getMultiTransformGroup().getTransformGroup(0);
    Transform3D View_Transform3D = new Transform3D();
    View_TransformGroup.getTransform(View_Transform3D);

    View_Transform3D.rotX( Math.PI / 180 * 180);
    
    //View_Transform3D.rotY( Math.PI / 180 * 0);
        
    View_Transform3D.setTranslation(new Vector3f(0.0f, -1.0f, 0.0f));
    
    
    Transform3D step = new Transform3D();
    step.rotY(Math.PI / 180 * 0);
    step.setTranslation (new Vector3f(0.0f, 0.0f, 0.0f));
    View_Transform3D.mul(step);
    View_TransformGroup.setTransform(View_Transform3D);
    
    step = new Transform3D();
    step.rotY(Math.PI / 180 * 0);
    step.setTranslation (new Vector3f(0.0f, 0.0f, 0.0f));
    View_Transform3D.mul(step);
    View_TransformGroup.setTransform(View_Transform3D);
    
    step = new Transform3D();
    step.rotY(Math.PI / 180 * 0);
    step.setTranslation (new Vector3f(0.0f, 0.0f, 0.0f));
    View_Transform3D.mul(step);
    View_TransformGroup.setTransform(View_Transform3D);
    
    
    // this will optimize your SceneGraph, not necessary, but it will allow your program to run faster.
    scene.compile(); 
    simpleU.addBranchGraph(scene); //add your SceneGraph to the SimpleUniverse   
  }
  
  public BranchGroup createSceneGraph() {      
    //
//    Note: 
//      The coordinate system is Right-Hand Rule. 
//      X coordinate is positive to the right of the origin, negative to the left. 
//      Y coordinate is positive above the origin, negative below. 
//      Z coordinate is negative INTO the screen, and positive OUT to the user. 
//      
      
    // in comparison, MicroSaintSharp used left hand system.
    
    
//    //Here we will create a basic SceneGraph with a ColorCube object
//    
//    // This BranchGroup is the root of the SceneGraph, 'objRoot' is the name I use,
//    // and it is typically the standard name for it, but can be named anything.

//    BranchGroup objRoot = new BranchGroup();
    
//    
//    TransformGroup cctg = new TransformGroup();
//    
//    // create a ColorCube object of size 0.5
//    ColorCube c = new ColorCube(0.5f);
//    
//    // add ColorCube to SceneGraph
//    cctg.addChild(c);
//    
//    objRoot.addChild(cctg);
//    
//    
    
    // objRoot -> 3 normal vector X Y Z. 
    Group lineGroupXYZ = new Group();
    
    Color3f red = new Color3f(1.0f, 0.0f, 0.0f);
    
    Color3f green = new Color3f(0.0f, 1.0f, 0.0f);
    
    Color3f blue = new Color3f(0.0f, 0.0f, 1.0f);
    
    Color3f black = new Color3f(0.0f, 0.0f, 0.0f);
    
    Color3f white = new Color3f(1.0f, 1.0f, 1.0f);
    
    Appearance appR = new Appearance();
    appR.setColoringAttributes(new ColoringAttributes(red, ColoringAttributes.SHADE_FLAT));
    
    Appearance appG = new Appearance();
    appG.setColoringAttributes(new ColoringAttributes(green, ColoringAttributes.SHADE_FLAT));
    
    Appearance appB = new Appearance();
    appB.setColoringAttributes(new ColoringAttributes(blue, ColoringAttributes.SHADE_FLAT));
    
    Appearance appBk = new Appearance();
    appBk.setColoringAttributes(new ColoringAttributes(black, ColoringAttributes.SHADE_FLAT));
    
    Appearance appW = new Appearance();
    appW.setColoringAttributes(new ColoringAttributes(white, ColoringAttributes.SHADE_FLAT));
    
    //Plain line x1
    Point3f[] plaPtsX = new Point3f[2];
    plaPtsX[0] = new Point3f(0.0f, 0.0f, 0.0f);
    plaPtsX[1] = new Point3f(1.0f, 0.0f, 0.0f);
    LineArray plaX = new LineArray(2, LineArray.COORDINATES);
    plaX.setCoordinates(0, plaPtsX);
    Shape3D plShapeX = new Shape3D(plaX, appR);
    lineGroupXYZ.addChild(plShapeX);
    
    
    //Plain line y1
    Point3f[] plaPtsY = new Point3f[2];
    plaPtsY[0] = new Point3f(0.0f, 0.0f, 0.0f);
    plaPtsY[1] = new Point3f(0.0f, 1.0f, 0.0f);
    LineArray plaY = new LineArray(2, LineArray.COORDINATES);
    plaY.setCoordinates(0, plaPtsY);
    Shape3D plShapeY = new Shape3D(plaY, appG);
    lineGroupXYZ.addChild(plShapeY);
    
    //Plain line z1
    Point3f[] plaPtsZ = new Point3f[2];
    plaPtsZ[0] = new Point3f(0.0f, 0.0f, 0.0f);
    plaPtsZ[1] = new Point3f(0.0f, 0.0f, 1.0f);
    LineArray plaZ = new LineArray(2, LineArray.COORDINATES);
    plaZ.setCoordinates(0, plaPtsZ);
    Shape3D plShapeZ = new Shape3D(plaZ, appB);
    lineGroupXYZ.addChild(plShapeZ);
    

    
    
//    objRoot.addChild(lineGroupXYZ);
    
    //end of objRoot -> 3 normal vector X Y Z. 
//    
//    
    //objRoot -> white background, added as a white sphere
    Background background = new Background(new Color3f(1f,1f,1f));
    BoundingSphere sphere = new BoundingSphere(new Point3d(0,0,0), 10000);
    background.setApplicationBounds(sphere);
//    objRoot.addChild(background);
    //end of white background
//    
//    
//    
//    
//    // transform
////    Transform3D cc3d = new Transform3D();
////    cc3d.setTranslation(new Vector3f (0.8f , 1.0f , -2.0f ));
////    cctg.setTransform(cc3d); 
//    
//    

//    
//    
    
    
    
    // return Scene Graph
//    return objRoot;
    
    BranchGroup objRoot = new BranchGroup();
    TransformGroup worldObjTrans = new TransformGroup();    
    
    
  //Plain line left
    Point3f[] plaPtsl = new Point3f[2];
    plaPtsl[0] = new Point3f(-2.0f, 0f, 1000.0f);
    plaPtsl[1] = new Point3f(-2.0f, 0f, -1000.0f);
    LineArray plal = new LineArray(2, LineArray.COORDINATES);
    plal.setCoordinates(0, plaPtsl);
    Shape3D plShapel = new Shape3D(plal, appBk);
    
    
    
    //Plain line right
    Point3f[] plaPtsr = new Point3f[2];
    plaPtsr[0] = new Point3f(2.0f, 0f, 1000.0f);
    plaPtsr[1] = new Point3f(2.0f, 0f, -1000.0f);
    LineArray plar = new LineArray(2, LineArray.COORDINATES);
    plar.setCoordinates(0, plaPtsr);
    Shape3D plShaper = new Shape3D(plar, appBk);
    
    
    // mark lines each 10 m
    Group markLines = new Group();
    float i;
    for (i = 10.0f; i <= 40.0 ; i += 10.0f){
      
      Point3f[] plaPtsTemp = new Point3f[2];
      plaPtsTemp[0] = new Point3f(-2.0f, 0f, i);
      plaPtsTemp[1] = new Point3f(2.0f, 0f, i);
      LineArray plaTemp = new LineArray(2, LineArray.COORDINATES);
      plaTemp.setCoordinates(0, plaPtsTemp);
      Shape3D plShapeTemp = new Shape3D(plaTemp, appBk);
      markLines.addChild(plShapeTemp);
      
    }
    
    
    worldObjTrans.addChild(lineGroupXYZ);
    worldObjTrans.addChild(plShapel);
    worldObjTrans.addChild(plShaper);  
    worldObjTrans.addChild(markLines);
    
//    Transform3D tran1 = new Transform3D();
//    tran1.rotY( Math.PI / 180 * 20);
//    worldObjTrans.setTransform(tran1);
    
    
    objRoot.addChild(background);
    objRoot.addChild(worldObjTrans);
    
    
    return objRoot;
  }
  
  public void destroy() { // this function will allow Java3D to clean up upon quiting
    simpleU.removeAllLocales();    
  }  
  
  public static void main(String[] args) {
       
    Frame frame = new MainFrame(new lesson01(), 800, 600);    
  }
}
