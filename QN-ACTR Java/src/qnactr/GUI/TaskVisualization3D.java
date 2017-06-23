package qnactr.GUI;

import javax.swing.JPanel;

import java.awt.BorderLayout;
import java.awt.Frame;
import java.applet.Applet;
import java.awt.*;
import java.awt.event.*;
import com.sun.j3d.utils.applet.MainFrame;
import com.sun.j3d.utils.geometry.*;
import com.sun.j3d.utils.universe.*;

import javax.media.j3d.*;
import javax.vecmath.*;


public class TaskVisualization3D extends JPanel 
{
  public SimpleUniverse simpleU;
  public ViewingPlatform vp;  // this represents driver's eyes
  public TransformGroup viewTransformGroup;
  public Transform3D vpViewTransform = new Transform3D(); 

  BranchGroup objRoot;
  
  Color3f red = new Color3f(1.0f, 0.0f, 0.0f);  
  Color3f green = new Color3f(0.0f, 1.0f, 0.0f);  
  Color3f blue = new Color3f(0.0f, 0.0f, 1.0f);  
  Color3f black = new Color3f(0.0f, 0.0f, 0.0f);  
  Color3f white = new Color3f(1.0f, 1.0f, 1.0f);  
  Appearance appR = new Appearance();  
  Appearance appG = new Appearance();  
  Appearance appB = new Appearance();  
  Appearance appBk = new Appearance();  
  Appearance appW = new Appearance();
  
  public TaskVisualization3D () {

    appR.setColoringAttributes(new ColoringAttributes(red, ColoringAttributes.SHADE_FLAT));
    appG.setColoringAttributes(new ColoringAttributes(green, ColoringAttributes.SHADE_FLAT));
    appB.setColoringAttributes(new ColoringAttributes(blue, ColoringAttributes.SHADE_FLAT));
    appBk.setColoringAttributes(new ColoringAttributes(black, ColoringAttributes.SHADE_FLAT));
    appW.setColoringAttributes(new ColoringAttributes(white, ColoringAttributes.SHADE_FLAT));
        
    initiateScene();
    
  }




  private void initiateScene()
  {
    //01
    // initiate universe, canvas, and objRoot
    setLayout(new BorderLayout());     
    Canvas3D canvas = new Canvas3D(SimpleUniverse.getPreferredConfiguration());
    add( canvas, BorderLayout.CENTER);
    simpleU = new SimpleUniverse(canvas);
    objRoot = new BranchGroup();
    
    //02
    //objRoot -> white background, added as a white sphere
    Background background = new Background(new Color3f(1f,1f,1f));
    BoundingSphere sphere = new BoundingSphere(new Point3d(0,0,0), 10000);
    background.setApplicationBounds(sphere);
    objRoot.addChild(background);
    
    
    //03
    // add some temp markers for orientation
    boolean useTempMarkers = false;
    if(useTempMarkers){
      
      TransformGroup tempMarkers = new TransformGroup();    
      
      //Plain line left
      Point3f[] plaPtsl = new Point3f[2];
      plaPtsl[0] = new Point3f(-2.0f, 0f, 1000.0f);
      plaPtsl[1] = new Point3f(-2.0f, 0f, -1000.0f);
      LineArray plal = new LineArray(2, LineArray.COORDINATES);
      plal.setCoordinates(0, plaPtsl);
      Shape3D plShapel = new Shape3D(plal, appBk);
      
      //Plain line left2
      Point3f[] plaPtsl2 = new Point3f[2];
      plaPtsl2[0] = new Point3f(-4.0f, 0f, 1000.0f);
      plaPtsl2[1] = new Point3f(-4.0f, 0f, -1000.0f);
      LineArray plal2 = new LineArray(2, LineArray.COORDINATES);
      plal2.setCoordinates(0, plaPtsl2);
      Shape3D plShapel2 = new Shape3D(plal2, appB);
      
      //Plain line right
      Point3f[] plaPtsr = new Point3f[2];
      plaPtsr[0] = new Point3f(2.0f, 0f, 1000.0f);
      plaPtsr[1] = new Point3f(2.0f, 0f, -1000.0f);
      LineArray plar = new LineArray(2, LineArray.COORDINATES);
      plar.setCoordinates(0, plaPtsr);
      Shape3D plShaper = new Shape3D(plar, appBk);
      
    //Plain line right2
      Point3f[] plaPtsr2 = new Point3f[2];
      plaPtsr2[0] = new Point3f(4.0f, 0f, 1000.0f);
      plaPtsr2[1] = new Point3f(4.0f, 0f, -1000.0f);
      LineArray plar2 = new LineArray(2, LineArray.COORDINATES);
      plar2.setCoordinates(0, plaPtsr2);
      Shape3D plShaper2 = new Shape3D(plar2, appR);
      
      // mark lines each 10 m, Z+ red
      Group markLines = new Group();
      float i;
      for (i = 10.0f; i <= 40.0 ; i += 10.0f){
        
        Point3f[] plaPtsTemp = new Point3f[2];
        plaPtsTemp[0] = new Point3f(-2.0f, 0f, i);
        plaPtsTemp[1] = new Point3f(2.0f, 0f, i);
        LineArray plaTemp = new LineArray(2, LineArray.COORDINATES);
        plaTemp.setCoordinates(0, plaPtsTemp);
        Shape3D plShapeTemp = new Shape3D(plaTemp, appR);
        markLines.addChild(plShapeTemp);        
      }   
      
   // mark lines each 10 m, Z- blue
      for (i = 10.0f; i <= 40.0 ; i += 10.0f){
        
        Point3f[] plaPtsTemp = new Point3f[2];
        plaPtsTemp[0] = new Point3f(-2.0f, 0f, -i);
        plaPtsTemp[1] = new Point3f(2.0f, 0f, -i);
        LineArray plaTemp = new LineArray(2, LineArray.COORDINATES);
        plaTemp.setCoordinates(0, plaPtsTemp);
        Shape3D plShapeTemp = new Shape3D(plaTemp, appB);
        markLines.addChild(plShapeTemp);        
      }   

      tempMarkers.addChild(plShapel);
      tempMarkers.addChild(plShapel2);
      tempMarkers.addChild(plShaper);      
      tempMarkers.addChild(plShaper2);  
      tempMarkers.addChild(markLines);
      
      objRoot.addChild(tempMarkers);
    }
    
    //04
    //whether to rotation. 
    //rotate = true : look at Z+;  rotate = false : look at Z- direction
    
//    When starting with looking at Z+ (North +)
//    X: East +
//    Y: down+, 0 is ground level
//    Z: North +
//    right hand system
//    Angle, rotation, follows right-hand rule,     
    
    boolean rotate = true;
    
    vp = simpleU.getViewingPlatform();
    viewTransformGroup = vp.getMultiTransformGroup().getTransformGroup(0);
    viewTransformGroup.getTransform(vpViewTransform);
    if(rotate){            
      vpViewTransform.rotX( Math.PI / 180 * 180);
      vpViewTransform.setTranslation(new Vector3f(0.0f, -1.0f, 0.0f));      // this is only for driving, moved to server logics //Animator3D initialization
    }
    else {
      vpViewTransform.rotX( Math.PI / 180 * 0);
      vpViewTransform.setTranslation(new Vector3f(0.0f, 1.0f, 0.0f));       // this is only for driving, moved to server logics //Animator3D initialization
    }
    viewTransformGroup.setTransform(vpViewTransform);
    
    
    //05 increase view depth    
    vp.getViewers()[0].getView().setFrontClipDistance(0);    //if larger, some near field of view will disappear 
    vp.getViewers()[0].getView().setBackClipDistance(10000); //if smaller, some far field of view will disappear
    
    //vp.getViewers()[0].getView().setFieldOfView(Math.PI / 180 * 45); //may use this to change field of view if needed
    
    
    
    objRoot.compile();
    simpleU.addBranchGraph(objRoot); // this must be the last step after adding all things in the branch group
    
    // if more things need to be added, must add them as new branchgroup to the universe
//    BranchGroup objRoot2 = new BranchGroup();
//    objRoot2.addChild(new ColorCube(0.5f));
//    simpleU.addBranchGraph(objRoot2);
    
    
//    //05 test move the view
//    //step 1
//    Transform3D viewTransform3D_Step = new Transform3D();
//    viewTransform3D_Step.rotY( Math.PI / 180 * 90);
//    viewTransform3D_Step.setTranslation(new Vector3f(0.0f, 0.0f, 0.0f));  
//    vpViewTransform.mul(viewTransform3D_Step);
//    viewTransformGroup.setTransform(vpViewTransform);
//    
//    //step 2
//    viewTransform3D_Step = new Transform3D();
//    viewTransform3D_Step.rotY( Math.PI / 180 * 0);
//    viewTransform3D_Step.setTranslation(new Vector3f(10.0f, 0.0f, 0.0f));  
//    vpViewTransform.mul(viewTransform3D_Step);
//    viewTransformGroup.setTransform(vpViewTransform);
//    
//    
//    //step 3
//    viewTransform3D_Step = new Transform3D();
//    viewTransform3D_Step.rotY( Math.PI / 180 * -90);
//    viewTransform3D_Step.setTranslation(new Vector3f(0.0f, 0.0f, 0.0f));  
//    vpViewTransform.mul(viewTransform3D_Step);
//    viewTransformGroup.setTransform(vpViewTransform);
//    
    
  }
  
  public void draw3DLine(double x1, double y1, double z1, double x2, double y2, double z2){
    BranchGroup tempRoot = new BranchGroup();
    Point3f[] plaPtsl = new Point3f[2];
    plaPtsl[0] = new Point3f((float)x1, (float)y1, (float)z1);
    plaPtsl[1] = new Point3f((float)x2, (float)y2, (float)z2);
    LineArray plal = new LineArray(2, LineArray.COORDINATES);
    plal.setCoordinates(0, plaPtsl);
    Shape3D plShapel = new Shape3D(plal, appBk);
    tempRoot.addChild(plShapel);
    simpleU.addBranchGraph(tempRoot);    
  }
  
  public void viewRotY(double rotDegree){
    Transform3D viewTransform3D_Step = new Transform3D();
    viewTransform3D_Step.rotY( Math.PI / 180 * rotDegree);
    vpViewTransform.mul(viewTransform3D_Step);
    viewTransformGroup.setTransform(vpViewTransform);
  }
  
  public void viewTranslationX(double distance){
    Transform3D viewTransform3D_Step = new Transform3D();
    viewTransform3D_Step.setTranslation(new Vector3f((float) distance , 0.0f,  0.0f) );  
    vpViewTransform.mul(viewTransform3D_Step);
    viewTransformGroup.setTransform(vpViewTransform);
  }  
  
  public void viewTranslationY(double distance){
    Transform3D viewTransform3D_Step = new Transform3D();
    viewTransform3D_Step.setTranslation(new Vector3f(0.0f, (float) distance , 0.0f) );  
    vpViewTransform.mul(viewTransform3D_Step);
    viewTransformGroup.setTransform(vpViewTransform);
  }
  
  public void viewTranslationZ(double distance){
    Transform3D viewTransform3D_Step = new Transform3D();
    viewTransform3D_Step.setTranslation(new Vector3f(0.0f, 0.0f, (float) distance));  
    vpViewTransform.mul(viewTransform3D_Step);
    viewTransformGroup.setTransform(vpViewTransform);
  }
  
}
