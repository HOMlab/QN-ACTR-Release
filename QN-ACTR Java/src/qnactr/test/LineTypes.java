package qnactr.test;

import java.applet.Applet;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GraphicsConfiguration;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.text.NumberFormat;

import javax.media.j3d.AmbientLight;
import javax.media.j3d.Appearance;
import javax.media.j3d.Background;
import javax.media.j3d.BoundingSphere;
import javax.media.j3d.BranchGroup;
import javax.media.j3d.Canvas3D;
import javax.media.j3d.ColoringAttributes;
import javax.media.j3d.DirectionalLight;
import javax.media.j3d.Group;
import javax.media.j3d.ImageComponent;
import javax.media.j3d.ImageComponent2D;
import javax.media.j3d.LineArray;
import javax.media.j3d.LineAttributes;
import javax.media.j3d.Screen3D;
import javax.media.j3d.Shape3D;
import javax.media.j3d.Transform3D;
import javax.media.j3d.TransformGroup;
import javax.media.j3d.View;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.vecmath.AxisAngle4f;
import javax.vecmath.Color3f;
import javax.vecmath.Point3d;
import javax.vecmath.Point3f;
import javax.vecmath.Vector3f;

import com.sun.image.codec.jpeg.JPEGCodec;
import com.sun.image.codec.jpeg.JPEGEncodeParam;
import com.sun.image.codec.jpeg.JPEGImageEncoder;
import com.sun.j3d.utils.applet.MainFrame;
import com.sun.j3d.utils.behaviors.mouse.MouseRotate;
import com.sun.j3d.utils.universe.SimpleUniverse;

public class LineTypes extends Applet {

  SimpleUniverse u;

  boolean isApplication;

  Canvas3D canvas;

  View view;

  /* image capture */
  OffScreenCanvas3D offScreenCanvas;

  float offScreenScale = 1.0f;

  String snapImageString = "Snap Image";

  // GUI elements
  JTabbedPane tabbedPane;

  // Temporaries that are reused
  Transform3D tmpTrans = new Transform3D();

  Vector3f tmpVector = new Vector3f();

  AxisAngle4f tmpAxisAngle = new AxisAngle4f();

  // colors for use in the cones
  Color3f red = new Color3f(1.0f, 0.0f, 0.0f);

  Color3f black = new Color3f(0.0f, 0.0f, 0.0f);

  Color3f white = new Color3f(1.0f, 1.0f, 1.0f);

  // geometric constants
  Point3f origin = new Point3f();

  Vector3f yAxis = new Vector3f(0.0f, 1.0f, 0.0f);

  // NumberFormat to print out floats with only two digits
  NumberFormat nf;

  // Returns the TransformGroup we will be editing to change the tranform
  // on the lines
  Group createLineTypes() {

    Group lineGroup = new Group();

    Appearance app = new Appearance();
    ColoringAttributes ca = new ColoringAttributes(black,
        ColoringAttributes.SHADE_FLAT);
    app.setColoringAttributes(ca);

    // Plain line
    Point3f[] plaPts = new Point3f[2];
    plaPts[0] = new Point3f(-0.9f, -0.7f, 0.0f);
    plaPts[1] = new Point3f(-0.5f, 0.7f, 0.0f);
    LineArray pla = new LineArray(2, LineArray.COORDINATES);
    pla.setCoordinates(0, plaPts);
    Shape3D plShape = new Shape3D(pla, app);
    lineGroup.addChild(plShape);

    // line pattern dot
    Point3f[] dotPts = new Point3f[2];
    dotPts[0] = new Point3f(-0.4f, -0.7f, 0.0f);
    dotPts[1] = new Point3f(-0.0f, 0.7f, 0.0f);
    LineArray dot = new LineArray(2, LineArray.COORDINATES);
    dot.setCoordinates(0, dotPts);
    LineAttributes dotLa = new LineAttributes();
    dotLa.setLineWidth(2.0f);
    dotLa.setLinePattern(LineAttributes.PATTERN_DOT);
    Appearance dotApp = new Appearance();
    dotApp.setLineAttributes(dotLa);
    dotApp.setColoringAttributes(ca);
    Shape3D dotShape = new Shape3D(dot, dotApp);
    lineGroup.addChild(dotShape);

    // line pattern dash
    Point3f[] dashPts = new Point3f[2];
    dashPts[0] = new Point3f(-0.0f, -0.7f, 0.0f);
    dashPts[1] = new Point3f(0.4f, 0.7f, 0.0f);
    LineArray dash = new LineArray(2, LineArray.COORDINATES);
    dash.setCoordinates(0, dashPts);
    LineAttributes dashLa = new LineAttributes();
    dashLa.setLineWidth(4.0f);
    dashLa.setLinePattern(LineAttributes.PATTERN_DASH);
    Appearance dashApp = new Appearance();
    dashApp.setLineAttributes(dashLa);
    dashApp.setColoringAttributes(ca);
    Shape3D dashShape = new Shape3D(dash, dashApp);
    lineGroup.addChild(dashShape);

    // line pattern dot-dash
    Point3f[] dotDashPts = new Point3f[2];
    dotDashPts[0] = new Point3f(0.5f, -0.7f, 0.0f);
    dotDashPts[1] = new Point3f(0.9f, 0.7f, 0.0f);
    LineArray dotDash = new LineArray(2, LineArray.COORDINATES);
    dotDash.setCoordinates(0, dotDashPts);
    LineAttributes dotDashLa = new LineAttributes();
    dotDashLa.setLineWidth(4.0f);
    dotDashLa.setLinePattern(LineAttributes.PATTERN_DASH_DOT);
    Appearance dotDashApp = new Appearance();
    dotDashApp.setLineAttributes(dotDashLa);
    dotDashApp.setColoringAttributes(ca);
    Shape3D dotDashShape = new Shape3D(dotDash, dotDashApp);
    lineGroup.addChild(dotDashShape);

    return lineGroup;

  }

  BranchGroup createSceneGraph() {
    // Create the root of the branch graph
    BranchGroup objRoot = new BranchGroup();

    // Create a TransformGroup to scale the scene down by 3.5x
    // TODO: move view platform instead of scene using orbit behavior
    TransformGroup objScale = new TransformGroup();
    Transform3D scaleTrans = new Transform3D();
    //scaleTrans.set(1 / 3.5f); // scale down by 3.5x
    objScale.setTransform(scaleTrans);
    objRoot.addChild(objScale);

    // Create a TransformGroup and initialize it to the
    // identity. Enable the TRANSFORM_WRITE capability so that
    // the mouse behaviors code can modify it at runtime. Add it to the
    // root of the subgraph.
    TransformGroup objTrans = new TransformGroup();
    objTrans.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
    objTrans.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
    objScale.addChild(objTrans);

    // Add the primitives to the scene
    objTrans.addChild(createLineTypes());

    BoundingSphere bounds = new BoundingSphere(new Point3d(), 100.0);    
    Background bg = new Background(new Color3f(1.0f, 1.0f, 1.0f));
    bg.setApplicationBounds(bounds);
    objTrans.addChild(bg);

    // set up the mouse rotation behavior
    MouseRotate mr = new MouseRotate();
    mr.setTransformGroup(objTrans);
    mr.setSchedulingBounds(bounds);
    mr.setFactor(0.007);
    objTrans.addChild(mr);

    // Set up the ambient light
    Color3f ambientColor = new Color3f(0.1f, 0.1f, 0.1f);
    AmbientLight ambientLightNode = new AmbientLight(ambientColor);
    ambientLightNode.setInfluencingBounds(bounds);
    objRoot.addChild(ambientLightNode);

    // Set up the directional lights
    Color3f light1Color = new Color3f(1.0f, 1.0f, 1.0f);
    Vector3f light1Direction = new Vector3f(0.0f, -0.2f, -1.0f);

    DirectionalLight light1 = new DirectionalLight(light1Color,
        light1Direction);
    light1.setInfluencingBounds(bounds);
    objRoot.addChild(light1);

    return objRoot;
  }

  public LineTypes() {
    this(false);
  }

  public LineTypes(boolean isApplication) {
    this.isApplication = isApplication;
  }

  public void init() {

    // set up a NumFormat object to print out float with only 3 fraction
    // digits
    nf = NumberFormat.getInstance();
    nf.setMaximumFractionDigits(3);

    setLayout(new BorderLayout());
    GraphicsConfiguration config = SimpleUniverse
        .getPreferredConfiguration();

    canvas = new Canvas3D(config);

    add("Center", canvas);

    // Create a simple scene and attach it to the virtual universe
    BranchGroup scene = createSceneGraph();
    u = new SimpleUniverse(canvas);

    if (isApplication) {
      offScreenCanvas = new OffScreenCanvas3D(config, true);
      // set the size of the off-screen canvas based on a scale
      // of the on-screen size
      Screen3D sOn = canvas.getScreen3D();
      Screen3D sOff = offScreenCanvas.getScreen3D();
      Dimension dim = sOn.getSize();
      dim.width *= 1.0f;
      dim.height *= 1.0f;
      sOff.setSize(dim);
      sOff.setPhysicalScreenWidth(sOn.getPhysicalScreenWidth()
          * offScreenScale);
      sOff.setPhysicalScreenHeight(sOn.getPhysicalScreenHeight()
          * offScreenScale);

      // attach the offscreen canvas to the view
      u.getViewer().getView().addCanvas3D(offScreenCanvas);
    }

    // This will move the ViewPlatform back a bit so the
    // objects in the scene can be viewed.
    u.getViewingPlatform().setNominalViewingTransform();
    u.addBranchGraph(scene);

    view = u.getViewer().getView();

    add("South", guiPanel());
  }

  // create a panel with a tabbed pane holding each of the edit panels
  JPanel guiPanel() {
    JPanel panel = new JPanel();
    panel.setLayout(new GridLayout(0, 1));

    if (isApplication) {
      JButton snapButton = new JButton(snapImageString);
      snapButton.setActionCommand(snapImageString);
      snapButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          doSnapshot();
        }
      });
      panel.add(snapButton);
    }

    return panel;
  }

  void doSnapshot() {
    Point loc = canvas.getLocationOnScreen();
    offScreenCanvas.setOffScreenLocation(loc);
    Dimension dim = canvas.getSize();
    dim.width *= offScreenScale;
    dim.height *= offScreenScale;
    nf.setMinimumIntegerDigits(3);
    offScreenCanvas.snapImageFile("lineTypes", dim.width, dim.height);
    nf.setMinimumIntegerDigits(0);
  }

  public void destroy() {
    u.removeAllLocales();
  }

  // The following allows LineTypes to be run as an application
  // as well as an applet
  //
  public static void main(String[] args) {
    new MainFrame(new LineTypes(true), 600, 600);
  }
}

class OffScreenCanvas3D extends Canvas3D {

  OffScreenCanvas3D(GraphicsConfiguration graphicsConfiguration,
      boolean offScreen) {

    super(graphicsConfiguration, offScreen);
  }

  private BufferedImage doRender(int width, int height) {

    BufferedImage bImage = new BufferedImage(width, height,
        BufferedImage.TYPE_INT_RGB);

    ImageComponent2D buffer = new ImageComponent2D(
        ImageComponent.FORMAT_RGB, bImage);
    //buffer.setYUp(true);

    setOffScreenBuffer(buffer);
    renderOffScreenBuffer();
    waitForOffScreenRendering();
    bImage = getOffScreenBuffer().getImage();
    return bImage;
  }

  void snapImageFile(String filename, int width, int height) {
    BufferedImage bImage = doRender(width, height);

    /*
     * JAI: RenderedImage fImage = JAI.create("format", bImage,
     * DataBuffer.TYPE_BYTE); JAI.create("filestore", fImage, filename +
     * ".tif", "tiff", null);
     */

    /* No JAI: */
    try {
      FileOutputStream fos = new FileOutputStream(filename + ".jpg");
      BufferedOutputStream bos = new BufferedOutputStream(fos);

      JPEGImageEncoder jie = JPEGCodec.createJPEGEncoder(bos);
      JPEGEncodeParam param = jie.getDefaultJPEGEncodeParam(bImage);
      param.setQuality(1.0f, true);
      jie.setJPEGEncodeParam(param);
      jie.encode(bImage);

      bos.flush();
      fos.close();
    } catch (Exception e) {
      System.out.println(e);
    }
  }
}
