/**
  * Copyright (C) 2007, Laboratorio di Valutazione delle Prestazioni - Politecnico di Milano

  * This program is free software; you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation; either version 2 of the License, or
  * (at your option) any later version.

  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.

  * You should have received a copy of the GNU General Public License
  * along with this program; if not, write to the Free Software
  * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
  */


/**
 * 2013. Modified for QN-Java project
 * 
 * [2013-06-25]  add zoomIn and zoomOut
 * 
 */

package jmt.gui.jmodel.controller;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import javax.help.HelpSet;
import javax.help.JHelp;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JOptionPane;

import jmt.framework.gui.components.JMTFrame;
import jmt.framework.gui.components.JMTMenuBar;
import jmt.framework.gui.components.JMTToolBar;
import jmt.framework.gui.image.ImageLoader;
import jmt.framework.gui.listeners.MenuAction;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.controller.DispatcherThread;
import jmt.gui.common.controller.ModelChecker;
import jmt.gui.common.controller.PADispatcherThread;
import jmt.gui.common.definitions.BlockingRegionDefinition;
import jmt.gui.common.definitions.GuiInterface;
import jmt.gui.common.definitions.ModelConverter;
import jmt.gui.common.definitions.PAResultsModel;
import jmt.gui.common.definitions.ResultsModel;
import jmt.gui.common.definitions.SimulationDefinition;
import jmt.gui.common.editors.DefaultsEditor;
import jmt.gui.common.panels.AboutDialogFactory;
import jmt.gui.common.panels.BlockingRegionParameterPanel;
import jmt.gui.common.panels.MeasurePanel;
import jmt.gui.common.panels.ResultsWindow;
import jmt.gui.common.panels.SimulationPanel;
import jmt.gui.common.panels.StationParameterPanel;
import jmt.gui.common.panels.WarningWindow;
import jmt.gui.common.panels.parametric.PAProgressWindow;
import jmt.gui.common.panels.parametric.PAResultsWindow;
import jmt.gui.common.panels.parametric.ParametricAnalysisPanel;
import jmt.gui.common.resources.JMTImageLoader;
import jmt.gui.common.xml.ModelLoader;
import jmt.gui.common.xml.XMLWriter;
import jmt.gui.exact.ExactModel;
import jmt.gui.exact.ExactWizard;
import jmt.gui.jmodel.DialogFactory;
import jmt.gui.jmodel.JGraphMod.BlockingRegion;
import jmt.gui.jmodel.JGraphMod.CellComponent;
import jmt.gui.jmodel.JGraphMod.CellFactory;
import jmt.gui.jmodel.JGraphMod.InputPort;
import jmt.gui.jmodel.JGraphMod.JmtCell;
import jmt.gui.jmodel.JGraphMod.JmtDefaultCellViewFactory;
import jmt.gui.jmodel.JGraphMod.JmtEdge;
import jmt.gui.jmodel.JGraphMod.JmtEdgeView;
import jmt.gui.jmodel.JGraphMod.JmtGraphConstants;
import jmt.gui.jmodel.JGraphMod.JmtJGraph;
import jmt.gui.jmodel.JGraphMod.JmtOverlapping;
import jmt.gui.jmodel.JGraphMod.OutputPort;
import jmt.gui.jmodel.JGraphMod.SinkCell;
import jmt.gui.jmodel.JGraphMod.SourceCell;
import jmt.gui.jmodel.controller.actions.About;
import jmt.gui.jmodel.controller.actions.AbstractJmodelAction;
import jmt.gui.jmodel.controller.actions.ActionCopy;
import jmt.gui.jmodel.controller.actions.ActionCut;
import jmt.gui.jmodel.controller.actions.ActionDelete;
import jmt.gui.jmodel.controller.actions.ActionPaste;
import jmt.gui.jmodel.controller.actions.ActionRedo;
import jmt.gui.jmodel.controller.actions.ActionRotate;
import jmt.gui.jmodel.controller.actions.ActionSetRight;
import jmt.gui.jmodel.controller.actions.ActionUndo;
import jmt.gui.jmodel.controller.actions.AddBlockingRegion;
import jmt.gui.jmodel.controller.actions.CloseModel;
import jmt.gui.jmodel.controller.actions.EditDefaults;
import jmt.gui.jmodel.controller.actions.EditMeasures;
import jmt.gui.jmodel.controller.actions.EditPAParams;
import jmt.gui.jmodel.controller.actions.EditSimParams;
import jmt.gui.jmodel.controller.actions.EditUserClasses;
import jmt.gui.jmodel.controller.actions.Exit;
import jmt.gui.jmodel.controller.actions.NewModel;
import jmt.gui.jmodel.controller.actions.OpenHelp;
import jmt.gui.jmodel.controller.actions.OpenModel;
import jmt.gui.jmodel.controller.actions.PauseSimulation;
import jmt.gui.jmodel.controller.actions.SaveModel;
import jmt.gui.jmodel.controller.actions.SaveModelAs;
import jmt.gui.jmodel.controller.actions.SetConnectState;
import jmt.gui.jmodel.controller.actions.SetOptions;
import jmt.gui.jmodel.controller.actions.SetSelectState;
import jmt.gui.jmodel.controller.actions.ShowResults;
import jmt.gui.jmodel.controller.actions.Simulate;
import jmt.gui.jmodel.controller.actions.SolveAnalytic;
import jmt.gui.jmodel.controller.actions.SolveApprox;
import jmt.gui.jmodel.controller.actions.StopSimulation;
import jmt.gui.jmodel.controller.actions.SwitchToExactSolver;
import jmt.gui.jmodel.controller.actions.TakeScreenShot;
import jmt.gui.jmodel.controller.actions.ZoomIn;
import jmt.gui.jmodel.controller.actions.ZoomOut;
import jmt.gui.jmodel.definitions.JMODELModel;
import jmt.gui.jmodel.definitions.JMTPoint;
import jmt.gui.jmodel.definitions.JmodelClassDefinition;
import jmt.gui.jmodel.definitions.JmodelStationDefinition;
import jmt.gui.jmodel.mainGui.ComponentBar;
import jmt.gui.jmodel.mainGui.MainWindow;
import jmt.gui.jmodel.panels.JModelProblemsWindow;
import jmt.gui.jmodel.panels.StationNamePanel;
import jmt.gui.jmodel.panels.jmodelClassesPanel;

import org.jgraph.JGraph;
import org.jgraph.graph.CellHandle;
import org.jgraph.graph.CellView;
import org.jgraph.graph.ConnectionSet;
import org.jgraph.graph.DefaultGraphModel;
import org.jgraph.graph.DefaultPort;
import org.jgraph.graph.Edge;
import org.jgraph.graph.EdgeView;
import org.jgraph.graph.GraphConstants;
import org.jgraph.graph.GraphModel;
import org.jgraph.graph.GraphUndoManager;
import org.jgraph.graph.PortView;
import org.jgraph.plaf.basic.BasicGraphUI;

/**
 * This class mantains a reference to all the main copmponents of the Gui,
 * in this way it's possible to divide the responsability of the actions &
 * every object know only about of himself & the mediator.
 * Other actions are made through the mediator without knowing who will actually
 * do it.
 *

 * @author Federico Granata
 * Date: 3-giu-2003
 * Time: 16.54.45

 * Heavily modified by Bertoli Marco 2-giu-2005

 * Modified by Francesco D'Aquino

 * Modyfied by Bertoli Marco to support JGraph 5.8 - 21/mar/2006

 * Modified by Giuseppe De Cicco & Fabio Granara
 * 
 * @author Ashanka Date: 11March 2010
 * Description: Added a logic to remove the duplicate name of the files from displaying.
 * 
 * Modified by Ashanka (May 2010): 
 * Patch: Multi-Sink Perf. Index 
 * Description: Added new Performance index for the capturing the 
 * 				1. global response time (ResponseTime per Sink)
 *              2. global throughput (Throughput per Sink)
 *              each sink per class.
 * Model validation of the perf indices.
 */
public class Mediator implements GuiInterface {
	private boolean isReleased = false;
	// making it final allows the compiler to skip code generation when false
	private GraphMouseListner mouseListner;
	// Dialog factory
	private DialogFactory dialogFactory;
	// Cell factory
	private CellFactory cellFactory;

	private AbstractJmodelAction closeModel, newModel, openHelp, openModel, saveModel, setConnect, actionCopy, actionCut, setOptions, actionPaste,
			setSelect, actionDelete, simulate, solveAnalitic, solveApp, editUserClasses, editMeasures, switchToExactSolver, exit,
			// Bertoli Marco
			editDefaults, saveModelAs, pauseSimulation, stopSimulation, editSimParams, showResults, about, addBlockingRegion, takeScreenShot,
			// end

			// Giuseppe De CIcco & Fabio Granara
			actionRotate, actionSetRight,

			// Conti Andrea
			editUndo, editRedo;
	// end

	private JmtJGraph graph;
	private MainWindow mainWindow;

	protected Object[] cells;
	protected Map cellsAttr;

	public static boolean advanced;
	private Cursor cursor;
	private Cursor oldCursor;

	// Bertoli Marco
	private JMODELModel model;
	private JFrame resultsWindow;
	private JmtClipboard clipboard;
	private DispatcherThread dispatcher = null; // To control simulation
	private ModelLoader modelLoader = new ModelLoader(ModelLoader.JMODEL); // To
	// Save
	// /
	// Load
	private JMTToolBar componentBar;
	// end

	// Francesco D'Aquino
	private ModelChecker mc;
	private JModelProblemsWindow pw;
	private PADispatcherThread batchThread;
	private PAProgressWindow progressWindow;
	private AbstractJmodelAction editPAParams;
	// end

	// Giuseppe De Cicco
	private JmtOverlapping overlapping;

	//QN-ACTR
	private ZoomIn zoomIn;
	private ZoomOut zoomOut;
	
	// end

	public Mediator(final JmtJGraph graph, MainWindow mainWindow) {
		this.mainWindow = mainWindow;
		dialogFactory = new DialogFactory(mainWindow);
		cellFactory = new CellFactory(this);
		this.graph = graph;
		closeModel = new CloseModel(this);
		newModel = new NewModel(this);
		openHelp = new OpenHelp(this);
		openModel = new OpenModel(this);
		saveModel = new SaveModel(this);
		setConnect = new SetConnectState(this);
		actionCopy = new ActionCopy(this);
		actionCut = new ActionCut(this);
		setOptions = new SetOptions(this);
		actionPaste = new ActionPaste(this);
		setSelect = new SetSelectState(this);
		actionDelete = new ActionDelete(this);
		simulate = new Simulate(this);

		solveAnalitic = new SolveAnalytic(this);
		solveApp = new SolveApprox(this);
		// Conti Andrea - undo
		undoManager = new GraphUndoManager();
		undoProxy = new UndoManagerProxy(undoManager);
		editUndo = new ActionUndo(this, undoManager);
		editRedo = new ActionRedo(this, undoManager);
		// end
		// Bertoli Marco
		pauseSimulation = new PauseSimulation(this);
		stopSimulation = new StopSimulation(this);
		exit = new Exit(this);
		clipboard = new JmtClipboard(this);
		editDefaults = new EditDefaults(this);
		saveModelAs = new SaveModelAs(this);
		editSimParams = new EditSimParams(this);
		editPAParams = new EditPAParams(this);
		showResults = new ShowResults(this);
		about = new About(this);
		addBlockingRegion = new AddBlockingRegion(this);
		// fg
		actionSetRight = new ActionSetRight(this);

		takeScreenShot = new TakeScreenShot(this);
		// end

		// GDC & FG
		actionRotate = new ActionRotate(this);
		overlapping = new JmtOverlapping(this);
		// end

		editUserClasses = new EditUserClasses(this);
		editMeasures = new EditMeasures(this);
		switchToExactSolver = new SwitchToExactSolver(this);
		// Initialize new Component bar
		componentBar = new ComponentBar(this);
		
		//QN-Java
		zoomIn = new ZoomIn(this);
		zoomOut = new ZoomOut(this);
	}

	/**
	 * Creates a toolbar to be displayed in main window.
	 * 
	 * // QN-Java 
	 * null is a separation line 
	 * TODO add buttons for zoomIn and zoomOut
	 * 
	 * 
	 * @return created toolbar.
	 */
	public JMTToolBar createToolbar() {
		JMTToolBar toolbar = new JMTToolBar(JMTImageLoader.getImageLoader());
		// Builds an array with all actions to be put in the toolbar
		AbstractJmodelAction[] actions = new AbstractJmodelAction[] { newModel, openModel, saveModel, null,
				// editUndo, editRedo, null,
				actionCut, actionCopy, actionPaste, null, editUserClasses, editMeasures, editSimParams, editPAParams, null, switchToExactSolver,
				null, simulate, pauseSimulation, stopSimulation, showResults, null, editDefaults 
				
				
				//, openHelp
				
				// QN-Java
				, null, zoomIn, null, zoomOut
				
		};
		toolbar.populateToolbar(actions);
		return toolbar;
	}

	/**
	 * Creates a menu to be displayed in main window.
	 * @return created menu.
	 */
	public JMTMenuBar createMenu() {
		JMTMenuBar menu = new JMTMenuBar(JMTImageLoader.getImageLoader());
		// File menu
		MenuAction action = new MenuAction("File", new AbstractJmodelAction[] { newModel, openModel, saveModel, saveModelAs, closeModel, null, exit });
		menu.addMenu(action);

		// Edit menu
		action = new MenuAction("Edit", new AbstractJmodelAction[] {
		// editUndo, editRedo, null
				actionCut, actionCopy, actionPaste, actionDelete, null, takeScreenShot });
		menu.addMenu(action);

		// Define menu
		action = new MenuAction("Define",
				new AbstractJmodelAction[] { editUserClasses, editMeasures, editSimParams, editPAParams, null, editDefaults });
		menu.addMenu(action);

		// Solve menu
		action = new MenuAction("Solve", new AbstractJmodelAction[] { simulate, pauseSimulation, stopSimulation, null, switchToExactSolver, null,
				showResults });
		menu.addMenu(action);

		// Help menu
		action = new MenuAction("Help", new AbstractJmodelAction[] { openHelp, null, about });
		menu.addMenu(action);

		return menu;
	}

	public JMTToolBar getComponentBar() {
		return componentBar;
	}

	public void setMouseListner(GraphMouseListner mouseListner) {
		this.mouseListner = mouseListner;
	}

	private File openedArchive;

	// Bertoli Marco
	public AbstractJmodelAction getEditUserClasses() {
		return editUserClasses;
	}

	public AbstractJmodelAction getExit() {
		return exit;
	}

	public JmodelStationDefinition getStationDefinition() {
		return model;
	}

	public JmodelClassDefinition getClassDefinition() {
		return model;
	}

	public SimulationDefinition getSimulationDefinition() {
		return model;
	}

	public BlockingRegionDefinition getBlockingRegionDefinition() {
		return model;
	}

	public void enableAddBlockingRegion(boolean state) {
		addBlockingRegion.setEnabled(state);
	}

	// Giuseppe De CIcco & Fabio Granara
	public void enableSetRight(boolean state) {
		actionSetRight.setEnabled(state);
	}

	// Giuseppe De CIcco & Fabio Granara
	public AbstractJmodelAction getSetRight() {
		return actionSetRight;
	}

	public AbstractJmodelAction getEditDefaults() {
		return editDefaults;
	}

	public AbstractJmodelAction getAddBlockingRegion() {
		return addBlockingRegion;
	}

	public AbstractJmodelAction getSaveModelAs() {
		return saveModelAs;
	}

	public AbstractJmodelAction getPauseSimulation() {
		return pauseSimulation;
	}

	public AbstractJmodelAction getStopSimulation() {
		return stopSimulation;
	}

	public AbstractJmodelAction getEditSimParams() {
		return editSimParams;
	}

	public AbstractJmodelAction getEditPAParams() {
		return editPAParams;
	}

	public AbstractJmodelAction getShowResults() {
		return showResults;
	}

	public AbstractJmodelAction getAbout() {
		return about;
	}

	// end

	// Conti Andrea - undo
	private GraphUndoManager undoManager;
	private UndoManagerProxy undoProxy;

	public void undo() {
		undoManager.undo();
	}

	public void redo() {
		undoManager.redo();
	}

	public void enableUndoAction(boolean state) {
		editUndo.setEnabled(state);
	}

	public void enableRedoAction(boolean state) {
		editRedo.setEnabled(state);
	}

	public AbstractJmodelAction getUndoAction() {
		return editUndo;
	}

	public AbstractJmodelAction getRedoAction() {
		return editRedo;
	}

	public GraphUndoManager getUndoManager() {
		return undoManager;
	}

	public void setUndoManager(GraphUndoManager um) {
		undoManager = um;
	}

	// end

	public void setConnectState() {
		setSelect.setEnabled(true);
		// DEK (Federico Granata) 14-11-2003
		oldCursor = cursor;
		cursor = new Cursor(Cursor.CROSSHAIR_CURSOR);
		setGraphCursor(cursor);

		// end 14-11-2003
		mouseListner.setConnectState();
	}

	public void setCopyState() {
		setSelect.setEnabled(true);
		// mouseListner.setCopyState();
	}

	public void setCutState() {
		setSelect.setEnabled(true);
		// mouseListner.setCutState();
	}

	public void setInsertState(String className) {
		setSelect.setEnabled(true);
		mouseListner.setInsertState(className);
		
		//JOptionPane.showMessageDialog(null, "setInsertState() className: " + className, "Mediator.java", JOptionPane.INFORMATION_MESSAGE); // CAO
	}

	public void setSelectState() {
		setSelect.setEnabled(true);
		mouseListner.setSelectState();
		// DEK (Federico Granata)
		// mainWindow.getAlbero().setSelectedButton(true);
	}

	public AbstractJmodelAction getDeleteAction() {
		return actionDelete;
	}

	public void activateSelect() {
		setSelect.setEnabled(true);
		componentBar.clickButton(setSelect);
	}

	public void enableCutAction(boolean state) {
		actionCut.setEnabled(state);
	}

	public void enablePasteAction(boolean state) {
		actionPaste.setEnabled(state);
	}

	public void enableCopyAction(boolean state) {
		actionCopy.setEnabled(state);
	}

	// Giuseppe De CIcco & Fabio Granara
	public void enableRotateAction(boolean state) {
		actionRotate.setEnabled(state);
	}

	public void enableDeleteAction(boolean state) {
		actionDelete.setEnabled(state);
	}

	public AbstractJmodelAction getTakeScreenShot() {
		return takeScreenShot;
	}

	public void setHandle(CellHandle handle) {
		this.mouseListner.setHandle(handle);
	}

	public AbstractJmodelAction getCloseModel() {
		return closeModel;
	}

	public AbstractJmodelAction getNewModel() {
		return newModel;
	}

	public AbstractJmodelAction getOpenHelp() {
		return openHelp;
	}

	public AbstractJmodelAction getOpenModel() {
		return openModel;
	}

	public AbstractJmodelAction getSaveModel() {
		return saveModel;
	}

	public AbstractJmodelAction getSetConnect() {
		return setConnect;
	}

	public AbstractJmodelAction getCopyAction() {
		return actionCopy;
	}

	public AbstractJmodelAction getCutAction() {
		return actionCut;
	}

	public AbstractJmodelAction getSetOptions() {
		return setOptions;
	}

	public AbstractJmodelAction getPasteAction() {
		return actionPaste;
	}

	public AbstractJmodelAction getSetSelect() {
		return setSelect;
	}

	public AbstractJmodelAction getSimulate() {
		return simulate;
	}

	public AbstractJmodelAction getSolveAnalitic() {
		return solveAnalitic;
	}

	public AbstractJmodelAction getSolveApp() {
		return solveApp;
	}

	/**
	 * Gets cell factory to create new graph cells
	 * @return cell factory
	 */
	public CellFactory getCellFactory() {
		return cellFactory;
	}

	public void newModel() {
		if (checkForSave("<html>Save changes before creating a new model?</html>")) {
			return;
		}
		resetMouseState();
		graph = new JmtJGraph(this);
		graph.setModel(new DefaultGraphModel());

		// Giuseppe De Cicco

		graph.getGraphLayoutCache().setFactory(new JmtDefaultCellViewFactory(this) {

			/**
			 * 
			 */
			private static final long serialVersionUID = -8352272370225918131L;

			// Override per creare il RENDERER - Giuseppe De Cicco
			protected EdgeView createEdgeView(Object cell) {

				if (cell instanceof Edge || cell instanceof JmtEdge) {
					// System.out.println("Lato personalizzato");
					return new JmtEdgeView(cell, mediator);
				} else {
					return new JmtEdgeView(cell, mediator);
				}
			}
		});
		// end

		// Sets the cloneable flag to 'false'
		graph.setCloneable(false);
		graph.setGridSize(20);
		graph.setGridVisible(true);
		if (advanced) {
			graph.setBackground(new Color(120, 120, 120));
		}

		graph.addMouseListener(mouseListner);
		graph.addMouseMotionListener(mouseListner);

		// Conti Andrea
		undoProxy.discardAllEdits();
		graph.getModel().addUndoableEditListener(undoProxy);
		// end

		// Bertoli Marco
		// Instantiates a new JMODELModel data structure to store the entire
		// model
		model = new JMODELModel();
		// end

		mainWindow.setGraph(graph);
		closeModel.setEnabled(true);
		saveModel.setEnabled(true);
		saveModelAs.setEnabled(true);
		editMeasures.setEnabled(true);
		// Bertoli Marco
		// Show only insert options on ComponentBar
		componentBar.clearButtonGroupSelection(0);
		componentBar.enableButtonGroup(0, true);

		// Disables show results button and measure definition, until simulation
		showResults.setSelected(false);
		showResults.setEnabled(false);
		if (resultsWindow != null) {
			resultsWindow.dispose();
		}
		resultsWindow = null;

		// Disables cut/copy/delete (leave paste enabled as clipboard is not
		// flushed)
		enableCopyAction(false);
		enableCutAction(false);
		enableDeleteAction(false);
		// end

		// Disables creation of blocking region
		enableAddBlockingRegion(false);
		setConnect.setEnabled(false);
		setSelect.setEnabled(false);

		// Enable the action to perform editing user classes
		editUserClasses.setEnabled(true);
		switchToExactSolver.setEnabled(true);
		// Enables the botton to start simualtion
		simulate.setEnabled(true);
		editSimParams.setEnabled(true);
		editPAParams.setEnabled(true);
		takeScreenShot.setEnabled(true);
		openedArchive = null;
		mainWindow.updateTitle(null);
		
    //QN-Java
    zoomIn.setEnabled(true);
    zoomOut.setEnabled(true);
		
		// Free same resources by forcing a garbage collection
		System.gc();
	}

	/**
	 * Opens a model from a data file.
	 * <br> Author: Bertoli Marco
	 */
	public void openModel() {
	  
	  //System.out.println("Mediator.java openModel");
	  
		isReleased = true;
		if (checkForSave("<html>Save changes before opening a saved model?</html>")) {
			return;
		}
		JMODELModel tmpmodel = new JMODELModel();
		int state = modelLoader.loadModel(tmpmodel, mainWindow);
		
		//System.out.println("Mediator.java openModel");
		
		if (state == ModelLoader.SUCCESS || state == ModelLoader.WARNING) {
			resetMouseState();
			// Avoid checkForSave again...
			if (model != null) {
				model.resetSaveState();
			}
			newModel();
			// At this point loading was successful, so substitutes old model
			// with loaded one
			model = tmpmodel;
			this.populateGraph();
			setSelect.setEnabled(true);
			componentBar.clickButton(setSelect);
			openedArchive = modelLoader.getSelectedFile();
			mainWindow.updateTitle(openedArchive.getName());
			// Removes selection
			graph.clearSelection();
			// If model contains results, enable Results Window
			if (model.containsSimulationResults()) {
				if (model.isParametricAnalysisEnabled()) {
					this.setResultsWindow(new PAResultsWindow(model.getParametricAnalysisModel(), (PAResultsModel) model.getSimulationResults()));
					showResults.setEnabled(true);
				} else {
					this.setResultsWindow(new ResultsWindow(model.getSimulationResults()));
					showResults.setEnabled(true);
				}
			}
			model.resetSaveState();
			
			
	    //QN-Java
	    zoomIn.setEnabled(true);
	    zoomOut.setEnabled(true);
			
			System.gc();
		} else if (state == ModelLoader.FAILURE) {
			showErrorMessage(modelLoader.getFailureMotivation());
		}
		// Shows warnings if any
		if (state == ModelLoader.WARNING) {
			new WarningWindow(modelLoader.getLastWarnings(), mainWindow, modelLoader.getInputFileFormat(), CommonConstants.JSIM).show();
		}

	}

	public void closeModel() {
		// Checks if there's an old graph to save
		if (checkForSave("<html>Save changes before closing?</html>")) {
			return;
		}
		resetMouseState();

		// clear undo history
		graph.getModel().removeUndoableEditListener(undoProxy);
		undoProxy.discardAllEdits();
		// end
		// graph.setModel(null); //wreaks quite a bit of havoc
		mainWindow.removeGraph();
		graph = null;
		closeModel.setEnabled(false);
		saveModel.setEnabled(false);
		editMeasures.setEnabled(false);
		saveModelAs.setEnabled(false);
		componentBar.clearButtonGroupSelection(0);
		componentBar.enableButtonGroup(0, false);
		setConnect.setEnabled(false);
		actionCopy.setEnabled(false);
		actionCut.setEnabled(false);
		actionPaste.setEnabled(false);
		actionDelete.setEnabled(false);
		
		// FG
		actionSetRight.setEnabled(false);
		actionRotate.setEnabled(false);

		setSelect.setEnabled(false);
		simulate.setEnabled(false);
		solveAnalitic.setEnabled(false);
		solveApp.setEnabled(false);
		editUserClasses.setEnabled(false);
		editMeasures.setEnabled(false);
		switchToExactSolver.setEnabled(false);
		// Disables the botton to start simualtion
		simulate.setEnabled(false);
		editSimParams.setEnabled(false);
		editPAParams.setEnabled(false);
		takeScreenShot.setEnabled(false);
		// Disables show results button and measure definition
		showResults.setSelected(false);
		showResults.setEnabled(false);
		if (resultsWindow != null) {
			resultsWindow.dispose();
		}
		resultsWindow = null;
		openedArchive = null;
		model = new JMODELModel();
		mainWindow.updateTitle(null);
		
    //QN-Java
    zoomIn.setEnabled(false);
    zoomOut.setEnabled(false);
    
		
		// Free same resources by forcing a garbage collection
		System.gc();
	}

	/** Inserts a new cell (vertex) in the desired point into the graph.
	 *
	 * @param newCell the new cell
	 * @param pt point in absolute coordinates in the
	 */
	public void InsertCell(Point2D pt, JmtCell newCell) {
		pt = graph.snap(pt);
		Object[] arg = new Object[] { newCell };
		graph.getModel().insert(arg, newCell.setAttributes(pt, graph), null, null, null);
		// Puts new cell on back to go under blocking regions
		graph.getModel().toBack(new Object[] { newCell });
		newCell.resetParent();
		setConnect.setEnabled(true);
	}

	/** Set the state of mouse listner to select & passes the event to the
	 * listner as if press event is generated.
	 *
	 * @param e
	 */
	public void selectAt(MouseEvent e) {
		activateSelect();
		mouseListner.mousePressed(e);
	}

	/**
	 * Determines whether this component is enabled. An enabled component
	 * can respond to user input and generate events. Components are
	 * enabled initially by default. A component may be enabled or disabled by
	 * calling its <code>setEnabled</code> method.
	 * @return <code>true</code> if the component is enabled,
	 * 		<code>false</code> otherwise
	 * @since JDK1.0
	 */
	public boolean isGraphEnabled() {
		return graph.isEnabled();
	}

	public void graphRequestFocus() {
		graph.requestFocus();
	}

	public int getTolerance() {
		return graph.getTolerance();
	}

	public Rectangle2D fromScreen(Rectangle2D r) {
		return graph.fromScreen(r);
	}

	public Point2D fromScreen(Point2D p) {
		return graph.fromScreen(p);
	}

	/**
	 * Returns this graph's graphics context, which lets you draw
	 * on a component. Use this method get a <code>Graphics</code> object and
	 * then invoke operations on that object to draw on the component.
	 * @return this components graphics context
	 */
	public Graphics2D getGraphGraphics() {
		// DEK (Federico Granata) 17-11-2003
		return (Graphics2D) graph.getGraphics();
		// end 17-11-2003
		// return mainWindow.getGraphics();
	}

	public CellView getNextViewAt(CellView current, double x, double y) {
		return graph.getNextViewAt(current, x, y);
	}

	/**
	 * Returning true signifies the marquee handler has precedence over
	 * other handlers, and is receiving subsequent mouse events.
	 */
	public boolean isForceMarqueeEvent(MouseEvent e) {
		return ((JmtGraphUI) graph.getUI()).isForceMarqueeEvent(e);
	}

	/**
	 * Returns the number of clicks for editing of the graph to start.
	 */
	public int getEditClickCount() {
		return graph.getEditClickCount();
	}

	/**
	 * Returning true signifies a mouse event on the cell should toggle
	 * the selection of only the cell under mouse.
	 */
	public boolean isToggleSelectionEvent(MouseEvent e) {
		return ((JmtGraphUI) graph.getUI()).isToggleSelectionEvent(e);
	}

	/**
	 * Returns true if the cell is currently selected.
	 * @param cell an object identifying a cell
	 * @return true if the cell is selected
	 */
	public boolean isCellSelected(Object cell) {
		return graph.isCellSelected(cell);
	}

	/**
	 * Messaged to update the selection based on a MouseEvent over a
	 * particular cell. If the event is a toggle selection event, the
	 * cell is either selected, or deselected. Otherwise the cell is
	 * selected.
	 */
	public void selectCellForEvent(Object cell, MouseEvent e) {
		((JmtGraphUI) graph.getUI()).selectCellForEvent(cell, e);
	}

	/**
	 * Scroll the graph for an event at <code>p</code>.
	 */
	public void autoscroll(Point p) {
		BasicGraphUI.autoscroll(graph, p);
	}

	/**
	 * Gets the cursor set in the graph. If the graph does
	 * not have a cursor set, the cursor of its parent is returned.
	 * If no cursor is set in the entire hierarchy,
	 * <code>Cursor.DEFAULT_CURSOR</code> is returned.
	 */
	public Cursor getGraphCursor() {
		return graph.getCursor();
	}

	/**
	 * Sets graph cursor
	 * @param cursor to be setted
	 */
	public void setGraphCursor(Cursor cursor) {
		graph.setCursor(cursor);
	}

	/**
	 * Returns true if the graph is being edited.  The item that is being
	 * edited can be returned by getEditingCell().
	 */
	public boolean isGraphEditing() {
		return graph.getUI().isEditing(graph);
	}

	/**
	 * Returns the given point applied to the grid.
	 * @param p a point in screen coordinates.
	 * @return the same point applied to the grid.
	 */
	public Point2D snap(Point2D p) {
		return graph.snap(p);
	}

	/**
	 * Upscale the given point in place, ie.
	 * using the given instance.
	 * @param p the point to be upscaled
	 * @return the upscaled point instance
	 */
	public Point2D toScreen(Point2D p) {
		return graph.toScreen(p);
	}

	/**
	 * Gets the background color of graph.
	 * @return this component's background color; if this component does
	 * 		not have a background color,
	 *		the background color of its parent is returned
	 */
	public Color getGraphBackground() {
		return graph.getBackground();
	}

	/**
	 * Returns the current marquee color of the graph.
	 */
	public Color getGraphMarqueeColor() {
		return graph.getMarqueeColor();
	}

	// Giuseppe De Cicco & Fabio Granara
	// protected boolean no = true;
	public void connect(Point2D start, Point2D current, PortView inPort, PortView outPort) {
		Point2D p = fromScreen(start);
		Point2D p2 = fromScreen(current);
		if (inPort != null && outPort != null) {
			ArrayList<Point2D> list = new ArrayList<Point2D>();
			list.add(p);
			list.add(p2);
			Map map = new Hashtable();
			GraphConstants.setPoints(map, list);
			GraphConstants.setRouting(map, GraphConstants.ROUTING_SIMPLE);
			GraphConstants.setRouting(map, JmtGraphConstants.ROUTING_JMT);
			GraphConstants.setEndFill(map, true);

			// 24/09/03 - Massimo Cattai
			// //////////////////////////////////////////
			// Add a Line End Attribute
			GraphConstants.setLineEnd(map, GraphConstants.ARROW_CLASSIC);
			// 24/09/03 - end
			// /////////////////////////////////////////////////////
			Map<Object, Map> viewMap = new Hashtable<Object, Map>();
			// ---- Adds connection into underlayng data structure -- BERTOLI
			// MARCO
			Object sourceKey = ((CellComponent) ((JmtCell) ((OutputPort) (outPort.getCell())).getUserObject()).getUserObject()).getKey();
			Object targetKey = ((CellComponent) ((JmtCell) ((InputPort) (inPort.getCell())).getUserObject()).getUserObject()).getKey();
			JmtEdge connection = new JmtEdge(sourceKey, targetKey, this);
			viewMap.put(connection, map);
			Object[] insert = new Object[] { connection };
			ConnectionSet cs = new ConnectionSet();
			cs.connect(connection, outPort.getCell(), true);
			cs.connect(connection, inPort.getCell(), false);
			// Visualize connection only if it can be created into data
			// structure
			if (model.setConnected(sourceKey, targetKey, true)) {
				graph.getModel().insert(insert, viewMap, cs, null, null);
				// FG
				// no = false;
			}
			// ---- End -- BERTOLI MARCO
		}

	}

	/**
	 * Creates a connection between given source and target JmtCells
	 * @param source source cell
	 * @param target target cell
	 * @return created component or null if connection between source and target cannot be created
	 *
	 * Author: Bertoli Marco
	 */
	public JmtEdge connect(JmtCell source, JmtCell target) {
		return connect(source, target, false);
	}

	/**
	 * Creates a connection between given source and target JmtCells
	 * @param source source cell
	 * @param target target cell
	 * @param forced true if connection must be shown also if could not be created into data structure.
	 * @return created component or null if connection between source and target cannot be created
	 *
	 * Author: Bertoli Marco
	 */
	public JmtEdge connect(JmtCell source, JmtCell target, boolean forced) {
		// If one of parameter is null, returns null
		if (source == null || target == null) {
			return null;
		}
		// Retrives source and target keys to create connection
		Object sourceKey = ((CellComponent) source.getUserObject()).getKey();
		Object targetKey = ((CellComponent) target.getUserObject()).getKey();
		// Initializes correct layout for routing edges
		Map map = new Hashtable();
		GraphConstants.setRouting(map, GraphConstants.ROUTING_SIMPLE);
		GraphConstants.setRouting(map, JmtGraphConstants.ROUTING_JMT);
		GraphConstants.setLineEnd(map, GraphConstants.ARROW_CLASSIC);
		GraphConstants.setEndFill(map, true);
		Map<Object, Map> viewMap = new Hashtable<Object, Map>();
		JmtEdge connection = new JmtEdge(sourceKey, targetKey, this);
		viewMap.put(connection, map);
		Object[] insert = new Object[] { connection };
		ConnectionSet cs = new ConnectionSet();
		// Finds sourcePort
		Iterator it;
		it = source.getChildren().iterator();
		DefaultPort tmpPort, sourcePort, targetPort;
		sourcePort = null;
		while (it.hasNext()) {
			tmpPort = (DefaultPort) it.next();
			if (tmpPort instanceof OutputPort) {
				sourcePort = tmpPort;
			}
		}
		// Finds targetPort
		it = target.getChildren().iterator();
		targetPort = null;
		while (it.hasNext()) {
			tmpPort = (DefaultPort) it.next();
			if (tmpPort instanceof InputPort) {
				targetPort = tmpPort;
			}
		}
		if (sourcePort != null && targetPort != null) {
			cs.connect(connection, sourcePort, true);
			cs.connect(connection, targetPort, false);
			// Adds connection to the graph only if it can be created into data
			// structure
			if (model.setConnected(sourceKey, targetKey, true) || forced) {
				graph.getModel().insert(insert, viewMap, cs, null, null);
				return connection;
			}
		}
		return null;
	}

	/**
	 * repaints the graph component
	 */
	public void graphRepaint() {
		graph.repaint();
	}

	/**
	 * Returns the parent of <I>child</I> in the model.
	 * <I>child</I> must be a node previously obtained from
	 * this data source. This returns null if <i>child</i> is
	 * a root in the model.
	 *
	 * @param   child  a node in the graph, obtained from this data source
	 * @return  the parent of <I>child</I>
	 */
	public Object getParent(Object child) {
		return graph.getModel().getParent(child);
	}

	/** gets the first portView of the input port of the cell at position
	 *
	 * @param x
	 * @param y
	 * @return portView of the input port
	 */
	public PortView getInPortViewAt(int x, int y) {
		return (PortView) graph.getGraphLayoutCache().getMapping(graph.getInPortAt(x, y), false);
	}

	/** gets the first portView of the output port of the cell at position
	 *
	 * @param x
	 * @param y
	 * @return portView of the output port
	 */
	public PortView getOutPortViewAt(int x, int y) {
		return (PortView) graph.getGraphLayoutCache().getMapping(graph.getOutPortAt(x, y), false);
	}

	/**
	 * Returns if a given cell is visible on graph
	 * @param cell
	 * @return true iff cell is visible
	 */
	public boolean isCellVisible(Object cell) {
		return ((JmtGraphUI) graph.getUI()).getGraphLayoutCache().isVisible(cell);
	}

	/**
	 * Returns the views for the specified array of cells. Returned
	 * array may contain null pointers if the respective cell is not
	 * mapped in this view and <code>create</code> is <code>false</code>.
	 */
	public CellView getViewOfCell(Object cell, boolean create) {
		return ((JmtGraphUI) graph.getUI()).getGraphLayoutCache().getMapping(cell, create);
	}

	/**
	 * Selects the specified cell and initiates editing.
	 * The edit-attempt fails if the <code>CellEditor</code>
	 * does not allow
	 * editing for the specified item.
	 */
	public void startEditingAtCell(Object cell) {
		graph.startEditingAtCell(cell);
		if ((cell != null) && (cell instanceof JmtCell)) {
			JmtCell jcell = (JmtCell) cell;
			StationParameterPanel stationPanel = new jmt.gui.common.panels.StationParameterPanel(model, model,
					((CellComponent) jcell.getUserObject()).getKey());
			// Adds on the top a panel to change station name
			stationPanel.add(new StationNamePanel(model, ((CellComponent) jcell.getUserObject()).getKey()), BorderLayout.NORTH);
			dialogFactory.getDialog(stationPanel, "Editing " + jcell.getUserObject().toString() + " Properties...");

			// Updates cell dimensions if name was changed too much...
			Hashtable<Object, Map> nest = new Hashtable<Object, Map>();
			Dimension cellDimension = jcell.getSize(graph);
			Map attr = jcell.getAttributes();
			Rectangle2D oldBounds = GraphConstants.getBounds(attr);
			if (oldBounds.getWidth() != cellDimension.getWidth()) {
				GraphConstants.setBounds(attr, new Rectangle2D.Double(oldBounds.getX(), oldBounds.getY(), cellDimension.getWidth(), cellDimension
						.getHeight()));
				nest.put(cell, attr);
				jcell.updatePortPositions(nest, GraphConstants.getIcon(attr), cellDimension);
				graph.getGraphLayoutCache().edit(nest);
			}
		}
		// Blocking region editing
		else if ((cell != null) && (cell instanceof BlockingRegion)) {
			Object regionKey = ((BlockingRegion) cell).getKey();
			dialogFactory.getDialog(new BlockingRegionParameterPanel(model, model, regionKey), "Editing " + model.getRegionName(regionKey)
					+ " Properties...");
		}
	}

	/**
	 * Cuts the selection of the graph.
	 */
	public void cutSelection() {
		// Bertoli Marco
		clipboard.cut();
	}

	/**
	 * Pastes the selection on the graph.
	 */
	public void pasteSelection() {
		// Bertoli Marco
		clipboard.paste();

		// If more than one stations are present enables link button
		if (graph.getModel().getRootCount() > 1) {
			setConnect.setEnabled(true);
		}
		// If one station is present show select button
		if (graph.getModel().getRootCount() >= 1) {
			activateSelect();
		}
	}

	/**
	 * Copies the selection of the graph.
	 */
	public void copySelection() {
		// Bertoli Marco
		clipboard.copy();
	}

	/**
	 * Displays an error message in the panel that is responable to make the
	 * user understand why a certain operation is not valid.
	 *
	 * @param message error to be displayed.
	 */
	public void displayGraphErrMsg(String message) {
		// per ora faccio printare nell'output, assolutamente provvisorio.
		System.out.println("message = " + message);
	}

	/**
	 * Deletes all the vertex & edgees that are selected. it deletes also the
	 * edges that are connected to the eliminated vertexes.

	 * Bertoli Marco 03-06-2005
	 */
	public void deleteSelected() {
		Object cells[] = graph.getSelectionCells();
		GraphModel graphmodel = graph.getModel();

		// If a cell is a blocking region avoid removing its edges and
		// select its element at the end of the removal process
		Set edges = new HashSet();
		Set<Object> select = new HashSet<Object>();

		// Set with all regions that can be deleted as its child were removed
		Set<Object> regions = new HashSet<Object>();
		// Set with all JmtCells that we are removing
		Set<Object> jmtCells = new HashSet<Object>();

		// Giuseppe De Cicco & Fabio Granara
		// for(int k=0; k<cells.length; k++){
		// if(cells[k] instanceof JmtEdge){
		// ((JmtCell)(graphmodel.getParent(graphmodel.getSource((JmtEdge)cells[k])))).SubOut();
		// ((JmtCell)(graphmodel.getParent(graphmodel.getTarget((JmtEdge)cells[k])))).SubIn();
		// }
		//
		// }
		for (int i = 0; i < cells.length; i++) {
			if (!(cells[i] instanceof BlockingRegion)) {
				// Adds edge for removal
				edges.addAll(DefaultGraphModel.getEdges(graphmodel, new Object[] { cells[i] }));
				// Giuseppe De Cicco & Fabio Granara
				// quando vado a eliminare un nodo, a cui �collegato un arco,
				// vado ad incrementare o diminuire il contatore per il
				// pulsanteAGGIUSTATUTTO
				// Iterator iter = edges.iterator();
				// while (iter.hasNext()) {
				// Object next = iter.next();
				// if (next instanceof JmtEdge){
				// ((JmtCell)(graphmodel.getParent(graphmodel.getSource((JmtEdge)next)))).SubOut();
				// ((JmtCell)(graphmodel.getParent(graphmodel.getTarget((JmtEdge)next)))).SubIn();
				// }
				//
				// }
				// Stores parents information and cell
				if (cells[i] instanceof JmtCell) {
					if (((JmtCell) cells[i]).getParent() instanceof BlockingRegion) {
						regions.add(((JmtCell) cells[i]).getParent());
					}
					jmtCells.add(cells[i]);
				}
			} else {
				// Adds node for selection
				Object[] nodes = graph.getDescendants(new Object[] { cells[i] });
				for (Object node : nodes) {
					if (node instanceof JmtCell || node instanceof JmtEdge) {
						select.add(node);
					}
				}
				// Removes blocking region from data structure
				model.deleteBlockingRegion(((BlockingRegion) cells[i]).getKey());
			}
		}

		if (!edges.isEmpty()) {
			graphmodel.remove(edges.toArray());
		}
		// removes cells from graph
		graphmodel.remove(cells);

		// Checks if all children of a blocking region have been removed
		Iterator<Object> it = regions.iterator();
		while (it.hasNext()) {
			jmtCells.add(null);
			BlockingRegion region = (BlockingRegion) it.next();
			List child = region.getChildren();
			boolean empty = true;
			for (int i = 0; i < child.size(); i++) {
				if (child.get(i) instanceof JmtCell && !jmtCells.contains(child.get(i))) {
					empty = false;
					break;
				}
			}
			if (empty) {
				model.deleteBlockingRegion(region.getKey());
				graphmodel.remove(new Object[] { region });
			}
		}

		// Removes cells from data structure
		for (Object cell : cells) {
			if (cell instanceof JmtCell) {
				model.deleteStation(((CellComponent) ((JmtCell) cell).getUserObject()).getKey());
			} else if (cell instanceof JmtEdge) {
				JmtEdge link = (JmtEdge) cell;
				model.setConnected(link.getSourceKey(), link.getTargetKey(), false);
			}
		}

		// If no stations remains gray select and link buttons
		if (graph.getModel().getRootCount() == 0) {
			componentBar.clearButtonGroupSelection(0);
			setConnect.setEnabled(false);
			setSelect.setEnabled(false);
		}

		// Selects components from removed blocking regions
		if (select.size() > 0) {
			graph.setSelectionCells(select.toArray());
			// Resets parent information of cells that changed parent
			it = select.iterator();
			while (it.hasNext()) {
				Object next = it.next();
				if (next instanceof JmtCell) {
					((JmtCell) next).resetParent();
				}
			}
		}
	}

	/**
	 * Shows the panel that opens when the rigth button is clicked.
	 *
	 * @param p point where the right button is cliecked on the graph
	 */
	public void showOPanel(Point p) {

	}

	/**
	 * Saves the current model into current file if exists, otherwise calls saveModelAs()
	 * Author: Bertoli Marco
	 */
	public void saveModel() {
		if (openedArchive == null) {
			saveModelAs();
			return;
		}

		// Updates station positions into data structure
		updateStationPositions();
		int status = modelLoader.saveModel(model, mainWindow, openedArchive);
		switch (status) {
			case ModelLoader.SUCCESS:
				model.resetSaveState();
				mainWindow.updateTitle(openedArchive.getName());
				break;
			case ModelLoader.FAILURE:
				showErrorMessage(modelLoader.getFailureMotivation());
				break;
		}
	}

	/**
	 * Saves the current model into a user specified file.
	 * Author: Bertoli Marco
	 */
	public void saveModelAs() {
		// Updates station positions into data structure
		updateStationPositions();
		int status = modelLoader.saveModel(model, mainWindow, null);
		switch (status) {
			case ModelLoader.SUCCESS:
				model.resetSaveState();
				openedArchive = modelLoader.getSelectedFile();
				mainWindow.updateTitle(openedArchive.getName());
				break;
			case ModelLoader.FAILURE:
				showErrorMessage(modelLoader.getFailureMotivation());
				break;
		}
	}

	/**
	 * Updates station positions into data structure to reflect the one shown on jgraph
	 * window. This method is called before saving model.
	 * Author: Bertoli Marco
	 */
	public void updateStationPositions() {
		Object key;
		Object[] cells = graph.getDescendants(graph.getRoots());
		for (Object cell : cells) {
			if (cell instanceof JmtCell) {
				JmtCell jcell = (JmtCell) cell;
				key = ((CellComponent) jcell.getUserObject()).getKey();
				// Sets cell coordinate into data structure
				model.setStationPosition(key, new JMTPoint(getCellCoordinates(jcell), !jcell.isLeftInputCell()));
			}
		}
	}

	/**
	 * Uses information retrived from data structure to recreate graph structure.
	 * This method has to be called after loading a model.
	 * <br>Author: Bertoli Marco
	 */
	public void populateGraph() {
		Object[] stations = model.getStationKeys().toArray();
		HashMap<Object, JmtCell> cells = new HashMap<Object, JmtCell>();
		JmtCell cell;

		// Variables for auto-placement. Currently items are placed on a grid...
		// Need to be improved!!!
		int count = 0;
		int X = 150; // distance on the X axis
		int Y = 50; // distance on the Y axis
		int X0 = 50;
		int Y0 = 15;
		int colCount = (graph.getHeight() - 2 * Y0) / Y;
		boolean containPosition = true;
		// Shows stations
		for (Object station : stations) {
			cell = cellFactory.createStationCell(station);
			JMTPoint position = model.getStationPosition(station);
			// If position is not present, auto-position this station

			while (position == null) {
				containPosition = false;
				JMTPoint tmp = new JMTPoint(X0 + X * (count / colCount), Y0 + Y * (count % colCount), false);
				if (!overlapCells(tmp, cell)) {
					position = tmp;
				}
				count++;
			}
			InsertCell(position, cell);
			if (position.isRotate()) {
				rotateComponent(new Object[] { cell });
			}
			cells.put(station, cell);
		}
		Vector forwardConnections;
		// Shows connections
		for (Object station : stations) {
			forwardConnections = model.getForwardConnections(station);
			for (int j = 0; j < forwardConnections.size(); j++) {
				// Forces connection as it's already present into data structure
				connect(cells.get(station), cells.get(forwardConnections.get(j)), true);
			}

		}
		// Now adds blocking regions
		Vector regions = model.getRegionKeys();
		for (int i = 0; i < regions.size(); i++) {
			Object key = regions.get(i);
			Set<JmtCell> regionStation = new HashSet<JmtCell>();
			Iterator stationKeys = model.getBlockingRegionStations(key).iterator();
			while (stationKeys.hasNext()) {
				regionStation.add(cells.get(stationKeys.next()));
			}
			// Adds cells to blocking region
			addCellsToBlockingRegion(regionStation.toArray(), key);
		}

		// graph.repaint();

		// whether the Position is Null, the application call the reposition's
		// method
		if (!containPosition) {
			adjustGraph();
		}
		graphRepaint();
		graph.getGraphLayoutCache().reload();
		// graph.repaint();
	}

	/**
	 * @return the system <code>JGraph</code>.
	 */
	public JGraph getGraph() {
		return graph;
	}

	/**
	 * Launches the <code>UserClass</code> editor.
	 */
	public void editUserClasses() {
		dialogFactory.getDialog(new jmodelClassesPanel(model, model), "Define customer classes");
	}

	/**
	 * Enables or not the <code>UserClass</code> editor function.
	 */
	public void enableEditUserClasses(boolean state) {
		editUserClasses.setEnabled(state);
	}

	/**
	 * Searches the cell with the given <i>name</i>.
	 * @param name A given cell name.
	 * @return <code>true</code> - if the searched cell will be found.
	 */
	public boolean existCell(String name) {
		int nCells = graph.getModel().getRootCount();
		for (int i = 0; i < nCells; i++) {
			Object cell = graph.getModel().getRootAt(i);
			if (cell instanceof JmtCell) {
				// Map attributes = ((JmtCell) cell).getAttributes();
				// String cellName = (String) attributes.get("NAME");
				String cellName = ((JmtCell) cell).getUserObject().toString();
				if (cellName.equals(name)) {
					return true;
				}
			}
		}
		return false;
	}

	// /**
	// * This function will put selected cells in place avoiding overlapping
	// with other cells
	// * in graph window
	// * <br>
	// * Author: Bertoli Marco
	// */
	// // VECCHIO ALGORITMO
	// public void putSelectedCellsInGoodPlace() {
	// Object[] cells = graph.getDescendants(graph.getSelectionCells());
	// for (int i=0; i<cells.length;i++)
	// if(cells[i] instanceof JmtCell)
	// putCellInGoodPlace((JmtCell) cells[i]);
	// }

	// /**
	// * This function will put given cell in place avoiding overlapping with
	// other cells
	// * in graph window
	// * <br>
	// * Author: Bertoli Marco
	// * @param cell Identifier of the cell to be moved
	// */
	// // VECCHIO ALGORITMO
	// public void putCellInGoodPlace(JmtCell cell) {
	// Rectangle bounds =
	// GraphConstants.getBounds(cell.getAttributes()).getBounds();
	// // Avoids negative starting point
	// if (bounds.getX() < 0)
	// bounds.setLocation(0, (int)bounds.getY());
	// if (bounds.getY() < 0)
	// bounds.setLocation((int)bounds.getX(), 0);
	//
	// Object[] overlapping = graph.getDescendants(graph.getRoots(bounds));
	// Point2D zero = new Point(0,0);
	// while (overlapping.length > 0) {
	// // Moves bounds until it doesn't overlap with anything
	// Point2D last = (Point2D) zero.clone();
	// for (int j=0; j<overlapping.length; j++) {
	// // Puts last to last corner of overlapping cells
	// if (overlapping[j] instanceof JmtCell && overlapping[j] != cell) {
	// Rectangle2D b =
	// GraphConstants.getBounds(((JmtCell)overlapping[j]).getAttributes());
	// // Consider only rectangles that intersects with given bound
	// if (b.intersects(bounds)) {
	// if (b.getMaxX() > last.getX())
	// last.setLocation(b.getMaxX(), last.getY());
	// if (b.getMaxY() > last.getY())
	// last.setLocation(last.getX(), b.getMaxY());
	// }
	// }
	// }
	// // if last is still zero, only Blocking section were found overlapping
	// // so leave everyting as before
	// if (last.equals(zero))
	// break;
	// // Rounds last and moves bounds to found point
	// bounds.setLocation(new
	// Point((int)(last.getX()+.5),(int)(last.getY()+.5)));
	// overlapping = graph.getDescendants(graph.getRoots(bounds));
	// }
	//
	// // Puts this cell in found position
	// GraphConstants.setBounds(cell.getAttributes(), bounds);
	// }

	/**
	 * This function will put selected cells in place avoiding overlapping with other cells
	 * in graph window
	 * <br>
	 * Author: Bertoli Marco
	 * Heavely modified by Giuseppe De Cicco & Fabio Granara
	 *
	 */
	public void putSelectedCellsInGoodPlace(Object[] cells, Integer[] X, Integer[] Y) {

		for (int i = 0; i < cells.length; i++) {

			if (cells[i] instanceof JmtCell) {

				putCellInGoodPlace((JmtCell) cells[i], X[i].intValue(), Y[i].intValue(), true);

			}
			if (cells[i] instanceof BlockingRegion) {
				Object[] tmp = new Object[1];
				tmp[0] = cells[i];
				Object[] children = graph.getDescendants(tmp);
				for (Object element : children) {
					if (element instanceof JmtCell) {

						putCellInGoodPlace((JmtCell) element, -1, -1, false);

					}
				}

				// putBlockingRegionInGoodPlace((BlockingRegion) cells[i]);
			}

		}
		graph.getGraphLayoutCache().reload();
		sp = 0;
	}

	/**
	 * This function will put given cell in place avoiding overlapping with other cells
	 * in graph window
	 * <br>
	 * Author: Bertoli Marco
	 * Heavely modified by Giuseppe De Cicco & Fabio Granara
	 * @param cell Identifier of the cell to be moved
	 */

	int resetOverLapping = 0;
	int sp = 0;

	public void putCellInGoodPlace(JmtCell cell, int x, int y, boolean flag) {
		// questo metodo viene chiamato solamente quando la classe viene
		// selezionata
		// e si ripete per il numero di classi selezionate, non risulta quindi
		// essere
		// pesante.
		// System.out.println("Valore di Sp "+sp);
		if (sp > 9) {
			sp = 0;
		}
		int oldPointX = 0;
		int oldPointY = 0;
		boolean inGroup = false;
		// Il flag �stato creato per capire sapere se e' una block regione e
		// quindi utilizzare
		// il vecchio metodo oppure se e' una cella quindi flag=true allora uso
		// il nuovo
		// System.out.println("------------PUT CELL IN GOOD PLACE
		// ----------------------");

		if (flag) {
			Rectangle bounds = GraphConstants.getBounds(cell.getAttributes()).getBounds();
			Rectangle bounds2 = new Rectangle((int) bounds.getX() - 20, (int) bounds.getY(), (int) bounds.getWidth() + 38, (int) bounds.getHeight());

			oldPointX = x;
			oldPointY = y;
			// ---------inzio
			// Object[] cells=(graph).getDescendants(graph.getRoots());
			// for(int j=0;j<cells.length;j++){
			// if((cells[j] instanceof JmtCell) &&(cells[j]!=cell)){
			// Rectangle
			// boundcell=GraphConstants.getBounds(((JmtCell)cells[j]).getAttributes()).getBounds();
			// if(boundcell.intersects(bounds2)){
			// System.out.println("true");
			// }
			// }
			// }

			// ---------------fine
			// check if a cell isInGroup
			if (isInGroup(cell)) {

				inGroup = true;
			}

			// Avoids negative starting point
			if (bounds.getX() < 20) {
				bounds.setLocation(20, (int) bounds.getY());
			}
			if (bounds.getY() < 0) {
				bounds.setLocation((int) bounds.getX(), 0);
			}

			// Qua ho le celle e archi che intersecano la mia cella
			// selezionata..bounds (molto efficente dal punto di vista della
			// pesantezza)
			Object[] overlapping = graph.getDescendants(graph.getRoots(bounds2));

			Point2D zero = new Point(20, 0);
			resetOverLapping = 0;
			while (overlapping.length > 0) {

				// Moves bounds until it doesn't overlap with anything
				Point2D last = (Point2D) zero.clone();
				for (int j = 0; j < overlapping.length; j++) {

					resetOverLapping++;
					// resetOverLapping is inserted for an anormall behavior of
					// Tool
					// in fact, if you disable this variable you can see that
					// the tools
					// stop and "for cycle" will be repeated infinite times
					if (resetOverLapping > 50) {
						bounds.setLocation(new Point(oldPointX, oldPointY));
						GraphConstants.setBounds(cell.getAttributes(), bounds);
						resetOverLapping = 0;
						return;
					}
					// System.out.println("---flag in for---");
					// if(overlapping[j] instanceof JmtEdge
					// &&((JmtEdge)overlapping[j]).intersects((EdgeView)(graph.getGraphLayoutCache()).getMapping(overlapping[j],
					// false),
					// GraphConstants.getBounds(((JmtCell)cell).getAttributes())))
					// System.out.println("Intersect �TRUE");

					// Puts last to last corner of overlapping cells
					if (overlapping[j] instanceof JmtCell && overlapping[j] != cell && inGroup) {
						Rectangle2D b2 = GraphConstants.getBounds(((JmtCell) overlapping[j]).getAttributes());
						if (b2.intersects(bounds)) {
							if (b2.getMaxX() > last.getX()) {
								last.setLocation(b2.getMaxX(), last.getY());
							}
							if (b2.getMaxY() > last.getY()) {
								last.setLocation(last.getX(), b2.getMaxY());
							}
						}
						last.setLocation(new Point((int) (last.getX() + .5), (int) (last.getY() + .5)));
					}

					int numberOfChild = cell.getChildCount();

					if (!inGroup && overlapping[j] instanceof JmtCell && overlapping[j] != cell) {

						Rectangle2D b = GraphConstants.getBounds(((JmtCell) overlapping[j]).getAttributes());
						// Consider only rectangles that intersects with given
						// bound
						if (b.intersects(bounds2)) {
							last.setLocation(new Point(oldPointX, oldPointY));

						}
					}
					// inizio a controllare se l intersezione e' un lato
					if (overlapping[j] instanceof JmtEdge
							&& overlapping[j] != cell
							&& !isInGroup(overlapping[j])
							&& ((JmtEdge) overlapping[j]).intersects((EdgeView) (graph.getGraphLayoutCache()).getMapping(overlapping[j], false),
									GraphConstants.getBounds(cell.getAttributes())) && ((JmtEdge) overlapping[j]).getSource() != cell.getChildAt(0)) {
						boolean access = false;
						boolean access2 = false;

						// Nonostatne SourceCell e SinkCell estendano JMTCell
						// avevo problemi di nullPointerException per questo
						// verifico di che
						// tipo sono

						if (cell instanceof SourceCell
								&& ((JmtEdge) overlapping[j]).intersects((EdgeView) (graph.getGraphLayoutCache()).getMapping(overlapping[j], false),
										GraphConstants.getBounds(((SourceCell) cell).getAttributes()))) {
							if (((JmtEdge) overlapping[j]).getSource() != cell.getChildAt(0)) {
								// _______INIZIO_____
								ArrayList<Point2D> intersectionPoints = ((JmtEdge) overlapping[j]).getIntersectionVertexPoint();
								Point2D tmp = (intersectionPoints.get(0));
								Rectangle2D cellBound = GraphConstants.getBounds(((SourceCell) cell).getAttributes());
								double vertexMaxX = (int) cellBound.getMaxX();
								double vertexMaxY = (int) cellBound.getMaxY();
								double vertexHeight = (int) cellBound.getHeight();
								double vertexWidth = (int) cellBound.getWidth();
								boolean upperSideIntersaction = ((JmtEdge) overlapping[j]).getUpperSideIntersaction();
								boolean lowerSideIntersaction = ((JmtEdge) overlapping[j]).getLowerSideIntersaction();
								boolean leftSideIntersaction = ((JmtEdge) overlapping[j]).getLeftSideIntersaction();
								boolean rightSideIntersaction = ((JmtEdge) overlapping[j]).getRightSideIntersaction();
								if (upperSideIntersaction && lowerSideIntersaction) {

									int valoreIntermedio = ((int) vertexMaxX - (int) (vertexWidth / 2));
									if ((int) tmp.getX() < valoreIntermedio) {

										Point newPosition = (this.overlapping).findFreePosition(((JmtEdge) overlapping[j]), cell, cellBound, tmp,
												false, false, true, false);
										bounds.setLocation(newPosition);
									} else {

										Point newPosition = (this.overlapping).findFreePosition(((JmtEdge) overlapping[j]), cell, cellBound, tmp,
												false, false, false, true);
										bounds.setLocation(newPosition);
									}
								} else if (leftSideIntersaction && rightSideIntersaction) {

									int valoreIntermedio = ((int) vertexMaxY - (int) (vertexHeight / 2));
									if ((int) tmp.getY() < valoreIntermedio) {
										Point newPosition = (this.overlapping).findFreePosition(((JmtEdge) overlapping[j]), cell, cellBound, tmp,
												false, true, false, false);
										Point newPosition2 = new Point(newPosition.x, newPosition.y + sp);
										bounds.setLocation(newPosition2);
										sp = sp + 2;
										// cellBounds.setLocation(newPosition);
										// GraphConstants.setBounds(((SinkCell)cell).getAttributes(),
										// cellBounds);
									} else {
										Point newPosition = (this.overlapping).findFreePosition(((JmtEdge) overlapping[j]), cell, cellBound, tmp,
												true, false, false, false);

										bounds.setLocation(newPosition);
									}
								} else if (upperSideIntersaction && rightSideIntersaction) {

									Point newPosition = (this.overlapping).findFreePosition(((JmtEdge) overlapping[j]), cell, cellBound, tmp, false,
											false, false, true);
									bounds.setLocation(newPosition);

								} else if (upperSideIntersaction && leftSideIntersaction) {

									Point newPosition = (this.overlapping).findFreePosition(((JmtEdge) overlapping[j]), cell, cellBound, tmp, false,
											false, true, false);
									bounds.setLocation(newPosition);
								} else if (lowerSideIntersaction && rightSideIntersaction) {

									Point2D tmp1 = (intersectionPoints.get(1));
									Point newPosition = (this.overlapping).findFreePosition(((JmtEdge) overlapping[j]), cell, cellBound, tmp1, false,
											false, false, true);
									bounds.setLocation(newPosition);
								} else if (lowerSideIntersaction && leftSideIntersaction) {

									Point newPosition = (this.overlapping).findFreePosition(((JmtEdge) overlapping[j]), cell, cellBound, tmp, false,
											false, true, false);
									bounds.setLocation(newPosition);
								}
								access = true;
							}

						}
						if (cell instanceof SinkCell) {

							if (((JmtEdge) overlapping[j]).getTarget() != cell.getChildAt(0)) {

								if (((JmtEdge) overlapping[j]).intersects((EdgeView) (graph.getGraphLayoutCache()).getMapping(overlapping[j], false),
										GraphConstants.getBounds(((SinkCell) cell).getAttributes()))) {

									ArrayList<Point2D> intersectionPoints = ((JmtEdge) overlapping[j]).getIntersectionVertexPoint();
									Point2D tmp = (intersectionPoints.get(0));
									Rectangle2D cellBound = GraphConstants.getBounds(((SinkCell) cell).getAttributes());
									double vertexMaxX = (int) cellBound.getMaxX();
									double vertexMaxY = (int) cellBound.getMaxY();
									double vertexHeight = (int) cellBound.getHeight();
									double vertexWidth = (int) cellBound.getWidth();
									boolean upperSideIntersaction = ((JmtEdge) overlapping[j]).getUpperSideIntersaction();
									boolean lowerSideIntersaction = ((JmtEdge) overlapping[j]).getLowerSideIntersaction();
									boolean leftSideIntersaction = ((JmtEdge) overlapping[j]).getLeftSideIntersaction();
									boolean rightSideIntersaction = ((JmtEdge) overlapping[j]).getRightSideIntersaction();
									if (upperSideIntersaction && lowerSideIntersaction) {

										int valoreIntermedio = ((int) vertexMaxX - (int) (vertexWidth / 2));
										if ((int) tmp.getX() < valoreIntermedio) {
											Point newPosition = (this.overlapping).findFreePosition(((JmtEdge) overlapping[j]), cell, cellBound, tmp,
													false, false, true, false);
											bounds.setLocation(newPosition);
										} else {

											Point newPosition = (this.overlapping).findFreePosition(((JmtEdge) overlapping[j]), cell, cellBound, tmp,
													false, false, false, true);
											bounds.setLocation(newPosition);
										}
									} else if (leftSideIntersaction && rightSideIntersaction) {

										int valoreIntermedio = ((int) vertexMaxY - (int) (vertexHeight / 2));
										if ((int) tmp.getY() < valoreIntermedio) {
											Point newPosition = (this.overlapping).findFreePosition(((JmtEdge) overlapping[j]), cell, cellBound, tmp,
													false, true, false, false);
											Point newPosition2 = new Point(newPosition.x, newPosition.y + sp);
											bounds.setLocation(newPosition2);
											sp = sp + 3;
											// bounds.setLocation(newPosition);
										} else {
											Point newPosition = (this.overlapping).findFreePosition(((JmtEdge) overlapping[j]), cell, cellBound, tmp,
													true, false, false, false);

											bounds.setLocation(newPosition);
										}
									} else if (upperSideIntersaction && rightSideIntersaction) {

										Point newPosition = (this.overlapping).findFreePosition(((JmtEdge) overlapping[j]), cell, cellBound, tmp,
												false, false, false, true);
										bounds.setLocation(newPosition);

									} else if (upperSideIntersaction && leftSideIntersaction) {

										Point newPosition = (this.overlapping).findFreePosition(((JmtEdge) overlapping[j]), cell, cellBound, tmp,
												false, false, true, false);
										bounds.setLocation(newPosition);
									} else if (lowerSideIntersaction && rightSideIntersaction) {

										Point2D tmp1 = (intersectionPoints.get(1));
										Point newPosition = (this.overlapping).findFreePosition(((JmtEdge) overlapping[j]), cell, cellBound, tmp1,
												false, false, false, true);
										bounds.setLocation(newPosition);
									} else if (lowerSideIntersaction && leftSideIntersaction) {

										Point newPosition = (this.overlapping).findFreePosition(((JmtEdge) overlapping[j]), cell, cellBound, tmp,
												false, false, true, false);
										bounds.setLocation(newPosition);
										// GraphConstants.setBounds(((SinkCell)cell).getAttributes(),
										// cellBounds);
									}
									access2 = true;
								}
							}

						}
						if (!isInGroup(overlapping[j]) && !access && !access2 && overlapping[j] instanceof JmtEdge) {
							if ((numberOfChild == 2)
									&& ((JmtEdge) overlapping[j]).getSource() != cell.getChildAt(0)
									&& ((JmtEdge) overlapping[j]).getSource() != cell.getChildAt(1)
									&& ((JmtEdge) overlapping[j]).getTarget() != cell.getChildAt(0)
									&& ((JmtEdge) overlapping[j]).getTarget() != cell.getChildAt(1)
									&& ((JmtEdge) overlapping[j]).intersects((EdgeView) (graph.getGraphLayoutCache()).getMapping(overlapping[j],
											false), GraphConstants.getBounds(cell.getAttributes()))) {

								access = access2 = false;

								ArrayList<Point2D> intersectionPoints = ((JmtEdge) overlapping[j]).getIntersectionVertexPoint();
								if ((intersectionPoints == null) || intersectionPoints.size() <= 0) {
									bounds.setLocation(new Point(oldPointX, oldPointY));
									GraphConstants.setBounds(cell.getAttributes(), bounds);
									resetOverLapping = 0;
									return;
								} else {
									Point2D tmp = (intersectionPoints.get(0));

									Rectangle2D cellBound = GraphConstants.getBounds(cell.getAttributes());
									double vertexMaxX = (int) cellBound.getMaxX();
									double vertexMaxY = (int) cellBound.getMaxY();
									double vertexHeight = (int) cellBound.getHeight();
									double vertexWidth = (int) cellBound.getWidth();
									boolean upperSideIntersaction = ((JmtEdge) overlapping[j]).getUpperSideIntersaction();
									boolean lowerSideIntersaction = ((JmtEdge) overlapping[j]).getLowerSideIntersaction();
									boolean leftSideIntersaction = ((JmtEdge) overlapping[j]).getLeftSideIntersaction();
									boolean rightSideIntersaction = ((JmtEdge) overlapping[j]).getRightSideIntersaction();
									if (upperSideIntersaction && lowerSideIntersaction) {

										int valoreIntermedio = ((int) vertexMaxX - (int) (vertexWidth / 2));
										if ((int) tmp.getX() < valoreIntermedio) {

											Point newPosition = (this.overlapping).findFreePosition(((JmtEdge) overlapping[j]), cell, cellBound, tmp,
													false, false, true, false);
											bounds.setLocation(newPosition);
										} else {

											;
											Point newPosition = (this.overlapping).findFreePosition(((JmtEdge) overlapping[j]), cell, cellBound, tmp,
													false, false, false, true);
											bounds.setLocation(newPosition);
										}
									} else if (leftSideIntersaction && rightSideIntersaction) {

										int valoreIntermedio = ((int) vertexMaxY - (int) (vertexHeight / 2));
										if ((int) tmp.getY() < valoreIntermedio) {

											Point newPosition = (this.overlapping).findFreePosition(((JmtEdge) overlapping[j]), cell, cellBound, tmp,
													false, true, false, false);
											Point newPosition2 = new Point(newPosition.x, newPosition.y + sp);
											bounds.setLocation(newPosition2);
											sp = sp + 3;
											// bounds.setLocation(newPosition);
											// GraphConstants.setBounds(((SinkCell)cell).getAttributes(),
											// cellBounds);
										} else {
											Point newPosition = (this.overlapping).findFreePosition(((JmtEdge) overlapping[j]), cell, cellBound, tmp,
													true, false, false, false);
											bounds.setLocation(newPosition);
										}
									} else if (upperSideIntersaction && rightSideIntersaction) {

										Point newPosition = (this.overlapping).findFreePosition(((JmtEdge) overlapping[j]), cell, cellBound, tmp,
												false, false, false, true);
										bounds.setLocation(newPosition);
									} else if (upperSideIntersaction && leftSideIntersaction) {

										Point newPosition = (this.overlapping).findFreePosition(((JmtEdge) overlapping[j]), cell, cellBound, tmp,
												false, false, true, false);
										bounds.setLocation(newPosition);
									} else if (lowerSideIntersaction && rightSideIntersaction) {

										Point2D tmp1 = (intersectionPoints.get(1));
										Point newPosition = (this.overlapping).findFreePosition(((JmtEdge) overlapping[j]), cell, cellBound, tmp1,
												false, false, false, true);
										bounds.setLocation(newPosition);
									} else if (lowerSideIntersaction && leftSideIntersaction) {

										Point newPosition = (this.overlapping).findFreePosition(((JmtEdge) overlapping[j]), cell, cellBound, tmp,
												false, false, true, false);
										bounds.setLocation(newPosition);
									}
								}
							}
						}
					} // end if of edge
				}

				if (last.equals(zero)) {
					break;
				}

				bounds.setLocation(new Point((int) (last.getX()), (int) (last.getY())));

				overlapping = graph.getDescendants(graph.getRoots(bounds));
			}

			// Puts this cell in found position
			GraphConstants.setBounds(cell.getAttributes(), bounds);

		} else {

			Rectangle bounds = GraphConstants.getBounds(cell.getAttributes()).getBounds();
			if (isInGroup(cell)) {
				inGroup = true;
			}

			// Avoids negative starting point
			if (bounds.getX() < 20) {
				bounds.setLocation(20, (int) bounds.getY());
			}
			if (bounds.getY() < 0) {
				bounds.setLocation((int) bounds.getX(), 0);
			}
			Object[] overlapping = graph.getDescendants(graph.getRoots(bounds));

			if (overlapping == null) {

				return;
			}

			Point2D zero = new Point(20, 0);
			while (overlapping.length > 0) {
				Point2D last = (Point2D) zero.clone();
				for (Object element : overlapping) {
					if (element instanceof JmtCell && element != cell && inGroup) {
						Rectangle2D b2 = GraphConstants.getBounds(((JmtCell) element).getAttributes());
						if (b2.intersects(bounds)) {
							if (b2.getMaxX() > last.getX()) {
								last.setLocation(b2.getMaxX(), last.getY());
							}
							if (b2.getMaxY() > last.getY()) {
								last.setLocation(last.getX(), b2.getMaxY());
							}

							last.setLocation(new Point((int) (last.getX() + .5), (int) (last.getY() + .5)));
						}

					}
					if (!inGroup && element instanceof JmtCell && element != cell) {
						last.setLocation(new Point((int) (last.getX() + .5), (int) (last.getY() + .5)));
					}
					int numberOfChild = cell.getChildCount();
					if (isInGroup(element) && element instanceof JmtEdge) {
						if ((numberOfChild == 2)
								&& ((JmtEdge) element).getSource() != cell.getChildAt(0)
								&& ((JmtEdge) element).getSource() != cell.getChildAt(1)
								&& ((JmtEdge) element).getTarget() != cell.getChildAt(0)
								&& ((JmtEdge) element).getTarget() != cell.getChildAt(1)
								&& ((JmtEdge) element).intersects((EdgeView) (graph.getGraphLayoutCache()).getMapping(element, false), GraphConstants
										.getBounds(cell.getAttributes()))) {
						}
						Rectangle2D b2 = GraphConstants.getBounds(((JmtEdge) element).getAttributes());
						if (b2.intersects(bounds)) {
							// ___
						}

						last.setLocation(new Point((int) (last.getX() + .5), (int) (last.getY() + .5)));
					}

				}
				if (last.equals(zero)) {
					break;
				}
				bounds.setLocation(new Point((int) (last.getX()), (int) (last.getY())));
				overlapping = graph.getDescendants(graph.getRoots(bounds));
			}
			GraphConstants.setBounds(cell.getAttributes(), bounds);

		}

	}

	/**
	 * Retrives the location of the given cell.
	 * @param cell The given cell
	 * @return The cell location
	 */
	public Point2D getCellCoordinates(JmtCell cell) {
		Rectangle2D bounds = GraphConstants.getBounds(cell.getAttributes());
		return new Point2D.Double(bounds.getMinX(), bounds.getMinY());
	}

	/**
	 * Checks whether the given cell overlaps an existing cell with its bounds.
	 * @param p The point where the given cell will be inserted.
	 * @param cell The given cell.
	 * @return <code>true</code> - whether there's an overlapping situation.
	 */
	public boolean overlapCells(Point2D p, JmtCell cell) {
		Map attributes;

		// Creates a rectangle representing the new cell bounds and position
		Dimension cellsize = cell.getSize(graph);
		Rectangle r = new Rectangle2D.Double(p.getX(), p.getY(), cellsize.getWidth(), cellsize.getHeight()).getBounds();

		// Gets all cells that can overlap with given one
		Object[] cells = graph.getDescendants(graph.getRoots(r));
		for (Object cell2 : cells) {
			// Gets the i-th cell
			Object c = cell2;
			if (c instanceof JmtCell) {
				if (!c.equals(cell)) {
					// Retrives the i-th cell attributes
					attributes = ((JmtCell) c).getAttributes();
					// Is there an intersection ?
					if (GraphConstants.getBounds(attributes).intersects(r)) {
						// Yes
						return true;
					}
				}
			}
		}
		return false;
	}

	/**
	 * Checks whether the given region overlaps an existing cell with its bounds.
	 * This method is used to control overlapping before inserting new cell into the
	 * Jgraph.
	 * @param p The point where the given cell will be inserted.
	 * @param d The dimensions of cell to be inserted
	 * @return <code>true</code> - whether there's an overlapping situation.
	 *
	 * Author: Bertoli Marco
	 *
	 * Heavily Modified by Giuseppe De Cicco & Fabio Granara
	 */
	public boolean overlapCells(Point p, Dimension d) {
		Rectangle r = new Rectangle(p, d);

		boolean overlapCells = false;
		Object[] cells = graph.getRoots(r);
		for (Object cell : cells) {
			if (cell instanceof JmtEdge) {
				if (((JmtEdge) cell).intersects((EdgeView) (graph.getGraphLayoutCache()).getMapping(cell, false), r)) {
					overlapCells = true;
				}
			}
			if (cell instanceof JmtCell || cell instanceof BlockingRegion || cell instanceof SourceCell || cell instanceof SinkCell) {
				overlapCells = true;
			}

		}
		return overlapCells;

	}

	/**
	 * Generates the xml to send to simulation engine.
	 *
	 * Author: Bertoli Marco
	 *
	 * Modified by Francesco D'Aquino
	 * Modified by Michael Fercu (Logger,2008,0.7.4)
	 */
	public void startSimulation() {
		// if simulation is not in pause state
		if (!stopSimulation.isEnabled()) {
			// Asks for confirmation before overwriting previous simulation data
			if (model.containsSimulationResults()) {
				// Find frame to show confirm dialog
				Component parent = mainWindow;
				if (resultsWindow != null && resultsWindow.isFocused()) {
					parent = resultsWindow;
				}

				int resultValue = JOptionPane.showConfirmDialog(parent, "This operation will overwrite old simulation results." + "Continue anyway?",
						"JMT - Warning", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);
				if (resultValue == JOptionPane.NO_OPTION) {
					return;
				}
			}
			// Asks for confirmation if a logger-file exists (and has existing data) [MF08 0.7.4 (Marco Bertoli)]
			String[] ln = model.getLoggerNameList();
			String ln2 = "";
			if (ln != null) {
				if (model.getLoggingGlbParameter("autoAppend")
						.equalsIgnoreCase(new Integer(jmt.engine.log.LoggerParameters.LOGGER_AR_ASK).toString())) {
					if (ln.length > 0) {
						// Cache the absolute log-path
						String logabspath;
						if (model.getLoggingGlbParameter("path").equalsIgnoreCase("") || (model.getLoggingGlbParameter("path").equalsIgnoreCase("."))) {
							logabspath = new File("").getAbsolutePath() + File.separator;
						} else {
							logabspath = new File(model.getLoggingGlbParameter("path")).getAbsolutePath() + File.separator;
						}

						// Find if the logfiles have data in them:
						try {
							//Code to remove duplicate file names from the list to obtain a unique list of File names.
							Arrays.sort(ln);
							int k = 0;
							for (int i = 0; i < ln.length; i++){
								if (i > 0 && ln[i].equals(ln[i -1])){
									continue;
								}
								ln[k++] = ln[i];
							}
							String[] unique = new String[k];
							System.arraycopy(ln, 0, unique, 0, k);

							for (String element : unique) {
								// if the files have data, print what will be overwritten
								if (new File(logabspath + element).length() > 0) {
										ln2 = ln2 + element + ", ";
								}
							}
							// remove the trailing comma
							if (ln2 != "") {
								ln2 = ln2.substring(0, ln2.length() - 2);
							}
						} catch (Exception e) {
							e.printStackTrace();
						}

						if (ln2 != "") {
							// Find frame to show dialog
							Component parent = mainWindow;
							if (resultsWindow != null && resultsWindow.isFocused()) {
								parent = resultsWindow;
							}

							int resultValue = JOptionPane.showConfirmDialog(parent, "This operation will modify the following logfile(s): " + ln2
									+ ".  " + "Continue anyway?", "JMT - Warning", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);
							if (resultValue == JOptionPane.NO_OPTION) {
								return;
							}
						}
					} else {
						//System.out.println("Empty file");
					}
				}
			} // end confirmation if file exists
			// Correct eventual problems on preloading for closed classes
			model.manageJobs();
			mc = new ModelChecker(model, model, model, model, false);
			pw = new JModelProblemsWindow(mainWindow, mc, this);
			if (!mc.isEverythingOkNormal()) {
				pw.show();
			}
			if (mc.isEverythingOkNormal() || ((!mc.isEverythingOkNormal()) && (pw.continued()))) {
				if (!model.isParametricAnalysisEnabled()) {
					try {
						// Removes previous ResultsWindow
						if (resultsWindow != null) {
							resultsWindow.dispose();
							showResults.setEnabled(false);
						}
						File temp = File.createTempFile("~JModelSimulation", ".xml");
						temp.deleteOnExit();
						XMLWriter.writeXML(temp, model);
						// Creates results data structure
						model.setSimulationResults(new ResultsModel(model.getPollingInterval()));
						showResults.setEnabled(true);
						dispatcher = new DispatcherThread(this, model, (ResultsModel) model.getSimulationResults());
						dispatcher.startSimulation(temp);
					} catch (Exception e) {
						handleException(e);
					}
				} else {
					// Removes previous ResultsWindow
					showResults.setEnabled(false);
					if (resultsWindow != null) {
						resultsWindow.dispose();
					}
					if (progressWindow == null) {
						progressWindow = new PAProgressWindow(mainWindow, simulate, pauseSimulation, stopSimulation, model
								.getParametricAnalysisModel());
					}
					batchThread = new PADispatcherThread(this, model, progressWindow);
					changeSimActionsState(false, true, true);
					progressWindow.initialize(model.getParametricAnalysisModel().getNumberOfSteps());
					progressWindow.start();
					progressWindow.show();
					batchThread.start();
				}
			}
		} else {
			if (!model.isParametricAnalysisEnabled()) {
				dispatcher.restartSimulation();
			} else {
				batchThread.restartSimulation();
			}
		}
	}

	/**
	 * Stops current simulation, aborting all measures
	 *
	 * Author: Bertoli Marco
	 */
	public void stopSimulation() {
		if (stopSimulation.isEnabled()) {
			if (!model.isParametricAnalysisEnabled()) {
				dispatcher.stopSimulation();
			} else {
				batchThread.stopSimulation();
			}
		}
	}

	/**
	 * Pauses current simulation
	 *
	 * Author: Bertoli Marco
	 *
	 * Modified by Francesco D'Aquino
	 */
	public void pauseSimulation() {
		if (model.isParametricAnalysisEnabled()) {
			batchThread.pauseSimulation();
		} else {
			dispatcher.pauseSimulation();
		}
	}

	/**
	 * Changes simulation action status. This method is called by DispatcherThread.
	 * @param start state for start action
	 * @param pause state for pause action
	 * @param stop state for stop action
	 */
	public void changeSimActionsState(boolean start, boolean pause, boolean stop) {
		simulate.setEnabled(start);
		stopSimulation.setEnabled(stop);
		pauseSimulation.setEnabled(pause);
	}

	// ///////////////////////////////////////////
	// METHODS THAT MANAGE MEASURES

	/**
	 * Launches the <code>Measure</code> editor.
	 * Author: Bertoli Marco
	 */
	public void editMeasures() {
		dialogFactory.getDialog(new MeasurePanel(model, model, model), "Define performance indices");
	}

	/**
	 * @return A refernce to the action <code>EditMeasures</code>.
	 */
	public AbstractJmodelAction getEditMeasures() {
		return editMeasures;
	}

	// ///////////////////////////////////////////

	/**
	 * Checks if there's an old graph to save. This methods is called when creates/closes/opens a graph.
	 * @param msg The message to display.
	 * @return <code>true</code> - whether the user accepts to save the graph, or he cancels the current action.
	 */
	public boolean checkForSave(String msg) {
		// Checks if there's an old graph to save
		if (model != null && model.toBeSaved()) {
			int resultValue = JOptionPane.showConfirmDialog(mainWindow, msg, "JMODEL - Warning", JOptionPane.YES_NO_CANCEL_OPTION,
					JOptionPane.WARNING_MESSAGE);
			if (resultValue == JOptionPane.YES_OPTION) {
				saveModel();
				return true;
			}
			if (resultValue == JOptionPane.CANCEL_OPTION) {
				return true;
			}
		}
		return false;
	}

	/**
	 * @return A refernce to the action <code>SwitchToExactSolver</code>.
	 */
	public AbstractJmodelAction getSwitchToWizard() {
		return switchToExactSolver;
	}

	// 13/10/03 - end /////////////////////////////////////////////////////

	public Cursor getOldCursor() {
		return oldCursor;
	}

	public void setOldCursor(Cursor oldCursor) {
		this.oldCursor = oldCursor;
	}

	public void setCursor(Cursor cursor) {
		oldCursor = this.cursor;
		this.cursor = cursor;
		setGraphCursor(cursor);
	}

	// --- Bertoli Marco ---------------------
	/**
	 * Sends an exit signal to main window
	 */
	public void exit() {
		// Send a closing signat to main window.
		mainWindow.dispatchEvent(new WindowEvent(mainWindow, WindowEvent.WINDOW_CLOSING));
	}

	/**
	 * Shows a DefaultEditor to edit Defaults parameters
	 */
	public void showDefaultsEditor() {
		DefaultsEditor.getInstance(mainWindow, DefaultsEditor.JMODEL).show();
	}

	/**
	 * Used to reset mouseListener to default (to avoid File_Save / File_New
	 * operations while in inserting mode)
	 */
	public void resetMouseState() {
		mouseListner.setDefaultState();
		componentBar.clearButtonGroupSelection(0);
		if (graph != null) {
			graph.clearSelection();
			setGraphCursor(Cursor.getDefaultCursor());
		}
	}

	/**
	 * Returns true iff specified cell is editable. This is used by <code>SelectState</code>
	 * to check if editor has to be showed upon double click event.
	 * @param cell specified cell
	 * @return true iff cell is editable
	 */
	public boolean isCellEditable(Object cell) {
		return cell instanceof JmtCell || cell instanceof BlockingRegion;
	}

	/**
	 * Shows a panel with catched exception
	 * @param e exception to be shown
	 */
	public void handleException(Exception e) {
		e.printStackTrace();
		showErrorMessage(e.getMessage());
	}

	/**
	 * Shows a panel with an error message
	 * @param message specified error message
	 */
	public synchronized void showErrorMessage(String message) {
		Component parent = mainWindow;
		if (resultsWindow != null && resultsWindow.hasFocus()) {
			parent = resultsWindow;
		}
		JOptionPane.showMessageDialog(parent, message, "Error", JOptionPane.ERROR_MESSAGE);
	}

	/**
	 * Switch current model to JMVA exact solver
	 */
	public void toJMVA() {
		mc = new ModelChecker(model, model, model, model, true);
		pw = new JModelProblemsWindow(mainWindow, mc, this);
		if (!mc.isEverythingOkToJMVA()) {
			pw.show();
		}
		if (mc.isEverythingOkToJMVA() || ((!mc.isEverythingOkToJMVA()) && (pw.continued()))) {
			if (checkForSave("<html>Save changes before switching?</html>")) {
				return;
			}
			// try {
			// New Converter by Bertoli Marco
			ExactModel output = new ExactModel();
			List res = ModelConverter.convertJSIMtoJMVA(model, output);
			ExactWizard jmva = new ExactWizard(output);
			// If problems are found, shows warnings
			if (res.size() > 0) {
				new WarningWindow(res, jmva, CommonConstants.JSIM, CommonConstants.JMVA).show();
			}

			/*
			 * Old code to use XSLT transformer (really bugged and unfinished)
			 * 
			 * File xmlTempFile = File.createTempFile("~SIMtoMVA", ".xml");
			 * xmlTempFile.deleteOnExit(); File destFile =
			 * File.createTempFile("~MVA", ".xml"); destFile.deleteOnExit();
			 * InputStream stream =
			 * XSDSchemaLoader.loadSchemaAsStream(XSDSchemaLoader.JSIM_TO_JMVA);
			 * if(stream==null){ System.out.println("stream is null"); return; }
			 * XMLWriter.writeXML(xmlTempFile, model); InputStream is = new
			 * BufferedInputStream(stream); Transformer transformer =
			 * TransformerFactory.newInstance().newTransformer(new
			 * StreamSource(is)); StreamSource ssrc = new
			 * StreamSource(xmlTempFile); StreamResult srst = new
			 * StreamResult(destFile); transformer.transform(ssrc, srst);
			 * xmlTempFile.delete(); ExactModel xm = new ExactModel();
			 * xm.loadDocument(new XMLUtils().loadXML(destFile)); new
			 * ExactWizard(xm); }catch (Exception e) { handleException(e); }
			 */
		}
	}

	/**
	 * Called when EditSimParams action is triggered
	 */
	public void editSimulationParameters() {
		dialogFactory.getDialog(new SimulationPanel(model, model, model, this), "Define Simulation Parameters");
	}

	/**
	 * Called when EditPAParams action is triggered
	 */
	public void editPAParameters() {
		dialogFactory.getDialog(new ParametricAnalysisPanel(model, model, model, this), "Define What-if analysis parameters");
	}

	/**
	 * Sets resultWindow to be shown. This method is used by pollerThread
	 * @param rsw window to be set as current ResultsWindow
	 */
	public void setResultsWindow(JFrame rsw) {
		this.resultsWindow = rsw;
		if (rsw instanceof ResultsWindow) {
			// Sets action for toolbar buttons
			((ResultsWindow) rsw).addButtonActions(simulate, pauseSimulation, stopSimulation);
		} else {
			showResults.setEnabled(true);
		}
		// Adds a listener that will unselect Show results button upon results
		// window closing
		rsw.addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				showResults.setSelected(false);
			}
		});
	}

	/**
	 * Called when showResults action is triggered
	 * @param selected Tells if show results button is selected or not
	 *
	 * Modified by Francesco D'Aquino
	 */
	public synchronized void showResultsWindow(boolean selected) {
		if (selected) {
			if (resultsWindow != null) {
				resultsWindow.show();
			}
		} else {
			if (resultsWindow != null) {
				resultsWindow.hide();
			}
		}
	}

	/**
	 * Shows results window and forces show results button to be selected
	 */
	public void showResultsWindow() {
		showResults.setSelected(true);
		showResultsWindow(true);
	}

	/**
	 * Returns current ResultsWindow
	 * @return current ResultsWindow or null if none was created
	 */
	public JFrame getResultsWindow() {
		return resultsWindow;
	}

	/**
	 * Returns current PAProgressWindow
	 * @return current PAProgressWindow or null if none was created
	 */
	public PAProgressWindow getPAProgressWindow() {
		return progressWindow;
	}

	/**
	 * Shows about window
	 */
	public void about() {
		AboutDialogFactory.showJMODEL(mainWindow);
	}

	/**
	 * Tells if something is selected into graph window
	 * @return true if something is selected
	 */
	public boolean isSomethingSelected() {
		return graph.getSelectionCell() != null;
	}

	/**
	 * Takes a screenshot of current jgraph. Shows a dialog to select image type and name
	 */
	public void takeScreenShot() {
		graph.clearSelection();
		graph.showScreenShotDialog();
	}

	// --- end Bertoli Marco ---------------------

	// --------------------------------- Francesco D'Aquino
	// -----------------------
	/**
	 *  Shows the panel to solve a problem
	 */
	public void showRelatedPanel(int problemType, int problemSubType, Object relatedStation, Object relatedClass) {
		// if it is a no class error show the class panel
		if ((problemSubType == ModelChecker.NO_CLASSES_ERROR) && (problemType == ModelChecker.ERROR_PROBLEM)) {
			dialogFactory.getDialog(new jmodelClassesPanel(model, model), "Manage User Classes");
			model.manageJobs(); // a close class may be added
		}
		// if it is a no station error show an error message dialog
		else if ((problemSubType == ModelChecker.NO_STATION_ERROR) && (problemType == ModelChecker.ERROR_PROBLEM)) {
			JOptionPane.showMessageDialog(null, "Please insert at least one server or delay before starting simulation.", "Error",
					JOptionPane.ERROR_MESSAGE);
		} else if ((problemSubType == ModelChecker.SIMULATION_ERROR) && (problemType == ModelChecker.ERROR_PROBLEM)) {
			dialogFactory.getDialog(new MeasurePanel(model, model, model), "Edit Performance Indices");
		}
		// if a measure is inconsistent (i.e have one or more 'null' field) show
		// performance indices panel
		else if ((problemSubType == ModelChecker.INCONSISTENT_MEASURE_ERROR) && (problemType == ModelChecker.ERROR_PROBLEM)) {
			dialogFactory.getDialog(new MeasurePanel(model, model, model), "Edit Performance Indices");
		}
		
		else if ((problemSubType == ModelChecker.SINK_PERF_IND_WITH_NO_SINK_ERROR) && (problemType == ModelChecker.ERROR_PROBLEM)) {
			dialogFactory.getDialog(new MeasurePanel(model, model, model), "Edit Performance Indices");
		}
		else if ((problemSubType == ModelChecker.SINK_PERF_WITH_CLOSED_CLASS_ERROR) && (problemType == ModelChecker.ERROR_PROBLEM)) {
			dialogFactory.getDialog(new MeasurePanel(model, model, model), "Edit Performance Indices");
		}
		// if a measure was defined more than once ask to erase all redundant
		// measure
		else if ((problemSubType == ModelChecker.DUPLICATE_MEASURE_ERROR) && (problemType == ModelChecker.ERROR_PROBLEM)) {
			int k = JOptionPane.showConfirmDialog(null, "Delete all redundant performance indices?\n", "Redundant performance indices found",
					JOptionPane.ERROR_MESSAGE);
			if (k == 0) {
				mc.deleteRedundantMeasure();
			}
		}
		// if it is a reference station error show the class panel
		else if ((problemSubType == ModelChecker.REFERENCE_STATION_ERROR) && (problemType == ModelChecker.ERROR_PROBLEM)) {
			dialogFactory.getDialog(new jmodelClassesPanel(model, model), "Manage User Classes");
			model.manageJobs(); // a close class may be added
		}
		// if a source has been inserted in the model but no open classes
		// defined show the class panel
		else if ((problemSubType == ModelChecker.SOURCE_WITH_NO_OPEN_CLASSES_ERROR) && (problemType == ModelChecker.ERROR_PROBLEM)) {
			dialogFactory.getDialog(new jmodelClassesPanel(model, model), "Manage User Classes");
		} else if ((problemSubType == ModelChecker.ROUTING_ERROR) && (problemType == ModelChecker.ERROR_PROBLEM)) {
			StationParameterPanel tempPanel = new StationParameterPanel(model, model, relatedStation);
			String stationName = model.getStationName(relatedStation);
			// set the station parameter panel to show the routing section
			tempPanel.showRoutingSectionPanel(relatedClass);
			dialogFactory.getDialog(tempPanel, "Editing " + stationName + " Properties...");
		}
		// if a class may be routed into a station whose forward stations are
		// all sink show an error message
		else if ((problemSubType == ModelChecker.ALL_FORWARD_STATION_ARE_SINK_ERROR) && (problemType == ModelChecker.ERROR_PROBLEM)) {
			String stationName = model.getStationName(relatedStation);
			String className = model.getClassName(relatedClass);
			JOptionPane.showMessageDialog(null, "Close class " + className + " may be routed into " + stationName
					+ " whose forward station are all sink.", "Error", JOptionPane.ERROR_MESSAGE);
		}
		// if no open classes defined but at least a sink has been defined show
		// the class panel
		else if ((problemSubType == ModelChecker.SINK_BUT_NO_OPEN_CLASSES_ERROR) && (problemType == ModelChecker.ERROR_PROBLEM)) {
			dialogFactory.getDialog(new jmodelClassesPanel(model, model), "Manage User Classes");
			// JOptionPane.showConfirmDialog(null,"Add an open class to the
			// model?",
			// "Error",JOptionPane.OK_CANCEL_OPTION,JOptionPane.ERROR_MESSAGE);
		}
		// if an open class defined but no sink have been defined show an error
		// message
		else if ((problemSubType == ModelChecker.NO_SINK_WITH_OPEN_CLASSES_ERROR) && (problemType == ModelChecker.ERROR_PROBLEM)) {
			JOptionPane.showMessageDialog(null, "Open classes defined but no sink, add a sink to the model.", "Error", JOptionPane.ERROR_MESSAGE);
		}
		// if an open class defined but no source show an error message
		else if ((problemSubType == ModelChecker.OPEN_CLASS_BUT_NO_SOURCE_ERROR) && (problemType == ModelChecker.ERROR_PROBLEM)) {
			JOptionPane.showMessageDialog(null, "Open classes defined but no source, add a source to the model.", "Error", JOptionPane.ERROR_MESSAGE);
		}
		// if there is a station link error show an error message
		else if ((problemSubType == ModelChecker.STATION_LINK_ERROR) && (problemType == ModelChecker.ERROR_PROBLEM)) {
			String stationName = model.getStationName(relatedStation);
			JOptionPane.showMessageDialog(null, "The station " + stationName + " is not forward linked, add a forward link", "Error",
					JOptionPane.ERROR_MESSAGE);
		} else if ((problemType == ModelChecker.ERROR_PROBLEM) && (problemSubType == ModelChecker.JOIN_WITHOUT_FORK_ERROR)) {
			JOptionPane.showMessageDialog(null, "One or more join found but no fork. Please, remove all join or add a fork");
		}
		// if it is a reference station error show the class panel
		/*
		 * else if ((problemSubType ==
		 * ModelChecker.OPEN_CLASS_REFERENCE_STATION_ERROR) && (problemType ==
		 * ModelChecker.ERROR_PROBLEM)) { DialogFactory.getDialog(new
		 * jmodelClassesPanel(model,model),"Manage User Classes");
		 * model.manageJobs(); //a close class may be added }
		 */
		// used only in JMVA conversion
		else if ((problemSubType == ModelChecker.BCMP_DIFFERENT_QUEUEING_STRATEGIES_WARNING) && (problemType == ModelChecker.WARNING_PROBLEM)) {
			String name = this.getStationDefinition().getStationName(relatedStation);
			int k = JOptionPane
					.showConfirmDialog(
							null,
							"According to BCMP theorem hypothesis each station must have the same queue\nstrategy for each class, but different per class queue strategy were found at "
									+ name + ".\nDo you want to edit " + name + " queue strategy?\n\n", "Mixed queue strategy found",
							JOptionPane.WARNING_MESSAGE);
			if (k == 0) {
				StationParameterPanel tempPanel = new StationParameterPanel(model, model, relatedStation);
				// set the station parameter panel to show the queue section
				tempPanel.showQueueSectionPanel();
				dialogFactory.getDialog(tempPanel, "Editing " + name + " Properties...");
			}
		}
		// used only in JMVA conversion
		else if ((problemSubType == ModelChecker.BCMP_FCFS_DIFFERENT_SERVICE_TYPES_WARNING) && (problemType == ModelChecker.WARNING_PROBLEM)) {
			String name = this.getStationDefinition().getStationName(relatedStation);
			int k = JOptionPane.showConfirmDialog(null,
					"According to BCMP theorem hypothesis, a FCFS server must have the same service times for each class,\nbut at " + name
							+ " the service strategy is mixed, i.e. both load dependent and independent were found.\nDo you want to edit " + name
							+ " service parameters?\n\n", "Mixed service strategies found", JOptionPane.WARNING_MESSAGE);
			if (k == 0) {
				StationParameterPanel tempPanel = new StationParameterPanel(model, model, relatedStation);
				// set the station parameter panel to show the queue section
				tempPanel.showServiceSectionPanel();
				dialogFactory.getDialog(tempPanel, "Editing " + name + " Properties...");
			}
		}
		// used only in JMVA conversion
		else if ((problemSubType == ModelChecker.BCMP_FCFS_EXPONENTIAL_WARNING) && (problemType == ModelChecker.WARNING_PROBLEM)) {
			String name = this.getStationDefinition().getStationName(relatedStation);
			int k = JOptionPane.showConfirmDialog(null,
					"According to BCMP theorem hypothesis, in a FCFS server all the service time distribution\nmust be exponential, but at " + name
							+ " at least one non exponential distribution was found.\nDo you want to edit " + name + " service parameters?\n\n",
					"Non exponential distribution in FCFS server", JOptionPane.WARNING_MESSAGE);
			if (k == 0) {
				StationParameterPanel tempPanel = new StationParameterPanel(model, model, relatedStation);
				// set the station parameter panel to show the queue section
				tempPanel.showServiceSectionPanel();
				dialogFactory.getDialog(tempPanel, "Editing " + name + " Properties...");
			}
		}
		// used only in JMVA conversion
		else if ((problemSubType == ModelChecker.BCMP_FCFS_DIFFERENT_SERVICE_TIMES_WARNING) && (problemType == ModelChecker.WARNING_PROBLEM)) {
			String name = this.getStationDefinition().getStationName(relatedStation);
			int k = JOptionPane
					.showConfirmDialog(
							null,
							"According to BCMP theorem hypothesis, in a FCFS server all the per class service time mean values\nmust be the same. If the service strategies are load dependent the mean value in each range\nhas to be the same for each class.\nDo you want to edit "
									+ name + " service parameters?\n\n", "Non exponential distribution in FCFS server", JOptionPane.WARNING_MESSAGE);
			if (k == 0) {
				StationParameterPanel tempPanel = new StationParameterPanel(model, model, relatedStation);
				// set the station parameter panel to show the queue section
				tempPanel.showServiceSectionPanel();
				dialogFactory.getDialog(tempPanel, "Editing " + name + " Properties...");
			}
		}
		// used only for to JMVA conversion, for non Random Routing routing
		// strategy errors
		else if ((problemType == ModelChecker.WARNING_PROBLEM) && (problemSubType == ModelChecker.BCMP_NON_STATE_INDEPENDENT_ROUTING_WARNING)) {
			int k = JOptionPane
					.showConfirmDialog(
							null,
							"Convert all non state independent routing strategies to Random Routing?\n\nAccording to the BCMP theorem the routing probabilities must be independent from the state of the model.\nChoosing ok all non state independent routing strategies inside a station will be converted to Random Routing.\nDo you want to convert all non state independent routing strategies to Random Routing?\n\n",
							"BCMP hypothesis not verified", JOptionPane.ERROR_MESSAGE);
			if (k == 0) {
				mc.setAllStateDependentRoutingStrategyToRandomRouting();
			}
		}
		// if there are more than one sink show a warning message
		else if ((problemSubType == ModelChecker.MORE_THAN_ONE_SINK_WARNING) && (problemType == ModelChecker.WARNING_PROBLEM)) {
			JOptionPane
					.showMessageDialog(
							null,
							"If more than one sink is reacheable by the same open class the computed throughput may not be accurate.\nPlease check the model before starting simulation.",
							"Warning", JOptionPane.WARNING_MESSAGE);
		}
		// if a station (server or delay) is not backward connected show a
		// warning message
		else if ((problemSubType == ModelChecker.NO_BACKWARD_LINK_WARNING) && (problemType == ModelChecker.WARNING_PROBLEM)) {
			String stationName = model.getStationName(relatedStation);
			JOptionPane.showMessageDialog(null, "The station " + stationName
					+ " is not backward linked. Please check the model before starting simulation.", "Warning", JOptionPane.WARNING_MESSAGE);
		} else if ((problemSubType == ModelChecker.PARAMETRIC_ANALYSIS_MODEL_MODIFIED_WARNING) && (problemType == ModelChecker.WARNING_PROBLEM)) {
			String message = "Check parametric analysis model?\n\nThe parametric analysis model previously defined had become inconsistent with the \nsimulation model. It will be automatically modified when simulation will be started.\nDo you want to autocorrect and check parametric analysis panel?\n\n";
			int k = JOptionPane.showConfirmDialog(null, message, "Inconsistent parametric analysis model", JOptionPane.WARNING_MESSAGE);
			if (k == 0) {
				model.getParametricAnalysisModel().checkCorrectness(true);
				ParametricAnalysisPanel paPanel = new ParametricAnalysisPanel(model, model, model, this);
				dialogFactory.getDialog(paPanel, "Edit what-if analysis parameters");
			}
		} else if ((problemSubType == ModelChecker.PARAMETRIC_ANALYSIS_NO_MORE_AVAIBLE_WARNING) && (problemType == ModelChecker.WARNING_PROBLEM)) {
			String message = "Parametric analysis was set, but no parametric analysis is now avaible,\nsince the simulation model was changed. It is only possible to execute normal simulation.\nDo you wish to continue anyway?\n\n";
			int k = JOptionPane.showConfirmDialog(null, message, "Parametric analysis not avaible", JOptionPane.WARNING_MESSAGE);
			if (k == 0) {
				model.setParametricAnalysisEnabled(false);
				model.setParametricAnalysisModel(null);
			}
		} else if ((problemSubType == ModelChecker.FORK_WITHOUT_JOIN_WARNING) && (problemType == ModelChecker.WARNING_PROBLEM)) {
			JOptionPane.showMessageDialog(null, "A fork was found but no join. Please check the topology");
		} else if ((problemSubType == ModelChecker.EMPTY_BLOCKING_REGION)) {
			int k = JOptionPane.showConfirmDialog(null, "Delete empty finite capacity regions?\n", "Empty finite capacity regions found",
					JOptionPane.ERROR_MESSAGE);
			if (k == 0) {
				mc.deleteEmptyBlockingRegions();
			}
		} else if (problemSubType == ModelChecker.PRELOADING_WITH_BLOCKING) {
			editSimulationParameters();
		}

	}

	/**
	 * Shows the class panel
	 */
	public void showClassPanel() {
		dialogFactory.getDialog(new jmodelClassesPanel(model, model), "Manage User Classes");
	}

	/**
	 * Used to discover if the instance can display simulation animation
	 *
	 * @return true if the instance can display simulation animation
	 */
	public boolean isAnimationDisplayable() {
		return true;
	}

	/**
	 * Gets the Dimension of a specified cell
	 * @param cell
	 * @return the cell Dimension
	 */
	public Rectangle2D getCellDimension(JmtCell cell) {
		return GraphConstants.getBounds(cell.getAttributes());
	}

	public void showHelp() {
		JHelp helpViewer = null;
		try {
			// Get the classloader of this class.
			ClassLoader cl = this.getClass().getClassLoader();
			// Use the findHelpSet method of HelpSet to create a URL referencing
			// the helpset file.
			URL url = HelpSet.findHelpSet(cl, "help/jMODEL_en/JSIMgraph.hs");
			// Create a new JHelp object with a new HelpSet.
			helpViewer = new JHelp(new HelpSet(cl, url));

			// Set the initial entry point in the table of contents.
			// helpViewer.setCurrentID("");
		} catch (Exception e) {
			e.printStackTrace();
			JOptionPane.showMessageDialog(mainWindow, "Sorry, jSIMgraph help is not available", "Help not found", JOptionPane.ERROR_MESSAGE);
			return;
		}

		// Create a new frame.
		JMTFrame frame = new JMTFrame();
		// Set it's size.
		frame.centerWindow(800, 600);
		// Add the created helpViewer to it.
		frame.getContentPane().add(helpViewer);
		// Make the frame visible.
		frame.setVisible(true);
	}

	// --- Methods to handle blocking regions - Bertoli Marco
	// -------------------------------------
	/**
	 * Adds given JmtCells to a freshly created blocking station. This method will not modify
	 * data structure and is used during load operation
	 */
	public void addCellsToBlockingRegion(Object[] cells, Object regionKey) {
		BlockingRegion bl = new BlockingRegion(this, regionKey);
		bl.addStations(cells);
	}

	/**
	 * Adds a new Blocking region that contains selected cells,
	 * if this doesn't overlap with existing one
	 */
	public void addSelectionToNewBlockingRegion() {
		Object[] cells = graph.getSelectionCells();
		// Data structure to hold all selected stations and their search's key
		HashMap<Object, Object> stations = new HashMap<Object, Object>();
		boolean canBeAdded = true;
		Object regionKey = model.addBlockingRegion();
		for (Object cell : cells) {
			if (cell instanceof JmtCell) {
				Object stationKey = ((CellComponent) ((JmtCell) cell).getUserObject()).getKey();
				if (!model.canRegionStationBeAdded(regionKey, stationKey)) {
					canBeAdded = false;
					break;
				} else {
					stations.put(cell, stationKey);
				}
			} else if (cell instanceof BlockingRegion) {
				// A blocking region cannot overlap another one
				canBeAdded = false;
				break;
			}
		}
		// If blocking region can be added, adds it to graph window, otherwise
		// deletes it
		if (canBeAdded && stations.size() > 0) {
			BlockingRegion bl = new BlockingRegion(this, regionKey);
			Object[] stationCells = stations.keySet().toArray();
			bl.addStations(stationCells);
			// Adds stations to blocking region into data structure
			for (Object stationCell : stationCells) {
				model.addRegionStation(regionKey, stations.get(stationCell));
			}
		} else {
			model.deleteBlockingRegion(regionKey);
		}
	}

	/**
	 * This method is used to reflect drag in and out a blocking region on data
	 * structure and to move dragged cells to background to use transparency of
	 * blocking region over them
	 */
	public void handlesBlockingRegionDrag() {
		Object[] cells = graph.getDescendants(graph.getSelectionCells());
		// Put cells not in a blocking region to back
		HashSet<Object> putBack = new HashSet<Object>();
		for (Object cell2 : cells) {
			if (cell2 instanceof JmtCell && ((JmtCell) cell2).parentChanged()) {
				// This cell was moved in, out or between blocking regions
				JmtCell cell = (JmtCell) cell2;
				Object key = ((CellComponent) cell.getUserObject()).getKey();
				Object oldRegionKey, newRegionKey;
				if (!(cell.getParent() instanceof BlockingRegion)) {
					// Object removed from blocking region
					putBack.add(cell2);
					oldRegionKey = ((BlockingRegion) cell.getPrevParent()).getKey();
					model.removeRegionStation(oldRegionKey, key);
					// If region is empty, removes region too
					if (model.getBlockingRegionStations(oldRegionKey).size() == 0) {
						model.deleteBlockingRegion(oldRegionKey);
					}
					// Allow adding of removed objects to a new blocking region
					enableAddBlockingRegion(true);
				} else if (cell.getPrevParent() instanceof BlockingRegion) {
					// Object changed blocking region
					oldRegionKey = ((BlockingRegion) cell.getPrevParent()).getKey();
					model.removeRegionStation(oldRegionKey, key);
					// If region is empty, removes region too
					if (model.getBlockingRegionStations(oldRegionKey).size() == 0) {
						model.deleteBlockingRegion(oldRegionKey);
					}
					newRegionKey = ((BlockingRegion) cell.getParent()).getKey();
					model.addRegionStation(newRegionKey, key);
				} else {
					// Object added to a blocking region
					newRegionKey = ((BlockingRegion) cell.getParent()).getKey();
					if (!model.addRegionStation(newRegionKey, key)) {
						// object cannot be added to blocking region (for
						// example it's a source)
						cell.removeFromParent();
						graph.getModel().insert(new Object[] { cell }, null, null, null, null);
						putBack.add(cell);
					}
					// Doesn't allow adding of selected objects to a new
					// blocking region
					enableAddBlockingRegion(false);
				}
				// Resets parent for this cell
				cell.resetParent();
			}
			// Avoid insertion of a blocking region in an other
			else if (cell2 instanceof BlockingRegion) {
				BlockingRegion region = (BlockingRegion) cell2;
				if (region.getParent() != null) {
					region.removeFromParent();
					graph.getModel().insert(new Object[] { region }, null, null, null, null);
				}
			}
		}
		// Puts cells removed from blocking regiont on background
		graph.getModel().toBack(putBack.toArray());
	}

	// --------------------------------------------------------------------------------------------
	// ___________________GIUSEPPE DE CICCO & FABIO
	// GRANARA____________________________

	/**
	 * Method that rotate components
	 *
	 * author Giuseppe De Cicco & Fabio Granara
	 */
	public void rotateComponent(Object[] cells) {
		if (cells == null) {
			cells = graph.getSelectionCells();
		}

		for (Object cell : cells) {

			if ((cell instanceof BlockingRegion) || cell instanceof JmtEdge) {
				continue;
			}

			JmtCell current = (JmtCell) cell;

			Map<Object, Map> nested = new Hashtable<Object, Map>();
			Map attributeMap = new Hashtable();
			ImageIcon icon;
			if (current.isLeftInputCell()) {
				icon = JMTImageLoader.loadImage(current.getIcon(), ImageLoader.MODIFIER_MIRROR);
			} else {
				icon = JMTImageLoader.loadImage(current.getIcon());
			}
			GraphConstants.setIcon(attributeMap, icon);

			nested.put(cell, attributeMap);
			current.setLeftInputCell(!current.isLeftInputCell());
			current.updatePortPositions(nested, icon, current.getSize(graph));
			// _____DA INSERIRE QUI L AGGIORNAMENTO DELLA DECORAZIONE DELLA
			// FRECCIA______
			graph.getGraphLayoutCache().edit(nested);
		}
		avoidOverlappingCell(cells);
	}

	// Giuseppe De Cicco and Fabio Granara
	public AbstractJmodelAction getRotate() {

		return actionRotate;
	}

	// Giuseppe De Cicco & Fabio Granara
	public boolean isInGroup(Object cell) {
		Object[] celgru = null;
		Object[] celless = null;

		cells = graph.getDescendants(graph.getRoots());
		if (cells.length > 0) {
			for (Object cell2 : cells) {
				if (cell2 instanceof BlockingRegion) {
					celgru = new Object[1];
					celgru[0] = cell2;
					// celle presenti nel blocking region incluse port e regione

					celless = graph.getDescendants(celgru);
					for (Object celles : celless) {
						if (celles.equals(cell)) {
							return true;
						}
					}
				}
			}
		}
		return false;
	}

	// Giuseppe De Cicco & Fabio Granara
	public void avoidOverlappingCell(Object[] cells2) {

		overlapping.avoidOverlappingCell(cells2);
		graphRepaint();
		graph.getGraphLayoutCache().reload();
	}

	// Giuseppe De Cicco & Fabio Granara
	int e = -1;
	int x = 25;
	int y = 90;
	int widthMax = 0;
	int heightMax = 0;
	private boolean flag = false;
	private boolean flag1 = false;
	private boolean flag2 = false;
	private boolean inRepositionSons = false;

	public void adjustGraph() {

		Object[] cells;

		// queste variabili mi servono per capire quali sono le celle che hanno
		// meno link in ingresso per iniziare l'algoritmo
		int inMin = 100;
		int inMax = 0;

		List<Object> min = new ArrayList<Object>();
		List<Object> max = new ArrayList<Object>();

		cells = graph.getDescendants(graph.getRoots());
		for (int i = 0; i < cells.length; i++) {
			if (cells[i] instanceof JmtCell) {
				// System.out.println("scandendo cella x cella: " +
				// (JmtCell)cells[i]);
				Rectangle bounds = GraphConstants.getBounds(((JmtCell) cells[i]).getAttributes()).getBounds();
				if (bounds.getWidth() > widthMax) {
					widthMax = (int) bounds.getWidth();
				}
				if (bounds.getHeight() > heightMax) {
					heightMax = (int) bounds.getHeight();
				}

				if (!((JmtCell) cells[i]).isLeftInputCell()) {
					rotateComponent(new Object[] { cells[i] });
				}
			}
		}

		boolean sourceIn = false;
		for (int i = 0; i < cells.length; i++) {
			if (cells[i] instanceof JmtCell) {
				((JmtCell) cells[i]).in = (DefaultGraphModel.getIncomingEdges(graph.getModel(), cells[i])).length;

			}

			if (cells[i] instanceof JmtCell) {

				if (((JmtCell) cells[i]).in < inMin) {
					inMin = ((JmtCell) cells[i]).in;
				}
				if (((JmtCell) cells[i]).in > inMax) {
					inMax = ((JmtCell) cells[i]).in;
				}

			}

		}
		boolean projectClose = true;
		for (Object cell : cells) {
			if (cell instanceof JmtCell && !sourceIn) {
				if (((JmtCell) cell).in == 0) {
					projectClose = false;
				}
			}
		}

		boolean serverWithZeroIn = false;
		for (int i = 0; i < cells.length; i++) {
			if (cells[i] instanceof JmtCell) {
				if (((JmtCell) cells[i]).in == 0) {
					serverWithZeroIn = true;
				}
				if (!min.contains(cells[i])) {
					if (((JmtCell) cells[i]).in == inMin) {
						min.add(cells[i]);
					} else if (((JmtCell) cells[i]).in == inMax) {
						max.add(cells[i]);
					}
				}
			}
		}
		// Questa parte gestisce quando abbiamo un progetto close
		if (!sourceIn && projectClose && !serverWithZeroIn) {

			int tmpMax = 0;
			JmtCell tmpCell = null;
			for (Object cell : cells) {
				if (cell instanceof JmtCell) {
					int tmpIn = (((JmtCell) cell).in);
					if (tmpMax < tmpIn) {
						tmpMax = tmpIn;
						tmpCell = ((JmtCell) cell);
					}
				}
			}
			min = new ArrayList<Object>();
			min.add(tmpCell);

		}

		int widthMaxMin = 0;
		for (int w = 0; w < min.size(); w++) {
			Rectangle bounds = GraphConstants.getBounds(((JmtCell) min.get(w)).getAttributes()).getBounds();
			if (bounds.getWidth() > widthMaxMin) {
				widthMaxMin = (int) bounds.getWidth();
			}
		}

		for (int q = 0; q < min.size(); q++) {
			Rectangle bounds = GraphConstants.getBounds(((JmtCell) min.get(q)).getAttributes()).getBounds();
			x = widthMaxMin / 2 + 25 - (int) (bounds.getWidth() / 2);
			searchNext((JmtCell) min.get(q));

			e += ((JmtCell) min.get(q)).sons;

			// x = 4 + widthMax/2;
			// w=w+10;
			flag1 = false;
			flag2 = false;
		}

		// controllo
		min = new ArrayList<Object>();
		for (int w2 = 0; w2 < cells.length; w2++) {
			if (cells[w2] instanceof JmtCell) {
				if (!((JmtCell) cells[w2]).seen) {

					min.add(cells[w2]);
				}
			}

		}
		flag1 = false;

		for (int q = 0; q < min.size(); q++) {
			searchNext((JmtCell) min.get(q));
			flag1 = false;

		}

		flag1 = false;
		flag2 = false;
		e = -1;
		x = 25;
		y = 90;
		widthMax = 0;
		heightMax = 0;
		for (int z = 0; z < cells.length; z++) {
			if (cells[z] instanceof JmtCell) {
				((JmtCell) cells[z]).sons = 1;
				((JmtCell) cells[z]).seen = false;
			}
		}

		graphRepaint();
		graph.getGraphLayoutCache().reload();
		avoidOverlappingCell(cells);

	}

	// Giuseppe De Cicco & Fabio Granara
	private int searchNext(JmtCell prev) {
		Rectangle boundspadre = GraphConstants.getBounds((prev).getAttributes()).getBounds();
		Object[] listEdges = null;
		GraphModel graphmodel = graph.getModel();
		List<Object> next = new ArrayList<Object>();

		if (flag1 == false && prev.seen == false) {

			// Rectangle bounds =
			// GraphConstants.getBounds(((JmtCell)prev).getAttributes()).getBounds();
			if (!flag2) {
				boundspadre.setLocation(x, y + ((e + 1) * (42 + heightMax)) - (int) (boundspadre.getHeight() / 2) + 30);
			} else {
				boundspadre.setLocation(x - (int) (boundspadre.getWidth() / 2), y + ((e + 1) * (42 + heightMax))
						- (int) (boundspadre.getHeight() / 2) + 30);
			}

			GraphConstants.setBounds(prev.getAttributes(), boundspadre);
			x = (int) boundspadre.getCenterX() + widthMax + 50;
			prev.seen = true;
			flag2 = true;
		}

		// inserisco tutti gli archi uscenti e entranti di min.get(j) in
		// listEdges
		listEdges = DefaultGraphModel.getOutgoingEdges(graphmodel, prev);
		Vector<Object> listEdgestmp = new Vector<Object>();
		for (Object listEdge : listEdges) {
			JmtCell qq = (JmtCell) (graphmodel.getParent(graphmodel.getTarget(listEdge)));
			if (!(qq).seen) {
				listEdgestmp.add(listEdge);
			}
		}
		listEdges = listEdgestmp.toArray();

		int numTarget = listEdges.length;
		if (numTarget == 0) {
			return 1;
		}

		for (int k = 0; k < numTarget; k++) {
			next.add((graphmodel.getParent(graphmodel.getTarget(listEdges[k]))));
		}

		int j = 1;
		if (inRepositionSons == false && ((JmtCell) next.get(0)).seen == false) {
			j = searchNext((JmtCell) next.get(0));
		} else if (inRepositionSons == true && ((JmtCell) next.get(0)).seen == false) {

			Rectangle bounds = GraphConstants.getBounds(((JmtCell) next.get(0)).getAttributes()).getBounds();
			bounds.setLocation((int) (boundspadre.getCenterX()) + widthMax + 50 - (int) (bounds.getWidth() / 2), (int) boundspadre.getCenterY()
					- (int) (bounds.getHeight() / 2));
			GraphConstants.setBounds(((JmtCell) next.get(0)).getAttributes(), bounds);

			((JmtCell) next.get(0)).seen = true;
			j = searchNext((JmtCell) next.get(0));
		}

		if (numTarget > 0) {
			if (!flag) {
				repositionSons(prev, next, j - 1, 1);
			} else {
				repositionSons(prev, next, -1, 0);
			}
			flag = false;
		}

		(prev).sons = 0;
		for (int w = 0; w < numTarget; w++) {
			prev.sons += ((JmtCell) next.get(w)).sons;
		}

		return prev.sons;
	}

	// Giuseppe De Cicco & Fabio Granara
	private void repositionSons(JmtCell padre, List<Object> sons, int numero, int cont) {
		inRepositionSons = true;
		Object[] listEdges = null;
		GraphModel graphmodel = graph.getModel();

		flag1 = true;

		int j = 0;
		Rectangle boundspadre = GraphConstants.getBounds(padre.getAttributes()).getBounds();
		int w = boundspadre.y + ((heightMax + 35) * (numero + 1)) - 38;

		for (int i = cont; i < sons.size(); i++) {

			if (((JmtCell) sons.get(i)).seen == false) {

				Rectangle bounds = GraphConstants.getBounds(((JmtCell) sons.get(i)).getAttributes()).getBounds();
				bounds.setLocation((int) (boundspadre.getCenterX()) + widthMax + 50 - (int) (bounds.getWidth() / 2), w
						- (int) (bounds.getHeight() / 2) + 80);

				GraphConstants.setBounds(((JmtCell) sons.get(i)).getAttributes(), bounds);
				((JmtCell) sons.get(i)).seen = true;

				listEdges = DefaultGraphModel.getOutgoingEdges(graphmodel, sons.get(i));

				if (listEdges.length > 0) {
					flag = true;

					j = searchNext((JmtCell) sons.get(i));
					inRepositionSons = true;
				}

				if (j > 0) {
					j = j - 1;
				}

				listEdges = null;
			}

			w = w + (heightMax + ((heightMax + 15) * j) + 30);
			j = 0;
		}

		inRepositionSons = false;
	}

	public void setIsReleased(boolean state) {
		isReleased = state;
	}

	public boolean getIsReleased() {
		return isReleased;
	}

	// Giuseppe De Cicco & Fabio Granara
	public void zoomIn() {
		//graph.setScale(graph.getScale() * 1.25);
	  
	  //QN-Java
	  if(graph != null) graph.setScale(graph.getScale() * 1.25);
	}

	// Giuseppe De Cicco & Fabio Granara
	public void zoomOut() {
	  //graph.setScale(graph.getScale() / 1.25);
	  
	  //QN-Java
	  if(graph != null) graph.setScale(graph.getScale() / 1.25);
	}
}
