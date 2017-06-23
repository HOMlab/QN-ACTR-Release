/**
  * Copyright (C) 2006, Laboratorio di Valutazione delle Prestazioni - Politecnico di Milano

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

package jmt.gui.jsim;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;
import java.net.URL;
import java.util.List;
import java.util.Locale;

import javax.help.HelpSet;
import javax.help.JHelp;
import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;

import jmt.framework.gui.components.JMTFrame;
import jmt.framework.gui.components.JMTMenuBar;
import jmt.framework.gui.components.JMTToolBar;
import jmt.framework.gui.listeners.AbstractJMTAction;
import jmt.framework.gui.listeners.MenuAction;
import jmt.framework.gui.wizard.Wizard;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.Defaults;
import jmt.gui.common.controller.DispatcherThread;
import jmt.gui.common.controller.ModelChecker;
import jmt.gui.common.controller.PADispatcherThread;
import jmt.gui.common.definitions.CommonModel;
import jmt.gui.common.definitions.GuiInterface;
import jmt.gui.common.definitions.ModelConverter;
import jmt.gui.common.definitions.PAResultsModel;
import jmt.gui.common.definitions.ResultsModel;
import jmt.gui.common.editors.DefaultsEditor;
import jmt.gui.common.panels.AboutDialogFactory;
import jmt.gui.common.panels.MeasurePanel;
import jmt.gui.common.panels.ResultsWindow;
import jmt.gui.common.panels.SimulationPanel;
import jmt.gui.common.panels.WarningWindow;
import jmt.gui.common.panels.parametric.PAProgressWindow;
import jmt.gui.common.panels.parametric.PAResultsWindow;
import jmt.gui.common.panels.parametric.ParametricAnalysisPanel;
import jmt.gui.common.resources.JMTImageLoader;
import jmt.gui.common.xml.ModelLoader;
import jmt.gui.common.xml.XMLWriter;
import jmt.gui.exact.ExactModel;
import jmt.gui.exact.ExactWizard;
import jmt.gui.jsim.definitions.JSIMModel;
import jmt.gui.jsim.panels.AllBlockingRegionsPanel;
import jmt.gui.jsim.panels.AllStationsParametersPanel;
import jmt.gui.jsim.panels.ClassesPanel;
import jmt.gui.jsim.panels.ConnectionsPanel;
import jmt.gui.jsim.panels.JSimProblemsWindow;
import jmt.gui.jsim.panels.RSPLPanel;
import jmt.gui.jsim.panels.StationsPanel;

/**
 * Created by IntelliJ IDEA.
 * User: orsotronIII
 * Date: 18-lug-2005
 * Time: 8.59.44
 * Modified by Bertoli Marco
 *
 * Modified by Francesco D'Aquino
 * Modified by Ashanka (May 2010): 
 * Patch: Multi-Sink Perf. Index 
 * Description: Added new Performance index for the capturing the 
 * 				1. global response time (ResponseTime per Sink)
 *              2. global throughput (Throughput per Sink)
 *              each sink per class.
 * Model validation of the perf indices.
 */
public class JSIMMain extends Wizard implements GuiInterface {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private static final String WINDOW_TITLE = "JSIMwiz - Queue Network Models Simulator";

	private CommonModel model = new JSIMModel();
	private File currentFile = null;
	private JFrame resultsWindow;
	private DispatcherThread dispatcher;

	private ModelLoader modelLoader = new ModelLoader(ModelLoader.JSIM);

	private ClassesPanel classes;
	private StationsPanel stations;
	private ConnectionsPanel connections;
	private AllStationsParametersPanel parameters;
	private MeasurePanel measures;
	private RSPLPanel rspl;
	private SimulationPanel simulation;
	private AllBlockingRegionsPanel blocking;

	//Francesco D'Aquino
	private ModelChecker mc;
	private JSimProblemsWindow pw;
	private PADispatcherThread batchThread;
	private PAProgressWindow progressWindow;
	private ParametricAnalysisPanel parametricAnalysis;
	// end

	private AbstractJMTAction FILE_NEW = new AbstractJMTAction("New") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			setIcon("New", JMTImageLoader.getImageLoader());
			putValue(Action.SHORT_DESCRIPTION, "New Model");
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_N, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_N));
		}

		public void actionPerformed(ActionEvent e) {
			newFile();
		}
	};

	private AbstractJMTAction FILE_OPEN = new AbstractJMTAction("Open") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			setIcon("Open", JMTImageLoader.getImageLoader());
			putValue(Action.SHORT_DESCRIPTION, "Open Saved Model");
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_O, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_O));
		}

		public void actionPerformed(ActionEvent e) {
			openFile();
		}
	};

	private AbstractJMTAction FILE_SAVE = new AbstractJMTAction("Save") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Save this model");
			setIcon("Save", JMTImageLoader.getImageLoader());
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_S, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_S));
		}

		public void actionPerformed(ActionEvent e) {
			saveFile();
		}
	};

	private AbstractJMTAction FILE_SAVEAS = new AbstractJMTAction("Save as") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			setMnemonicKey(KeyEvent.VK_A);
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_S, ActionEvent.CTRL_MASK | ActionEvent.SHIFT_MASK));
			setIcon("Save", JMTImageLoader.getImageLoader());
			putValue(Action.SHORT_DESCRIPTION, "Save this model as ");
		}

		public void actionPerformed(ActionEvent e) {
			saveFileAs();
		}
	};

	private AbstractJMTAction FILE_EXIT = new AbstractJMTAction("Exit") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Exit JSIM");
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_Q, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_X));
		}

		public void actionPerformed(ActionEvent e) {
			close();
		}
	};

	private AbstractJMTAction ACTION_SWITCH_JMVA = new AbstractJMTAction("Export to JMVA...") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Export current model to JMVA...");
			setIcon("toJMVA", JMTImageLoader.getImageLoader());
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_W, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_W));
		}

		public void actionPerformed(ActionEvent e) {
			toJMVA();
		}
	};

	private AbstractJMTAction ACTION_RAND = new AbstractJMTAction("Randomize") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Generate Randomized Model");
			setIcon("Dice", JMTImageLoader.getImageLoader());
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_R));
		}

		public void actionPerformed(ActionEvent e) {
			randomizeModel();
		}
	};

	private AbstractJMTAction SIM_START = new AbstractJMTAction("Start Simulation") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Start Simulation");
			setIcon("Sim", JMTImageLoader.getImageLoader());
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_M, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_M));
		}

		public void actionPerformed(ActionEvent e) {
			startSimulation();
		}
	};

	private AbstractJMTAction SIM_PAUSE = new AbstractJMTAction("Pause") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Pause Simulation");
			setIcon("Pause", JMTImageLoader.getImageLoader());
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_P, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_P));
			//start disabled
			setEnabled(false);
		}

		public void actionPerformed(ActionEvent e) {
			pauseSimulation();
		}
	};

	private AbstractJMTAction SIM_STOP = new AbstractJMTAction("Stop") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Stop Simulation");
			setIcon("Stop", JMTImageLoader.getImageLoader());
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_T, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_T));
			//start disabled
			setEnabled(false);
		}

		public void actionPerformed(ActionEvent e) {
			stopSimulation();
		}
	};

	private AbstractJMTAction OPTIONS_DEFAULTS = new AbstractJMTAction("Default parameters...") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Define default value of model's parameters");
			setIcon("Options", JMTImageLoader.getImageLoader());
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_D, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_D));
		}

		public void actionPerformed(ActionEvent e) {
			showOptions();
		}
	};

	private AbstractJMTAction HELP_SHOWHELP = new AbstractJMTAction("Help") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Show Help");
			setIcon("Help", JMTImageLoader.getImageLoader());
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_H, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_H));
		}

		public void actionPerformed(ActionEvent e) {
			showHelp();
		}
	};

	private AbstractJMTAction HELP_CREDITS = new AbstractJMTAction("About JSIMwiz") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Credits");
		}

		public void actionPerformed(ActionEvent e) {
			showCredits();
		}
	};

	public AbstractJMTAction SHOW_RESULTS = new AbstractJMTAction("Results") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Show simulation results");
			setIcon("Results", JMTImageLoader.getImageLoader());
			setSelectable(true); // A toggle button
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_R, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_R));
			//start disabled
			setEnabled(false);
		}

		public void actionPerformed(ActionEvent e) {
			showResultsWindow(isSelected());
		}
	};

	private void showOptions() {
		JFrame jf = new JFrame("Defaults Editor");
		jf.setIconImage(getIconImage());
		jf.setSize(getSize());
		new DefaultsEditor(jf, DefaultsEditor.JSIM).show();
	}

	/**
	 * Open a stored model from a file
	 * <br>Author: Bertoli Marco
	 */
	private void openFile() {
		if (checkForSave("<html>Save changes before opening a saved model?</html>")) {
			return;
		}
		JSIMModel tmpmodel = new JSIMModel();
		int state = modelLoader.loadModel(tmpmodel, this);
		if (state == ModelLoader.SUCCESS || state == ModelLoader.WARNING) {
			currentFile = modelLoader.getSelectedFile();
			// At this point loading was successful, so substitutes old model with loaded one
			model = tmpmodel;
			// Clears old resultsWindow
			if (resultsWindow != null) {
				resultsWindow.dispose();
			}
			// Clears parametric window too...
			if (progressWindow != null) {
				progressWindow.stopAnimation();
				progressWindow.dispose();
			}
			SHOW_RESULTS.setSelected(false);
			SHOW_RESULTS.setEnabled(false);
			setPanelsData(model);
			setTitle(WINDOW_TITLE + " - " + currentFile.getName());
			// If simulation results are present, adds a Result Window
			if (model.containsSimulationResults()) {
				if (model.isParametricAnalysisEnabled()) {
					this.setResultsWindow(new PAResultsWindow(model.getParametricAnalysisModel(), (PAResultsModel) model.getSimulationResults()));
					SHOW_RESULTS.setEnabled(true);
				} else {
					this.setResultsWindow(new ResultsWindow(model.getSimulationResults()));
					SHOW_RESULTS.setEnabled(true);
				}
			}
			model.resetSaveState();
			System.gc();
		} else if (state == ModelLoader.FAILURE) {
			showErrorMessage(modelLoader.getFailureMotivation());
		}

		// Shows warnings if any
		if (state == ModelLoader.WARNING) {
			new WarningWindow(modelLoader.getLastWarnings(), this, modelLoader.getInputFileFormat(), CommonConstants.JSIM).show();
		}

	}

	private void newFile() {
		if (checkForSave("<html>Save changes before creating a new model?</html>")) {
			return;
		}
		setPanelsData(new JSIMModel());
		currentFile = null;
		SHOW_RESULTS.setSelected(false);
		SHOW_RESULTS.setEnabled(false);
		setTitle(WINDOW_TITLE);
		// Disposes results (if any)
		if (resultsWindow != null) {
			resultsWindow.dispose();
		}
		if (progressWindow != null) {
			progressWindow.stopAnimation();
			progressWindow.dispose();
		}
		model.resetSaveState();
	}

	private void setPanelsData(CommonModel simModel) {
		model = simModel;
		classes.setData(model);
		stations.setData(model, model);
		connections.setData(model);
		parameters.setData(model, model);
		measures.setData(model, model, model);
		rspl.setData(model, model, model);
		simulation.setData(model, model, model);
		parametricAnalysis.setData(model, model, model);
		blocking.setData(model, model, model);
	}

	/**
	 * Saves current model
	 * <br>Author: Bertoli Marco
	 */
	private void saveFile() {
		if (currentFile == null) {
			saveFileAs();
			return;
		}

		int status = modelLoader.saveModel(model, this, currentFile);

		if (status == ModelLoader.FAILURE) {
			showErrorMessage(modelLoader.getFailureMotivation());
		}

		setTitle(WINDOW_TITLE + " - " + currentFile.getName());
		model.resetSaveState();
	}

	/**
	 * Saves current model with a new name
	 * <br>Author: Bertoli Marco
	 */
	private void saveFileAs() {
		int status = modelLoader.saveModel(model, this, null);

		if (status == ModelLoader.FAILURE) {
			showErrorMessage(modelLoader.getFailureMotivation());
		} else if (status == ModelLoader.SUCCESS) {
			currentFile = modelLoader.getSelectedFile();
			setTitle(WINDOW_TITLE + " - " + currentFile.getName());
			model.resetSaveState();
		}
	}

	/**
	 * Exits from JSIM
	 */
	@Override
	public boolean cancel() {
		if (checkForSave("<html>Save changes before closing?</html>")) {
			return false;
		}
		Dimension d = getSize();
		Defaults.set("JSIMWindowWidth", String.valueOf(d.width));
		Defaults.set("JSIMWindowHeight", String.valueOf(d.height));
		Defaults.save();
		// Stops simulation if active
		if (SIM_STOP.isEnabled()) {
			stopSimulation();
		}
		// Disposes resultswindow and this
		if (resultsWindow != null) {
			resultsWindow.dispose();
		}
		if (progressWindow != null) {
			progressWindow.stopAnimation();
			progressWindow.dispose();
		}
		return true;
	}

	private void toJMVA() {
		mc = new ModelChecker(model, model, model, model, true);
		//pw.setToJMVAConversion(true);
		pw.setModelChecker(mc);
		pw.updateProblemsShown(false);
		if (!mc.isEverythingOkToJMVA()) {
			pw.show();
		} else {
			pw.setVisible(false);
			launchToJMVA();
		}
	}

	public void launchToJMVA() {
		// New Converter by Bertoli Marco
		ExactModel output = new ExactModel();
		List res = ModelConverter.convertJSIMtoJMVA(model, output);
		ExactWizard jmva = new ExactWizard(output);
		// If problems are found, shows warnings
		if (res.size() > 0) {
			new WarningWindow(res, jmva, CommonConstants.JSIM, CommonConstants.JMVA).show();
		}
	}

	private void randomizeModel() {
	}

	private void startSimulation() {
		//if simulation is not in pause state
		if (!SIM_STOP.isEnabled()) {
			// Asks for confirmation before overwriting previous simulation data
			if (model.containsSimulationResults()) {
				// Find frame to show confirm dialog
				Component parent = this;
				if (resultsWindow != null && resultsWindow.isFocused()) {
					parent = resultsWindow;
				}

				int resultValue = JOptionPane.showConfirmDialog(parent, "This operation will overwrite old simulation results." + "Continue anyway?",
						"JMT - Warning", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);
				if (resultValue == JOptionPane.NO_OPTION) {
					return;
				}
			}

			// Correct eventual problems on preloading for closed classes
			model.manageJobs();
			// Removes previous ResultsWindow
			if (resultsWindow != null) {
				resultsWindow.dispose();
				SHOW_RESULTS.setEnabled(false);
			}
			mc = new ModelChecker(model, model, model, model, false);
			pw.setModelChecker(mc);
			//pw.setToJMVAConversion(false);
			pw.updateProblemsShown(false);
			if (!mc.isEverythingOkNormal()) {
				WindowListener wl = pw.getWindowListeners()[0];
				pw.show();
			} else {
				pw.setVisible(false);
				launchSimulation();
			}

		} else {
			if (!model.isParametricAnalysisEnabled()) {
				dispatcher.restartSimulation();
			} else {
				batchThread.restartSimulation();
			}
		}
	}

	public void launchSimulation() {
		if (!model.isParametricAnalysisEnabled()) {
			try {
				File temp = File.createTempFile("~JModelSimulation", ".xml");
				temp.deleteOnExit();
				XMLWriter.writeXML(temp, model);
				// Creates results data structure
				model.setSimulationResults(new ResultsModel(model.getPollingInterval()));
				SHOW_RESULTS.setEnabled(true);
				dispatcher = new DispatcherThread(this, model, (ResultsModel) model.getSimulationResults());
				dispatcher.startSimulation(temp);
			} catch (Exception e) {
				handleException(e);
			}
		} else {
			SHOW_RESULTS.setEnabled(false);
			// Removes previous ResultsWindow
			//if (parametricResultsWindow != null) {
			//    parametricResultsWindow.dispose();
			//}
			if (progressWindow == null) {
				progressWindow = new PAProgressWindow(this, SIM_START, SIM_PAUSE, SIM_STOP, model.getParametricAnalysisModel());
			}
			batchThread = new PADispatcherThread(this, model, progressWindow);
			changeSimActionsState(false, true, true);
			progressWindow.initialize(model.getParametricAnalysisModel().getNumberOfSteps());
			progressWindow.start();
			progressWindow.show();
			batchThread.start();
		}
	}

	private void pauseSimulation() {
		if (model.isParametricAnalysisEnabled()) {
			batchThread.pauseSimulation();
		} else {
			dispatcher.pauseSimulation();
		}
	}

	private void stopSimulation() {
		if (!model.isParametricAnalysisEnabled()) {
			dispatcher.stopSimulation();
		} else {
			batchThread.stopSimulation();
		}
	}

	public void changeSimActionsState(boolean start, boolean pause, boolean stop) {
		SIM_START.setEnabled(start);
		SIM_STOP.setEnabled(stop);
		SIM_PAUSE.setEnabled(pause);
	}

	private void showHelp() {
		JHelp helpViewer = null;
		try {
			// Get the classloader of this class.
			ClassLoader cl = this.getClass().getClassLoader();
			// Use the findHelpSet method of HelpSet to create a URL referencing the helpset file.
			URL url = HelpSet.findHelpSet(cl, "help/jSIM_en/jsim_en.hs");
			// Create a new JHelp object with a new HelpSet.
			helpViewer = new JHelp(new HelpSet(cl, url));

			// Set the initial entry point in the table of contents.
			//helpViewer.setCurrentID("");
		} catch (Exception e) {
			e.printStackTrace();
			JOptionPane.showMessageDialog(this, "Sorry, jSIM help is not available", "Help not found", JOptionPane.ERROR_MESSAGE);
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

	/**
	 * Shows JSIM Credits
	 * <br>Author: Bertoli Marco
	 */
	private void showCredits() {
		AboutDialogFactory.showJSIM(this);
	}

	public JSIMMain() {
		this(null);
	}

	public JSIMMain(CommonModel model) {
		super();
		Defaults.reload();
		if (model == null) {
			this.model = new JSIMModel();
		} else {
			this.model = model;
		}
		model = this.model;
		setSize(Defaults.getAsInteger("JSIMWindowWidth").intValue(), Defaults.getAsInteger("JSIMWindowHeight").intValue());
		setTitle(WINDOW_TITLE);
		setIconImage(JMTImageLoader.loadImageAwt("JSIMIcon"));
		this.centerWindow();
		classes = new ClassesPanel(model);
		stations = new StationsPanel(model, model);
		connections = new ConnectionsPanel(model);
		parameters = new AllStationsParametersPanel(model, model);
		measures = new MeasurePanel(model, model, model);
		rspl = new RSPLPanel(model, model, model);
		simulation = new SimulationPanel(model, model, model, this);
		parametricAnalysis = new ParametricAnalysisPanel(model, model, model, this);
		blocking = new AllBlockingRegionsPanel(model, model, model);
		initComponents();
		mc = new ModelChecker(model, model, model, model, false);
		pw = new JSimProblemsWindow(mc, this);
	}

	private void initComponents() {
		setJMenuBar(createMenuBar());
		getContentPane().add(createToolBar(), BorderLayout.NORTH);
		//add panels
		WizardPanel[] panels = new WizardPanel[] { classes, stations, connections, parameters, measures, rspl, blocking, simulation,
				parametricAnalysis };
		for (WizardPanel panel : panels) {
			addPanel(panel);
		}
	}

	private JMTToolBar createToolBar() {
		JMTToolBar toolbar = new JMTToolBar(JMTImageLoader.getImageLoader());
		AbstractJMTAction[] items = new AbstractJMTAction[] { FILE_NEW, FILE_OPEN, FILE_SAVE, null, ACTION_SWITCH_JMVA, null, SIM_START, SIM_PAUSE,
				SIM_STOP, SHOW_RESULTS, null, OPTIONS_DEFAULTS, null, HELP_SHOWHELP };
		toolbar.populateToolbar(items);
		toolbar.setFloatable(false);
		return toolbar;
	}

	private JMTMenuBar createMenuBar() {
		JMTMenuBar menuBar = new JMTMenuBar(JMTImageLoader.getImageLoader());
		AbstractJMTAction[] menus = new AbstractJMTAction[] {
		//File menu
				new MenuAction("File", new AbstractJMTAction[] { FILE_NEW, FILE_OPEN, FILE_SAVE, FILE_SAVEAS, null, FILE_EXIT }),
				//ActionMenu
				new MenuAction("Action", new AbstractJMTAction[] { ACTION_NEXT, ACTION_PREV, null, ACTION_SWITCH_JMVA }),
				//Simulation Menu
				new MenuAction("Simulation", new AbstractJMTAction[] { SIM_START, SIM_PAUSE, SIM_STOP, null, SHOW_RESULTS }),
				//Options menu
				new MenuAction("Define", new AbstractJMTAction[] { OPTIONS_DEFAULTS }),
				//Help Menu
				new MenuAction("Help", new AbstractJMTAction[] { HELP_SHOWHELP, null, HELP_CREDITS }) };
		menuBar.populateMenu(menus);
		return menuBar;
	}

	public CommonModel getModel() {
		return model;
	}

	/**
	 * Sets resultWindow to be shown. This method is used by pollerThread
	 * @param rsw window to be set as current ResultsWindow
	 */
	public void setResultsWindow(JFrame rsw) {
		this.resultsWindow = rsw;
		if (rsw instanceof ResultsWindow) {
			// Sets action for toolbar buttons
			((ResultsWindow) rsw).addButtonActions(SIM_START, SIM_PAUSE, SIM_STOP);
		} else {
			SHOW_RESULTS.setEnabled(true);
		}
		// Adds a listener that will unselect Show results button upon results window closing
		rsw.addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosing(WindowEvent e) {
				SHOW_RESULTS.setSelected(false);
			}
		});
	}

	/**
	 * Called when showResults action is triggered
	 * @param selected Tells if show results button is selected or not
	 */
	public void showResultsWindow(boolean selected) {
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
		SHOW_RESULTS.setSelected(true);
		showResultsWindow(true);
	}

	// ------------------------------ Francesco D'Aquino ---------------------------------------

	/**
	 * Shows the panel to solve a problem
	 */
	public void showRelatedPanel(int problemType, int problemSubType, Object relatedStation, Object relatedClass) {
		// if it's a no class error show the class panel
		if ((problemType == ModelChecker.ERROR_PROBLEM) && (problemSubType == ModelChecker.NO_CLASSES_ERROR)) {
			tabbedPane.setSelectedIndex(0);
		}
		// if it's a no station error show the station panel
		else if ((problemType == ModelChecker.ERROR_PROBLEM) && (problemSubType == ModelChecker.NO_STATION_ERROR)) {
			tabbedPane.setSelectedIndex(1);
		}
		// if it's a link error show the link panel
		else if ((problemType == ModelChecker.ERROR_PROBLEM) && (problemSubType == ModelChecker.STATION_LINK_ERROR)) {
			tabbedPane.setSelectedIndex(2);
		}
		//if it's an allForwardSinkError show the link panel
		else if ((problemType == ModelChecker.ERROR_PROBLEM) && (problemSubType == ModelChecker.ALL_FORWARD_STATION_ARE_SINK_ERROR)) {
			tabbedPane.setSelectedIndex(2);
		}
		// if it's a routing error show the link panel
		else if ((problemType == ModelChecker.ERROR_PROBLEM) && (problemSubType == ModelChecker.ROUTING_ERROR)) {
			AllStationsParametersPanel temp = (AllStationsParametersPanel) tabbedPane.getComponent(3);
			//set temp to show routing section panel
			temp.showStationParameterPanel(relatedStation, relatedClass, 2);
			tabbedPane.setSelectedIndex(3);
		}
		//if no measure have been defined show the measure panel
		else if ((problemType == ModelChecker.ERROR_PROBLEM) && (problemSubType == ModelChecker.SIMULATION_ERROR)) {
			tabbedPane.setSelectedIndex(4);
		}
		//if a measure is inconsistent (i.e have one or more 'null' field) show performance indices panel
		else if ((problemType == ModelChecker.ERROR_PROBLEM) && (problemSubType == ModelChecker.INCONSISTENT_MEASURE_ERROR)) {
			tabbedPane.setSelectedIndex(4);
		}
		//if a measure was defined more than once ask to delete the redundant measure
		else if ((problemSubType == ModelChecker.DUPLICATE_MEASURE_ERROR) && (problemType == ModelChecker.ERROR_PROBLEM)) {
			int k = JOptionPane.showConfirmDialog(null, "Delete all redundant performance indices?\n", "Redundant performance indices found",
					JOptionPane.ERROR_MESSAGE);
			if (k == 0) {
				mc.deleteRedundantMeasure();
			}
		}
		//if it is a reference station error show the reference station panel
		else if ((problemType == ModelChecker.ERROR_PROBLEM) && (problemSubType == ModelChecker.REFERENCE_STATION_ERROR)) {
			tabbedPane.setSelectedIndex(5);
		} else if ((problemType == ModelChecker.ERROR_PROBLEM) && (problemSubType == ModelChecker.JOIN_WITHOUT_FORK_ERROR)) {
			JOptionPane.showMessageDialog(this, "One or more join found but no fork. Please, remove al join or add a fork");
			tabbedPane.setSelectedIndex(1);
		}

		//used only in JMVA conversion
		else if ((problemSubType == ModelChecker.BCMP_DIFFERENT_QUEUEING_STRATEGIES_WARNING) && (problemType == ModelChecker.WARNING_PROBLEM)) {
			String name = getModel().getStationName(relatedStation);
			int k = JOptionPane
					.showConfirmDialog(
							null,
							"According to BCMP theorem hypothesis each station must have the same queue\nstrategy for each class, but different per class queue strategy were found at "
									+ name + ".\nDo you want to edit " + name + " queue strategy?\n\n", "Mixed queue strategy found",
							JOptionPane.WARNING_MESSAGE);
			if (k == 0) {
				AllStationsParametersPanel temp = (AllStationsParametersPanel) tabbedPane.getComponent(3);
				//set the station parameter panel to show the queue section
				temp.showStationParameterPanel(relatedStation, null, 0);
				tabbedPane.setSelectedIndex(3);
			}
		}
		//used only in JMVA conversion
		else if ((problemSubType == ModelChecker.BCMP_FCFS_DIFFERENT_SERVICE_TYPES_WARNING) && (problemType == ModelChecker.WARNING_PROBLEM)) {
			String name = getModel().getStationName(relatedStation);
			int k = JOptionPane.showConfirmDialog(null,
					"According to BCMP theorem hypothesis, a FCFS server must have the same service times for each class,\nbut at " + name
							+ " the service strategy is mixed, i.e. both load dependent and independent were found.\nDo you want to edit " + name
							+ " service parameters?\n\n", "Mixed service strategies found", JOptionPane.WARNING_MESSAGE);
			if (k == 0) {
				AllStationsParametersPanel temp = (AllStationsParametersPanel) tabbedPane.getComponent(3);
				//set the station parameter panel to show the service section
				temp.showStationParameterPanel(relatedStation, null, 1);
				tabbedPane.setSelectedIndex(3);
			}
		}
		//used only in JMVA conversion
		else if ((problemSubType == ModelChecker.BCMP_FCFS_EXPONENTIAL_WARNING) && (problemType == ModelChecker.WARNING_PROBLEM)) {
			String name = getModel().getStationName(relatedStation);
			int k = JOptionPane.showConfirmDialog(null,
					"According to BCMP theorem hypothesis, in a FCFS server all the service time distribution\nmust be exponential, but at " + name
							+ " at least one non exponential distribution was found.\nDo you want to edit " + name + " service parameters?\n\n",
					"Non exponential distribution in FCFS server", JOptionPane.WARNING_MESSAGE);
			if (k == 0) {
				AllStationsParametersPanel temp = (AllStationsParametersPanel) tabbedPane.getComponent(3);
				//set the station parameter panel to show the service section
				temp.showStationParameterPanel(relatedStation, null, 1);
				tabbedPane.setSelectedIndex(3);
			}
		}
		//used only in JMVA conversion
		else if ((problemSubType == ModelChecker.BCMP_FCFS_DIFFERENT_SERVICE_TIMES_WARNING) && (problemType == ModelChecker.WARNING_PROBLEM)) {
			String name = getModel().getStationName(relatedStation);
			int k = JOptionPane
					.showConfirmDialog(
							null,
							"According to BCMP theorem hypothesis, in a FCFS server all the per class service time mean values\nmust be the same. If the service strategies are load dependent the mean value in each range\nhas to be the same for each class.\nDo you want to edit "
									+ name + " service parameters?\n\n", "Non exponential distribution in FCFS server", JOptionPane.WARNING_MESSAGE);
			if (k == 0) {
				AllStationsParametersPanel temp = (AllStationsParametersPanel) tabbedPane.getComponent(3);
				//set the station parameter panel to show the queue section
				temp.showStationParameterPanel(relatedStation, null, 1);
				tabbedPane.setSelectedIndex(3);
			}
		}
		//used only in JMVA conversion
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

		//if it is a no backward link warning show the link panel
		else if ((problemType == ModelChecker.WARNING_PROBLEM) && (problemSubType == ModelChecker.NO_BACKWARD_LINK_WARNING)) {
			tabbedPane.setSelectedIndex(2);
		} else if ((problemType == ModelChecker.WARNING_PROBLEM) && (problemSubType == ModelChecker.PARAMETRIC_ANALYSIS_MODEL_MODIFIED_WARNING)) {
			String message = "Check parametric analysis model?\n\nThe parametric analysis model previously defined had become inconsistent with the \nsimulation model. It will be automatically modified when simulation will be started.\nDo you want to autocorrect and check parametric analysis panel?\n\n";
			int k = JOptionPane.showConfirmDialog(null, message, "Inconsistent parametric analysis model", JOptionPane.WARNING_MESSAGE);
			if (k == 0) {
				model.getParametricAnalysisModel().checkCorrectness(true);
				tabbedPane.setSelectedIndex(8);
				SimulationPanel simPanel = (SimulationPanel) tabbedPane.getComponentAt(6);
				//model.getParametricAnalysisModel().setModified(true);
			}
		} else if ((problemSubType == ModelChecker.PARAMETRIC_ANALYSIS_NO_MORE_AVAIBLE_WARNING) && (problemType == ModelChecker.WARNING_PROBLEM)) {
			String message = "Parametric analysis was set, but no parametric analysis is now avaible,\nsince the simulation model was changed. It is only possible to execute normal simulation.\nDo you wish to continue anyway?\n\n";
			int k = JOptionPane.showConfirmDialog(null, message, "Parametric analysis not avaible", JOptionPane.WARNING_MESSAGE);
			if (k == 0) {
				model.setParametricAnalysisEnabled(false);
				model.setParametricAnalysisModel(null);
			}
		} else if ((problemSubType == ModelChecker.FORK_WITHOUT_JOIN_WARNING) && (problemType == ModelChecker.WARNING_PROBLEM)) {
			JOptionPane.showMessageDialog(this, "A fork was found but no join. Please check the topology");
		} else if ((problemSubType == ModelChecker.EMPTY_BLOCKING_REGION)) {
			int k = JOptionPane.showConfirmDialog(null, "Delete empty finite capacity regions?\n", "Empty finite capacity regions found",
					JOptionPane.ERROR_MESSAGE);
			if (k == 0) {
				mc.deleteEmptyBlockingRegions();
			}
		} else if (problemSubType == ModelChecker.PRELOADING_WITH_BLOCKING) {
			tabbedPane.setSelectedIndex(7);
		}
		else if ((problemType == ModelChecker.ERROR_PROBLEM) && (problemSubType == ModelChecker.SINK_PERF_IND_WITH_NO_SINK_ERROR)) {
			tabbedPane.setSelectedIndex(4);
		}
		else if ((problemType == ModelChecker.ERROR_PROBLEM) && (problemSubType == ModelChecker.SINK_PERF_WITH_CLOSED_CLASS_ERROR)) {
			tabbedPane.setSelectedIndex(4);
		}
	}

	/**
	 * Shows the class panel
	 */
	public void showClassPanel() {
		tabbedPane.setSelectedIndex(0);
	}

	/**
	 * Sets the Parametric Analysis Results Window
	 *
	 * @param parw the Parametric Analysis Results Window to be set
	 */
	/*public void setPAResultsWindow(PAResultsWindow parw) {
	    parametricResultsWindow = parw;
	    parw.addWindowListener(new WindowAdapter() {
	        public void windowClosing(WindowEvent e) {
	            resultsButton.setSelected(false);
	        }
	    });
	}*/

	/**
	 * Sets the availability of results in case of parametric analysis
	 *
	 * @param avaible true if they are avaible
	 */
	/*public void setPAResultsAvaible(boolean avaible) {
	    resultsButton.setEnabled(avaible);
	}*/

	/**
	 * Used to discover if the instance can display simulation animation
	 *
	 * @return true if the instance can display simulation animation
	 */
	public boolean isAnimationDisplayable() {
		return false;
	}

	//-------------------------------------- end Francesco D'Aquino --------------------------------

	/**
	 * Shows a panel with catched exception
	 * @param e exception to be shown
	 */
	public void handleException(Exception e) {
		showErrorMessage(e.getMessage());
	}

	/**
	 * Shows a panel with an error message
	 * @param message specified error message
	 */
	public void showErrorMessage(String message) {
		JOptionPane.showMessageDialog(this, message, "Error", JOptionPane.ERROR_MESSAGE);
	}

	/**
	 * Checks if there's an old graph to save. This methods is called when creates/closes/opens a graph.
	 * @param msg The message to display.
	 * @return <code>true</code> - whether the user accepts to save the graph, or he cancels the current action.
	 */
	public boolean checkForSave(String msg) {
		// Checks if there's an old graph to save
		if (model != null && model.toBeSaved()) {
			int resultValue = JOptionPane.showConfirmDialog(this, msg, "JSIM - Warning", JOptionPane.YES_NO_CANCEL_OPTION,
					JOptionPane.WARNING_MESSAGE);
			if (resultValue == JOptionPane.YES_OPTION) {
				saveFile();
				return true;
			}
			if (resultValue == JOptionPane.CANCEL_OPTION) {
				return true;
			}
		}
		return false;
	}

	public static void main(String[] args) {
		new JSIMMain().setVisible(true);
	}

	/**
	 * Overrides default Solve button pression...
	 * @see Wizard
	 */
	@Override
	protected void finish() {
		SIM_START.actionPerformed(null);
	}

}
