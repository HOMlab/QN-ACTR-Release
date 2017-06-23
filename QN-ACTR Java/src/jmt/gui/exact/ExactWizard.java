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

package jmt.gui.exact;

import java.awt.BorderLayout;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import javax.help.HelpSet;
import javax.help.JHelp;
import javax.swing.AbstractButton;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.KeyStroke;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.WindowConstants;

import jmt.common.exception.InputDataException;
import jmt.common.exception.SolverException;
import jmt.framework.gui.components.JMTFrame;
import jmt.framework.gui.components.JMTMenuBar;
import jmt.framework.gui.components.JMTToolBar;
import jmt.framework.gui.help.HoverHelp;
import jmt.framework.gui.listeners.AbstractJMTAction;
import jmt.framework.gui.listeners.MenuAction;
import jmt.framework.gui.wizard.Wizard;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.definitions.ModelConverter;
import jmt.gui.common.panels.AboutDialogFactory;
import jmt.gui.common.panels.WarningWindow;
import jmt.gui.common.resources.JMTImageLoader;
import jmt.gui.common.xml.ModelLoader;
import jmt.gui.exact.link.SolverClient;
import jmt.gui.exact.panels.ClassesPanel;
import jmt.gui.exact.panels.DescriptionPanel;
import jmt.gui.exact.panels.ForceUpdatablePanel;
import jmt.gui.exact.panels.GraphPanel;
import jmt.gui.exact.panels.IterationSelector;
import jmt.gui.exact.panels.QueueLenPanel;
import jmt.gui.exact.panels.ResTimePanel;
import jmt.gui.exact.panels.ServiceDemandsPanel;
import jmt.gui.exact.panels.ServiceTimesPanel;
import jmt.gui.exact.panels.StationsPanel;
import jmt.gui.exact.panels.SynopsisPanel;
import jmt.gui.exact.panels.SysPowerPanel;
import jmt.gui.exact.panels.ThroughputPanel;
import jmt.gui.exact.panels.UtilizationPanel;
import jmt.gui.exact.panels.VisitsPanel;
import jmt.gui.exact.panels.WhatIfPanel;
import jmt.gui.jsim.JSIMMain;
import jmt.gui.jsim.definitions.JSIMModel;

/**
 * This is the object you use to define your system structure and parameters
 * @author alyf (Andrea Conti)
 * @version Date: 11-set-2003 Time: 14.47.11
 *
 * Modified by Bertoli Marco 01-mar-2006 (added model conversion and solved bug
 * with stored visits)
 *
 */
public class ExactWizard extends Wizard {
	private static final long serialVersionUID = 1L;
	private static final String TITLE = "JMVA - Product form queueing network exact solver";
	private final static boolean DEBUG = false;

	private ExactModel data;
	private JLabel helpLabel;
	private HoverHelp help;
	private SolverClient solver;

	// New Bertoli Marco
	private ModelLoader modelLoader = new ModelLoader(ModelLoader.JMVA);
	// End

	//NEW Dall'Orso
	//A link to the last modified model's temporary file - used to display synopsis
	private File tempFile = null;
	//END

	//keep a reference to these three components to enable switching
	private WizardPanel serviceTimesPanel, serviceDemandsPanel, visitsPanel;

	private AbstractJMTAction FILE_SAVE = new AbstractJMTAction("Save...") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "Save Model");
			setIcon("Save", JMTImageLoader.getImageLoader());
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_S, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_S));
		}

		public void actionPerformed(ActionEvent e) {
			save();
		}
	};

	private AbstractJMTAction FILE_OPEN = new AbstractJMTAction("Open...") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "Open Saved Model");
			setIcon("Open", JMTImageLoader.getImageLoader());
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_O, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_O));
		}

		public void actionPerformed(ActionEvent e) {
			open();
		}
	};

	private AbstractJMTAction FILE_NEW = new AbstractJMTAction("New...") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "Create New Model");
			setIcon("New", JMTImageLoader.getImageLoader());
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_N, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_N));
		}

		public void actionPerformed(ActionEvent e) {
			newModel();
		}
	};

	private AbstractJMTAction FILE_EXIT = new AbstractJMTAction("Exit") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "Exits Application");
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_Q, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_Q));
		}

		public void actionPerformed(ActionEvent e) {
			close();
		}
	};

	private AbstractJMTAction SWITCH_TO_SIMULATOR = new AbstractJMTAction("Import in JSIM...") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "Import current model into JSIMwiz...");
			setIcon("toJSIM", JMTImageLoader.getImageLoader());
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_G, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_G));
		}

		public void actionPerformed(ActionEvent e) {
			switchToSimulator();
		}
	};

	private AbstractJMTAction ACTION_RANDOMIZE_MODEL = new AbstractJMTAction("Randomize") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "Random generation of service demands");
			setIcon("dice", JMTImageLoader.getImageLoader());
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_R, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_R));
		}

		public void actionPerformed(ActionEvent e) {
			randomizeModel();
		}
	};

	private AbstractJMTAction HELP = new AbstractJMTAction("JMVA help") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "Show JMVA help");
			setIcon("Help", JMTImageLoader.getImageLoader());
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_H, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_H));
		}

		public void actionPerformed(ActionEvent e) {
			showHelp(e);

		}
	};

	private AbstractJMTAction ABOUT = new AbstractJMTAction("About JMVA...") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "About JMVA");
			/*
			putValue(Action.SMALL_ICON, ImageLoader.loadImage("helpIcon"));
			putValue(Action.ACCELERATOR_KEY,
			        KeyStroke.getKeyStroke(KeyEvent.VK_H,
			                ActionEvent.ALT_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_H));
			*/
		}

		public void actionPerformed(ActionEvent e) {
			showAbout();
		}
	};

	private AbstractJMTAction ACTION_SOLVE = new AbstractJMTAction("Solve") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "Solve model");
			setIcon("Sim", JMTImageLoader.getImageLoader());

			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_L, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_L));

		}

		public void actionPerformed(ActionEvent e) {

			if (checkFinish()) {
				finish();
			}

		}
	};

	public ExactWizard() {
		this(new ExactModel());
	}

	public ExactWizard(ExactModel data) {
		super(TITLE);
		setSize(800, 600);
		this.centerWindow();
		setIconImage(JMTImageLoader.loadImageAwt("JMVAIcon"));
		this.data = data;
		data.resetChanged();
		this.setJMenuBar(makeMenubar());
		getContentPane().add(makeToolbar(), BorderLayout.NORTH);
		addPanel(new ClassesPanel(this));
		addPanel(new StationsPanel(this));
		serviceTimesPanel = new ServiceTimesPanel(this);
		visitsPanel = new VisitsPanel(this);
		serviceDemandsPanel = new ServiceDemandsPanel(this);
		if (data.areVisitsSet()) {
			addPanel(serviceTimesPanel);
			addPanel(visitsPanel);
		} else {
			addPanel(serviceDemandsPanel);
		}
		addPanel(new WhatIfPanel(this));
		addPanel(new DescriptionPanel(this));

		show();
	}

	/**
	 * @return the toolbar for the exact wizard. Shamelessly uses icon from the main jmt frame
	 */
	protected JMTToolBar makeToolbar() {

		JMTToolBar tb = new JMTToolBar(JMTImageLoader.getImageLoader());
		tb.setFloatable(false);

		//null values add a gap between toolbar icons
		AbstractJMTAction[] actions = { FILE_NEW, FILE_OPEN, FILE_SAVE, null, ACTION_SOLVE, SWITCH_TO_SIMULATOR, ACTION_RANDOMIZE_MODEL, null, HELP };
		String[] htext = { "Creates a new model", "Opens a saved model", "Saves the current model", "Solves the current model",
				"Import current model to JSIMwiz to solve it with the simulator", "Randomize model data", "Show help" };
		ArrayList buttons = tb.populateToolbar(actions);
		// Adds help
		for (int i = 0; i < buttons.size(); i++) {
			AbstractButton button = (AbstractButton) buttons.get(i);
			help.addHelp(button, htext[i]);
		}
		return tb;
	}

	private JMTMenuBar makeMenubar() {
		JMTMenuBar jmb = new JMTMenuBar(JMTImageLoader.getImageLoader());
		AbstractJMTAction[] menuItems = new AbstractJMTAction[] {
				new MenuAction("File", new AbstractJMTAction[] { FILE_NEW, FILE_OPEN, FILE_SAVE, null, FILE_EXIT }),
				new MenuAction("Action", new AbstractJMTAction[] { ACTION_SOLVE, ACTION_RANDOMIZE_MODEL, null, SWITCH_TO_SIMULATOR, null,
						ACTION_NEXT, ACTION_PREV }), new MenuAction("Help", new AbstractJMTAction[] { HELP, null, ABOUT }), };

		jmb.populateMenu(menuItems);
		return jmb;
	}

	/**
	 * @return the button panel
	 */
	@Override
	protected JComponent makeButtons() {
		help = new HoverHelp();
		helpLabel = help.getHelpLabel();

		helpLabel.setBorder(BorderFactory.createEtchedBorder());
		//helpLabel.setHorizontalAlignment(SwingConstants.CENTER);

		ACTION_FINISH.putValue(Action.NAME, "Solve");
		ACTION_CANCEL.putValue(Action.NAME, "Exit");

		JPanel buttons = new JPanel();

		JButton button_finish = new JButton(ACTION_FINISH);
		help.addHelp(button_finish, "Validates the system and starts the solver");
		JButton button_cancel = new JButton(ACTION_CANCEL);
		help.addHelp(button_cancel, "Exits the wizard discarding all changes");
		JButton button_next = new JButton(ACTION_NEXT);
		help.addHelp(button_next, "Moves on to the next step");
		JButton button_previous = new JButton(ACTION_PREV);
		help.addHelp(button_previous, "Goes back to the previous step");
		JButton button_help = new JButton(ACTION_HELP);
		help.addHelp(button_help, "Displays help for the current panel");
		buttons.add(button_previous);
		buttons.add(button_next);
		buttons.add(button_finish);
		buttons.add(button_cancel);
		buttons.add(button_help);

		JPanel labelbox = new JPanel();
		labelbox.setLayout(new BorderLayout());
		labelbox.add(Box.createVerticalStrut(20), BorderLayout.WEST);
		labelbox.add(helpLabel, BorderLayout.CENTER);

		Box buttonBox = Box.createVerticalBox();
		buttonBox.add(buttons);
		buttonBox.add(labelbox);
		return buttonBox;
	}

	//BEGIN Federico Dall'Orso 8/3/2005
	//NEW
	private void newModel() {
		currentPanel.lostFocus();
		if (checkForSave("<html>Save changes before creating a new model?</html>")) {
			return;
		}
		Rectangle bounds = this.getBounds();
		ExactWizard ew = new ExactWizard();
		updateTile(null);
		ew.setBounds(bounds);
		ew.show();
		this.hide();
		this.dispose();
	}

	//END Federico Dall'Orso 8/3/2005

	/**
	 * Shows a confirmation dialog to save before new model or exit operations
	 * @param msg The message to display.
	 * @return <code>true</code> - if the user select cancel button.
	 */
	public boolean checkForSave(String msg) {
		// Checks if there's an old graph to save
		if (data != null && data.isChanged()) {
			int resultValue = JOptionPane.showConfirmDialog(this, msg, "JMVA - Warning", JOptionPane.YES_NO_CANCEL_OPTION,
					JOptionPane.WARNING_MESSAGE);
			if (resultValue == JOptionPane.YES_OPTION) {
				save();
				return true;
			}
			if (resultValue == JOptionPane.CANCEL_OPTION) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Saves current model
	 * <br>Author: Bertoli Marco
	 */
	private void save() {
		currentPanel.lostFocus();
		if (!checkFinish()) {
			return; // panels with problems are expected to notify the user by themselves
		}
		int retval = modelLoader.saveModel(data, this, null);
		switch (retval) {
			case ModelLoader.SUCCESS:
				data.resetChanged();
				updateTile(modelLoader.getSelectedFile().getName());
				break;
			case ModelLoader.FAILURE:
				JOptionPane.showMessageDialog(this, modelLoader.getFailureMotivation(), "Error", JOptionPane.ERROR_MESSAGE);
				break;
		}
	}

	/**
	 * Opens a new model
	 * <br>Author: Bertoli Marco
	 */
	private void open() {
		currentPanel.lostFocus();
		if (checkForSave("<html>Save changes before opening a saved model?</html>")) {
			return;
		}
		ExactModel newdata = new ExactModel();
		int retval = modelLoader.loadModel(newdata, this);
		switch (retval) {
			case ModelLoader.SUCCESS:
			case ModelLoader.WARNING:
				data = newdata;
				currentPanel.gotFocus();
				// Shows right panels
				if (data.areVisitsSet()) {
					removePanel(serviceDemandsPanel);
					((ForceUpdatablePanel) serviceTimesPanel).retrieveData();
					((ForceUpdatablePanel) visitsPanel).retrieveData();
					addPanel(visitsPanel, 2);
					addPanel(serviceTimesPanel, 2);
				} else {
					removePanel(visitsPanel);
					removePanel(serviceTimesPanel);
					((ForceUpdatablePanel) serviceDemandsPanel).retrieveData();
					addPanel(serviceDemandsPanel, 2);
				}
				updateTile(modelLoader.getSelectedFile().getName());
				tabbedPane.setSelectedIndex(0);
				break;
			case ModelLoader.FAILURE:
				JOptionPane.showMessageDialog(this, modelLoader.getFailureMotivation(), "Error", JOptionPane.ERROR_MESSAGE);
				break;

		}
		tempFile = modelLoader.getSelectedFile();
		if (data.hasResults()) {
			this.createSolutionWindow();
		}
		updatePanels();

		// Shows warnings if any
		if (retval == ModelLoader.WARNING) {
			new WarningWindow(modelLoader.getLastWarnings(), this, modelLoader.getInputFileFormat(), CommonConstants.JMVA).show();
		}
	}

	public ExactModel getData() {
		return data;
	}

	@Override
	protected void finish() {
		//OLD
		//do not call this method!!! It's already called inside checkFinish() method.
		//currentPanel.lostFocus();

		solve();
	}

	@Override
	protected boolean cancel() {
		if (currentPanel != null) {
			currentPanel.lostFocus();
		}
		return !checkForSave("<html>Save changes before closing?</html>");
	}

	protected void switchToSimulator() {
		JSIMModel output = new JSIMModel();
		// New Converter by Bertoli Marco
		List res = ModelConverter.convertJMVAtoJSIM(data, output);
		JSIMMain jsim = new JSIMMain(output);
		jsim.show();
		// If problems are found, shows warnings
		if (res.size() > 0) {
			new WarningWindow(res, jsim, CommonConstants.JMVA, CommonConstants.JSIM).show();
		}

	}

	public HoverHelp getHelp() {
		return help;
	}

	/**switches service times and visits panels to service demands panel in order to change
	 * data representation.*/
	public void switchFromSTVtoSD() {
		((ForceUpdatablePanel) serviceTimesPanel).commitData();
		((ForceUpdatablePanel) visitsPanel).retrieveData();
		((ForceUpdatablePanel) visitsPanel).commitData();
		removePanel(serviceTimesPanel);
		removePanel(visitsPanel);
		((ForceUpdatablePanel) serviceDemandsPanel).retrieveData();
		addPanel(serviceDemandsPanel, 2);
	}

	/**switches service times and visits panels to service demands panel in order to change
	 * data representation.*/
	public void switchFromSDtoSTV() {
		((ForceUpdatablePanel) serviceDemandsPanel).commitData();
		removePanel(serviceDemandsPanel);
		((ForceUpdatablePanel) serviceTimesPanel).retrieveData();
		((ForceUpdatablePanel) visitsPanel).retrieveData();
		addPanel(visitsPanel, 2);
		addPanel(serviceTimesPanel, 2);
	}

	private void solve() {

		if (solver == null) {
			solver = new SolverClient(this);
		}

		ExactModel newdata = new ExactModel(data); // Yields the mean performance indices

		// Checks saturation
		int state = data.checkSaturation();
		switch (state) {
			case ExactModel.SATURATION:
				JOptionPane.showMessageDialog(this, "Error: input data will cause model saturation. Please adjust arrival rates or service demands.",
						"Input data error", JOptionPane.ERROR_MESSAGE);
				return;
			case ExactModel.SATURATION_WHATIF:
				JOptionPane.showMessageDialog(this,
						"Error: input data will cause model saturation during what-if analysis. Please adjust what-if analysis parameters.",
						"Input data error", JOptionPane.ERROR_MESSAGE);
				return;
		}

		try {
			//OLD
			/*
			solver.solve(newdata);
			*/
			//NEW Dall'Orso
			tempFile = solver.solve(newdata);
			//OLD
			//NEW
			//@author Stefano Omini
		} catch (InputDataException rse) {
			JOptionPane.showMessageDialog(this, rse.getMessage(), "Input data error", JOptionPane.ERROR_MESSAGE);
			return;
			//end NEW
		} catch (SolverException e) {
			JOptionPane.showMessageDialog(this, e.getMessage(), "Solver error", JOptionPane.ERROR_MESSAGE);
			return;
		} catch (OutOfMemoryError e) {
			JOptionPane.showMessageDialog(this, "Out of memory error. Try to run Java Virtual Machine with more heap size (-Xmx<num>m)",
					"Out of Memory", JOptionPane.ERROR_MESSAGE);
			return;
		}
		this.data = newdata;
		if (data.hasResults()) {
			createSolutionWindow();
		}
		updatePanels();
		currentPanel.gotFocus();
	}

	//NEW
	//@author Stefano Omini
	private void showHelp(ActionEvent event) {
		/*
		try {
		    ClassLoader cl = this.getClass().getClassLoader();

		    //TODO: cerca nella cartella class... � l'unica soluzione attualmente funzionante
		    //URL url = HelpSet.findHelpSet(cl, "help/mvaIt/MVA.hs");
		    URL url = HelpSet.findHelpSet(cl, "help/jmva_en/jmva_eng.hs");
		    //System.out.println(url.toString());

		    HelpSet hs = new HelpSet(cl, url);
		    HelpBroker hb = hs.createHelpBroker();
		    CSH.DisplayHelpFromSource display = new CSH.DisplayHelpFromSource(hb);
		    display.actionPerformed(event);

		//} catch (HelpSetException e) {
		//TODO: specializzare exception
		} catch (Exception e) {
		    e.printStackTrace();
		    JOptionPane.showMessageDialog(this, "Sorry, jMVA help is not available", "Help not found", JOptionPane.ERROR_MESSAGE);
		}

		return;
		*/

		JHelp helpViewer = null;
		try {
			// Get the classloader of this class.
			ClassLoader cl = this.getClass().getClassLoader();
			// Use the findHelpSet method of HelpSet to create a URL referencing the helpset file.
			URL url = HelpSet.findHelpSet(cl, "help/jMVA_en/jmva_eng.hs");
			// Create a new JHelp object with a new HelpSet.
			helpViewer = new JHelp(new HelpSet(cl, url));

			// Set the initial entry point in the table of contents.
			//helpViewer.setCurrentID("");
		} catch (Exception e) {
			e.printStackTrace();
			JOptionPane.showMessageDialog(this, "Sorry, jMVA help is not available", "Help not found", JOptionPane.ERROR_MESSAGE);
			return;
		}

		// Create a new frame.
		JMTFrame frame = new JMTFrame();
		// Set it's size.
		frame.centerWindow(650, 510);
		// Add the created helpViewer to it.
		frame.getContentPane().add(helpViewer);
		// Make the frame visible.
		frame.setVisible(true);

	}

	//end NEW

	//NEW
	//@author Bertoli Marco
	private void showAbout() {
		AboutDialogFactory.showJMVA(this);
	}

	//end NEW

	//NEW
	//@author Stefano Omini
	/**
	* find the helpset file and create a HelpSet object
	*/
	public HelpSet getHelpSet(String helpsetfile) {
		HelpSet hs = null;
		ClassLoader cl = this.getClass().getClassLoader();
		try {
			URL hsURL = HelpSet.findHelpSet(cl, helpsetfile);
			hs = new HelpSet(null, hsURL);
		} catch (Exception ee) {
			//System.out.println("HelpSet: "+ee.getMessage());
			//System.out.println("HelpSet: "+ helpsetfile + " not found");
			JOptionPane.showMessageDialog(this, ee.getMessage(), "Help not found", JOptionPane.ERROR_MESSAGE);
		}
		return hs;
	}

	//end NEW

	//NEW Dall'Orso
	private void createSolutionWindow() {
		JTabbedPane jtp = new JTabbedPane();
		JFrame solutionWindow = new JFrame("jMVA Solutions");
		solutionWindow.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		solutionWindow.getContentPane().add(jtp);
		solutionWindow.setIconImage(this.getIconImage());
		IterationSelector selector = null;
		if (data.isWhatIf()) {
			// Graphic panel (Bertoli Marco)
			jtp.add(new GraphPanel(data));
			selector = new IterationSelector(data);
		}
		ThroughputPanel throughput = new ThroughputPanel(this);
		QueueLenPanel queuelength = new QueueLenPanel(this);
		ResTimePanel restimes = new ResTimePanel(this);
		UtilizationPanel utilizations = new UtilizationPanel(this);
		//Added by ASHANKA START
		//Introducing the new System Power Panel as a Tabbed Pane
		SysPowerPanel systemPower = new SysPowerPanel(this);
		//Added by ASHANKA STOP
		if (selector != null) {
			selector.addSolutionPanel(throughput);
			selector.addSolutionPanel(queuelength);
			selector.addSolutionPanel(restimes);
			selector.addSolutionPanel(utilizations);
			//Added by ASHANKA START
			// for System Power
			selector.addSolutionPanel(systemPower);
			//Added by ASHANKA STOP
			jtp.add(selector);
		} else {
			jtp.add(throughput);
			jtp.add(queuelength);
			jtp.add(restimes);
			jtp.add(utilizations);
			//Added by ASHANKA START
			//for System Power
			jtp.add(systemPower);
			//Added by ASHANKA STOP
		}
		//NEW Dall'Orso 5-5-2005
		SynopsisPanel synPane;
		if (tempFile != null) {
			synPane = new SynopsisPanel(this, tempFile);
		} else {
			synPane = new SynopsisPanel(this);
		}
		jtp.add(synPane);
		//END
		//BoundingBox of main window
		Rectangle rect = this.getBounds();
		solutionWindow.setBounds(rect.x + 20, rect.y + 20, rect.width, rect.height);
		solutionWindow.show();
	}

	//randomizes model data
	private void randomizeModel() {
		//TODO: CANCELLARE UNA VOLTA TROVATO IL BUG
		if (DEBUG) {
			System.out.println("Classes: " + data.getClasses() + "; Stations: " + data.getStations());
		}
		//first get infos about classes and station
		for (int i = 0; i < panels.size() && i < 2; i++) {
			Object o = panels.get(i);
			if (o instanceof ForceUpdatablePanel) {
				((ForceUpdatablePanel) o).commitData();
			}
		}
		//then randomize data
		data.randomizeModelData();
		//and then update all those data into panels
		ForceUpdatablePanel[] fuPanes = { (ForceUpdatablePanel) serviceDemandsPanel, (ForceUpdatablePanel) serviceTimesPanel,
				(ForceUpdatablePanel) serviceDemandsPanel };
		for (ForceUpdatablePanel fuPane : fuPanes) {
			fuPane.retrieveData();
		}
		repaint();
	}

	//END

	private void updatePanels() {
		if (data == null) {
			return;
		}
		//boolean enable = data.hasResults();
		//OLD Federico Dall'Orso
		/*
		for (int i = 0; i < panelCount; i++) {
			if (panels.get(i) instanceof SolutionPanel) {
				tabbedPane.setEnabledAt(i, enable);
			}
		}

		// make sure we aren't left on a disabled tab
		if ((!tabbedPane.isEnabledAt(tabbedPane.getSelectedIndex())) && (tabbedPane.getSelectedIndex() > -1)) {
			tabbedPane.setSelectedIndex(tabbedPane.getSelectedIndex() - 1);
		}
		*/
		//NEW
		for (int i = 0; i < panelCount; i++) {
			if (panels.get(i) instanceof WizardPanel) {
				(panels.get(i)).gotFocus();
			}
		}
		//END
	}

	@Override
	protected void updateActions() {
		super.updateActions();
		if (currentIndex < (panelCount - 1)) {
			if (!tabbedPane.isEnabledAt(currentIndex + 1)) {
				ACTION_NEXT.setEnabled(false);
			}
		}
		if (currentIndex > 0 && currentIndex < tabbedPane.getComponentCount()) {
			if (!tabbedPane.isEnabledAt(currentIndex - 1)) {
				ACTION_PREV.setEnabled(false);
			}
		}
		updatePanels();
	}

	// JMVA MAIN
	public static void main(String[] args) {
		try {
			UIManager.setLookAndFeel(new com.jgoodies.looks.plastic.Plastic3DLookAndFeel());

		} catch (UnsupportedLookAndFeelException e) {
			e.printStackTrace();
		}
		Locale.setDefault(Locale.ENGLISH);
		new ExactWizard(new ExactModel());
	}

	/**
	 * Sets the file name to be shown in the title
	 * @param filename the file name or null to remove it
	 */
	public void updateTile(String filename) {
		if (filename != null) {
			setTitle(TITLE + " - " + filename);
		} else {
			setTitle(TITLE);
		}
	}

}
