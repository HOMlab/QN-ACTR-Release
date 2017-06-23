package jmt.gui.jwat;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.Locale;

import javax.help.HelpSet;
import javax.help.JHelp;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JToolBar;
import javax.swing.KeyStroke;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;

import jmt.engine.jwat.JwatSession;
import jmt.engine.jwat.ProgressStatusListener;
import jmt.engine.jwat.fitting.FittingSession;
import jmt.engine.jwat.input.EventFinishAbort;
import jmt.engine.jwat.input.EventSessionLoaded;
import jmt.engine.jwat.input.EventStatus;
import jmt.engine.jwat.input.Loader;
import jmt.engine.jwat.input.ProgressMonitorShow;
import jmt.engine.jwat.trafficAnalysis.TrafficAnalysisSession;
import jmt.engine.jwat.workloadAnalysis.WorkloadAnalysisSession;
import jmt.framework.gui.help.HoverHelp;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.common.panels.AboutDialogFactory;
import jmt.gui.common.resources.JMTImageLoader;
import jmt.gui.jwat.fitting.panels.FittingPanel;
import jmt.gui.jwat.fitting.panels.LoadDemoFittingPanel;
import jmt.gui.jwat.trafficAnalysis.panels.EpochPanel;
import jmt.gui.jwat.trafficAnalysis.panels.GraphArrivalPanel;
import jmt.gui.jwat.trafficAnalysis.panels.GraphPanel;
import jmt.gui.jwat.trafficAnalysis.panels.TextualPanel;
import jmt.gui.jwat.workloadAnalysis.panels.ClusterPanel;
import jmt.gui.jwat.workloadAnalysis.panels.ClusteringInfoPanel;
import jmt.gui.jwat.workloadAnalysis.panels.InputPanel;
import jmt.gui.jwat.workloadAnalysis.panels.LoadDemoPanel;
import jmt.gui.jwat.workloadAnalysis.panels.StatsPanel;

public class MainJwatWizard extends JWatWizard {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	//jWAT tool icons
	private String IMG_JWATICON = "JWATIcon";
	//private JToolBar toolBar = null;
	private static final String TITLE = "jWAT";
	private static final String WORK_LOAD_TITLE = "Workload Analysis";
	private static final String BURSTINESS_TITLE = "Traffic Analysis  - Burstiness";
	private static final String FITTING_TITLE = "Fitting Workload Data";
	private JPanel menus = null;
	private JMenuBar mMenuBar = null;

	//private JWatModel model = null;
	private JwatSession session = null;

	//Last panel visited, used to control correct next step
	private int lastPanel = 0, currentPanel = 0;

	private HoverHelp help = null;
	// List of panels create for Workload Analysis tool
	private ArrayList<WizardPanel> JWatPanels = new ArrayList<WizardPanel>();
	// First panel
	private JWatMainPanel mainPanel = null;

	/**
	 * Constructor.
	 */
	public MainJwatWizard() {
		initGUI();
	}

	private JFileChooser fileSaveF = new JFileChooser(".") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			setApproveButtonText("Save");
			setFileSelectionMode(JFileChooser.FILES_ONLY);
		}
	};

	private JFileChooser fileOpenF = new JFileChooser(".") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			setApproveButtonText("Open");
			setFileSelectionMode(JFileChooser.FILES_ONLY);
		}
	};

	/*
	 * Initializes jWAT start screen GUI
	 */
	private void initGUI() {
		this.setIconImage(JMTImageLoader.loadImage(IMG_JWATICON).getImage());
		//this.setResizable(false);
		this.setTitle(TITLE);
		this.setSize(800, 600);
		centerWindow();
		menus = new JPanel(new BorderLayout());
		help = this.getHelp();
		getContentPane().add(menus, BorderLayout.NORTH);
		//Aggiunta del pannello principale dell'applicazione
		mainPanel = new JWatMainPanel(this);
		this.addPanel(mainPanel);
	}
	
	public void setFittingEnv(String mode) {
		this.setTitle(TITLE + " - " + FITTING_TITLE);
		session = new FittingSession();
		
		//Creates and adds all necessary panels to jWAT main screen
		WizardPanel p;
		
		if (mode.equals("load")) {
			p = new jmt.gui.jwat.fitting.panels.InputPanel(this);
		} else {
			p = new LoadDemoFittingPanel(this);
		}
		
		JWatPanels.add(p);
		this.addPanel(p);
		
		p = new FittingPanel(this,FittingPanel.PARETO);
		JWatPanels.add(p);
		this.addPanel(p);
		
		p = new FittingPanel(this,FittingPanel.EXPO);
		JWatPanels.add(p);
		this.addPanel(p);
		
		getFittingToolbar();
		getFittingMenubar();
		
		this.setEnableButton("Solve", false);
		//Shows next panel, the first of traffic analysis wizard
		showNextPanel();
	}

	// Set correct enviornement for traffic analysis
	public void setTrafficEnv() {
		this.setTitle(TITLE + " - " + BURSTINESS_TITLE);
		session = new TrafficAnalysisSession();

		//Creates and adds all necessary panels to jWAT main screen
		WizardPanel p = new jmt.gui.jwat.trafficAnalysis.panels.InputPanel(this);
		JWatPanels.add(p);
		this.addPanel(p);

		p = new EpochPanel(this);
		JWatPanels.add(p);
		this.addPanel(p);

		p = new TextualPanel(this);
		JWatPanels.add(p);
		this.addPanel(p);

		p = new GraphPanel(this);
		JWatPanels.add(p);
		this.addPanel(p);

		p = new GraphArrivalPanel(this);
		JWatPanels.add(p);
		this.addPanel(p);
		//Sets menu and tool bars
		getTrafficToolbar();
		getTrafficMenubar();
		//Disables Saolve button
		this.setEnableButton("Solve", false);
		//Shows next panel, the first of traffic analysis wizard
		showNextPanel();
	}

	//Adds all necessary panes concernig with Workload analysis
	public void setWorkloadEnv(String mode) {
		this.setTitle(TITLE + " - " + WORK_LOAD_TITLE);
		session = new WorkloadAnalysisSession();
		WizardPanel p;
		if (mode.equals("load")) {
			p = new InputPanel(this);
		} else {
			p = new LoadDemoPanel(this);
		}
		JWatPanels.add(p);
		this.addPanel(p);
		p = new StatsPanel(this);
		JWatPanels.add(p);
		this.addPanel(p);
		p = new ClusterPanel(this);
		JWatPanels.add(p);
		this.addPanel(p);
		p = new ClusteringInfoPanel(this);
		JWatPanels.add(p);
		this.addPanel(p);
		//Set Workload ToolBar
		getWorkloadToolbar();
		//Set Workload MenuBar
		getWorkloadMenuBar();
		setEnableButton("Next >", false);
		setEnableButton("Solve", false);
		lastPanel = 1;
		showNextPanel();
	}

	/**
	 * Main method.
	 * @param args no args.
	 */
	public static void main(String[] args) {
		new MainJwatWizard().setVisible(true);
	}

	public void setToolBar(JToolBar bar) {
		if (toolBar != null) {
			menus.remove(toolBar);
		}
		menus.add(bar, BorderLayout.SOUTH);
		toolBar = bar;
	}

	public void setMenuBar(JMenuBar bar) {
		if (mMenuBar != null) {
			menus.remove(mMenuBar);
		}
		menus.add(bar, BorderLayout.NORTH);
		mMenuBar = bar;
	}

	// Riseleziona il pannello da cui si e' arrivati
	public void setLastPanel() {
		tabbedPane.setSelectedIndex(lastPanel);
	}

	// Salva il pannello che da cui si e' arrivati
	public void setLastPanel(int panel) {
		lastPanel = panel;
	}

	public void setCurrentPanel(int panel) {
		currentPanel = panel;
	}

	public JWatModel getModel() {
		JWatModel mode = null;
		if (session != null) {
			mode = session.getDataModel();
		}
		return mode;
	}

	public JwatSession getSession() {
		return session;
	}

	/*
	 * 
	 */
	protected AbstractAction WL_EXIT_ACTION = new AbstractAction("Exit") {
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
			cancel();
		}
	};
	private AbstractAction WL_HELP_SHOWHELP = new AbstractAction("Help") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Show Help");
			putValue(Action.SMALL_ICON, JMTImageLoader.loadImage("Help"));
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_H, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_H));
		}

		public void actionPerformed(ActionEvent e) {
			showHelp(e);
		}
	};
	private AbstractAction WL_HELP_CREDITS = new AbstractAction("About JWAT") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Credits");
		}

		public void actionPerformed(ActionEvent e) {
			AboutDialogFactory.showJWAT(MainJwatWizard.this);
		}
	};

	private void showHelp(ActionEvent event) {
		JHelp helpViewer = null;
		try {
			// Get the classloader of this class.
			ClassLoader cl = this.getClass().getClassLoader();
			// Use the findHelpSet method of HelpSet to create a URL referencing the helpset file.
			URL url = HelpSet.findHelpSet(cl, "./help/JWat_eng/JWatWorkload.hs");
			// Create a new JHelp object with a new HelpSet.
			helpViewer = new JHelp(new HelpSet(cl, url));

			// Set the initial entry point in the table of contents.
			//helpViewer.setCurrentID("");
		} catch (Exception e) {
			e.printStackTrace();
			JOptionPane.showMessageDialog(this, "Sorry, JWAT help is not available yet, "
					+ "but you can see the JWAT users manual installed with the application", "Help not found", JOptionPane.ERROR_MESSAGE);
			return;
		}
		// Create a new frame.
		JFrame frame = new JFrame();
		// Set it's size.
		frame.setSize(650, 510);
		// Add the created helpViewer to it.
		frame.getContentPane().add(helpViewer);
		// Set a default close operation.
		frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		// Make the frame visible.
		frame.setVisible(true);

	}

	private AbstractAction WL_FILE_NEW = new AbstractAction("New") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "New input file");
			putValue(Action.SMALL_ICON, JMTImageLoader.loadImage("New"));
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_N, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_N));
		}

		public void actionPerformed(ActionEvent e) {
			if (JOptionPane.showConfirmDialog(MainJwatWizard.this, "This operation will reset data. Continue?", "Warning", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
				//Reset model and set first panel
				session.resetSession();
				tabbedPane.setSelectedIndex(1);
				try {
					((InputPanel) tabbedPane.getComponentAt(1)).resetOnNew();
				} catch (ClassCastException cce) {
					return;
				}
			}
		}
	};
	private AbstractAction WL_FILE_SAVE = new AbstractAction("Save") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Save session");
			putValue(Action.SMALL_ICON, JMTImageLoader.loadImage("Save"));
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_S, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_S));
		}

		public void actionPerformed(ActionEvent e) {
			JwatSession session;
			if (fileSaveF.showOpenDialog(MainJwatWizard.this) == JFileChooser.APPROVE_OPTION) {
				File fFile = fileSaveF.getSelectedFile();
				String fileName = fFile.getAbsolutePath();
				System.out.println(fileName);
				MainJwatWizard.this.session.saveSession(fileName.substring(0, fileName.lastIndexOf("\\")) + "\\", fileName.substring(fileName
						.lastIndexOf("\\") + 1), JwatSession.WORKLOAD_SAVE);
			}
		}
	};
	private AbstractAction WL_FILE_OPEN = new AbstractAction("Open...") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Open session");
			putValue(Action.SMALL_ICON, JMTImageLoader.loadImage("Open"));
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_O, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_O));
		}

		public void actionPerformed(ActionEvent e) {
			if (fileOpenF.showOpenDialog(MainJwatWizard.this) == JFileChooser.APPROVE_OPTION) {
				if (currentPanel != JWATConstants.WORKLOAD_INPUT_PANEL) {
					tabbedPane.setSelectedIndex(JWATConstants.WORKLOAD_INPUT_PANEL);
				}
				File fFile = fileOpenF.getSelectedFile();
				String fileName = fFile.getAbsolutePath();
				Loader.loadSession(fileName, new ProgressMonitorShow(tabbedPane.getComponent(currentPanel), "Loading Session...", 1000),
						new SessionStatusListener(), session);
			}

		}
	};

	private class SessionStatusListener implements ProgressStatusListener {
		public void statusEvent(EventStatus e) {
			switch (e.getType()) {
				case EventStatus.ABORT_EVENT:
					abortEvent((EventFinishAbort) e);
					break;
				case EventStatus.DONE_EVENT:
					finishedEvent((EventSessionLoaded) e);
					break;
			}
		}

		//Abort caricamento file input
		private void abortEvent(EventFinishAbort e) {
			JWatWizard wizard = (JWatWizard) ((WizardPanel) tabbedPane.getComponent(currentPanel)).getParentWizard();
			JOptionPane.showMessageDialog(tabbedPane.getComponent(currentPanel), e.getMessage(), "LOADING ABORTED!!", JOptionPane.WARNING_MESSAGE);
			((InputPanel) tabbedPane.getComponent(JWATConstants.WORKLOAD_INPUT_PANEL)).setCanGoForward(false);
			wizard.setEnableButton("Next >", false);
			wizard.setEnableButton("Solve", false);
		}

		//dati caricati
		private void finishedEvent(final EventSessionLoaded e) {
			JButton[] optBtn = new JButton[2];
			JOptionPane pane;
			((InputPanel) tabbedPane.getComponent(JWATConstants.WORKLOAD_INPUT_PANEL)).setCanGoForward(false);
			JWatWizard wizard = (JWatWizard) ((WizardPanel) tabbedPane.getComponent(currentPanel)).getParentWizard();

			optBtn[0] = new JButton("Continue");
			optBtn[1] = new JButton("Cancel");

			pane = new JOptionPane("Load session done: ", JOptionPane.QUESTION_MESSAGE, JOptionPane.DEFAULT_OPTION, null, optBtn, null);
			final JDialog dialog = pane.createDialog(wizard, "Loading Complete");
			pane.selectInitialValue();

			optBtn[0].addActionListener(new ActionListener() {

				public void actionPerformed(ActionEvent ev) {
					JwatSession newSession = e.getSession();
					dialog.dispose();
					JWatWizard wizard = (JWatWizard) ((WizardPanel) tabbedPane.getComponent(currentPanel)).getParentWizard();
					session.copySession(newSession);
					wizard.setEnableButton("Next >", true);
					wizard.setEnableButton("Solve", false);
					((InputPanel) tabbedPane.getComponent(JWATConstants.WORKLOAD_INPUT_PANEL)).setCanGoForward(true);
					wizard.showNextPanel();

				}
			});

			optBtn[1].addActionListener(new ActionListener() {

				public void actionPerformed(ActionEvent ev) {
					JWatWizard wizard = (JWatWizard) ((WizardPanel) tabbedPane.getComponent(currentPanel)).getParentWizard();
					dialog.dispose();
					((InputPanel) tabbedPane.getComponent(JWATConstants.WORKLOAD_INPUT_PANEL)).setCanGoForward(true);
					System.gc();
					wizard.setEnableButton("Next >", false);
					wizard.setEnableButton("Solve", false);
				}

			});

			dialog.show();
		}
	}

	private AbstractAction WL_ACTION_SOLVE = new AbstractAction("Clusterize") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Clusterize");
			putValue(Action.SMALL_ICON, JMTImageLoader.loadImage("Sim"));
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_L, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_L));
		}

		public void actionPerformed(ActionEvent e) {
		}
	};
	
	/**
	 * @return the toolbar for the jaba wizard. Shamelessly uses icon from the main jmt frame
	 */
	protected void getFittingToolbar() {
		JToolBar tb = new JToolBar();
		tb.setRollover(true);
		tb.setOrientation(SwingConstants.HORIZONTAL);
		tb.setFloatable(false);

		Action[] actions = { FI_FILE_NEW, null, FI_HELP };
		//Action[] actions = {FILE_NEW,null, ACTION_FINISH,null, HELP};
		//String[] icons = {"New","Sim", "Help"};
		//String[] htext = {"Creates a new model","Solves the current model", "Show help"};
		String[] icons = { "New", "Help" };
		String[] htext = { "Creates a new model", "Show help" };

		JButton button;
		tb.setBorderPainted(true);

		for (int i = 0, j = 0; i < actions.length; i++, j++) {
			if (actions[i] == null) {
				j--;
				tb.addSeparator(new Dimension(20, 2));
			} else {
				button = new JButton(actions[i]);
				button.setText("");
				button.setIcon(JMTImageLoader.loadImage(icons[j]));
				button.setRolloverIcon(JMTImageLoader.loadImage(icons[j] + "RO"));
				button.setPressedIcon(JMTImageLoader.loadImage(icons[j] + "P"));
				button.setFocusPainted(false);
				button.setContentAreaFilled(false);
				button.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 0));
				tb.add(button);
				help.addHelp(button, htext[j]);
			}
		}
		setToolBar(tb);
		
	}

	private void getFittingMenubar() {

		JMenuBar jmb = new JMenuBar();

		/*JMenuItem[][] menuItems = {{new JMenuItem(FILE_NEW),null, new JMenuItem(FILE_EXIT)},
		                           {new JMenuItem(ACTION_SOLVE),
		                            null, new JMenuItem(ACTION_NEXT), new JMenuItem(ACTION_PREV)},
		                           {new JMenuItem(HELP), null, new JMenuItem(ABOUT)} };*/
		JMenuItem[][] menuItems = { { new JMenuItem(FI_FILE_NEW), null, new JMenuItem(FI_FILE_EXIT) },
				{ new JMenuItem(FI_HELP), null, new JMenuItem(FI_ABOUT) } };

		String[] menuTitles = { "File", "Help" };
		char[] chars = { 'F', 'e' };
		for (int i = 0; i < menuItems.length; i++) {
			JMenu menu = new JMenu(menuTitles[i]);
			menu.setMnemonic(chars[i]);
			for (int j = 0; j < menuItems[i].length; j++) {
				if (menuItems[i][j] == null) {
					menu.addSeparator();
				} else {
					menu.add(menuItems[i][j]);
				}
			}
			jmb.add(menu);
		}
		setMenuBar(jmb);
	}

	/**
	 * @return the toolbar for the exact wizard. Shamelessly uses icon from the main jmt frame
	 */
	protected void getWorkloadToolbar() {
		JToolBar workloadToolbar = new JToolBar();
		workloadToolbar.setRollover(true);
		workloadToolbar.setOrientation(SwingConstants.HORIZONTAL);
		workloadToolbar.setFloatable(false);
		//null values add a gap between toolbar icons
		Action[] actions = { WL_FILE_NEW, WL_FILE_OPEN, WL_FILE_SAVE, null, WL_ACTION_SOLVE, null, WL_HELP_SHOWHELP };
		String[] icons = { "New", "Open", "Save", "Sim", "Help" };
		String[] htext = { "Select new input file", "Opens a saved session", "Saves the current session", "Clusterize", "Show help" };
		JButton button;
		workloadToolbar.setBorderPainted(true);
		//i index scans actions' array which includes null values, while j scans other arrays.
		//so j must be decremented when a null value is found in action array.
		for (int i = 0, j = 0; i < actions.length; i++, j++) {
			if (actions[i] == null) {
				j--;
				workloadToolbar.addSeparator(new Dimension(20, 2));
			} else {
				button = new JButton(actions[i]);
				button.setText("");
				button.setIcon(JMTImageLoader.loadImage(icons[j]));
				button.setRolloverIcon(JMTImageLoader.loadImage(icons[j] + "RO"));
				button.setPressedIcon(JMTImageLoader.loadImage(icons[j] + "P"));
				button.setFocusPainted(false);
				button.setContentAreaFilled(false);
				button.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 0));
				workloadToolbar.add(button);
				help.addHelp(button, htext[j]);
				if (j == 1 || j == 2 || j == 3 /*|| j == 4*/) {
					button.setEnabled(false);
				}
			}
		}
		setToolBar(workloadToolbar);
	}

	/**
	 * Creates workload analysis menu
	 * @return menu
	 */
	private void getWorkloadMenuBar() {

		JMenuBar workloadMenubar = new JMenuBar();
		JMenuItem[][] menuItems = {
				{ new JMenuItem(WL_FILE_NEW), new JMenuItem(WL_FILE_SAVE), new JMenuItem(WL_FILE_OPEN), null, new JMenuItem(WL_EXIT_ACTION) },
				{ new JMenuItem(WL_ACTION_SOLVE) {
					{
						setEnabled(false);
					}
				} }, { new JMenuItem(WL_HELP_SHOWHELP) {
					{
						setEnabled(true);
					}
				}, null, new JMenuItem(WL_HELP_CREDITS) } };
		String[] menuTitles = { "File", "Action", "Help" };
		char[] chars = { 'F', 'A', 'e' };
		for (int i = 0; i < menuItems.length; i++) {
			JMenu menu = new JMenu(menuTitles[i]);
			menu.setMnemonic(chars[i]);
			for (int j = 0; j < menuItems[i].length; j++) {
				if (menuItems[i][j] == null) {
					menu.addSeparator();
				} else {
					menu.add(menuItems[i][j]);
				}
			}
			workloadMenubar.add(menu);
		}
		setMenuBar(workloadMenubar);
	}

	public void resetScreen() {
		for (int i = 0; i < JWatPanels.size(); i++) {
			tabbedPane.remove(JWatPanels.get(i));
		}
		JWatPanels.clear();
		mainPanel.makeMenubar();
		mainPanel.makeToolbar();
		this.validate();
	}

	/**
	 * @return the toolbar for the jaba wizard. Shamelessly uses icon from the main jmt frame
	 */
	protected void getTrafficToolbar() {

		JToolBar tb = new JToolBar();
		tb.setRollover(true);
		tb.setOrientation(SwingConstants.HORIZONTAL);
		tb.setFloatable(false);

		Action[] actions = { TR_FILE_NEW, null, TR_HELP };
		//Action[] actions = {FILE_NEW,null, ACTION_FINISH,null, HELP};
		//String[] icons = {"New","Sim", "Help"};
		//String[] htext = {"Creates a new model","Solves the current model", "Show help"};
		String[] icons = { "New", "Help" };
		String[] htext = { "Creates a new model", "Show help" };

		JButton button;
		tb.setBorderPainted(true);

		for (int i = 0, j = 0; i < actions.length; i++, j++) {
			if (actions[i] == null) {
				j--;
				tb.addSeparator(new Dimension(20, 2));
			} else {
				button = new JButton(actions[i]);
				button.setText("");
				button.setIcon(JMTImageLoader.loadImage(icons[j]));
				button.setRolloverIcon(JMTImageLoader.loadImage(icons[j] + "RO"));
				button.setPressedIcon(JMTImageLoader.loadImage(icons[j] + "P"));
				button.setFocusPainted(false);
				button.setContentAreaFilled(false);
				button.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 0));
				tb.add(button);
				help.addHelp(button, htext[j]);
			}
		}
		setToolBar(tb);
	}

	private void getTrafficMenubar() {

		JMenuBar jmb = new JMenuBar();

		/*JMenuItem[][] menuItems = {{new JMenuItem(FILE_NEW),null, new JMenuItem(FILE_EXIT)},
		                           {new JMenuItem(ACTION_SOLVE),
		                            null, new JMenuItem(ACTION_NEXT), new JMenuItem(ACTION_PREV)},
		                           {new JMenuItem(HELP), null, new JMenuItem(ABOUT)} };*/
		JMenuItem[][] menuItems = { { new JMenuItem(TR_FILE_NEW), null, new JMenuItem(TR_FILE_EXIT) },
				{ new JMenuItem(ACTION_NEXT), new JMenuItem(ACTION_PREV) }, { new JMenuItem(TR_HELP), null, new JMenuItem(TR_ABOUT) } };

		String[] menuTitles = { "File", "Action", "Help" };
		char[] chars = { 'F', 'A', 'e' };
		for (int i = 0; i < menuItems.length; i++) {
			JMenu menu = new JMenu(menuTitles[i]);
			menu.setMnemonic(chars[i]);
			for (int j = 0; j < menuItems[i].length; j++) {
				if (menuItems[i][j] == null) {
					menu.addSeparator();
				} else {
					menu.add(menuItems[i][j]);
				}
			}
			jmb.add(menu);
		}
		setMenuBar(jmb);
	}

	private AbstractAction TR_FILE_NEW = new AbstractAction("New...") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Create New Model");
			putValue(Action.SMALL_ICON, JMTImageLoader.loadImage("New"));
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_N, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_N));
		}

		public void actionPerformed(ActionEvent e) {
			trafficNewModel();
		}
	};
	
	private AbstractAction FI_FILE_NEW = new AbstractAction("New...") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Create New Model");
			putValue(Action.SMALL_ICON, JMTImageLoader.loadImage("New"));
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_N, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_N));
		}

		public void actionPerformed(ActionEvent e) {
			fittingNewModel();
		}
	};

	private AbstractAction TR_FILE_EXIT = new AbstractAction("Exit") {
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
			cancel();
		}
	};
	
	private AbstractAction FI_FILE_EXIT = new AbstractAction("Exit") {
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
			cancel();
		}
	};

	private AbstractAction TR_HELP = new AbstractAction("Burstiness help") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Show Burstiness help");
			putValue(Action.SMALL_ICON, JMTImageLoader.loadImage("Help"));
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_H, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_H));
		}

		public void actionPerformed(ActionEvent e) {
			showHelp(e);
		}
	};
	
	private AbstractAction FI_HELP = new AbstractAction("Fitting help") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Show Fitting help");
			putValue(Action.SMALL_ICON, JMTImageLoader.loadImage("Help"));
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_H, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_H));
		}

		public void actionPerformed(ActionEvent e) {
			showHelp(e);
		}
	};

	private AbstractAction TR_ABOUT = new AbstractAction("About Burstiness...") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "About Burstiness");
			putValue(Action.SMALL_ICON, JMTImageLoader.loadImage("helpIcon"));
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_H, ActionEvent.ALT_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_H));
		}

		public void actionPerformed(ActionEvent e) {
			trafficShowAbout();
		}
	};
	
	private AbstractAction FI_ABOUT = new AbstractAction("About Fitting...") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "About Fitting");
			putValue(Action.SMALL_ICON, JMTImageLoader.loadImage("helpIcon"));
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_H, ActionEvent.ALT_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_H));
		}

		public void actionPerformed(ActionEvent e) {
			trafficShowAbout();
		}
	};
	
	private AbstractAction TR_ACTION_SOLVE = new AbstractAction("Solve") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "Solve model");
			putValue(Action.SMALL_ICON, JMTImageLoader.loadImage("Sim"));
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_L, ActionEvent.CTRL_MASK));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_L));
		}

		public void actionPerformed(ActionEvent e) {
			if (checkFinish()) {
				finish();
			}
		}
	};

	private void trafficNewModel() {
		if (JOptionPane.showConfirmDialog(MainJwatWizard.this, "This operation will reset data. Continue?", "Warning", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
			//Reset model and set first panel
			session.resetSession();
			tabbedPane.setSelectedIndex(1);
			try {
				((jmt.gui.jwat.trafficAnalysis.panels.InputPanel) tabbedPane.getComponentAt(1)).resetOnNew();
			} catch (ClassCastException cce) {
				return;
			}
		}
	}
	
	private void fittingNewModel() {
		if (JOptionPane.showConfirmDialog(MainJwatWizard.this, "This operation will reset data. Continue?", "Warning", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
			//Reset model and set first panel
			session.resetSession();
			tabbedPane.setSelectedIndex(1);
			try {
				((jmt.gui.jwat.fitting.panels.InputPanel) tabbedPane.getComponentAt(1)).resetOnNew();
			} catch (ClassCastException cce) {
				return;
			}
		}
	}

	private void trafficShowAbout() {
		JOptionPane.showMessageDialog(this, "Sorry, is not available", "About Burstiness not found", JOptionPane.ERROR_MESSAGE);
	}
}
