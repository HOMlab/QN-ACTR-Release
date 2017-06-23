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

package jmt.gui.common.startScreen;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.net.URL;
import java.util.HashMap;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.ScrollPaneConstants;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;

import jmt.framework.gui.components.JMTFrame;
import jmt.framework.gui.components.QuickHTMLViewer;
import jmt.gui.common.resources.JMTImageLoader;
import jmt.gui.common.startScreen.sampleAnimation.SampleQNAnimation;
import jmt.gui.exact.ExactWizard;
import jmt.gui.jmodel.mainGui.MainWindow;
import jmt.gui.jsim.JSIMMain;
import jmt.jmarkov.MMQueues;

/**
 * Created by IntelliJ IDEA.
 * User: Federico Dall'Orso
 * Date: 6-dic-2004
 * Time: 12.47.03
 * This window shows the jmt's start screen. It contains links to the jmt's main programs and
 * a short description of their purpose.
 */
public class UniqueStartScreen extends JMTFrame {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	/*Data structure that must contain all of the images shown in this window. Will be initialized by
	 *loadImages() method*/
	HashMap<String, ImageIcon> imageIcons = new HashMap<String, ImageIcon>();
	/*symbolic names for the images represented as string constants initialized with relative path
	 *of the images themselves*/
	private static final int FONT_SIZE = 5;
	private static final String IMG_LOGOPOLI = "logo",
			IMG_JMODELICON = "JMODELIcon",
			IMG_JMVAICON = "JMVAIcon",
			IMG_JSIMICON = "JSIMIcon",
			IMG_JABAICON = "JABAIcon",
			IMG_JMCHICON = "JMCHIcon",
			IMG_JWATICON = "JWATIcon",
			IMG_SUITEICON = "JMTIcon",
			//names for main messages to be displayed in this window.
			MSG_JMVA_DESCR_ITA = "Risolutore tramite algoritmo MVA e definizione alfanumerica del modello",
			MSG_JMVA_DESCR_ENG = "MVA algorithm solver with alphanumeric description of the model",
			MSG_JSIM_DESCR_ITA = "Simulatore con definizione alfanumerica del modello",
			MSG_JSIM_DESCR_ENG = "Simulator with alphanumeric model definition", MSG_JMCH_DESCR_ITA = "Simulatore di catene di Markov",
			MSG_JMCH_DESCR_ENG = "Markov chain simulator", MSG_JWAT_DESCR_ITA = "Analisi statistica di file di log (tecniche di clustering)",
			MSG_JWAT_DESCR_ENG = "Workload Analyzer Tool (Clustering Techniques)",
			MSG_JABA_DESCR_ITA = "Risolutore tramite metodi di analisi asintotica", MSG_JABA_DESCR_ENG = "Asyntotic bound analysis methods solver",
			MSG_JMODEL_DESCR_ITA = "Simulatore e risolutore MVA tramite definizione grafica del modello",
			MSG_JMODEL_DESCR_ENG = "Simulator and MVA solver with graphical definition of the model",
			//names for URLS of documents to be shown as description of main applications
			URL_JMT_INTRO_ITA = "IntroIta.html", URL_JMT_INTRO_ENG = "IntroEng.html",
			//content for logo panel
			FONT_TYPE = "Arial", HTML_CONTENT_TITLE = "<html><body align=\"center\"><b>" + "<font face=\"" + FONT_TYPE + "\" size=\"" + FONT_SIZE
					+ "\">JMT - Java Modelling Tools v.0.3</font><br>" + "<font face=\"" + FONT_TYPE + "\" size=\"" + (FONT_SIZE - 1)
					+ "\">Laboratorio di Valutazione delle prestazioni</font><br>" + "<font face=\"" + FONT_TYPE + "\" size=\"" + (FONT_SIZE - 2)
					+ "\">Performance Evaluation Laboratory</font><br>" + "<font face=\"" + FONT_TYPE + "\" size=\"" + (FONT_SIZE - 1)
					+ "\">Dipartimento di Elettronica e Informazione<br>" + "Politecnico di Milano - Italy</font><b>" + "</font></body></html>";

	private AbstractAction startJMVA = new AbstractAction("Starts JMVA") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "Starts Alphanumeric MVA solver");
			putValue(Action.SMALL_ICON, resizeButtonIcon(JMTImageLoader.loadImage(IMG_JMVAICON)));
		}

		public void actionPerformed(ActionEvent e) {
			new ExactWizard();
		}
	};

	private AbstractAction startJMCH = new AbstractAction("Starts JMCH") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "Starts graphic markov chain solver");
			putValue(Action.SMALL_ICON, resizeButtonIcon(JMTImageLoader.loadImage(IMG_JMCHICON)));
		}

		public void actionPerformed(ActionEvent e) {
			MMQueues.main(null);
		}
	};

	private AbstractAction startJWAT = new AbstractAction("Starts JWAT") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "Starts Workload Analyaer Tool");
			putValue(Action.SMALL_ICON, resizeButtonIcon(JMTImageLoader.loadImage(IMG_JWATICON)));
			setEnabled(false);
		}

		public void actionPerformed(ActionEvent e) {
			//TODO: to be activated once this tool is ready
		}
	};

	private AbstractAction startJSIM = new AbstractAction("Starts JSIM") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "Starts alphanumeric simulator");
			putValue(Action.SMALL_ICON, resizeButtonIcon(JMTImageLoader.loadImage(IMG_JSIMICON)));
		}

		public void actionPerformed(ActionEvent e) {
			JSIMMain.main(null);
		}
	};

	private AbstractAction startJMODEL = new AbstractAction("Starts JMODEL") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "Starts graphic simulation and MVA solver");
			putValue(Action.SMALL_ICON, resizeButtonIcon(JMTImageLoader.loadImage(IMG_JMODELICON)));
		}

		public void actionPerformed(ActionEvent e) {
			MainWindow.main(null);
			UniqueStartScreen.this.dispose();
		}
	};

	private AbstractAction startJABA = new AbstractAction("Starts JABA") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "Starts asyntotic bound analysis solver");
			putValue(Action.SMALL_ICON, resizeButtonIcon(JMTImageLoader.loadImage(IMG_JABAICON)));
			//TODO:mettere il collegamento quando è pronto il tool e abilitare
			setEnabled(false);
		}

		public void actionPerformed(ActionEvent e) {
		}
	};

	//Sample queue net animation
	SampleQNAnimation sampleQNAni = null;

	//dimensions of the window
	private static final int WIDTH = 700, HEIGHT = 480;

	JButton mvaAppl,//starts mva application
			simAppl,//starts mva application
			introIta, introEng;//shows a document of explaination for the jmt applications

	//returns a reference to a new instance of StartScreen
	public UniqueStartScreen() {
		super(true, "JMT - Java Modelling Tools");
		this.centerWindow(WIDTH, HEIGHT);
		this.setResizable(false);
		//loads all of the Images that will be shown in this window
		loadImages();
		//add components of this window
		buildWindowComponents();
		//add all of the listeners
		addListeners();
		if (isImageLoaded(IMG_SUITEICON)) {
			Image jmtIcon = imageIcons.get(IMG_SUITEICON).getImage();
			this.setIconImage(jmtIcon.getScaledInstance(16, 16, Image.SCALE_SMOOTH));
		}
		sampleQNAni.start();

	}

	//loads all of the images that must be shown in this window
	private void loadImages() {
		String[] imageNames = { IMG_LOGOPOLI, IMG_JMODELICON, IMG_JMVAICON, IMG_JABAICON, IMG_JMCHICON, IMG_JWATICON, IMG_JSIMICON, IMG_SUITEICON };
		//load each image referenced in the above array
		for (String imageName : imageNames) {
			ImageIcon img = JMTImageLoader.loadImage(imageName);
			if (imageName.equals(IMG_LOGOPOLI)) {
				//                Image imgSmall = img.getImage().getScaledInstance(400,(150*(400))/739,Image.SCALE_SMOOTH);
				Image imgSmall = img.getImage().getScaledInstance(100, 100, Image.SCALE_SMOOTH);
				img.setImage(imgSmall);
			}
			imageIcons.put(imageName, img);
		}
	}

	//builds components of this window
	private void buildWindowComponents() {
		JPanel mainPane = new JPanel(new BorderLayout());
		mainPane.setBorder(new EmptyBorder(5, 5, 5, 5));
		this.getContentPane().add(mainPane);
		/*the two main parts of the start screen. The upper one contains politecnico logo and title,
		 *the lower one, buttons and icons of the certain applications.*/
		JPanel upperArea = new JPanel(new FlowLayout(FlowLayout.CENTER, 3, 3));
		JLabel upperAreaDescr;
		//components of the upper area
		if (isImageLoaded(IMG_LOGOPOLI)) {
			upperAreaDescr = new JLabel(imageIcons.get(IMG_LOGOPOLI));
		} else {
			upperAreaDescr = new JLabel("Couldn't load image from location " + IMG_LOGOPOLI);
		}
		upperArea.add(upperAreaDescr);
		JLabel upperAreaDescrText = new JLabel(HTML_CONTENT_TITLE);
		upperArea.add(upperAreaDescrText);
		//components of the intro area
		//        JPanel introButtonArea=new JPanel(new FlowLayout(FlowLayout.CENTER,3,3));
		JPanel introButtonArea = new JPanel();
		introIta = new JButton("Introduzione al JMT");
		introEng = new JButton("Introduction to JMT");
		introButtonArea.add(introIta);
		introButtonArea.add(introEng);

		//bottom area
		JPanel bottomArea = new JPanel(new BorderLayout(5, 5));
		//Components of the lower area.
		JPanel buttonsArea = new JPanel(new GridLayout(2, 1, 5, 5));
		buttonsArea.setSize(300, 200);
		buttonsArea.setLayout(new GridLayout(6, 1));
		sampleQNAni = new SampleQNAnimation();
		JScrollPane jsp = new JScrollPane(sampleQNAni, ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		jsp.setPreferredSize(new Dimension(200, 120));
		jsp.setBorder(new EtchedBorder(EtchedBorder.LOWERED));
		JPanel bottomRightPanel = new JPanel(new GridLayout(2, 1));
		upperArea.add(jsp);

		//Components of the buttonArea
		addApplPanel(buttonsArea, startJSIM, MSG_JSIM_DESCR_ENG, MSG_JSIM_DESCR_ITA);
		addApplPanel(buttonsArea, startJMODEL, MSG_JMODEL_DESCR_ENG, MSG_JMODEL_DESCR_ITA);
		addApplPanel(buttonsArea, startJMVA, MSG_JMVA_DESCR_ENG, MSG_JMVA_DESCR_ITA);
		addApplPanel(buttonsArea, startJABA, MSG_JABA_DESCR_ENG, MSG_JABA_DESCR_ITA);
		addApplPanel(buttonsArea, startJMCH, MSG_JMCH_DESCR_ENG, MSG_JMCH_DESCR_ITA);
		addApplPanel(buttonsArea, startJWAT, MSG_JWAT_DESCR_ENG, MSG_JWAT_DESCR_ITA);

		//set preferred size for all buttons
		//resizeButtons();
		//inserts each panel in its own father component
		bottomArea.add(buttonsArea, BorderLayout.CENTER);
		//bottomArea.add(bottomRightPanel, BorderLayout.EAST);
		mainPane.add(upperArea, BorderLayout.NORTH);
		mainPane.add(introButtonArea, BorderLayout.CENTER);
		mainPane.add(bottomArea, BorderLayout.SOUTH);
	}

	//assigns each component its own listener
	private void addListeners() {
		introIta.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				URL url = getClass().getResource(URL_JMT_INTRO_ITA);
				showDescrWin(url, "Introduzione al JMT");
			}
		});
		introEng.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				URL url = getClass().getResource(URL_JMT_INTRO_ENG);
				showDescrWin(url, "Introduction to JMT");
			}
		});
		this.addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosing(WindowEvent e) {
				close();
			}
		});
	}

	//resizes images to be displayed inside buttons
	private ImageIcon resizeButtonIcon(ImageIcon ii) {
		return new ImageIcon(ii.getImage().getScaledInstance(32, 32, Image.SCALE_SMOOTH));
	}

	// adds a new line in the button area
	private void addApplPanel(JPanel container, AbstractAction action, String engDescr, String itaDescr) {
		FlowLayout fl = new FlowLayout(FlowLayout.LEFT, 3, 3);
		JPanel newArea = new JPanel(fl);
		newArea.setMaximumSize(new Dimension(400, 40));
		newArea.setBorder(new EtchedBorder());
		JButton newAppl = new JButton(action);
		newAppl.setText(null);
		newAppl.setPreferredSize(new Dimension(36, 36));
		JLabel newDescr = new JLabel("<html><body>" + itaDescr + "<br>" + engDescr + "</body></html>");
		newArea.add(newAppl);
		newArea.add(newDescr);
		container.add(newArea);
	}

	private void showDescrWin(URL url, String title) {
		if (url != null) {
			QuickHTMLViewer qhv = new QuickHTMLViewer(url, title);
			qhv.show();
			qhv.setIconImage(getIconImage());
		}
	}

	@Override
	protected void doClose() {
		sampleQNAni.stop();
	}

	//sets parameters of the specified JTextArea
	private void formatJTextArea(JTextArea jta) {
		jta.setOpaque(false);
		jta.setEditable(false);
	}

	//checks if the specified image was previouslyloaded
	private boolean isImageLoaded(String image) {
		return imageIcons.get(image) != null;
	}

	private void resizeButtons() {
		JButton[] buttons = { introIta, introEng };
		for (JButton button : buttons) {
			button.setPreferredSize(new Dimension(140, 25));
		}
	}

	public static void main(String[] args) {
		UniqueStartScreen sScr = new UniqueStartScreen();
		sScr.show();
	}

}
