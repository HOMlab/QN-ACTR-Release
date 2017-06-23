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

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.ScrollPaneConstants;
import javax.swing.WindowConstants;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;

import jmt.framework.gui.components.JMTFrame;
import jmt.framework.gui.components.QuickHTMLViewer;
import jmt.gui.common.resources.JMTImageLoader;
import jmt.gui.common.startScreen.sampleAnimation.SampleQNAnimation;
import jmt.gui.jmodel.mainGui.MainWindow;

/**
 * Created by IntelliJ IDEA.
 * User: Federico Dall'Orso
 * Date: 6-dic-2004
 * Time: 12.47.03
 * This window shows the jmt's start screen. It contains links to the jmt's main programs and
 * a short description of their purpose.
 */
public class StartScreen extends JMTFrame {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	/*Data structure that must contain all of the images shown in this window. Will be initialized by
	 *loadImages() method*/
	HashMap<String, ImageIcon> imageIcons = new HashMap<String, ImageIcon>();
	/*symbolic names for the images represented as string constants initialized with relative path
	 *of the images themselves*/
	private static final String IMG_LOGOPOLI = "JMT_LOGO", IMG_MVAICON = "EXACT_ICON", IMG_SIMICON = "GRAPH_ICON",
			IMG_ANIMATION = "AnimatedQueueNet", IMG_SUITEICON = "JMTIcon",
			//names for main messages to be displayed in this window.
			MSG_MVA_DESCR_ITA = "Descrizione Alfanumerica del modello", MSG_MVA_DESCR_ENG = "Alphanumeric description of the model",
			MSG_SIM_DESCR_ITA = "Descrizione Grafica del modello", MSG_SIM_DESCR_ENG = "Graphic description of the model",
			//names for URLS of documents to be shown as description of main applications
			URL_JMT_INTRO_ITA = "IntroIta.html", URL_JMT_INTRO_ENG = "IntroEng.html";
	//Sample queue net animation
	SampleQNAnimation sampleQNAni = null;

	//dimensions of the window
	private static final int WIDTH = 600, HEIGHT = 310;

	JButton mvaAppl,//starts mva application
			simAppl,//starts mva application
			introIta, introEng;//shows a document of explaination for the jmt applications

	//returns a reference to a new instance of StartScreen
	public StartScreen() {
		super(true, "JMT - Java Modelling Tools");
		this.centerWindow(WIDTH, HEIGHT);
		//close this window if proper button pressed
		this.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
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

		//TODO: ONCE SIMULATOR WILL BE WORKING PERFECTLY, REMOVE THIS LINE.
		simAppl.setEnabled(false);
	}

	//loads all of the images that must be shown in this window
	private void loadImages() {
		String[] imageNames = { IMG_LOGOPOLI, IMG_MVAICON, IMG_SIMICON, IMG_ANIMATION, IMG_SUITEICON };
		//load each image referenced in the above array
		for (String imageName : imageNames) {
			ImageIcon img = JMTImageLoader.loadImage(imageName);
			//if this image is an icon, i need a smaller version
			if (imageName.equals(IMG_MVAICON) || imageName.equals(IMG_SIMICON)) {
				Image imgSmall = img.getImage().getScaledInstance(32, 32, Image.SCALE_SMOOTH);
				img.setImage(imgSmall);
			}
			if (imageName.equals(IMG_LOGOPOLI)) {
				Image imgSmall = img.getImage().getScaledInstance(WIDTH - 20, (150 * (WIDTH - 20)) / 739, Image.SCALE_SMOOTH);
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
		JLabel upperArea;
		//components of the upper area
		if (isImageLoaded(IMG_LOGOPOLI)) {
			upperArea = new JLabel(imageIcons.get(IMG_LOGOPOLI));
		} else {
			upperArea = new JLabel("Couldn't load image from location " + IMG_LOGOPOLI);
		}
		//components of the intro area
		JPanel introButtonArea = new JPanel(new FlowLayout(FlowLayout.CENTER, 5, 5));
		//new BoxLayout(introButtonArea, BoxLayout.X_AXIS);
		introIta = new JButton("Introduzione al JMT");
		introEng = new JButton("Introduction to JMT");
		introButtonArea.add(introIta);
		introButtonArea.add(introEng);

		//bottom area
		JPanel bottomArea = new JPanel(new BorderLayout(5, 5));
		//Components of the lower area.
		JPanel buttonsArea = new JPanel(new GridLayout(2, 1, 5, 5));
		buttonsArea.setSize(300, 100);
		sampleQNAni = new SampleQNAnimation();
		JScrollPane jsp = new JScrollPane(sampleQNAni, ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		jsp.setPreferredSize(new Dimension(200, 100));
		jsp.setBorder(new EtchedBorder(EtchedBorder.LOWERED));
		//Components of the buttonArea
		FlowLayout fl = new FlowLayout(FlowLayout.LEFT, 5, 5);
		JPanel mvaArea = new JPanel(fl), simArea = new JPanel(fl);
		mvaArea.setBorder(new EtchedBorder());
		simArea.setBorder(new EtchedBorder());
		//add components to the mva area
		if (isImageLoaded(IMG_MVAICON)) {
			mvaAppl = new JButton(imageIcons.get(IMG_MVAICON));
		} else {
			mvaAppl = new JButton("Start MVA");
		}
		JTextArea mvaDescr = new JTextArea(MSG_MVA_DESCR_ITA + "\n" + MSG_MVA_DESCR_ENG);
		formatJTextArea(mvaDescr);
		mvaArea.add(mvaAppl);
		mvaArea.add(mvaDescr);
		//add components to the sim area
		if (isImageLoaded(IMG_SIMICON)) {
			simAppl = new JButton(imageIcons.get(IMG_SIMICON));
		} else {
			simAppl = new JButton("Start SIM");
		}
		JTextArea simDescr = new JTextArea(MSG_SIM_DESCR_ITA + "\n" + MSG_SIM_DESCR_ENG);
		formatJTextArea(simDescr);
		simArea.add(simAppl);
		simArea.add(simDescr);

		//set preferred size for all buttons
		resizeButtons();
		//inserts each panel in its own father component
		buttonsArea.add(mvaArea);
		buttonsArea.add(simArea);
		bottomArea.add(buttonsArea, BorderLayout.CENTER);
		bottomArea.add(jsp, BorderLayout.EAST);
		mainPane.add(upperArea, BorderLayout.NORTH);
		mainPane.add(introButtonArea, BorderLayout.CENTER);
		mainPane.add(bottomArea, BorderLayout.SOUTH);
	}

	//assigns each component its own listener
	private void addListeners() {
		
		
		mvaAppl.addActionListener(new ActionListener() {
			
			public void actionPerformed(ActionEvent e) {
				new AlphaNumStartScreen().show();
				close();
			}
		});
		simAppl.addActionListener(new ActionListener() {
			
			public void actionPerformed(ActionEvent e) {
				new MainWindow();
				close();
			}
		});
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
		buttons = new JButton[] { mvaAppl, simAppl };
		for (JButton button : buttons) {
			button.setPreferredSize(new Dimension(40, 40));
		}
	}

	public static void main(String[] args) {
		StartScreen sScr = new StartScreen();
		sScr.show();
	}
}
