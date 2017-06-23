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
import java.util.HashMap;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.ScrollPaneConstants;
import javax.swing.WindowConstants;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;

import jmt.framework.gui.components.JMTFrame;
import jmt.gui.common.resources.JMTImageLoader;
import jmt.gui.common.startScreen.sampleAnimation.SampleQNAnimation;
import jmt.gui.exact.ExactModel;
import jmt.gui.exact.ExactWizard;

/**
 * Created by IntelliJ IDEA.
 * User: orsotroniii
 * Date: 4-mar-2005
 * Time: 15.06.49
 * This Frame shows option screen for selection of particular tool with alphanumeric
 * description.
 */
public class AlphaNumStartScreen extends JMTFrame {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	/*Data structure that must contain all of the images shown in this window. Will be initialized by
	 *loadImages() method*/
	HashMap<String, ImageIcon> imageIcons = new HashMap<String, ImageIcon>();
	/*symbolic names for the images represented as string constants initialized with relative path
	 *of the images themselves*/
	private static final String IMG_LOGOPOLI = "JMT_LOGO",
			IMG_SUITEICON = "JMTIcon",
			//names for main messages to be displayed in this window.
			MSG_MVA_DESCR_ITA = "Soluzione analitica di reti di code multiclasse, aperte chiuse e miste.",
			MSG_MVA_DESCR_ENG = "Analytic solution of open, closed and mixed multiclass queueing network.",
			MSG_SIM_DESCR_ITA = "Simulazione di reti di code multiclasse.", MSG_SIM_DESCR_ENG = "Simulation of multiclass queueing network.",
			MSG_ABA_DESCR_ITA = "Analisi asintotica dei colli di bottiglia per reti di code chiuse multiclasse.",
			MSG_ABA_DESCR_ENG = "Asynthotic analysis of bottlenecks for closed multiclass queueing network.";

	//Sample queue net animation
	SampleQNAnimation sampleQNAni = null;

	//dimensions of the window
	private static final int WIDTH = 800, HEIGHT = 270;

	JButton mvaAppl,//starts mva application
			simAppl,//starts sim application
			abaAppl;//starts aba application

	//returns a reference to a new instance of StartScreen
	public AlphaNumStartScreen() {
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
	}

	//loads all of the images that must be shown in this window
	private void loadImages() {
		String[] imageNames = { IMG_LOGOPOLI, IMG_SUITEICON };
		//load each image referenced in the above array
		for (String imageName : imageNames) {
			ImageIcon img = JMTImageLoader.loadImage(imageName);
			if (imageName.equals(IMG_LOGOPOLI)) {
				Image imgSmall = img.getImage().getScaledInstance(400, (150 * 400) / 739, Image.SCALE_SMOOTH);
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

		//bottom area
		JPanel bottomArea = new JPanel(new BorderLayout(5, 5));
		//Components of the lower area.
		JPanel buttonsArea = new JPanel(new GridLayout(3, 1, 5, 5));
		sampleQNAni = new SampleQNAnimation();
		JScrollPane jsp = new JScrollPane(sampleQNAni, ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		jsp.setPreferredSize(new Dimension(200, 100));
		jsp.setBorder(new EtchedBorder(EtchedBorder.LOWERED));
		//Components of the buttonArea
		FlowLayout fl = new FlowLayout(FlowLayout.LEFT, 5, 5);
		JPanel mvaArea = new JPanel(fl), simArea = new JPanel(fl), abaArea = new JPanel(fl);
		mvaArea.setBorder(new EtchedBorder());
		simArea.setBorder(new EtchedBorder());
		abaArea.setBorder(new EtchedBorder());
		//add components to the mva area
		mvaAppl = new JButton("Start jMVA");
		JTextArea mvaDescr = new JTextArea(MSG_MVA_DESCR_ITA + "\n" + MSG_MVA_DESCR_ENG);
		formatJTextArea(mvaDescr);
		mvaArea.add(mvaAppl);
		mvaArea.add(mvaDescr);
		//add components to the sim area
		simAppl = new JButton("Start jSIM");
		JTextArea simDescr = new JTextArea(MSG_SIM_DESCR_ITA + "\n" + MSG_SIM_DESCR_ENG);
		formatJTextArea(simDescr);
		simArea.add(simAppl);
		simArea.add(simDescr);
		//TODO:abilitare il pulsante quando sarà pronto il simulatore alfanumerico
		simAppl.setEnabled(false);
		//add components to the aba area
		abaAppl = new JButton("Start jABA");
		JTextArea abaDescr = new JTextArea(MSG_ABA_DESCR_ITA + "\n" + MSG_ABA_DESCR_ENG);
		formatJTextArea(abaDescr);
		abaArea.add(abaAppl);
		abaArea.add(abaDescr);
		//TODO:abilitare il pulsante quando sarà pronto il jaba alfanumerico
		abaAppl.setEnabled(false);

		//set preferred size for all buttons
		resizeButtons();
		//inserts each panel in its own father component
		buttonsArea.add(mvaArea);
		buttonsArea.add(simArea);
		buttonsArea.add(abaArea);
		bottomArea.add(buttonsArea, BorderLayout.CENTER);
		bottomArea.add(jsp, BorderLayout.EAST);
		mainPane.add(upperArea, BorderLayout.NORTH);
		mainPane.add(bottomArea, BorderLayout.SOUTH);
	}

	//assigns each component its own listener
	private void addListeners() {
		mvaAppl.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				new ExactWizard(new ExactModel());
				close();
			}
		});
		simAppl.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				/*TODO:quando sarà pronto il programma alfanumerico del simulatore,
				TODO:  inserire qui l'istruzione di avvio.*/
			}
		});
		abaAppl.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				/*TODO:quando sarà pronto il programma aba,
				TODO:  inserire qui l'istruzione di avvio.*/
			}
		});
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
		JButton[] buttons = { mvaAppl, simAppl, abaAppl };
		for (JButton button : buttons) {
			button.setPreferredSize(new Dimension(100, 25));
		}
	}

}
