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

package jmt.gui.exact.panels;

import java.awt.GridLayout;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import javax.swing.Box;
import javax.swing.JEditorPane;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.ScrollPaneConstants;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.exact.ExactWizard;

/**
 * Created by IntelliJ IDEA.
 * User: OrsotronIII
 * Date: 3-mag-2005
 * Time: 11.20.42
 * To change this template use Options | File Templates.
 */
public class SynopsisPanel extends WizardPanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	//constant for transformer path
	private static final String XSLT_FILE = "report.xslt";

	//GUI components
	private JEditorPane synView;
	private JScrollPane synScroll;

	//data source
	private ExactWizard ew;

	public SynopsisPanel(ExactWizard ew) {
		super();
		this.ew = ew;
		initComponents();
	}

	public SynopsisPanel(ExactWizard ew, InputStream xmlFile) {
		this(ew);
		setDoc(xmlFile);
	}

	public SynopsisPanel(ExactWizard ew, File xmlFile) {
		this(ew);
		setDoc(xmlFile);
	}

	private void initComponents() {
		Box vBox = Box.createVerticalBox();
		Box hBox = Box.createHorizontalBox();
		synView = new JTextPane();
		synView.setContentType("text/html");
		synView.setEditable(false);
		synScroll = new JScrollPane(synView);
		synScroll.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		synScroll.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
		vBox.add(Box.createVerticalStrut(20));
		vBox.add(hBox);
		vBox.add(Box.createVerticalStrut(20));
		hBox.add(Box.createHorizontalStrut(20));
		hBox.add(synScroll);
		hBox.add(Box.createHorizontalStrut(20));
		this.setLayout(new GridLayout(1, 1));
		this.add(vBox);
	}

	@Override
	public String getName() {
		return "Synopsis";
	}

	public void setDoc(InputStream xmlFile) {
		BufferedInputStream bufIS = new BufferedInputStream(xmlFile);
		StreamSource sSource = new StreamSource(bufIS);
		Transformer fileTransf;
		File tempFile;
		StreamResult sResult;
		try {
			InputStream transfUrlStream = SynopsisPanel.class.getResourceAsStream(XSLT_FILE);
			//System.out.println(transfUrlStream);
			if (transfUrlStream == null) {
				synView.setText("<html><body><center><font face=\"bold\" size=\"2\">Resource " + XSLT_FILE
						+ " not found.</font></center></body></html>");
				return;
			}
			fileTransf = TransformerFactory.newInstance().newTransformer(new StreamSource(transfUrlStream));
			tempFile = File.createTempFile("~temp" + new Long((long) (Math.random() * 10E16)), ".html");
			tempFile.deleteOnExit();
			sResult = new StreamResult(tempFile);
			fileTransf.transform(sSource, sResult);
			synView.setPage(new URL("file", "localhost", tempFile.getPath()));
			synScroll.setViewportView(synView);
		} catch (TransformerConfigurationException e) {
			JOptionPane.showMessageDialog(null, e.getMessage(), "Transform Exception", JOptionPane.ERROR_MESSAGE);
			return;
		} catch (TransformerFactoryConfigurationError tfce) {
			JOptionPane.showMessageDialog(null, tfce.getMessage(), "Transform Error", JOptionPane.ERROR_MESSAGE);
			return;
		} catch (IOException ioe) {
			JOptionPane.showMessageDialog(null, ioe.getMessage(), "File Exception", JOptionPane.ERROR_MESSAGE);
			return;
		} catch (TransformerException e) {
			JOptionPane.showMessageDialog(null, e.getMessage(), "Transform Exception", JOptionPane.ERROR_MESSAGE);
			return;
		}/*
			       catch (FileNotFoundException fnfe){
			           JOptionPane.showMessageDialog(null, fnfe.getMessage(), "File Error", JOptionPane.ERROR_MESSAGE);
			           return;
			       }
			       */
	}

	public void setDoc(File xmlFile) {
		try {
			setDoc(new FileInputStream(xmlFile));
		} catch (FileNotFoundException e) {
			JOptionPane.showMessageDialog(null, e.getMessage(), "File Error", JOptionPane.ERROR_MESSAGE);
		}
	}

}
