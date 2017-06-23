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
package jmt.gui.jaba.panels;

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
import jmt.gui.exact.panels.SynopsisPanel;
import jmt.gui.jaba.JabaWizard;

/**
 * <p>Title: Sectors Textual Panel</p>
 * <p>Description: This panel will show textual results</p>
 *
 * @author Bertoli Marco
 *         Date: 8-feb-2006
 *         Time: 11.43.34
 */
public class SectorsTextualPanel extends WizardPanel {

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
	private JabaWizard jabawizard;

	public SectorsTextualPanel(JabaWizard jabawizard) {
		super();
		this.jabawizard = jabawizard;
		initComponents();
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
		synView
				.setText("<html><body><center><font face=\"bold\" size=\"3\">Saturation Sectors will be here displayed once you solve the model.</font></center></body></html>");
	}

	@Override
	public String getName() {
		return "Saturation Sectors - Text";
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
			if (transfUrlStream != null) {
				synView.setText("<html><body><center><font face=\"bold\" size=\"2\">Saturation Sectors will be here.</font></center></body></html>");
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
		} catch (TransformerFactoryConfigurationError tfce) {
			JOptionPane.showMessageDialog(null, tfce.getMessage(), "Transform Error", JOptionPane.ERROR_MESSAGE);
		} catch (IOException ioe) {
			JOptionPane.showMessageDialog(null, ioe.getMessage(), "File Exception", JOptionPane.ERROR_MESSAGE);
		} catch (TransformerException e) {
			JOptionPane.showMessageDialog(null, e.getMessage(), "Transform Exception", JOptionPane.ERROR_MESSAGE);
		}
	}

	public void setDoc(File xmlFile) {
		try {
			setDoc(new FileInputStream(xmlFile));
		} catch (FileNotFoundException e) {
			JOptionPane.showMessageDialog(null, e.getMessage(), "File Error", JOptionPane.ERROR_MESSAGE);
		}
	}

	@Override
	public void gotFocus() {
		redraw();
		repaint();
	}

	public void redraw() {
		if (jabawizard.getData().hasResults() && jabawizard.getData().areResultsOK() && jabawizard.getData().getResults().size() > 0
				&& jabawizard.getData().getClasses() >= 2 && jabawizard.getData().getClasses() <= 3) {
			String res = "<html>" + "<body align=\"Left\">";
			for (int i = 0; i < (jabawizard.getData().getResults()).size(); i++) {
				String temp = ((jabawizard.getData().getResults().get(i))).toString();
				res = res.concat(temp);
				res = res.concat("<br><br>");
			}
			res = res.concat("</body>" + "</html>");
			synView.setText(res);
		} else {
			synView
					.setText("<html><body><center><font face=\"bold\" size=\"3\">Saturation Sectors will be here displayed once you solve the model.</font></center></body></html>");
		}
	}

}
