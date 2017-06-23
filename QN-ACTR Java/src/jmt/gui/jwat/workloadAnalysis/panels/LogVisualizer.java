package jmt.gui.jwat.workloadAnalysis.panels;

import java.awt.BorderLayout;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import javax.swing.DefaultListModel;
import javax.swing.JDialog;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import jmt.gui.jwat.JWATConstants;

public class LogVisualizer extends JPanel implements JWATConstants {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private JList logList;
	private JDialog d;

	public LogVisualizer(JDialog d) {
		this.d = d;
		this.setLayout(new BorderLayout());
		logList = new JList(new DefaultListModel());
		JScrollPane p = new JScrollPane(logList);
		this.add(p, BorderLayout.CENTER);
		loadData();
	}

	private void loadData() {
		BufferedReader br = null;
		String line = null;
		DefaultListModel model = (DefaultListModel) logList.getModel();
		try {
			br = new BufferedReader(new FileReader(LogFileName));
			line = br.readLine();
			while (line != null) {
				model.addElement(line);
				line = br.readLine();
			}
		} catch (FileNotFoundException e) {
			d.dispose();
			return;
		} catch (IOException e) {
			d.dispose();
		}
		try {
			br.close();
		} catch (IOException e) {
		}
	}
}
