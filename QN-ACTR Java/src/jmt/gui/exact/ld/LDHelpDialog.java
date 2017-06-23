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

package jmt.gui.exact.ld;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dialog;
import java.awt.event.ActionEvent;
import java.net.URL;

import javax.swing.AbstractAction;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.SwingConstants;
import javax.swing.UIManager;

import jmt.framework.gui.components.HtmlPanel;
import jmt.framework.gui.components.JMTDialog;
import jmt.gui.exact.ld.eval.Evaluator;

/**

 * @author alyf (Andrea Conti)
 * Date: 20-set-2003
 * Time: 14.44.38

 */

/**
 * the "help" dialog for the LD editor
 */
public class LDHelpDialog extends JMTDialog {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private static final String generalHelp = "<html>To enter values, single-click on the desired cell"
			+ " and start typing.<br> To select multiple cells drag the mouse on them; click or drag on"
			+ " row/column headers to select whole rows/columns.<br> <b>For a list of the available operations right-click"
			+ " on the table</b>; all operations except pasting affect selected cells.<br>"
			+ " To copy one value to multiple cells click on the cell containing the value, select the"
			+ " target cells by dragging and select <b>Fill</b>.<br>"
			+ " <b>Copy down</b> copies the value in the focused cell down to all cells in the same column."
			+ "<br><br>The expression evaluator allows you to automatically set service times as a function of the number of"
			+ " customers. Just select the cells you want to fill, enter an expression and press Enter or click <b>Evaluate</b>.<br>"
			+ " For detailed information on expression syntax and supported functions select the <b>Evaluator</b> tab above.</html>";
	/*" Cells can be filled by evaluating a mathematical expression:<br> enter it in the text field,"+
	" make sure the desired cells are selected and click on <b>\"Evaluate\"</b>.<br><br>";*/

	private URL evalHelp;

	private AbstractAction close = new AbstractAction("OK") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public void actionPerformed(ActionEvent e) {
			hide();
		}
	};

	public LDHelpDialog(Dialog owner, Evaluator eval) {
		super(owner, true);
		this.setTitle("Help");

		evalHelp = eval.getHelpURL();

		setResizable(false);
		this.centerWindow(600, 400);
		initComponents();
		setDefaultCloseOperation(HIDE_ON_CLOSE);
	}

	private void initComponents() {
		Container cp = getContentPane();
		cp.setLayout(new BorderLayout());
		JPanel butPane = new JPanel();
		butPane.add(new JButton(close), BorderLayout.SOUTH);
		cp.add(butPane, BorderLayout.SOUTH);
		JTabbedPane jtp = new JTabbedPane();

		JPanel general = new JPanel(new BorderLayout());
		JLabel ghelp = new JLabel(generalHelp);
		ghelp.setHorizontalAlignment(SwingConstants.CENTER);
		ghelp.setVerticalAlignment(SwingConstants.CENTER);
		JLabel icon = new JLabel(UIManager.getIcon("OptionPane.informationIcon"));
		Box lbox = Box.createHorizontalBox();
		lbox.add(Box.createHorizontalStrut(10));
		lbox.add(icon);
		lbox.add(Box.createHorizontalStrut(10));

		general.add(ghelp, BorderLayout.CENTER);
		general.add(lbox, BorderLayout.WEST);
		general.add(Box.createHorizontalStrut(10), BorderLayout.EAST);

		jtp.add("General", general);

		JPanel evaluator = new JPanel(new BorderLayout());

		HtmlPanel ehelp = new HtmlPanel(evalHelp);

		evaluator.add(Box.createHorizontalStrut(10), BorderLayout.WEST);
		evaluator.add(new JScrollPane(ehelp), BorderLayout.CENTER);
		evaluator.add(Box.createHorizontalStrut(10), BorderLayout.EAST);

		jtp.add("Evaluator", evaluator);

		cp.add(jtp, BorderLayout.CENTER);
	}

}
