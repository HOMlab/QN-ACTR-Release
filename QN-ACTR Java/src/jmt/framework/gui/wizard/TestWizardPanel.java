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

package jmt.framework.gui.wizard;

import java.awt.BorderLayout;
import java.awt.Font;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.SwingConstants;

/**

 * @author alyf (Andrea Conti)
 * Date: 11-set-2003
 * Time: 23.48.19

 */

/**
 * a silly empty panel for testing purposes
 */
public final class TestWizardPanel extends WizardPanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private static int counter;
	private String name;

	public TestWizardPanel() {
		counter++;
		name = "Panel " + counter;
		JLabel jl = new JLabel("<html><center>This is <b>" + name + "</b>.<br>Have a nice day!</center></html>");
		jl.setHorizontalAlignment(SwingConstants.CENTER);
		jl.setHorizontalTextPosition(SwingConstants.CENTER);
		jl.setFont(new Font("Arial", Font.PLAIN, 16));
		jl.setBorder(BorderFactory.createEtchedBorder());

		setLayout(new BorderLayout());

		add(jl, BorderLayout.CENTER);
	}

	@Override
	public String getName() {
		return name;
	}

	public void commit() {
		System.out.println(name + ".commit()");
	}

	@Override
	public void gotFocus() {
		System.out.println(name + ".gotFocus()");
	}

	@Override
	public boolean canGoForward() {
		System.out.println(name + ".canGoForward()");
		return JOptionPane.showConfirmDialog(this, "Can " + name + " go forward?", "", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE) == JOptionPane.YES_OPTION;
	}

	@Override
	public boolean canGoBack() {
		System.out.println(name + ".canGoBack()");
		return JOptionPane.showConfirmDialog(this, "Can " + name + " go back?", "", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE) == JOptionPane.YES_OPTION;
	}

	@Override
	public boolean canFinish() {
		System.out.println(name + ".canFinish()");
		return JOptionPane.showConfirmDialog(this, "Can " + name + " finish?", "", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE) == JOptionPane.YES_OPTION;
	}

	@Override
	public void help() {
		JOptionPane.showMessageDialog(this, "This is the help for " + name, "help", JOptionPane.INFORMATION_MESSAGE);
	}

}
