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

package jmt.gui.jwat;

import javax.swing.JFrame;
import javax.swing.ProgressMonitor;

/**
 * Created by IntelliJ IDEA.
 * User: Administrator
 * Date: 22-gen-2004
 * Time: 9.56.37
 * To change this template use Options | File Templates.
 */
public class LoadPanel extends SwingWorker {
	public LoadPanel(JFrame frm, String MSG, int MAX) {
		own = frm;
		msg = MSG;
		max = MAX;
	}

	@Override
	public Object construct() {

		ProgressMonitor pm = new ProgressMonitor(own, msg, "", 0, max);

		pm.setMillisToDecideToPopup(0);
		pm.setMillisToPopup(0);
		pm.setProgress(0);

		return pm;
	}

	// private JInternalFrame own;
	private JFrame own;
	private String msg;
	private int max;
}
