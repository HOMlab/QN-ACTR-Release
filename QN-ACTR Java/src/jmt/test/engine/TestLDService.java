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

package jmt.test.engine;

import javax.swing.JFrame;

import jmt.engine.simDispatcher.Dispatcher_jSIMschema;
import jmt.gui.common.definitions.StoredResultsModel;
import jmt.gui.common.panels.ResultsWindow;
import jmt.gui.common.xml.XMLReader;
import jmt.gui.common.xml.XMLResultsReader;

/**
 * <p>Title: Test Load Dependent Service</p>
 * <p>Description: Tests a load dependent service section</p>
 * 
 * @author Bertoli Marco
 *         Date: 10-ott-2005
 *         Time: 17.36.04
 */
public class TestLDService {
	public static final String filename = "LDtest.jmt";
	public static final String path = "D:\\";

	public static void main(String[] args) throws Exception {
		Dispatcher_jSIMschema simulator = new Dispatcher_jSIMschema("" + path + filename);
		simulator.solveModel();
		String resfilename = path + "res_sim_" + filename + ".xml";
		StoredResultsModel srm = new StoredResultsModel();
		XMLResultsReader.parseXML(XMLReader.loadXML(resfilename), srm);
		ResultsWindow res = new ResultsWindow(srm);
		res.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		res.show();
	}
}
