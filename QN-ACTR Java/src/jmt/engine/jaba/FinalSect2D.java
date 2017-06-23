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

package jmt.engine.jaba;

import java.util.Vector;

/**
 * Created by IntelliJ IDEA.
 * User: Andrea
 * Date: 18-ott-2005
 * Time: 17.10.40
 * To change this template use File | Settings | File Templates.
 */
public class FinalSect2D {

	private double beta1;
	private double beta11;
	private double beta2;
	private double beta22;
	private Vector<Station2D> stations = new Vector<Station2D>();
	private String[] classNames;

	public FinalSect2D(Sector2D sect, Vector<Station2D> stats, String[] classNames) {
		this.classNames = classNames;
		beta1 = sect.getBeta1();
		beta11 = sect.getBeta11();
		beta2 = sect.getBeta2();
		beta22 = sect.getBeta22();

		for (int i = 0; i < stats.size(); i++) {
			if ((stats.get(i).getVert()).equals(sect.getP1()) || (stats.get(i).getVert()).equals(sect.getP2())) {
				stations.addElement(stats.get(i));
			}
		}

		if (sect.hasCollinear()) {
			Vector<newPoint> coll = sect.getCollinear();

			for (int c = 0; c < coll.size(); c++)

			{
				for (int i = 0; i < stats.size(); i++) {
					if ((stats.get(i).getVert()).equals(coll.get(c))) {
						stations.addElement(stats.get(i));
					}
				}
			}
		}
	}

	public double getBeta1() {
		return beta1;
	}

	public double getBeta11() {
		return beta11;
	}

	public double getBeta2() {
		return beta2;
	}

	public double getBeta22() {
		return beta22;
	}

	public Vector<Station2D> getstation() {
		return stations;
	}

	public int countStation() {
		return stations.size();
	}

	@Override
	public String toString() {
		String out = "";
		{
			String statname = "";
			for (int i = 0; i < stations.size(); i++) {
				String temp = stations.get(i).toString();
				statname = statname.concat(temp + " ");
			}
			double dispbeta1 = beta1 * 1000;
			double dispbeta11 = beta11 * 1000;
			double dispbeta2 = beta2 * 1000;
			double dispbeta22 = beta22 * 1000;
			dispbeta1 = Math.round(dispbeta1);
			dispbeta2 = Math.round(dispbeta2);
			dispbeta11 = Math.round(dispbeta11);
			dispbeta22 = Math.round(dispbeta22);
			out = "<font size=\"4\">" + "<b>" + "Bottleneck Regions for: " + statname + "</b>" + "</font><font size=\"3\">" + "<br>" + classNames[0]
					+ ":  " + dispbeta1 / 1000 + "   " + classNames[1] + ":  " + dispbeta11 / 1000 + "<br>" + classNames[0] + ":  " + dispbeta2
					/ 1000 + "   " + classNames[1] + ":  " + dispbeta22 / 1000;
		}
		return out;
	}

}
