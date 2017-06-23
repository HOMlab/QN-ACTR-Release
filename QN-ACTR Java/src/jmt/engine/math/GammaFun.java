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

/*
 * InvFDistribution.java
 *
 * Created on 31 ottobre 2002, 18.41
 */

package jmt.engine.math;

/** This class is a conversion of the Fortran function to evaluate the Log of
 * the  Gamma Function
 *
 * @author  Federico Granata
 */
public class GammaFun {

	/** return the Log of the Gamma function */
	public static double lnGamma(double c) {
		double[] cof = { 76.18009172947146, -86.50532032941677, 24.01409824083091, -1.231739572450155, 0.1208650973866179e-2, -0.5395239384953e-5 };
		double xx = c;
		double yy = c;
		double tmp = xx + 5.5 - (xx + 0.5) * Math.log(xx + 5.5);
		double ser = 1.000000000190015;
		for (int j = 0; j <= 5; j++) {
			ser += (cof[j] / ++yy);
		}
		return (Math.log(2.5066282746310005 * ser / xx) - tmp);
	}

	/** some tests */
	public static void test() {
		boolean OK = true;
		if (lnGamma(1.96) != -0.01639106214800501) {
			OK = false;
		}
		if (lnGamma(4) != 1.791759469228055) {
			OK = false;
		}
		if (lnGamma(1.3) != -0.10817480950786562) {
			OK = false;
		}
		if (lnGamma(1.6) != -0.112591765696759) {
			OK = false;
		}
		if (OK) {
			System.out.println("OK. all tests passed!");
		} else {
			System.out.println("There is a problem!");
		}
	}

}
