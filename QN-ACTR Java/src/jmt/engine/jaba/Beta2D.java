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

import Jama.Matrix;

/**
 * Created by IntelliJ IDEA.
 * User: PoliMi
 * Date: 25-lug-2005
 * Time: 10.48.18
 * To change this template use File | Settings | File Templates.
 */
public class Beta2D {

	double EPSYLON = 0.0001; //costante per le approssimazioni

	/**
	 * Dati due punti p1 e p2 il metodo ritorna le beta del settore di saturazione
	 * attraverso la soluzione di un sistema lineare.
	 * @param p1
	 * @param p2
	 * @return un settore Sector2D con le coordinate nello spazio delle Beta e i Service
	 * Demands associati.
	 */
	public Sector2D BetaCalc2D(newPoint p1, newPoint p2) {
		double[][] arraya = { { 1, 1, 0, 0 }, { 0, 0, 1, 1 }, { -p2.getX(), 0, p1.getX(), 0 }, { 0, p2.getY(), 0, -p1.getY() } };
		double[][] arrayb = { { 1 }, { 1 }, { 0 }, { 0 } };
		Matrix A = new Matrix(arraya);
		Matrix b = new Matrix(arrayb);
		Matrix x = A.solve(b);
		// Creo un nuovo settore con le soluzioni e i punti associati
		Sector2D out = new Sector2D(x.get(0, 0), x.get(1, 0), x.get(2, 0), x.get(3, 0), p1, p2);
		return out;
	}

	/**
	 * Dato un vettore di vertici ritorna un vettore di double[] che rappresentano
	 * le beta e i punti del settore a cui è associato.
	 * @param verticesnp    il vettore di vertici passato
	 * @return              un vettore con le beta con la struttura dati di BetaCalc2D
	 */
	public Vector<Sector2D> BetaVector(Vector<newPoint> verticesnp) {
		Vector<Sector2D> sector = new Vector<Sector2D>();
		for (int i = 0; i < (verticesnp.size() - 1); i++) {
			Sector2D out = new Sector2D();
			out = BetaCalc2D(verticesnp.get(i), verticesnp.get(i + 1));
			sector.addElement(out);
		}
		return sector;
	}

	/**
	 * Il metodo associa ad un vettore passato da BetaVector ad ogni settore la sua
	 * stazione, "esplodendolo" in modo da avere anche i settori dove satura una singola
	 * stazione.
	 *
	 * @param sector        un vettore passato da BetaVector
	 * @return              {b1,1-b1,b2,1-b2,xa,ya,xb,yb}
	 *                      se nel settore sat 1sola staz=>xb,yb=-1
	 */
	public Vector<Sector2D> StatAss(Vector<Sector2D> sector) {
		//Util2d uti = new Util2d();
		Vector<Sector2D> out = new Vector<Sector2D>();
		// supponiamo di avere 2 intervalli [b1,1-b1]->[b2,1-b2] e [b3,1-b3]->[b4,1-b4]

		// se b1!=0 associo alla stazione A l'intervallo [0,1]->[b1,1-b1]
		if (sector.get(0).getBeta1() > EPSYLON) {
			Sector2D sss = new Sector2D(0, 1, sector.get(0).getBeta1(), sector.get(0).getBeta11(), sector.get(0).getP1());
			out.addElement(sss);
		}

		// Adesso procedo alla scansione di tutti gli intervalli
		for (int i = 0; i < (sector.size()); i++) {

			// Settore normale

			Sector2D sss = new Sector2D(sector.get(i).getBeta1(), sector.get(i).getBeta11(), sector.get(i).getBeta2(), sector.get(i).getBeta22(),
					sector.get(i).getP1(), sector.get(i).getP2());
			out.addElement(sss);

			// se gli estremi b2 e b3 non coincidono (a meno di EPSYLON) associo b2-->b3 alla stazione di b2
			if (i != (sector.size() - 1) //todo controllare l'estensione
					&& (sector.get(i + 1).getBeta1() - sector.get(i).getBeta2()) > EPSYLON) {
				Sector2D ss1 = new Sector2D(sector.get(i).getBeta2(), sector.get(i).getBeta22(), sector.get(i + 1).getBeta1(), sector.get(i + 1)
						.getBeta11(), sector.get(i).getP2());
				out.addElement(ss1);
			}

			//Associo all'ultima stazione l'intervallo b4->1 se b4!=0
			else if (i == (sector.size() - 1) && sector.get(i).getBeta2() != 1) {
				Sector2D ssf = new Sector2D(sector.get(i).getBeta2(), sector.get(i).getBeta22(), 1, 0, sector.get(i).getP2()); //aggiungere ,end
				out.addElement(ssf);
			}

		}
		return out;
	}

}
