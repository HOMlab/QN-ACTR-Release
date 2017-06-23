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

import jmt.engine.jaba.Hull.ConvexHull;
import jmt.engine.jaba.Hull.ConvexHullException;
import jmt.engine.jaba.Hull.Polygon;
import jmt.engine.jaba.Hull.Vertex;

/**
 * Created by IntelliJ IDEA.
 * User: Andrea
 * Date: 15-giu-2005
 * Time: 17.55.46
 * To change this template use File | Settings | File Templates.
 */

public class Faces3D {

	/**
	 * Questo metodo restituisce una vettore con i 3 vertici della faccia e il "colore"
	 * @param vertices
	 * @return
	 * @throws ConvexHullException
	 */
	public Vector<newFace> Hull3D(Vector<Vertex> vertices) throws ConvexHullException {
		Vector<newFace> newfaces = new Vector<newFace>();

		// Lancio del metodo ConvexHull
		ConvexHull hull = new ConvexHull(vertices);

		// Recupero delle informazioni
		Vector<Polygon> faces = new Vector<Polygon>(hull.getFaces());
		Vector vertoffaces = new Vector();
		Vector<Vertex> vertof0 = new Vector<Vertex>(vertoffaces);

		for (int k = 0; k < faces.size(); k++) {
			vertof0 = faces.get(k).getVertices();
			newFace sect0 = new newFace(vertof0.get(0), vertof0.get(1), vertof0.get(2), k);
			newfaces.addElement(sect0);
		}

		int NSettori = newfaces.size();

		// Controllo della complanarità ed assegnazione dei colori
		for (int i = 0; i < NSettori; i++) {
			//TODO Meglio mettere j=i o j=0???
			for (int j = i; j < NSettori; j++) {
				if (newfaces.get(i).Complanar(newfaces.get(i), newfaces.get(j)) && i != j
						&& newfaces.get(i).confSect(newfaces.get(i), newfaces.get(j))) {
					newfaces.get(j).setContr(newfaces.get(i).getContr());
				}
			}
		}

		// A questo punto si ha a disposizione il vettore newfaces (che è un vettore di newFace),
		// con al suo interno i vertici delle facce e in ultimo il "colore" associato.

		return newfaces;
	}

	/**
	 * Il metodo "esplode" un vettore di vertici 3D (vertex) aggiungendo le proiezioni sugli
	 * assi di tutti i punti.
	 *
	 * @param lin       Il vettore di vertici passato
	 * @return          Un vettore di vertici contenente anche le proiezioni sugli assi
	 */
	public Vector<Vertex> LExplode3D(Vector<Vertex> lin) {
		// Creo un vettore L contenente le coordinate e le loro proiezioni sugli assi
		Vector<Vertex> lout = new Vector<Vertex>();

		// Cerco le coordinate di ogni vertice passato e creo le proiezioni
		int[] coord = {};

		for (int i = 0; i < lin.size(); i++) {

			coord = lin.get(i).getCoords();

			lout.addElement(new Vertex(coord[0], coord[1], coord[2])); //x,y,z
			lout.addElement(new Vertex(coord[0], 0, 0)); //x,0,0
			lout.addElement(new Vertex(coord[0], coord[1], 0)); //x,y,0
			lout.addElement(new Vertex(coord[0], 0, coord[2])); //x,0,z
			lout.addElement(new Vertex(0, coord[1], 0)); //0,y,0
			lout.addElement(new Vertex(0, coord[1], coord[2])); //0,y,z
			lout.addElement(new Vertex(0, 0, coord[2])); //0,0,z

		}

		return lout;
	}

	/**
	 * Il metodo toglie da un vettore di vertici 3D (vertex) le proiezioni confrontandolo
	 * con il vettore dei vertici originali
	 * @param lin
	 * @param ori
	 * @return
	 */
	public Vector LImplode3D(Vector lin, Vector ori) {
		Vector out = new Vector();
		for (int i = 0; i < lin.size(); i++) {
			for (int j = 0; j < ori.size(); j++) {
				if (lin.get(i).equals(ori.get(j))) {
					out.addElement(lin.get(i));
				}
			}
		}
		return out;
	}

	/**
	 * Controlla se i due vertici v1 e v2 sono uguali
	 *
	 * @param v1
	 * @param v2
	 * @return      true se sono uguali
	 */
	public boolean SameVertex(Vertex v1, Vertex v2) {
		boolean out = false;
		int[] v1c = v1.getCoords();
		int[] v2c = v2.getCoords();
		if (v1c[0] == v2c[0] && v1c[1] == v2c[1] && v1c[2] == v2c[2]) {
			out = true;
		}
		return out;
	}

	/**
	 * Controlla se il vertice v è presente nel vettore di vertici vertices
	 *
	 * @param v
	 * @param vertices
	 * @return      true se v è presente in vertices
	 */
	public boolean HasVertex(Vertex v, Vector<Vertex> vertices) {
		boolean out = false;
		for (int i = 0; i < vertices.size(); i++) {
			if (SameVertex(v, vertices.get(i))) {
				out = true;
				break;
			}
		}
		return out;
	}

	/**
	 * Rimuove dal vettore di newFace faces le facce che non sono composte solo da
	 * punti contenuti nel vettore di vertici ori.
	 *
	 * Usato per rimuovere le facce contenenti proiezioni.
	 *
	 * @param faces
	 * @param ori
	 * @return      un vettore con le facce rimaste
	 */
	public Vector<newFace> RemoveP(Vector<newFace> faces, Vector<Vertex> ori) {
		Vector<newFace> out2 = new Vector<newFace>();
		for (int i = 0; i < faces.size(); i++) {
			if ((HasVertex((faces.get(i).getV0()), ori)) && HasVertex((faces.get(i).getV1()), ori) && HasVertex((faces.get(i).getV2()), ori)) {
				out2.addElement(faces.get(i));
			}

		}
		return out2;
	}

	/**
	 * Trasforma un vettore di vertici 2D in un vettore di punti 2D scartando la coord z
	 * @param vertices
	 * @return
	 */
	public Vector<newPoint> VertexRemoveZ(Vector<Vertex> vertices) {
		Vector<newPoint> out = new Vector<newPoint>();

		for (int i = 0; i < vertices.size(); i++) {
			int[] coord = vertices.get(i).getCoords();
			newPoint p = new newPoint(coord[0], coord[1]);
			out.addElement(p);
		}
		return out;
	}

	public Vector<newPoint> VertexRemoveY(Vector<Vertex> vertices) {
		Vector<newPoint> out = new Vector<newPoint>();

		for (int i = 0; i < vertices.size(); i++) {
			int[] coord = vertices.get(i).getCoords();
			newPoint p = new newPoint(coord[0], coord[2]);
			out.addElement(p);
		}
		return out;
	}

	public Vector<newPoint> VertexRemoveX(Vector<Vertex> vertices) {
		Vector<newPoint> out = new Vector<newPoint>();

		for (int i = 0; i < vertices.size(); i++) {
			int[] coord = vertices.get(i).getCoords();
			newPoint p = new newPoint(coord[1], coord[2]);
			out.addElement(p);
		}
		return out;
	}

	/**
	 * Controlla se ci sono delle facce che giacciono sullo stesso piano.
	 * @param s
	 * @return
	 */
	public boolean ExistsComplanar(Vector<newFace> s) {
		boolean out = false;
		if (s.size() < 2) {
			return false;
		} else {
			for (int i = 0; i < s.size(); i++) {
				for (int j = i + 1; j < s.size(); j++) {
					{
						if (s.get(i).getContr() == s.get(j).getContr()) {
							out = true;
						}
					}
				}
			}
		}
		return out;
	}

}
