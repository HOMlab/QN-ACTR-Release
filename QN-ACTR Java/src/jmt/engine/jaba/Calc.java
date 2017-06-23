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

import jmt.engine.jaba.Hull.ConvexHullException;
import jmt.engine.jaba.Hull.Vertex;

/**
 * Created by IntelliJ IDEA.
 * User: Andrea Zanzottera
 * Date: 1-ago-2005
 * Time: 10.30.07
 */
public class Calc {

	// Istanzio la classe ViewResults per stampare a video i risultati
	//ViewResults vres = new ViewResults();
	// Inizializzo il vettore che sarà poi passato dalla funzione faces3d
	Vector<newFace> faces = new Vector<newFace>();
	// Inizializzo il vettore dei lati da unire per il calcolo
	Vector<Object> lati = new Vector<Object>();
	// Inizializzo il vettore contenente i settori dove saturano 3 stazioni contemporaneamente
	Vector<Object> triangles = new Vector<Object>();
	// Inizializzo il vettore contenente i settori dove saturano 2 stazioni contemporaneamente
	Vector<Object> sett2staz = new Vector<Object>();
	// Inizializzo il vettore contenente i settori dove satura 1 stazione
	Vector<Object> sett1staz = new Vector<Object>();
	// Duplico il vettore dei vertici per fare dei controlli finali
	Vector<Vertex> original3D = new Vector<Vertex>();

	/**
	 * Passando un vettore di newPoint contenente le D di due classi il metodo restituisce
	 * un Vector di Sector2D con le informazioni sul mix di popolazione e le stazioni che
	 * saturano per quel mix.
	 *
	 * @param vertices2D
	 * @return un Vector di Sector2D
	 */
	public Vector<Sector2D> Calc2D(Vector<newPoint> vertices2D) {
		Vector<newPoint> original2D = new Vector<newPoint>(vertices2D);
		Util2d util = new Util2d();

		// Aggiungo le proiezioni
		vertices2D = util.LExplode2D(vertices2D);

		//Elimino i punti ridondanti (ALTRIMENTI GRAHAMSCAN CRASHA!!!)
		Vector<newPoint> verticesnew = new Vector<newPoint>();
		for (int i = 0; i < vertices2D.size(); i++) {
			if (util.VPresent(vertices2D.get(i), verticesnew) == false) {
				verticesnew.addElement(vertices2D.get(i));
			}
		}
		vertices2D = verticesnew;

		//Elimino i punti dominati
		vertices2D = util.DomRemove2D(vertices2D);

		// Aggiungo l'origine per rendere effettivamente convesso il poligono
		// Serve perchè il Graham Scan è stato pensato per ogni caso
		vertices2D.addElement(new newPoint(0, 0));

		// Lancio la funzione doGraham che chiede come parametro un vettore di
		// newPoint e restituisce il vettore lines di newPoint
		grahamScan gra = new grahamScan();
		vertices2D = gra.doGraham(vertices2D);

		//todo controllare meglio le proiezioni: caso (10,4) - (10,0)
		// bisogna fare in modo che un punto venga cmq scartato se è collineare
		//Elimino le proiezioni v1.5
		Vector<newPoint> verticesnp = new Vector<newPoint>();
		for (int i = 0; i < vertices2D.size(); i++) {
			if (util.VPresent(vertices2D.get(i), original2D)) {
				verticesnp.addElement(vertices2D.get(i));

			}
		}
		// Elimino le proiezioni nel caso siano collineari ad altri punti
		// Ad esempio (10,4) - (10,0) oppure (0,5) - (5,5)
		if (verticesnp.size() > 1) {
			verticesnp = util.RemoveCollinear(verticesnp);
		}

		//Faccio il mapping sui punti rimasti
		Beta2D b2d = new Beta2D();
		Vector<Sector2D> sector = new Vector<Sector2D>(); // è il vettore con le Beta
		sector = b2d.BetaVector(verticesnp);

		//System.out.println(sector.size());

		/*
		//Aggiungo i punti collineari "diagonali" inserendoli nel giusto posto
		if (verticesnp.size()>1 && original2D.size()>1)
		{
		for (int i=0;i<(verticesnp.size()-1);i++)
		{
		for (int j=0;j<original2D.size();j++)
		{
		// un vertice è buono quando non è già incluso,
		// è collineare ai 2 del settore
		// la sua x è maggiore del primo v del settore
		// la sua x è minore del secondo v del settore

		if (verticesnp.contains(original2D.get(j))==false &&
		util.Collinear((newPoint)verticesnp.get(i),(newPoint)verticesnp.get(i+1),(newPoint)original2D.get(j)) &&
		(((newPoint)verticesnp.get(i)).getX()<((newPoint)original2D.get(j)).getX()) &&
		(((newPoint)verticesnp.get(i+1)).getX()>((newPoint)original2D.get(j)).getX())
		)
		{
		verticesnp.insertElementAt(original2D.get(j),i);
		//i--;
		}
		}
		}
		}
		*/

		Vector<Sector2D> res = new Vector<Sector2D>();

		if (verticesnp.size() > 1) {
			// Associo ai settori le stazioni
			res = b2d.StatAss(sector);

			/*
			for (int i=0;i<sector.size();i++)
			System.out.println(sector.get(i));
			*/

			// Aggiunta dei punti collineari
			for (int i = 0; i < res.size(); i++) //per tutti i settori
			{
				for (int j = 0; j < original2D.size(); j++) //per tutti i punti originali
				{
					// Collineare Orizzontale Iniziale
					if (i == 0 && (res.get(i).getP1()).getX() != 0) {
						res.get(i).addCollinearFirst(res.get(i), original2D.get(j));
					}

					// Collieari Diagonali
					res.get(i).addCollinear(res.get(i), original2D.get(j));

					// Collineare sul lato verticale
					if (i == res.size() - 1 && (res.get(i).getP1()).getY() != 0) {
						res.get(i).addCollinearLast(res.get(i), original2D.get(j));
					}

				}
			}
		} else
		// Se satura soltanto una stazione
		{
			res.addElement(new Sector2D(1, 0, 0, 1, verticesnp.get(0)));
		}

		return res;
	}

	public Vector<Object> Calc2D(Vector<newPoint> vertices2D, String[] stationNames, String[] classNames) {
		Vector<newPoint> original2D = new Vector<newPoint>(vertices2D);
		Util2d util = new Util2d();

		//creo il vettore con le stazioni, serve per associare nome a coordinate
		Vector<Station2D> stations = new Vector<Station2D>();
		for (int i = 0; i < vertices2D.size(); i++) {
			Station2D stat = new Station2D(vertices2D.get(i), stationNames[i]);
			stations.addElement(stat);
		}
		// Aggiungo le proiezioni
		vertices2D = util.LExplode2D(vertices2D);

		//Elimino i punti ridondanti (ALTRIMENTI GRAHAMSCAN CRASHA!!!)
		Vector<newPoint> verticesnew = new Vector<newPoint>();
		for (int i = 0; i < vertices2D.size(); i++) {
			if (util.VPresent(vertices2D.get(i), verticesnew) == false) {
				verticesnew.addElement(vertices2D.get(i));
			}
		}
		vertices2D = verticesnew;

		//Elimino i punti dominati
		vertices2D = util.DomRemove2D(vertices2D);

		// Aggiungo l'origine per rendere effettivamente convesso il poligono
		// Serve perchè il Graham Scan è stato pensato per ogni caso
		vertices2D.addElement(new newPoint(0, 0));

		// Lancio la funzione doGraham che chiede come parametro un vettore di
		// newPoint e restituisce il vettore lines di newPoint

		grahamScan gra = new grahamScan();
		vertices2D = gra.doGraham(vertices2D);

		// todo controllare è il nuovo (e non funziona!)
		/*
		newGraham gra = new newGraham();
		vertices2D=gra.GrahamScan(vertices2D);
		*/

		//todo controllare meglio le proiezioni: caso (10,4) - (10,0)
		// bisogna fare in modo che un punto venga cmq scartato se è collineare
		//Elimino le proiezioni v1.5
		Vector<newPoint> verticesnp = new Vector<newPoint>();
		for (int i = 0; i < vertices2D.size(); i++) {
			if (util.VPresent(vertices2D.get(i), original2D)) {
				verticesnp.addElement(vertices2D.get(i));

			}
		}
		// Elimino le proiezioni nel caso siano collineari ad altri punti
		// Ad esempio (10,4) - (10,0) oppure (0,5) - (5,5)
		if (verticesnp.size() > 1) {
			verticesnp = util.RemoveCollinear(verticesnp);
		}

		//Faccio il mapping sui punti rimasti
		Beta2D b2d = new Beta2D();
		Vector<Sector2D> sector = new Vector<Sector2D>(); // è il vettore con le Beta
		sector = b2d.BetaVector(verticesnp);

		//System.out.println(sector.size());

		/*
		//Aggiungo i punti collineari "diagonali" inserendoli nel giusto posto
		if (verticesnp.size()>1 && original2D.size()>1)
		{
		for (int i=0;i<(verticesnp.size()-1);i++)
		{
		for (int j=0;j<original2D.size();j++)
		{
		// un vertice è buono quando non è già incluso,
		// è collineare ai 2 del settore
		// la sua x è maggiore del primo v del settore
		// la sua x è minore del secondo v del settore

		if (verticesnp.contains(original2D.get(j))==false &&
		util.Collinear((newPoint)verticesnp.get(i),(newPoint)verticesnp.get(i+1),(newPoint)original2D.get(j)) &&
		(((newPoint)verticesnp.get(i)).getX()<((newPoint)original2D.get(j)).getX()) &&
		(((newPoint)verticesnp.get(i+1)).getX()>((newPoint)original2D.get(j)).getX())
		)
		{
		verticesnp.insertElementAt(original2D.get(j),i);
		//i--;
		}
		}
		}
		}
		*/

		Vector<Sector2D> res = new Vector<Sector2D>();

		if (verticesnp.size() > 1) {
			// Associo ai settori le stazioni
			res = b2d.StatAss(sector);

			/*
			for (int i=0;i<sector.size();i++)
			System.out.println(sector.get(i));
			*/

			// Aggiunta dei punti collineari
			for (int i = 0; i < res.size(); i++) //per tutti i settori
			{
				for (int j = 0; j < original2D.size(); j++) //per tutti i punti originali
				{
					// Collineare Orizzontale Iniziale
					if (i == 0 && (res.get(i).getP1()).getX() != 0) {
						res.get(i).addCollinearFirst(res.get(i), original2D.get(j));
					}

					// Collieari Diagonali
					res.get(i).addCollinear(res.get(i), original2D.get(j));

					// Collineare sul lato verticale
					if (i == res.size() - 1 && (res.get(i).getP1()).getY() != 0) {
						res.get(i).addCollinearLast(res.get(i), original2D.get(j));
					}

				}
			}
		} else
		// Se satura soltanto una stazione
		{
			res.addElement(new Sector2D(1, 0, 0, 1, verticesnp.get(0)));
		}

		Vector<Object> finalres = new Vector<Object>();

		for (int i = 0; i < res.size(); i++) {
			FinalSect2D fs = new FinalSect2D(res.get(i), stations, classNames);
			finalres.addElement(fs);
		}

		return finalres;
	}

	/**
	 * CASO 3 CLASSI
	 *
	 */
	public Vector<Object> Calc3D(Vector<Vertex> vertices) throws ConvexHullException {
		Vector<Object> out = new Vector<Object>();
		// I vertici sono contentuti in vertices(si modifica coi calcoli) e original
		// Le facce del CHull sono in faces
		// I settori triangolari in cui saturano 3 stazioni sono in triangles
		// I settori quadrangolari in cui saturano 2 stazioni sono in sett2staz

		original3D = vertices;

		Vector<newPoint> vertices2Dxy = new Vector<newPoint>();
		Vector<newPoint> vertices2Dxz = new Vector<newPoint>();
		Vector<newPoint> vertices2Dyz = new Vector<newPoint>();

		Faces3D faces3d = new Faces3D();

		// Istanzio la classe Beta3D
		Beta3D b3d = new Beta3D();

		// Istanzio la classe Segment3D
		Segment3D seg = new Segment3D();

		// Inizializzo il nome del file pdf che sarà creato
		String name = "noname";

		//------------------------------------- START  ----------------------------------------------------//

		// Se c'è una sola stazione oppure una sola è dominante finisce qui
		OneDominator od = new OneDominator();
		od.setVertices(vertices);
		if (od.IsOneDominator()) {
			Vertex dom = od.getDominator();
			//System.out.println(dom);
			sett1staz = b3d.OneDominator(od.getDominator());

			out.addElement(faces);
			out.addElement(original3D);
			out.addElement(triangles);
			out.addElement(sett2staz);
			out.addElement(sett1staz);
			out.addElement(name);

			return out;
		}

		if (vertices.size() == 1) {
			sett1staz = b3d.OneDominator(vertices.get(0));

			out.addElement(faces);
			out.addElement(original3D);
			out.addElement(triangles);
			out.addElement(sett2staz);
			out.addElement(sett1staz);
			out.addElement(name);

			return out;
		}

		//------------------------------------- FASE 3D   -----------------------------------------------//

		// Controllo che ci siano almeno 3 stazioni prima di fare il CHull
		if (vertices.size() > 2) {

			// Esplodo il vettore dei vertici creando anche le proiezioni
			vertices = faces3d.LExplode3D(vertices);

			// Chiamo il metodo Hull3D della classe faces3d che mi restituisce
			// le facce del poligono già contrassegnando quelle complanari
			faces = faces3d.Hull3D(vertices);

			// Controllo che ci sia almeno una faccia
			if (faces.size() > 0) {
				// Tolgo le facce contenenti proiezioni
				faces = faces3d.RemoveP(faces, original3D);

				// Associazione delle Beta alle facce triangolari
				triangles = b3d.BetaTriangles(faces);

				// Genero i lati da unire
				lati = seg.CreateLt(triangles);

				// Join dei triangoli
				if (triangles.size() > 1) {
					sett2staz = b3d.JoinTriangles(triangles);
				}

				// Tolgo i lati usati dal join e aggiungo quelli creati
				lati = seg.FixLtFromJoin(sett2staz, lati);

				//------------------------------------- FASE 2D ----------------------------------------------------//

			}// if (faces.size()>0)
		}//if (vertices.size()>2)
		//else System.out.println("non ci sono abbastanza vertici");

		vertices2Dxy = faces3d.VertexRemoveZ(original3D);
		vertices2Dxz = faces3d.VertexRemoveY(original3D);
		vertices2Dyz = faces3d.VertexRemoveX(original3D);

		Vector<Sector2D> resxy = new Vector<Sector2D>();
		Vector<Sector2D> resxz = new Vector<Sector2D>();
		Vector<Sector2D> resyz = new Vector<Sector2D>();

		Calc calcolo2d = new Calc();

		// Genero il lato del triangolo con beta3=0
		resxy = calcolo2d.Calc2D(vertices2Dxy);

		// Genero il lato del triangolo con beta2=0
		resxz = calcolo2d.Calc2D(vertices2Dxz);

		// Genero il lato del triangolo con beta1=0
		resyz = calcolo2d.Calc2D(vertices2Dyz);

		// Differenzio la procedura nel caso ci sia un settore con 3 stazioni saturanti contemp.
		// dal quale partire per unire i segmenti a lato del triangolo o meno.

		//LATI XY
		Vector<Vector<Object>> newresxy = new Vector<Vector<Object>>();
		Vector<Object> latixy = new Vector<Object>();
		latixy = seg.FixLtFrom2Dxy(resxy);

		//LATI XZ
		Vector<Vector<Object>> newresxz = new Vector<Vector<Object>>();
		Vector<Object> latixz = new Vector<Object>();
		latixz = seg.FixLtFrom2Dxz(resxz);

		//LATI YZ
		Vector<Vector<Object>> newresyz = new Vector<Vector<Object>>();
		Vector<Object> latiyz = new Vector<Object>();
		latiyz = seg.FixLtFrom2Dyz(resyz);

		if (faces.size() > 0) {
			newresxy = b3d.Join2Statxy(latixy, lati, sett2staz);
			sett2staz = newresxy.get(0);
			lati = newresxy.get(1);
			latixy = newresxy.get(2);

			newresxz = b3d.Join2Statxz(latixz, lati, sett2staz);
			sett2staz = newresxz.get(0);
			lati = newresxz.get(1);
			latixz = newresxz.get(2);

			newresyz = b3d.Join2Statyz(latiyz, lati, sett2staz);
			sett2staz = newresyz.get(0);
			lati = newresyz.get(1);
			latiyz = newresyz.get(2);

			/*
			// Stampa dei settori in cui saturano 2 stazioni
			System.out.println("Numero di settori in cui saturano 2 stazioni contemp.: "+sett2staz.size());
			vres.ViewRes3D2Stat(sett2staz,original3D);
			*/

			//------------------------------ 1 STAZIONE --------------------------------------------------------
			sett1staz = b3d.Join1Staz(latixy, latixz, latiyz, lati, sett1staz);

			sett1staz = b3d.Join1Staz(latixy, latixz, latiyz, lati, sett1staz);

			//------------------------------ FACCE COMPLANARI -------------------------------------------------

			// Controllo se ci sono facce complanari
			if (faces3d.ExistsComplanar(faces)) {
				//System.out.println("ESISTONO FACCE COMPLANARI");
				triangles = b3d.JoinComplanars(triangles);
				//System.out.println(triangles.size());
				sett2staz = b3d.DeleteFake(sett2staz);
			}
			//todo creare metodo che aggiunge stazioni complanari ulteriori
			// (ossia quelle che pur essendo complanari vengono scartate dal cHull)

		}// if faces.size()>0

		//------------------------------ CASO IN CUI NON CI SONO FACCE ------------------------------------

		else //(if faces.size==0)
		{
			Vector<Vector<Object>> resN31 = new Vector<Vector<Object>>();
			//System.out.println("Non ci sono facce");

			// Ricerca dei settori in cui saturano 2 stazioni
			resN31 = b3d.Join2StazN3(latixy, latixz, latiyz, sett1staz, sett2staz);

			//System.out.println("resN31.size(): "+resN31.size());

			lati = resN31.get(0);
			latixy = resN31.get(1);
			latixz = resN31.get(2);
			latiyz = resN31.get(3);
			sett1staz = resN31.get(4);
			sett2staz = resN31.get(5);

			sett1staz = b3d.Join1Staz(latixy, latixz, latiyz, lati, sett1staz);
			sett1staz = b3d.Join1Staz(latixy, latixz, latiyz, lati, sett1staz);

		}

		/*
		//Generazione dei risultati
		out.addElement(faces);
		out.addElement(original3D);
		out.addElement(triangles);
		out.addElement(sett2staz);
		out.addElement(sett1staz);
		out.addElement(name);
		*/

		Mapping3D map = new Mapping3D();
		map.RemapAllSectors(triangles);
		map.RemapAllSectors(sett2staz);
		map.RemapAllSectors(sett1staz);

		Vector<Object> allres = new Vector<Object>();
		for (int i = 0; i < sett1staz.size(); i++) {
			allres.addElement(sett1staz.get(i));
		}
		for (int i = 0; i < sett2staz.size(); i++) {
			allres.addElement(sett2staz.get(i));
		}
		for (int i = 0; i < triangles.size(); i++) {
			allres.addElement(triangles.get(i));
		}

		return allres;
		//return out;
	}

	public Vector<Object> Calc3D(Vector<Vertex> vertices, String[] stationNames, String[] classNames) throws ConvexHullException {
		Vector<Object> out = new Vector<Object>();
		// I vertici sono contentuti in vertices(si modifica coi calcoli) e original
		// Le facce del CHull sono in faces
		// I settori triangolari in cui saturano 3 stazioni sono in triangles
		// I settori quadrangolari in cui saturano 2 stazioni sono in sett2staz

		Vector<Station3D> stations = new Vector<Station3D>();

		// Creo un vettore con le stazioni associate ai loro nomi
		for (int i = 0; i < vertices.size(); i++) {
			stations.add(new Station3D(stationNames[i], vertices.get(i)));
		}

		original3D = vertices;

		Vector<newPoint> vertices2Dxy = new Vector<newPoint>();
		Vector<newPoint> vertices2Dxz = new Vector<newPoint>();
		Vector<newPoint> vertices2Dyz = new Vector<newPoint>();

		Faces3D faces3d = new Faces3D();

		// Istanzio la classe Beta3D
		Beta3D b3d = new Beta3D();

		// Istanzio la classe Segment3D
		Segment3D seg = new Segment3D();

		// Inizializzo il nome del file pdf che sarà creato
		String name = "noname";

		//------------------------------------- START  ----------------------------------------------------//

		// Se c'è una sola stazione oppure una sola è dominante finisce qui
		OneDominator od = new OneDominator();
		od.setVertices(vertices);
		if (od.IsOneDominator()) {
			Vertex dom = od.getDominator();
			//System.out.println(dom);
			sett1staz = b3d.OneDominator(od.getDominator());

			//out.addElement(faces);
			out.addElement(original3D);
			//out.addElement(triangles);
			//out.addElement(sett2staz);
			out.addElement(sett1staz);
			//out.addElement(name);
			Mapping3D map = new Mapping3D();
			//map.RemapAllSectors(triangles);
			//map.RemapAllSectors(sett2staz);
			map.RemapAllSectors(sett1staz);

			Vector<Object> allres = new Vector<Object>();
			for (int i = 0; i < sett1staz.size(); i++) {
				allres.addElement(sett1staz.get(i));
			}/*
				           for (int i = 0; i<sett2staz.size();i++){
				               allres.addElement((Sector3D)sett2staz.get(i));
				           }
				           for (int i = 0; i<triangles.size();i++){
				               allres.addElement((Sector3D)triangles.get(i));
				           }*/

			//System.out.println("allres.size() = "+allres.size());
			for (int i = 0; i < allres.size(); i++) {
				((Sector3D) allres.get(i)).givename(stations);
				((Sector3D) allres.get(i)).setClassNames(classNames);
			}

			return allres;
			//todo il nome e tutto il resto!
			//return out;
		}

		if (vertices.size() == 1) {
			sett1staz = b3d.OneDominator(vertices.get(0));

			out.addElement(faces);
			out.addElement(original3D);
			out.addElement(triangles);
			out.addElement(sett2staz);
			out.addElement(sett1staz);
			out.addElement(name);

			return out;
		}

		//------------------------------------- FASE 3D   -----------------------------------------------//

		// Controllo che ci siano almeno 3 stazioni prima di fare il CHull
		if (vertices.size() > 2) {

			// Esplodo il vettore dei vertici creando anche le proiezioni
			vertices = faces3d.LExplode3D(vertices);

			// Chiamo il metodo Hull3D della classe faces3d che mi restituisce
			// le facce del poligono già contrassegnando quelle complanari
			faces = faces3d.Hull3D(vertices);

			// Controllo che ci sia almeno una faccia
			if (faces.size() > 0) {
				// Tolgo le facce contenenti proiezioni
				faces = faces3d.RemoveP(faces, original3D);

				// Associazione delle Beta alle facce triangolari
				triangles = b3d.BetaTriangles(faces);

				// Genero i lati da unire
				lati = seg.CreateLt(triangles);

				// Join dei triangoli
				if (triangles.size() > 1) {
					sett2staz = b3d.JoinTriangles(triangles);
				}

				// Tolgo i lati usati dal join e aggiungo quelli creati
				lati = seg.FixLtFromJoin(sett2staz, lati);

				//------------------------------------- FASE 2D ----------------------------------------------------//

			}// if (faces.size()>0)
		}//if (vertices.size()>2)
		//else System.out.println("non ci sono abbastanza vertici");

		vertices2Dxy = faces3d.VertexRemoveZ(original3D);
		vertices2Dxz = faces3d.VertexRemoveY(original3D);
		vertices2Dyz = faces3d.VertexRemoveX(original3D);

		Vector<Sector2D> resxy = new Vector<Sector2D>();
		Vector<Sector2D> resxz = new Vector<Sector2D>();
		Vector<Sector2D> resyz = new Vector<Sector2D>();

		Calc calcolo2d = new Calc();

		// Genero il lato del triangolo con beta3=0
		resxy = calcolo2d.Calc2D(vertices2Dxy);

		// Genero il lato del triangolo con beta2=0
		resxz = calcolo2d.Calc2D(vertices2Dxz);

		// Genero il lato del triangolo con beta1=0
		resyz = calcolo2d.Calc2D(vertices2Dyz);

		// Differenzio la procedura nel caso ci sia un settore con 3 stazioni saturanti contemp.
		// dal quale partire per unire i segmenti a lato del triangolo o meno.

		//LATI XY
		Vector<Vector<Object>> newresxy = new Vector<Vector<Object>>();
		Vector<Object> latixy = new Vector<Object>();
		latixy = seg.FixLtFrom2Dxy(resxy);

		//LATI XZ
		Vector<Vector<Object>> newresxz = new Vector<Vector<Object>>();
		Vector<Object> latixz = new Vector<Object>();
		latixz = seg.FixLtFrom2Dxz(resxz);

		//LATI YZ
		Vector<Vector<Object>> newresyz = new Vector<Vector<Object>>();
		Vector<Object> latiyz = new Vector<Object>();
		latiyz = seg.FixLtFrom2Dyz(resyz);

		if (faces.size() > 0) {
			newresxy = b3d.Join2Statxy(latixy, lati, sett2staz);
			sett2staz = newresxy.get(0);
			lati = newresxy.get(1);
			latixy = newresxy.get(2);

			newresxz = b3d.Join2Statxz(latixz, lati, sett2staz);
			sett2staz = newresxz.get(0);
			lati = newresxz.get(1);
			latixz = newresxz.get(2);

			newresyz = b3d.Join2Statyz(latiyz, lati, sett2staz);
			sett2staz = newresyz.get(0);
			lati = newresyz.get(1);
			latiyz = newresyz.get(2);

			/*
			// Stampa dei settori in cui saturano 2 stazioni
			System.out.println("Numero di settori in cui saturano 2 stazioni contemp.: "+sett2staz.size());
			vres.ViewRes3D2Stat(sett2staz,original3D);
			*/

			//------------------------------ 1 STAZIONE --------------------------------------------------------
			sett1staz = b3d.Join1Staz(latixy, latixz, latiyz, lati, sett1staz);

			sett1staz = b3d.Join1Staz(latixy, latixz, latiyz, lati, sett1staz);

			//------------------------------ FACCE COMPLANARI -------------------------------------------------

			// Controllo se ci sono facce complanari
			if (faces3d.ExistsComplanar(faces)) {
				//System.out.println("ESISTONO FACCE COMPLANARI");
				triangles = b3d.JoinComplanars(triangles);
				//System.out.println(triangles.size());
				sett2staz = b3d.DeleteFake(sett2staz);
			}
			//todo creare metodo che aggiunge stazioni complanari ulteriori
			// (ossia quelle che pur essendo complanari vengono scartate dal cHull)

		}// if faces.size()>0

		//------------------------------ CASO IN CUI NON CI SONO FACCE ------------------------------------

		else //(if faces.size==0)
		{
			Vector<Vector<Object>> resN31 = new Vector<Vector<Object>>();
			//System.out.println("Non ci sono facce");

			// Ricerca dei settori in cui saturano 2 stazioni
			resN31 = b3d.Join2StazN3(latixy, latixz, latiyz, sett1staz, sett2staz);

			//System.out.println("resN31.size(): "+resN31.size());

			lati = resN31.get(0);
			latixy = resN31.get(1);
			latixz = resN31.get(2);
			latiyz = resN31.get(3);
			sett1staz = resN31.get(4);
			sett2staz = resN31.get(5);

			sett1staz = b3d.Join1Staz(latixy, latixz, latiyz, lati, sett1staz);
			sett1staz = b3d.Join1Staz(latixy, latixz, latiyz, lati, sett1staz);

		}

		//------------------------------ MAPPATURA NELLE 2 DIMENSIONI DELLO SPAZIO BETA -------------------

		Mapping3D map = new Mapping3D();
		if (faces.size() > 0) {
			map.RemapAllSectors(triangles);
		}
		map.RemapAllSectors(sett2staz);
		map.RemapAllSectors(sett1staz);

		Vector<Object> allres = new Vector<Object>();
		for (int i = 0; i < sett1staz.size(); i++) {
			allres.addElement(sett1staz.get(i));
		}
		for (int i = 0; i < sett2staz.size(); i++) {
			allres.addElement(sett2staz.get(i));
		}
		if (faces.size() > 0) {
			for (int i = 0; i < triangles.size(); i++) {
				allres.addElement(triangles.get(i));
			}
		}

		//System.out.println("allres.size() = "+allres.size());
		for (int i = 0; i < allres.size(); i++) {
			((Sector3D) allres.get(i)).givename(stations);
			((Sector3D) allres.get(i)).setClassNames(classNames);
		}

		return allres;
		//return out;
	}

	// Metodi per avere i risultati
	public Vector<newFace> getFaces() {
		return faces;
	}

	public Vector<Object> gettriangles() {
		return triangles;
	}

	public Vector<Object> getsett1staz() {
		return sett1staz;
	}

	public Vector<Object> getsett2staz() {
		return sett2staz;
	}

}
