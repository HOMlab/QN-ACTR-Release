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

package jmt.gui.jmodel.JGraphMod;

import java.awt.Rectangle;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.List;

import jmt.gui.jmodel.controller.Mediator;

import org.jgraph.graph.DefaultEdge;
import org.jgraph.graph.DefaultGraphModel;
import org.jgraph.graph.DefaultPort;
import org.jgraph.graph.EdgeView;
import org.jgraph.graph.GraphConstants;
import org.jgraph.graph.GraphModel;

/**
 * <p>Title: JmtEdge connection structure</p>
 * <p>Description: This class is used to connect two elements into JmtGraph. It is designed to
 * store keys of source and target stations that are used when deleting or copying a connection</p>
 * 
 * @author Bertoli Marco
 *         Date: 17-giu-2005
 *         Time: 19.25.45
 * 
 * @author Giuseppe De Cicco & Fabio Granara
 * 		Date: 23-sett-2006
 * 
 */
public class JmtEdge extends DefaultEdge {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected Object sourceKey;
	protected Object targetKey;
	protected List latiDiIntersezione = new ArrayList();

	//	these variables are used to know where the edge intersect the cell
	protected boolean upperSide = false;
	protected boolean lowerSide = false;
	protected boolean rightSide = false;
	protected boolean isRing = false;
	protected boolean leftSide = false;
	protected boolean pointSharedModified = false;
	//    ______________________end____________________ 

	protected ArrayList<Point2D> intersectionVertexPoint = null;
	protected Point2D intersectionPoint = null;
	private Mediator mediator;
	public ArrayList<Point2D> intersectionEdgePoint = new ArrayList<Point2D>();

	//    public ArrayList intersectionEdgePoint2=new ArrayList();

	public JmtEdge() {
		super("");
	}

	public JmtEdge(Object o) {
		super(o);
	}

	/**
	 * Creates a new JmtEdge connecting source to target
	 * @param sourceKey key of source station
	 * @param targetKey key of target station
	 * @param mediator 
	 */
	public JmtEdge(Object sourceKey, Object targetKey, Mediator mediator) {

		this();
		this.sourceKey = sourceKey;
		this.targetKey = targetKey;
		this.mediator = mediator;
	}

	/**
	 * Gets source station search key
	 * @return source key
	 */
	public Object getSourceKey() {
		return sourceKey;
	}

	/**
	 * Gets target station search key
	 * @return target key
	 */
	public Object getTargetKey() {
		return targetKey;
	}

	//  Giuseppe De Cicco & Fabio Granara
	public ArrayList<Point2D> getIntersectionVertexPoint() {
		return intersectionVertexPoint;
	}

	//  Giuseppe De Cicco & Fabio Granara
	public boolean intersects(EdgeView viewtmp, Rectangle2D rett) {
		double maxX = (int) rett.getMaxX();
		double maxY = (int) rett.getMaxY();
		double minX = (int) rett.getMinX();
		double minY = (int) rett.getMinY();
		//    	System.out.println("maxX="+maxX);
		//    	System.out.println("minX="+minX);
		//    	System.out.println("maxY="+maxY);
		//    	System.out.println("minY="+minY);
		int controlPoint = viewtmp.getPointCount();
		boolean intersecato = false;
		leftSide = false;
		upperSide = false;
		rightSide = false;
		lowerSide = false;
		intersectionVertexPoint = new ArrayList<Point2D>();
		Point2D upperPoint = null;
		Point2D leftPoint = null;
		Point2D rightPoint = null;
		Point2D lowPoint = null;
		JmtCell source = (JmtCell) ((DefaultPort) this.getSource()).getParent();
		JmtCell target = (JmtCell) ((DefaultPort) this.getTarget()).getParent();
		Rectangle boundsSource = GraphConstants.getBounds(source.getAttributes()).getBounds();
		Rectangle boundsTarget = GraphConstants.getBounds(target.getAttributes()).getBounds();
		//    	Verifico se la cella e' il source o il target.
		if (boundsSource.equals(rett) || (boundsTarget.equals(rett))) {
			return false;
		}

		Point2D[] controlPoints = new Point2D[controlPoint];
		for (int i = 0; i < controlPoint; i++) {
			controlPoints[i] = viewtmp.getPoint(i);
			if ((controlPoints[i] == null)) {
				return false;
			}
			//Immagazzino le coordinate dei Control Point in un array di Point2D
			controlPoints[i].setLocation((int) controlPoints[i].getX(), (int) controlPoints[i].getY());
			//    		System.out.println("ControlPoint "+(i+1)+" X= "+controlPoints[i].getX()+" Y= "+controlPoints[i].getY());
		}

		//    	_____________INIZIO_______________
		//    	ArrayList intersectionVertexPoint2=new ArrayList();
		for (int i = 0; i < (controlPoint - 1); i++) {
			//    		System.out.println("Valore di i="+i+" e di i+1 ="+(i+1));
			Point2D tmpMin = controlPoints[i];
			Point2D tmpMax = controlPoints[i + 1];
			//nel caso di lati che vanno nel verso oposto
			if ((tmpMin.getX() > tmpMax.getX()) || (tmpMin.getY() > tmpMax.getY())) {
				tmpMin = controlPoints[i + 1];
				tmpMax = controlPoints[i];
			}
			//    		System.out.println("TmpMin : x="+tmpMin.getX()+", Y="+tmpMin.getY());

			//    		System.out.println("TmpMax : x="+tmpMax.getX()+", Y="+tmpMax.getY());

			if (!((maxX < tmpMin.getX()) || (minX > tmpMax.getX()) || (minY > tmpMax.getY()) || (maxY < tmpMin.getY()))) {
				intersecato = true;
				//da qua devo distinguere se la scansione avviene in orizzontale o in verticale, in quanto l'intersezione e' stata verificata
				//    			se il resto e' 0 allora scansione orizzontale
				if (!(i % 2 == 0)) {

					if (minY < tmpMax.getY() && minY > tmpMin.getY()) {
						upperSide = true;
						//    					System.out.println("Valore del controlMin "+(int)tmpMin.getX());
						//    					System.out.println("Valore del controlMax "+tmpMax.getX());
						upperPoint = new Point2D.Double((int) tmpMin.getX(), (int) minY);
						intersectionVertexPoint.add(upperPoint);
						//    					System.out.println("upper INTERSECTION");
						//    					System.out.println("Intersezione Sopra: x="+tmpMin.getX()+", y="+minY);
					}
					if (maxY < tmpMax.getY() && maxY > tmpMin.getY()) {
						lowerSide = true;
						lowPoint = new Point2D.Double((int) tmpMin.getX(), (int) maxY);
						intersectionVertexPoint.add(lowPoint);

						//    					System.out.println("lower INTERSECTION");
						//    					System.out.println("Intersezione Sotto: x="+tmpMin.getX()+", y="+maxY);
					}

				} else {
					if (minX < tmpMax.getX() && minX > tmpMin.getX()) {
						//    					System.out.println("LEFT INTERSECTION");
						leftSide = true;
						//    					Il punto di intersezione sara
						//    					System.out.println("Intersezione Sinistra: x="+minX+", y="+tmpMin.getY());
						leftPoint = new Point2D.Double((int) minX, (int) tmpMin.getY());
						intersectionVertexPoint.add(leftPoint);
					}
					if (maxX < tmpMax.getX() && maxX > tmpMin.getX()) {
						rightSide = true;
						//    					System.out.println("Intersezione destra: x="+maxX+", y="+tmpMin.getY());
						rightPoint = new Point2D.Double((int) maxX, (int) tmpMin.getY());
						intersectionVertexPoint.add(rightPoint);
						//    					System.out.println("RIGHT INTERSECTION");
					}
				}

			}

		}

		//    	if(intersectionVertexPoint2.size()>0){
		//    		System.out.println("Subito dopo il for: primo punto: x= "+((Point2D)(intersectionVertexPoint2.get(0))).getX()+", y="+((Point2D)(intersectionVertexPoint2.get(0))).getY());
		//    		System.out.println("Subito dopo il for: secondo punto: x= "+((Point2D)(intersectionVertexPoint2.get(1))).getX()+", y="+((Point2D)(intersectionVertexPoint2.get(1))).getY());
		//        	
		//    	}

		//		Questa parte si occupa di ordinare l'arraylist dei punti di intersezione
		//    	if((rightSide && leftSide) || (lowerSide && rightSide)){
		//    		Point2D tmpPoint=(Point2D) intersectionVertexPoint.get(0);
		//    		Point2D tmpPoint2=(Point2D) intersectionVertexPoint.get(1);
		////    		intersectionVertexPoint2=null;
		//    		if(tmpPoint.getX()<tmpPoint2.getX()){
		//    			intersectionVertexPoint.set(0, intersectionVertexPoint.get(1));
		//    			intersectionVertexPoint.set(1, tmpPoint);
		//    			}
		if (leftSide && upperSide) {
			intersectionVertexPoint.set(0, upperPoint);
			intersectionVertexPoint.set(1, leftPoint);

		} else if (upperSide && rightSide) {
			intersectionVertexPoint.set(0, upperPoint);
			intersectionVertexPoint.set(1, rightPoint);

		} else if (lowerSide && rightSide) {
			intersectionVertexPoint.set(0, rightPoint);
			intersectionVertexPoint.set(1, lowPoint);
		} else if (leftSide && rightSide) {
			intersectionVertexPoint.set(0, leftPoint);
			intersectionVertexPoint.set(1, rightPoint);

		} else if (leftSide && lowerSide) {
			intersectionVertexPoint.set(0, lowPoint);
			intersectionVertexPoint.set(1, leftPoint);
		} else if (upperSide && lowerSide) {
			intersectionVertexPoint.set(0, upperPoint);
			intersectionVertexPoint.set(1, lowPoint);

		}
		//    	if(intersectionVertexPoint2.size()>0){
		//    		System.out.println("Subito dopo il tutto: primo punto: x= "+((Point2D)(intersectionVertexPoint2.get(0))).getX()+", y="+((Point2D)(intersectionVertexPoint2.get(0))).getY());
		//    		System.out.println("Subito dopo il tutto: secondo punto: x= "+((Point2D)(intersectionVertexPoint2.get(1))).getX()+", y="+((Point2D)(intersectionVertexPoint2.get(1))).getY());
		//        	
		//    	}
		//    	
		//    	return intersecato;
		//    	if(intersectionVertexPoint2.size()>0){
		//    		Point2D tmp=(Point2D) intersectionVertexPoint2.get(0);
		//    		intersectionVertexPoint2.set(0, intersectionVertexPoint2.get(1));
		//    		intersectionVertexPoint2.set(1,tmp);
		//    	}

		return intersecato;

		//    	_____________FINE_________________

		/*
		
		//    	scandisce da sx a dx il lato superiore
		for(int i=(int)minX; i<maxX;i++){
			if(((int)(controlPoints[1].getX())== i) && ((controlPoints[1].getY()<minY && controlPoints[2].getY()>minY)||((controlPoints[1].getY()>minY && controlPoints[2].getY()<minY)))){
				intersecato=true;
				upperSide=true;
				intersectionVertexPoint.add(new Point2D.Double(i,(int)minY));
			}
			
			if(controlPoint>4){
			if(((int)(controlPoints[3].getX())== i) && ((controlPoints[3].getY()<minY && controlPoints[4].getY()>minY)||((controlPoints[3].getY()>minY && controlPoints[4].getY()<minY)))){
				intersecato=true;
				upperSide=true;
				intersectionVertexPoint.add(new Point2D.Double(i,(int)minY));
				}
			}
		}
		//    	scandisce da sopra a sotto il lato destro del vertice
		for(int i=(int)minY+1;i<maxY;i++){
		
			if(((int)controlPoints[0].getY())==i &&(((int)controlPoints[0].getX()<(int)maxX && (int)controlPoints[1].getX()>(int)maxX)||((int)controlPoints[0].getX()>(int)maxX && (int)controlPoints[1].getX()<(int)maxX))){
				rightSide=true;
				intersecato=true;
				intersectionVertexPoint.add(new Point2D.Double((int)maxX,i));
			}
			if(((int)controlPoints[2].getY())==i &&(((int)controlPoints[2].getX()<(int)maxX && (int)controlPoints[3].getX()>(int)maxX)||((int)controlPoints[2].getX()>(int)maxX && (int)controlPoints[3].getX()<(int)maxX))){
				rightSide=true;
				intersecato=true;
				intersectionVertexPoint.add(new Point2D.Double((int)maxX,i));
			}
			if(controlPoint>4){
				if(((int)controlPoints[4].getY())==i &&(((int)controlPoints[4].getX()<(int)maxX && (int)controlPoints[5].getX()>(int)maxX)||((int)controlPoints[4].getX()>(int)maxX && (int)controlPoints[5].getX()<(int)maxX))){
					rightSide=true;
					intersecato=true;
					intersectionVertexPoint.add(new Point2D.Double((int)maxX,i));
				}
				
			}
		}
		//    	scandisce da sx a dx il lato inferiore del vertice
		
		for(int i=(int)maxX-1; i>minX;i--){
			if(((int)(controlPoints[1].getX())== i) && ((controlPoints[1].getY()<maxY && controlPoints[2].getY()>maxY)||((controlPoints[1].getY()>maxY && controlPoints[2].getY()<maxY)))){
				intersecato=true;
				lowerSide=true;
				intersectionVertexPoint.add(new Point2D.Double(i,(int)maxY));
			}
				
			if(controlPoint>4){
				if(((int)(controlPoints[3].getX())== i) && ((controlPoints[3].getY()<maxY && controlPoints[4].getY()>maxY)||((controlPoints[3].getY()>maxY && controlPoints[4].getY()<maxY)))){
					intersecato=true;
					lowerSide=true;
					intersectionVertexPoint.add(new Point2D.Double(i,(int)maxY));
					}
				}
			
				
		}
		//    	scandisce da sotto a sopra il lato sinistro del vertice
		for(int i=(int)maxY-1;i>(minY-1);i--){
			if(((int)controlPoints[0].getY())==i &&(((int)controlPoints[0].getX()<(int)minX && (int)controlPoints[1].getX()>(int)minX)||((int)controlPoints[0].getX()>(int)minX && (int)controlPoints[1].getX()<(int)minX))){
				intersecato=true;
				leftSide=true;
				intersectionVertexPoint.add((new Point2D.Double((int)minX,i)));
			}
			if(((int)controlPoints[2].getY())==i &&(((int)controlPoints[2].getX()<(int)minX && (int)controlPoints[3].getX()>(int)minX)||((int)controlPoints[2].getX()>(int)minX && (int)controlPoints[3].getX()<(int)minX))){
				intersecato=true;
				leftSide=true;
				intersectionVertexPoint.add((new Point2D.Double((int)minX,i)));
			}
			if(controlPoint>4){
				if(((int)controlPoints[4].getY())==i &&(((int)controlPoints[4].getX()<(int)minX && (int)controlPoints[5].getX()>(int)minX)||((int)controlPoints[4].getX()>(int)minX && (int)controlPoints[5].getX()<(int)minX))){
					intersecato=true;
					leftSide=true;
					intersectionVertexPoint.add((new Point2D.Double((int)minX,i)));
				}
			}
				
		}
		if(intersecato){
			System.out.println("Numero di intersezioni2: "+intersectionVertexPoint2.size());
			System.out.println("2primo punto: x= "+((Point2D)(intersectionVertexPoint2.get(0))).getX()+", y="+((Point2D)(intersectionVertexPoint2.get(0))).getY());
			System.out.println("2secondo punto: x= "+((Point2D)(intersectionVertexPoint2.get(1))).getX()+", y="+((Point2D)(intersectionVertexPoint2.get(1))).getY());
			System.out.println("_________________________________________________");
			System.out.println("Numero di intersezioni: "+intersectionVertexPoint.size());
			System.out.println("primo punto: x= "+((Point2D)(intersectionVertexPoint.get(0))).getX()+", y="+((Point2D)(intersectionVertexPoint.get(0))).getY());
			System.out.println("secondo punto: x= "+((Point2D)(intersectionVertexPoint.get(1))).getX()+", y="+((Point2D)(intersectionVertexPoint.get(1))).getY());
		
		}
		
		return intersecato;*/

	}

	//  Giuseppe De Cicco & Fabio Granara
	public boolean intersectsEdge() {

		//    	System.out.println("chiamata di intersectsEdge in JmtEdge");
		//    	System.out.println("DImensione di lati di intersezione"+latiDiIntersezione.size());

		Object[] cells = (mediator.getGraph()).getDescendants(mediator.getGraph().getRoots());
		JmtEdge cell = this;
		JmtEdgeView view = (JmtEdgeView) (mediator.getGraph().getGraphLayoutCache()).getMapping(this, false);
		//    	Mediator.prova(this);
		boolean intersecato = false;

		//Per ottimizzare l algoritmo avevo pensato di ottenere solo le celle che intersecano
		//ma purtroppo se chiamo getRoots con un bound in questa classe , nella classe JmtEdgeRenderer
		//e nella clase JmtEdgeView vengono tirate tante eccezioni o si formano strane forme
		//per cui siamo obbligati a chiamare tutte le celle. 

		// avevo anche pensato di creare un rettangolo con i controlpoint del lato e poi fare il getRoots...ma non funziona lo stesso		
		//		Point2D primo=((Point2D)(view.getPoint(0)));
		//		Point2D ultimo=((Point2D)(view.getPoint(view.getPointCount()-1)));
		//		Rectangle rett=new Rectangle((int)primo.getX(),(int)primo.getY(),(int)ultimo.getX()-(int)primo.getX(),(int)ultimo.getY()-(int)primo.getY());

		//		Rectangle2D rett= GraphConstants.getBounds(view.getAllAttributes()); 
		//		Object[] cells2=(Mediator.getGraph()).getDescendants(Mediator.getGraph().getRoots(rett.getBounds()));
		//		System.out.println("Valore di cells2_"+cells2.length);
		//		System.out.println("Valore di cells: "+cells.length+", Valore di cells2: "+cells2.length);

		this.intersectionEdgePoint = new ArrayList<Point2D>();

		for (int i = 0; i < cells.length; i++) {
			if (cells[i] instanceof JmtEdge && !(cells[i].equals(cell))) {

				JmtEdgeView viewtmp = (JmtEdgeView) (mediator.getGraph().getGraphLayoutCache()).getMapping(cells[i], false);
				//	        		 view.intersects(Mediator.getGraph(), viewtmp.getBounds().getBounds());
				//	        		 System.out.println( view.intersects(Mediator.getGraph(), viewtmp.getBounds().getBounds()));
				if (!(viewtmp == null)) {
					if (cell.intersectsEdge(viewtmp)) {
						intersecato = true;
					}
				}
			}
		}

		//		if(intersecato){
		//			System.out.println("Grandezza di intersezioni:"+intersectionEdgePoint.size());
		//			for(int i=0;i<intersectionEdgePoint.size();i++){
		//				System.out.println("Punto x= "+((Point2D)(intersectionEdgePoint.get(i))).getX()+", y= "+((Point2D)(intersectionEdgePoint.get(i))).getY());
		//			}
		//		}
		return intersecato;
	}

	//  Giuseppe De Cicco & Fabio Granara
	public boolean intersectsEdge(EdgeView otherEdgeView) {
		boolean sourcesame = false;
		//    	edgeView del lato uno
		EdgeView edgeView = (EdgeView) (mediator.getGraph().getGraphLayoutCache()).getMapping(this, false);
		JmtEdge edge2 = (JmtEdge) otherEdgeView.getCell();

		JmtCell sourceOfEdge1 = null;
		JmtCell targetOfEdge1 = null;
		JmtCell sourceOfEdge2 = null;
		JmtCell targetOfEdge2 = null;

		//    	if inseriti per evitare eccezioni di tipo nullPointerException
		if ((DefaultPort) edge2.getSource() != null) {
			sourceOfEdge2 = (JmtCell) ((DefaultPort) edge2.getSource()).getParent();
		}
		if ((DefaultPort) edge2.getTarget() != null) {
			targetOfEdge2 = (JmtCell) ((DefaultPort) edge2.getTarget()).getParent();
		}
		if ((DefaultPort) this.getSource() != null) {
			sourceOfEdge1 = (JmtCell) ((DefaultPort) this.getSource()).getParent();
		}
		if ((DefaultPort) this.getTarget() != null) {
			targetOfEdge1 = (JmtCell) ((DefaultPort) this.getTarget()).getParent();
		}

		if (edgeView == null) {
			return false;
		}

		int controlPoint = edgeView.getPointCount();
		int controlPoint2 = otherEdgeView.getPointCount();

		//control point del lato 
		Point2D[] controlPoints = new Point2D[controlPoint];
		for (int i = 0; i < controlPoint; i++) {

			controlPoints[i] = edgeView.getPoint(i);
			if ((controlPoints[i] == null)) {
				return false;
			}
			//ho inserito il BigDecimal per approssimare i valori a due cifre ma poi mi sono accorto che occorre approx a intero purche le computazioni
			//siano efficenti per cui ho richiamato il metodo intValue
			//    		BigDecimal bg=new BigDecimal(controlPoints[i].getX());
			//			bg=bg.setScale(2,BigDecimal.ROUND_HALF_UP);
			//			BigDecimal bg2=new BigDecimal(controlPoints[i].getY());
			//			bg2=bg2.setScale(2,BigDecimal.ROUND_HALF_UP);
			//			int x=(int)controlPoints[i].getX();
			double x = Math.floor(controlPoints[i].getX());
			//			System.out.println("Valore di x"+x+", valore del non approssimato "+controlPoints[i].getX());
			double y = Math.floor(controlPoints[i].getY());
			//			int y=(int)controlPoints[i].getY();
			controlPoints[i].setLocation(x, y);

		}

		boolean intersecato = false;
		boolean sharedTheSameSourceTarget = false;
		if ((sourceOfEdge2 != null && sourceOfEdge1 != null) && (sourceOfEdge2.equals(sourceOfEdge1) || targetOfEdge1.equals(targetOfEdge2))) {
			sharedTheSameSourceTarget = true;

			GraphModel graphmodel = mediator.getGraph().getModel();
			Object[] listEdgesIn = null;
			Object[] listEdgesOut = null;
			if (targetOfEdge1.equals(targetOfEdge2)) {
				sourcesame = true;

				listEdgesIn = DefaultGraphModel.getEdges(graphmodel, targetOfEdge1, true);

				for (Object element : listEdgesIn) {

					JmtEdgeView edgeView1 = (JmtEdgeView) (mediator.getGraph().getGraphLayoutCache()).getMapping(element, false);
					int controlPoint3 = edgeView1.getPointCount();

					Point2D[] controlPoints3 = new Point2D[controlPoint3];

					for (int x = 0; x < controlPoint3; x++) {
						controlPoints3[x] = edgeView1.getPoint(x);
						if ((controlPoints3[x] == null)) {
							return false;
						}
						double y = Math.floor(controlPoints3[x].getY());

						double x1 = Math.floor(controlPoints3[x].getX());

						controlPoints3[x].setLocation(x1, y);

					}//end controlPoints3

					if (((controlPoints3[controlPoints3.length - 2].getX() > controlPoints[controlPoints.length - 1].getX()) && (controlPoints[controlPoints.length - 2]
							.getX() > controlPoints3[controlPoints3.length - 2].getX()))
							|| (controlPoints3[controlPoints3.length - 2].getX() < controlPoints[controlPoints.length - 1].getX())
							&& (controlPoints[controlPoints.length - 2].getX() < controlPoints3[controlPoints3.length - 2].getX())) {
						boolean contiene = false;
						if (intersectionEdgePoint.contains(controlPoints3[controlPoints3.length - 2])) {
							contiene = true;
						}
						if (intersectionEdgePoint == null || !contiene) {

							intersectionEdgePoint.add(controlPoints3[controlPoints3.length - 2]);
							intersecato = true;
						}
					}
				}

			} else if (sourceOfEdge2.equals(sourceOfEdge1)) {
				sourcesame = true;

				listEdgesOut = DefaultGraphModel.getEdges(graphmodel, sourceOfEdge1, false);

				for (Object element : listEdgesOut) {
					JmtEdgeView edgeView1 = (JmtEdgeView) (mediator.getGraph().getGraphLayoutCache()).getMapping(element, false);
					int controlPoint3 = edgeView1.getPointCount();
					Point2D[] controlPoints3 = new Point2D[controlPoint3];
					for (int x = 0; x < controlPoint3; x++) {
						controlPoints3[x] = edgeView1.getPoint(x);
						if ((controlPoints3[x] == null)) {
							return false;
						}
						double y = Math.floor(controlPoints3[x].getY());

						double x1 = Math.floor(controlPoints3[x].getX());

						controlPoints3[x].setLocation(x1, y);

					}//end controlPoints3
					for (int x = 0; x < controlPoints.length - 1; x++) {
						if (x % 2 == 0 && x < controlPoints3.length) {

							if ((controlPoints[x].getX() > controlPoints3[x + 1].getX())
									&& (controlPoints[x + 1].getX() < controlPoints3[x + 1].getX())
									&& ((int) controlPoints3[x].getY() == (int) controlPoints[x].getY())) {
								boolean contiene = false;
								if (intersectionEdgePoint.contains(controlPoints3[x + 1])) {
									contiene = true;
								}
								if (intersectionEdgePoint == null || !contiene) {

									intersectionEdgePoint.add(controlPoints3[x + 1]);
									intersecato = true;
								}
							} else if ((controlPoints[x + 1].getX() > controlPoints3[x + 1].getX())
									&& (controlPoints[x].getX() < controlPoints3[x + 1].getX())
									&& (controlPoints[x].getY() == controlPoints[x + 1].getY())) {
								boolean contiene = false;
								if (intersectionEdgePoint.contains(controlPoints3[x + 1])) {
									contiene = true;
								}
								if (intersectionEdgePoint == null || !contiene) {

									intersectionEdgePoint.add(controlPoints3[x + 1]);
									intersecato = true;
								}
							}
							//        			}else if(((controlPoints[0].getX()>controlPoints3[1].getX())&&(controlPoints[1].getX()<controlPoints3[1].getX())&&controlPoints3[1].getY()==controlPoints[1].getY())){
							//        				System.out.println("Ciaooooooooooo");
							//        				boolean contiene=false;
							//        				if(intersectionEdgePoint.contains(controlPoints3[1])){
							//        					contiene=true;
							//        				}
							//        				if(intersectionEdgePoint==null || !contiene){
							//
							//        				intersectionEdgePoint.add(controlPoints3[1]);
							//        				intersecato=true;
							//        				}
							//        			}
						}
					}
				}
			}

		}
		//control point del secondo lato
		Point2D[] controlPoints2 = new Point2D[controlPoint2];
		for (int i = 0; i < controlPoint2; i++) {
			controlPoints2[i] = otherEdgeView.getPoint(i);
			if ((controlPoints2[i] == null)) {
				return false;
			}
			//    		BigDecimal bg=new BigDecimal(controlPoints2[i].getX());
			//			bg=bg.setScale(2,BigDecimal.ROUND_HALF_UP);
			//			BigDecimal bg2=new BigDecimal(controlPoints2[i].getY());
			//			bg2=bg2.setScale(2,BigDecimal.ROUND_HALF_UP);
			//    		controlPoints2[i].setLocation(bg.intValue(), bg2.intValue());
			double y = Math.floor(controlPoints2[i].getY());
			double x = Math.floor(controlPoints2[i].getX());
			//			System.out.println("Valore di x"+x+", valore del non approssimato "+controlPoints2[i].getX());
			//			int y=(int)controlPoints2[i].getY();
			controlPoints2[i].setLocation(x, y);
			//			controlPoints2[i].setLocation(x, y);

		}
		//Introduco la variabile xPosition perche a me interessano solo i lati orizzontali per quanto riguarda l intersezione

		boolean xPosition = true;

		for (int i = 0; i < controlPoint - 1; i++) {
			if (xPosition) {
				if (i == 0 && sharedTheSameSourceTarget) {
					Point2D tmpMin = controlPoints[i];
					Point2D tmpMax = controlPoints[i + 1];
					//nel caso di lati che vanno nel verso oposto
					if ((tmpMin.getX() > tmpMax.getX())) {
						tmpMin = controlPoints[i + 1];
						tmpMax = controlPoints[i];
					}
					for (int q = 0; q < controlPoints2.length - 1; q++) {
						//scandisco solo i lati verticali
						if (!(q % 2 == 0)) {
							Point2D tmpMin2 = controlPoints2[q];
							Point2D tmpMax2 = controlPoints2[q + 1];
							if ((tmpMin2.getY() > tmpMax2.getY())) {
								tmpMin2 = controlPoints2[q + 1];
								tmpMax2 = controlPoints2[q];
							}
							if ((tmpMin2.getY() < tmpMin.getY()) && (tmpMax2.getY() > tmpMin.getY()) && (tmpMin2.getX() > tmpMin.getX())
									&& (tmpMin2.getX() < tmpMax.getX())) {
								Point2D tm1 = new Point2D.Double(tmpMin2.getX(), tmpMin.getY());
								if (!intersectionEdgePoint.contains(tm1)) {
									intersectionEdgePoint.add(tm1);

									intersecato = true;
								}
							}
						}
					}
				} else if (!sharedTheSameSourceTarget || sourcesame) {
					Point2D tmpMin = controlPoints[i];
					Point2D tmpMax = controlPoints[i + 1];
					if ((tmpMin.getX() > tmpMax.getX())) {
						tmpMin = controlPoints[i + 1];
						tmpMax = controlPoints[i];
					}
					for (int q = 0; q < controlPoints2.length - 1; q++) {
						//scandisco solo i lati verticali
						if (!(q % 2 == 0)) {
							Point2D tmpMin2 = controlPoints2[q];
							Point2D tmpMax2 = controlPoints2[q + 1];
							if ((tmpMin2.getY() > tmpMax2.getY())) {
								tmpMin2 = controlPoints2[q + 1];
								tmpMax2 = controlPoints2[q];
							}
							if ((tmpMin2.getY() < tmpMin.getY()) && (tmpMax2.getY() > tmpMin.getY()) && (tmpMin2.getX() > tmpMin.getX())
									&& (tmpMin2.getX() < tmpMax.getX())) {
								Point2D tm1 = new Point2D.Double(tmpMin2.getX(), tmpMin.getY());
								if (!intersectionEdgePoint.contains(tm1)) {
									intersectionEdgePoint.add(tm1);

									intersecato = true;
								}
							}
						}//end dell if con controllo di disparita'
					}//end del for

				}
			}
			xPosition = !xPosition;
		}

		return intersecato;
	}

	//  Giuseppe De Cicco & Fabio Granara
	public boolean getLeftSideIntersaction() {
		return leftSide;
	}

	// Giuseppe De Cicco & Fabio Granara
	public boolean getRightSideIntersaction() {
		return rightSide;
	}

	// Giuseppe De Cicco & Fabio Granara
	public boolean getLowerSideIntersaction() {
		return lowerSide;
	}

	//  Giuseppe De Cicco & Fabio Granara
	public boolean getUpperSideIntersaction() {
		return upperSide;
	}

	//  Giuseppe De Cicco & Fabio Granara
	public Point2D getIntersactionPoint() {
		return intersectionPoint;
	}

	//  Giuseppe De Cicco & Fabio Granara
	public ArrayList<Point2D> getIntersactionEdgePoint() {
		return intersectionEdgePoint;
	}

	//  Giuseppe De Cicco & Fabio Granara
	public boolean isRing() {
		return isRing;
	}

	//	 Giuseppe De Cicco & Fabio Granara
	public void setIsRing(boolean value) {
		isRing = value;

	}

	//	 Giuseppe De Cicco & Fabio Granara
	public void insertEdgeOfIntersection(List latiDiIntersezione) {
		this.latiDiIntersezione = new ArrayList();
		this.latiDiIntersezione = latiDiIntersezione;

	}

	//	 Giuseppe De Cicco & Fabio Granara
	public boolean isPointSharedModified() {
		return pointSharedModified;
	}

	//	 Giuseppe De Cicco & Fabio Granara
	public void setPointShareModified(boolean value) {
		pointSharedModified = value;

	}

	//	 Giuseppe De Cicco & Fabio Granara

	public int getOffset() {
		JmtCell sourceOfEdge = (JmtCell) ((DefaultPort) this.getSource()).getParent();
		Rectangle boundsSource = GraphConstants.getBounds(sourceOfEdge.getAttributes()).getBounds();
		JmtCell targetOfEdge = (JmtCell) ((DefaultPort) this.getTarget()).getParent();
		Rectangle boundsTarget = GraphConstants.getBounds(targetOfEdge.getAttributes()).getBounds();
		Object[] listEdges = null;
		GraphModel graphmodel = mediator.getGraph().getModel();
		//		System.out.println("Padre: "+targetOfEdge);
		Object[] fathers = (DefaultGraphModel.getIncomingEdges(graphmodel, targetOfEdge));
		int max = (int) boundsSource.getMaxX();
		for (Object father : fathers) {
			//			System.out.println("Dentro il for");
			if (father instanceof JmtEdge) {
				JmtCell sourceOfEdge2 = (JmtCell) ((DefaultPort) ((JmtEdge) father).getSource()).getParent();
				Rectangle boundsSource2 = GraphConstants.getBounds(sourceOfEdge2.getAttributes()).getBounds();
				if (sourceOfEdge != sourceOfEdge2 && boundsSource.getMaxX() < boundsTarget.getMinX() - 5
						&& boundsSource2.getMaxX() < boundsTarget.getMinX() - 5) {
					if (max < boundsSource2.getMaxX() && (int) boundsSource.getMaxX() > (int) boundsSource2.getMinX()) {
						max = (int) boundsSource2.getMaxX();
					}
				}
			}
		}
		return (int) (max - boundsSource.getMaxX());
	}

}
