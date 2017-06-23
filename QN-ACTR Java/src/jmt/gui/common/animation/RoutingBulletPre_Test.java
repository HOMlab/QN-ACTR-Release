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

package jmt.gui.common.animation;

import java.awt.Point;
import java.awt.Rectangle;

import javax.swing.JFrame;

/**
 * Created by IntelliJ IDEA.
 * User: orsotroniii
 * Date: 10-gen-2005
 * Time: 11.00.19
 * To change this template use Options | File Templates.
 */
public class RoutingBulletPre_Test extends JFrame {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private Animator anim = null;
	private QueueNetAnimation qnAni = null;

	public RoutingBulletPre_Test() {
		super();
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		prepareQueueNet();
		this.setBounds(300, 200, 400, 300);
		this.getContentPane().add(qnAni);
		this.show();
		anim.start();
	}

	private void prepareQueueNet() {
		Point[] link01ps = { new Point(80, 35), new Point(200, 35) };
		Point[] link02ps = { new Point(80, 35), new Point(150, 35), new Point(150, 115), new Point(200, 115) };
		Point[] link03ps = { new Point(260, 35), new Point(280, 35), new Point(280, 70), new Point(180, 70), new Point(180, 115), new Point(200, 115) };
		Point[] link04ps = { new Point(260, 115), new Point(280, 115), new Point(280, 140), new Point(10, 140), new Point(10, 35), new Point(20, 35) };
		Point[] link05ps = { new Point(260, 115), new Point(280, 115), new Point(280, 140), new Point(10, 140), new Point(10, 115),
				new Point(30, 115) };
		Point[] link06ps = { new Point(90, 115), new Point(200, 115) };
		StationAnimation station1 = new StationAnimation("source", new Rectangle(20, 20, 70, 30), 50);
		StationAnimation station2 = new StationAnimation("sink", new Rectangle(200, 20, 70, 30), 0);
		StationAnimation station3 = new StationAnimation("server", new Rectangle(200, 100, 70, 30), 500);
		StationAnimation station4 = new StationAnimation("server", new Rectangle(30, 100, 70, 30), 500);
		EdgeAnimation link1 = new EdgeAnimation(link01ps, 20);
		EdgeAnimation link2 = new EdgeAnimation(link02ps, 20);
		EdgeAnimation link3 = new EdgeAnimation(link03ps, 20);
		EdgeAnimation link4 = new EdgeAnimation(link04ps, 20);
		EdgeAnimation link5 = new EdgeAnimation(link05ps, 20);
		EdgeAnimation link6 = new EdgeAnimation(link06ps, 20);
		qnAni = new QueueNetAnimation();
		qnAni.setBounds(new Rectangle(0, 0, 300, 100));
		anim = new Animator(20, qnAni);
		qnAni.addStation(station1);
		qnAni.addStation(station2);
		qnAni.addStation(station3);
		qnAni.addStation(station4);
		qnAni.addEdge(link1, station1, station2);
		qnAni.addEdge(link2, station1, station3);
		qnAni.addEdge(link3, station2, station3);
		qnAni.addEdge(link4, station3, station1);
		qnAni.addEdge(link5, station3, station4);
		qnAni.addEdge(link6, station4, station3);
		qnAni.addJob(new JobAnimation(0.1, new JobPath[] { new RandomPath(qnAni, station1) }, new Rectangle(10, 10)), station1);
	}

	/*
	public static void main(String[] args){
	    new RoutingBulletPre_Test();
	}
	*/
}
