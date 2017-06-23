// Copyright (C) Chris Pudney, The University of Western Australia, 1998.
// All rights reserved.
//  
// Permission to use, copy, modify and distribute this software and its
// documentation only for the purposes of teaching and research is hereby
// granted without fee, provided that the above copyright notice and this
// permission notice appear in all copies of this software/documentation
// and that you do not sell the software.  No commercial use or
// distribution of the software is permitted without the consent of the
// copyright owners.  Commercial licensing is available by contacting the
// author(s).
// 
// THIS SOFTWARE/DOCUMENTATION IS PROVIDED WITH NO WARRANTY, EXPRESS OR
// IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF MERCHANTABILITY OR
// FITNESS FOR A PARTICULAR PURPOSE.
package jmt.engine.jaba.Hull;

import java.util.Enumeration;
import java.util.Vector;

/**
 * A class defining a triangle
 */
public class Triangle extends Polygon {
	/**
	 * Construct a triangle given three vertices.
	 *
	 * @param v1, v2, v3  the vertices
	 */
	public Triangle(Vertex v1, Vertex v2, Vertex v3) {
		super();
		Vector<Vertex> v = getVertices();
		v.addElement(v1);
		v.addElement(v2);
		v.addElement(v3);
	}

	/**
	 * Tests whether two triangles are the same.  Order of vertices is ignored.
	 *
	 * @param t1, t2  The triangles to compare
	 */
	public static boolean sameTriangle(Triangle t1, Triangle t2) {
		if (t1 == t2) {
			return true;
		}

		Vector<Vertex> v1 = t1.getVertices();
		Vector<Vertex> v2 = t2.getVertices();
		if (v1 == v2) {
			return true;
		}

		Vertex t1A = v1.firstElement();
		Vertex t1B = v1.elementAt(1);
		Vertex t1C = v1.lastElement();
		Vertex t2A = v2.firstElement();
		Vertex t2B = v2.elementAt(1);
		Vertex t2C = v2.lastElement();
		return (Vertex.sameVertex(t1A, t2A) || Vertex.sameVertex(t1A, t2B) || Vertex.sameVertex(t1A, t2C))
				&& (Vertex.sameVertex(t1B, t2A) || Vertex.sameVertex(t1B, t2B) || Vertex.sameVertex(t1B, t2C))
				&& (Vertex.sameVertex(t1C, t2A) || Vertex.sameVertex(t1C, t2B) || Vertex.sameVertex(t1C, t2C));
	}

	/**
	 * Tests whether a triangle is present in a vector.
	 *
	 * @param vec  vector of triangles to match
	 */
	public boolean matches(Vector vec) {
		for (Enumeration e = vec.elements(); e.hasMoreElements();) {
			if (sameTriangle(this, (Triangle) e.nextElement())) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Returns the index of a triangle in a vector.
	 *
	 * @param vec  vector of triangles to match
	 * @return     index of the match or -1 if no match
	 */
	public int matchIndex(Vector vec) {
		int i = 0;
		for (Enumeration e = vec.elements(); e.hasMoreElements();) {
			if (sameTriangle(this, (Triangle) e.nextElement())) {
				return i;
			}
			i++;
		}
		return -1;
	}

	/**
	 * Returns the third vertex of a triangle given the other two.
	 *
	 * @param v1, v2  the other two vertices
	 * @return        the third vertex or null if v1 and v2 are not
	 *                vertices
	 */
	public Vertex thirdVertex(Vertex v1, Vertex v2) {
		Vector<Vertex> vertices = getVertices();
		Vertex vert1 = vertices.elementAt(0);
		Vertex vert2 = vertices.elementAt(1);
		Vertex vert3 = vertices.elementAt(2);
		if (v1 == vert1 && v2 == vert2 || v2 == vert1 && v1 == vert2) {
			return vert3;
		}
		if (v1 == vert1 && v2 == vert3 || v2 == vert1 && v1 == vert3) {
			return vert2;
		}
		if (v1 == vert2 && v2 == vert3 || v2 == vert2 && v1 == vert3) {
			return vert1;
		}
		return null;
	}

	/**
	 * Returns the triangle that shares a particular edge.
	 *
	 * @params tris   the list of triangles to search
	 *         v1 v2  the vertices that define the edge
	 * @return        the neighbouring triangle or null if not found
	 */
	public Triangle edgeNeighbour(Vector tris, Vertex v1, Vertex v2) {
		for (Enumeration e = tris.elements(); e.hasMoreElements();) {
			Triangle t = (Triangle) e.nextElement();
			if (!sameTriangle(t, this)) {
				Vector<Vertex> verts = t.getVertices();
				if (verts.contains(v1) && verts.contains(v2)) {
					return t;
				}
			}
		}
		return null;
	}

	/**
	 * Returns the sign of the volume of the tetrahedron formed by a
	 * triangle and a vertex.  VolumeSign is positive iff the vertex is
	 * on the negative side of the triangle, where the positive side is
	 * determined by the rh-rule.  So the volume is positive if the ccw
	 * normal to points outside the tetrahedron.  The final
	 * fewer-multiplications form is due to Bob Williamson. 
	 *
	 * @param v  vertex to test
	 * @return   -1/0/1 if vertex is on the negative/coplanar/positive
	 *           side of the triangle according to the rh-rule
	 */
	public int volumeSign(Vertex v) {
		Vector<Vertex> vertices = getVertices();
		int[] v1 = vertices.firstElement().getCoords();
		int[] v2 = vertices.elementAt(1).getCoords();
		int[] v3 = vertices.lastElement().getCoords();
		int[] v4 = v.getCoords();
		long ax = v1[0] - v4[0];
		long ay = v1[1] - v4[1];
		long az = v1[2] - v4[2];
		long bx = v2[0] - v4[0];
		long by = v2[1] - v4[1];
		long bz = v2[2] - v4[2];
		long cx = v3[0] - v4[0];
		long cy = v3[1] - v4[1];
		long cz = v3[2] - v4[2];

		long vol = ax * (by * cz - bz * cy) + ay * (bz * cx - bx * cz) + az * (bx * cy - by * cx);

		/* The volume should be an integer. */
		if (vol > 0) {
			return 1;
		} else if (vol < 0) {
			return -1;
		} else {
			return 0;
		}
	}

	/**
	 * Returns six times the volume of the tetrahedron formed by a
	 * triangle and vertex.  The volume is positive iff the vertex is on
	 * the negative side of the triangle, where the positive side is
	 * determined by the rh-rule. 
	 *
	 * @param v the vertex
	 */
	public int volume6(Vertex v) {
		Vector<Vertex> vertices = getVertices();
		int[] v1 = vertices.firstElement().getCoords();
		int[] v2 = vertices.elementAt(1).getCoords();
		int[] v3 = vertices.lastElement().getCoords();
		int[] v4 = v.getCoords();
		int ax = v1[0];
		int ay = v1[1];
		int az = v1[2];
		int bx = v2[0];
		int by = v2[1];
		int bz = v2[2];
		int cx = v3[0];
		int cy = v3[1];
		int cz = v3[2];
		int dx = v4[0];
		int dy = v4[1];
		int dz = v4[2];
		int bxdx = bx - dx;
		int bydy = by - dy;
		int bzdz = bz - dz;
		int cxdx = cx - dx;
		int cydy = cy - dy;
		int czdz = cz - dz;
		return (az - dz) * (bxdx * cydy - bydy * cxdx) + (ay - dy) * (bzdz * cxdx - bxdx * czdz) + (ax - dx) * (bydy * czdz - bzdz * cydy);
	}
}
