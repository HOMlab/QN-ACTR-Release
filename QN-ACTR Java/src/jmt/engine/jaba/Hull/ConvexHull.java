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
 * A class representing the convex hull of a set of points.
 *
 * @author - Chris Pudney <mailto:cpudney@alphapharm.pharm.uwa.edu.au>
 * @version - 1.1
 */
public class ConvexHull extends DSurface {
	/**
	 * Acceptable range of coordinates.
	 */
	private static final int COORD_RANGE = 1000000;

	// private static final int COORD_RANGE = 512;   Use with Volume6()

	/**
	 * Constructs a convex hull initialised to a null surface.
	 */
	public ConvexHull() {
		super();
	}

	/**
	 * Constructs the convex hull of a set of vertices.
	 *
	 * @param - vertices  the set of vertices
	 * @exception - ConvexHullException
	 *              if hull construction fails
	 */
	public ConvexHull(Vector<Vertex> vertices) throws ConvexHullException {
		super();

		if (vertices.size() < 4) {
			throw new ConvexHullException("Too few (" + vertices.size() + ") vertices to form hull");
		}
		Enumeration<Vertex> e = vertices.elements();
		if (e.hasMoreElements()) {
			Vertex v1 = e.nextElement();
			Vertex v2 = null;
			for (; e.hasMoreElements();) {
				v2 = e.nextElement();
				if (!Vertex.sameVertex(v1, v2)) {
					break;
				}
			}

			Vertex v3 = null;
			Triangle t = null;
			Vector<Vertex> coVerts = new Vector<Vertex>();
			for (; e.hasMoreElements();) {
				v3 = e.nextElement();
				if (Vertex.collinear(v1, v2, v3)) {
					coVerts.addElement(v3);
				} else {
					t = new Triangle(v1, v2, v3);
					break;
				}
			}

			Vertex v4 = null;
			for (; e.hasMoreElements();) {
				v4 = e.nextElement();
				int volSign = t.volumeSign(v4);
				if (volSign == 0) {
					coVerts.addElement(v4);
				} else {
					addFace(t);
					triToTet(t, v4, volSign);
					break;
				}
			}

			// Check vertex coords
			checkVertex(v1);
			checkVertex(v2);
			checkVertex(v3);
			checkVertex(v4);

			int i = 0;

			// Add vertices to the hull one at a time
			for (; e.hasMoreElements();) {
				addVertex(e.nextElement());
			}

			// Reprocess the previously found co-linear/planar vertices
			if (getFaces().size() > 0) {
				for (e = coVerts.elements(); e.hasMoreElements();) {
					addVertex(e.nextElement());
				}

			} else {
				throw new ConvexHullException("Vertices coplanar");
			}
		}
	}

	/**
	 * Determine the hull faces that have vertices in each of two sets
	 *
	 * @param - s1, s2  the vertex sets
	 * @return - returns a vector of triangles that lie on the hull
	 */
	public Vector<Triangle> interSetFaces(Vector s1, Vector s2) {
		Vector<Polygon> faces = getFaces();
		Vector<Triangle> xFaces = new Vector<Triangle>();

		for (Polygon polygon : faces) {
			Triangle t = (Triangle) polygon;
			Vector<Vertex> v = t.getVertices();
			Vertex v1 = v.firstElement();
			Vertex v2 = v.elementAt(1);
			Vertex v3 = v.lastElement();
			if ((s1.contains(v1) || s1.contains(v2) || s1.contains(v3)) && (s2.contains(v1) || s2.contains(v2) || s2.contains(v3))) {
				xFaces.addElement(t);
			}
		}
		return xFaces;
	}

	/**
	 * Check that the vertex coordinates are within bounds 
	 *
	 * @param - vertex  the vertex to check
	 * @return - True/false if the vertex is inside/outside the legal
	 *           bounds
	 * @see - COORD_RANGE
	 */
	private static boolean checkVertex(Vertex vertex) {
		int[] c = vertex.getCoords();
		if (Math.abs(c[0]) > COORD_RANGE || Math.abs(c[1]) > COORD_RANGE || Math.abs(c[2]) > COORD_RANGE) { /*
																											System.out.println
																											("Warning: vertex coordinates > " + COORD_RANGE + " or < " +
																											-COORD_RANGE + " may create problems"); */
			return false;
		}
		return true;
	}

	/**
	 * Form a tetrahedron from vertex and the existing triangular hull.
	 *
	 * @param face - a triangular face of the tetrahedron
	 * @param vertex - the fourth point of the tetrahedron
	 * @param vol - indicates on which side of face vertex lies
	 */
	private void triToTet(Polygon face, Vertex vertex, int vol) {
		Vector<Vertex> v = face.getVertices();
		Vertex v1 = v.elementAt(0);
		Vertex v2 = v.elementAt(1);
		Vertex v3 = v.elementAt(2);

		// Store the vertices in CCW order
		if (vol < 0) {
			v.setElementAt(v3, 0);
			v.setElementAt(v1, 2);
			Vertex tv = v1;
			v1 = v3;
			v3 = tv;
		}
		addFace(new Triangle(v3, v2, vertex));
		addFace(new Triangle(v2, v1, vertex));
		addFace(new Triangle(v1, v3, vertex));
	}

	/**
	 * Add a vertex to a convex hull.  Determine all faces visible from
	 * the vertex.  If none are visible then the point is marked as
	 * inside the hull.  Delete the visible faces and construct faces
	 * between the vertex and the edges that border the visible
	 * faces. 
	 *
	 * @param vertex - the vertex to add to the convex hull.
	 */
	private void addVertex(Vertex vertex) {
		Vector<Edge> visEdges = new Vector<Edge>();
		Vector<Triangle> visFaces = new Vector<Triangle>();

		// Check vertex coordinates
		checkVertex(vertex);

		// Delete visible faces
		for (Polygon polygon : getFaces()) {
			Triangle face = (Triangle) polygon;
			if (face.volumeSign(vertex) < 0) {
				visFaces.addElement(face);
				// System.out.println(vertex + " visible from " + face);
			}
			// else
			// {
			// System.out.println(vertex + " NOT visible from " + face);
			// }
		}
		// Delete visible faces and construct visible edges list
		for (Enumeration<Triangle> e = visFaces.elements(); e.hasMoreElements();) {
			Polygon face = e.nextElement();
			deleteVisibleFace(face, visEdges);
		}

		// System.out.println("Visible edges: " + visEdges);

		// Construct new faces using visible edges
		for (Edge edge : visEdges) {
			Vertex ends[] = edge.getVertices();
			addFace(new Triangle(ends[0], ends[1], vertex));
		}
	}

	/**
	 * Delete a visible face from the convex hull.  Adjust the list
	 * of visible edges accordingly.
	 *
	 * @param face -  a face visible from a vertex to be deleted
	 * @param visibleEdges - the list of hull edges visible from a vertex
	 */
	private void deleteVisibleFace(Polygon face, Vector<Edge> visibleEdges) {
		Vector<Vertex> v = face.getVertices();
		Vertex v1 = v.elementAt(0);
		Vertex v2 = v.elementAt(1);
		Vertex v3 = v.elementAt(2);
		Edge e1 = new Edge(v1, v2);
		Edge e2 = new Edge(v2, v3);
		Edge e3 = new Edge(v3, v1);
		updateVisibleEdges(e1, visibleEdges);
		updateVisibleEdges(e2, visibleEdges);
		updateVisibleEdges(e3, visibleEdges);
		deleteFace(face);
	}

	/**
	 * Update the visible edge list.  If e is not in the list then add
	 * it if it is then delete it from the list 
	 *
	 * @param e - a visible edge
	 * @param visibleEdges - a list of edges visible from a vertex
	 */
	private void updateVisibleEdges(Edge e, Vector<Edge> visibleEdges) {
		Enumeration<Edge> f;
		boolean same = false;

		for (f = visibleEdges.elements(); f.hasMoreElements();) {
			Edge edge = f.nextElement();
			if (Edge.sameEdge(e, edge)) {
				same = true;
				e = edge;
				break;
			}
		}
		if (same) {
			visibleEdges.removeElement(e);
		} else {
			visibleEdges.addElement(e);
		}
	}
}
