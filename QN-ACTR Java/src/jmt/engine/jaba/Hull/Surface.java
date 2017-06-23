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

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Vector;

/**
 * A class representing a surface.
 *
 * @author  Chris Pudney <cpudney@alphapharm.pharm.uwa.edu.au>
 * @version 1.1
 */
public class Surface {
	/**
	 * The list of polygonal faces defining the surface.
	 */
	private Vector<Polygon> faces;

	/**
	 * Construct a surface containing no faces.
	 */
	public Surface() {
		faces = new Vector<Polygon>();
	}

	/**
	 * Construct a surface given a list of faces.
	 *
	 * @param f  the list of faces
	 */
	public Surface(Vector<Polygon> f) {
		faces = f;
	}

	/**
	 * Get the faces of a surfaces.
	 *
	 * @return The vector of polygons that make up the surface.
	 */
	public Vector<Polygon> getFaces() {
		return faces;
	}

	/**
	 * Set the faces of a surfaces.
	 */
	public void setFaces(Vector<Polygon> f) {
		faces = f;
	}

	/**
	 * Get the vertices of a surface.
	 *
	 * @return The vector of vertices that make up a surface.
	 */
	public Vector<Vertex> getVertices() {
		Vector<Vertex> vertices = new Vector<Vertex>();

		// Get the vertices of each face
		for (Polygon polygon : faces) {
			Vector<Vertex> face_verts = polygon.getVertices();
			for (Vertex vertex : face_verts) {
				if (vertices.indexOf(vertex) == -1) {
					vertices.addElement(vertex);
				}
			}
		}
		return vertices;
	}

	/**
	 * Write an OFF file that describes a surface.  OFF files can be viewed
	 * using Geomview http://www.geom.umn.edu/software/download/geomview.html
	 *
	 * @param pw A PrintWriter for the file
	 */
	public void writeOFF(PrintWriter pw) throws IOException {
		/* Write header */
		pw.println("OFF");
		Vector<Vertex> vertices = getVertices();
		pw.println(vertices.size() + " " + " " + faces.size() + " " + (3 * faces.size()));

		/* Write vertex list */
		for (Vertex vertex : vertices) {
			int[] c = vertex.getCoords();
			pw.println(c[0] + " " + c[1] + " " + c[2]);
		}

		/* Write polygon list */
		for (Polygon polygon : faces) {
			Vector<Vertex> pV = polygon.getVertices();
			pw.print(pV.size());
			for (Vertex vertex : pV) {
				pw.print(" " + vertices.indexOf(vertex));
			}
			pw.println();
		}
	}

	/**
	 * Returns a string describing a surface.
	 */
	@Override
	public String toString() {
		String s = new String();

		for (Polygon polygon : faces) {
			s += polygon + "\n";
		}
		return s;
	}
}
