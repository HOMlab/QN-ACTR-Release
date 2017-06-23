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

/**
 * A class representing exceptions during construction of a convex hull.
 * These can occur for example when all vertices are coplanar.
 *
 * @author Chris Pudney <mailto:cpudney@alphapharm.pharm.uwa.edu.au>
 * @see    ConvexHull
 */
public class ConvexHullException extends Exception {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	   * Construct a convex hull exception.
	   */
	public ConvexHullException() {
		super();
	}

	/**
	 * Construct a convex hull exception with a message.
	 *
	 * @param s  a descriptive error message
	 */
	public ConvexHullException(String s) {
		super(s);
	}
}
