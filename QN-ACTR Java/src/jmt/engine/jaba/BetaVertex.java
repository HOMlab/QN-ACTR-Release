package jmt.engine.jaba;

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

/**
 * A class defining a vertex.
 *
 * @author  Chris Pudney <cpudney@alphapharm.pharm.uwa.edu.au>
 * @version 1.1
 */
public class BetaVertex {

	private double x, y, z;

	private double EPSYLON = 0.000001;

	public BetaVertex() {
		x = 0;
		y = 0;
		z = 0;
	}

	public BetaVertex(double x, double y, double z) {
		this.x = x;
		this.y = y;
		this.z = z;
	}

	public BetaVertex(double[] v) {
		x = v[0];
		y = v[1];
		z = v[2];
	}

	public double[] getCoords() {
		double[] c = new double[3];

		c[0] = x;
		c[1] = y;
		c[2] = z;
		return c;
	}

	public double getX() {
		return x;
	}

	public double getY() {
		return y;
	}

	public double getZ() {
		return z;
	}

	/**
	 * Tests whether two vertices are the same point.
	 *
	 * @return True/false if the vertices are the same/different.
	 */
	public static boolean sameVertex(BetaVertex v1, BetaVertex v2) {
		return v1 == v2 || (v1.x == v2.x && v1.y == v2.y && v1.z == v2.z);
	}

	/**
	 * Tests whether three vertices are collinear.
	 * @return True/false if the vertices are collinear/non-collinear
	 */
	public static boolean collinear(BetaVertex v1, BetaVertex v2, BetaVertex v3) {
		double x1 = v1.x, y1 = v1.y, z1 = v1.z;
		double x2 = v2.x, y2 = v2.y, z2 = v2.z;
		double x3 = v3.x, y3 = v3.y, z3 = v3.z;
		return (z3 - z1) * (y2 - y1) - (z2 - z1) * (y3 - y1) == 0 && (z2 - z1) * (x3 - x1) - (x2 - x1) * (z3 - z1) == 0
				&& (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1) == 0;
	}

	/**
	 * Returns the pair of vertices that bound three collinear vertices.
	 *
	 * @return An array of two vertices that bound the tested vertices
	 */
	public static BetaVertex[] bounds(BetaVertex v1, BetaVertex v2, BetaVertex v3) {
		BetaVertex min12, max12;

		// Compare v1 and v2    
		if (v1.x == v2.x) {
			if (v1.y == v2.y) {
				if (v1.z <= v2.z) {
					min12 = v1;
					max12 = v2;
				} else {
					min12 = v2;
					max12 = v1;
				}
			} else if (v1.y < v2.y) {
				min12 = v1;
				max12 = v2;
			} else {
				min12 = v2;
				max12 = v1;
			}
		} else if (v1.x < v2.x) {
			min12 = v1;
			max12 = v2;
		} else {
			min12 = v2;
			max12 = v1;
		}

		BetaVertex[] bounds = new BetaVertex[2];

		// Compare min of v1, v2 with v3
		if (min12.x == v3.x) {
			if (min12.y == v3.y) {
				bounds[0] = min12.z <= v3.z ? min12 : v3;
			} else {
				bounds[0] = min12.y < v3.y ? min12 : v3;
			}
		} else {
			bounds[0] = min12.x < v3.x ? min12 : v3;
		}

		// Compare max of v1, v2 with v3
		if (max12.x == v3.x) {
			if (max12.y == v3.y) {
				bounds[1] = max12.z >= v3.z ? max12 : v3;
			} else {
				bounds[1] = max12.y > v3.y ? max12 : v3;
			}
		} else {
			bounds[1] = max12.x > v3.x ? max12 : v3;
		}
		return bounds;
	}

	/** 
	 * Returns the square of the distance between two vertices.
	 *
	 * @return        the distance squared between them
	 */
	public static double distanceSquared(BetaVertex v1, BetaVertex v2) {
		double x1 = v1.x, y1 = v1.y, z1 = v1.z;
		double x2 = v2.x, y2 = v2.y, z2 = v2.z;
		return (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2) + (z1 - z2) * (z1 - z2);
	}

	public boolean CircaEquals(BetaVertex b) {
		boolean out = false;

		if (Math.abs(x - b.x) < EPSYLON && Math.abs(y - b.y) < EPSYLON && Math.abs(z - b.z) < EPSYLON) {
			out = true;
		}

		return out;
	}

	/**
	 * Returns a string that describes a vertex.
	 */
	@Override
	public String toString() {
		return ("(" + x + ", " + y + ", " + z + ")");
	}
}
