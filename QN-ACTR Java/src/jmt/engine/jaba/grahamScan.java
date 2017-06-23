package jmt.engine.jaba;

// grahamScan.java
// 
// Mark F. Hulber
// May 1996
//
//
// grahamScan implements the Graham Scan convex hull algorithm.  Given
//   a vector containing points it will return a vector of points forming
//   the convex hull of the input.  This class relies on extensions to the
//   point class called newPoints.  grahamScan does not begin computing the
//   convex hull until three points have been provided.
//
//

public class grahamScan {

	java.util.Vector<newPoint> lines = new java.util.Vector<newPoint>(100, 100);
	java.util.Stack<newPoint> stk = new java.util.Stack<newPoint>();
	java.util.Vector<newPoint> s = new java.util.Vector<newPoint>(100, 100);

	public java.util.Vector<newPoint> doGraham(java.util.Vector<newPoint> q) {

		int m = 0;
		newPoint temp, temp2;
		int n = q.size();
		int a, i;
		if (n > 2) {
			s.removeAllElements();
			s = (java.util.Vector<newPoint>) q.clone();
			for (i = 1; i < n; i++) {
				if (s.elementAt(i).y < s.elementAt(m).y || ((s.elementAt(i).y == s.elementAt(m).y) && (s.elementAt(i).x < s.elementAt(m).x))) {
					m = i;
				}
			}
			temp = s.elementAt(0);
			s.setElementAt(s.elementAt(m), 0);
			s.setElementAt(temp, m);

			// stage 2
			temp2 = s.elementAt(0);
			for (i = 2; i < n; i++) {
				for (int j = n - 1; j >= i; j--) {
					if (temp2.polarCmp(s.elementAt(j - 1), s.elementAt(j)) == 1) {
						temp = s.elementAt(j - 1);
						s.setElementAt(s.elementAt(j), j - 1);
						s.setElementAt(temp, j);
					}
				}
			}
			for (i = 1; i + 1 < s.size() && (s.elementAt(i + 1).classify(s.elementAt(0), s.elementAt(i)) == 3); i++) {
				; //TODO quick fix by Bertoli Marco ( i+1 < s.size() )
			}
			stk.removeAllElements();
			stk.push(s.elementAt(0));
			stk.push(s.elementAt(i));

			boolean blah;
			for (i = i + 1; i < n; i++) {
				blah = true;
				while (blah) {
					temp2 = stk.pop();
					//TODO quick fix by Bertoli Marco ( stk.empty() )
					if (stk.empty() || s.elementAt(i).classify(stk.peek(), temp2) == 0) {
						stk.push(temp2);
						blah = false;
					}
				}
				stk.push(s.elementAt(i));
			}

			lines.removeAllElements();

			while (!stk.empty()) {
				lines.addElement(stk.pop());
			}
			return lines;
		}
		return null;
	}
}
