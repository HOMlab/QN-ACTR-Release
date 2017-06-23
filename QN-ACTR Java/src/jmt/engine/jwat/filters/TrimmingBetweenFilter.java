package jmt.engine.jwat.filters;

import jmt.engine.jwat.Observation;

public class TrimmingBetweenFilter implements FilterOnVariable {
	private int min;
	private int max;

	public TrimmingBetweenFilter(int mi, int mx) {
		min = mi;
		max = mx;
	}

	/**
	 * Ritorna true se l'osservazione matcha il filtro
	 */
	public boolean isMatching(Observation o, int pos) {
		if (pos <= max && pos >= min) {
			return true;
		}
		return false;
	}

}
