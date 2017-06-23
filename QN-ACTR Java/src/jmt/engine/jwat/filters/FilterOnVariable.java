package jmt.engine.jwat.filters;

import jmt.engine.jwat.Observation;

public interface FilterOnVariable {
	public boolean isMatching(Observation o, int pos);
}
