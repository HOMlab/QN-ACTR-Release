package jmt.engine.jwat.input;

import jmt.engine.jwat.JwatSession;

public class EventSessionLoaded implements EventStatus {

	private JwatSession loadedSession;
	protected String msg;

	public EventSessionLoaded(JwatSession session) {
		loadedSession = session;
	}

	public int getType() {
		return DONE_EVENT;
	}

	public JwatSession getSession() {
		return loadedSession;
	}

}
