package jmt.engine.jwat.workloadAnalysis.exceptions;

/**
 * Created by IntelliJ IDEA.
 * User: Administrator
 * Date: 28-gen-2004
 * Time: 11.21.44
 * To change this template use Options | File Templates.
 */
public class TrasformException extends Exception {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public TrasformException(String m) {
		msg = m;
	}

	public String getMsg() {
		return msg;
	}

	String msg;
}
