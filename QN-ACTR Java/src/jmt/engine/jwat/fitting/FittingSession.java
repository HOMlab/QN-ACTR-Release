package jmt.engine.jwat.fitting;

import java.io.IOException;
import java.util.zip.ZipOutputStream;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import jmt.engine.jwat.JwatSession;
import jmt.engine.jwat.fitting.utils.ModelFitting;
import jmt.engine.jwat.trafficAnalysis.ModelTrafficAnalysis;
import jmt.gui.jwat.JWatModel;

public class FittingSession extends JwatSession {
	
	public FittingSession() {
		super(new ModelFitting());
	}


	protected FittingSession(JWatModel model) {
		super(new ModelFitting());
		// TODO Auto-generated constructor stub
	}

	@Override
	public void appendXMLResults(Document doc, Element root, ZipOutputStream zos) {
		// TODO Auto-generated method stub		
	}

	@Override
	public void copySession(JwatSession newSession) {
		// TODO Auto-generated method stub		
	}

	@Override
	public void resetSession() {
		model.resetModel();		
	}

	@Override
	public void saveResultsFile(Document doc, Element root, ZipOutputStream zos)
			throws IOException {
		// TODO Auto-generated method stub		
	}

}
