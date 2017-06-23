package jmt.engine.jwat.trafficAnalysis;

import java.io.IOException;
import java.util.ArrayList;
import java.util.zip.ZipOutputStream;

import jmt.engine.jwat.JwatSession;
import jmt.engine.jwat.MatrixOsservazioni;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class TrafficAnalysisSession extends JwatSession {

	private BurstEngine engine = null;
	private int epochs = -1;
	private ArrayList<OnSetParamtersListener> setParameters = new ArrayList<OnSetParamtersListener>();

	public TrafficAnalysisSession() {
		super(new ModelTrafficAnalysis());
	}

	public TrafficAnalysisSession(ModelTrafficAnalysis model, int epochs) {
		super(model);
		engine = new BurstEngine(model.getMatrix().getVariables()[0], epochs, null);
		this.epochs = epochs;
	}

	public TrafficAnalysisSession(ModelTrafficAnalysis model, String filepath, String filename, int epochs) {
		super(model, filepath, filename);
		engine = new BurstEngine(model.getMatrix().getVariables()[0], epochs, null);
		this.epochs = epochs;
	}

	public void addSetParamsListener(OnSetParamtersListener lst) {
		if (!setParameters.contains(lst)) {
			setParameters.add(lst);
		}
	}

	public void removeSetParamsListener(OnSetParamtersListener lst) {
		setParameters.remove(lst);
	}

	private void notifySetParams() {
		for (int i = 0; i < setParameters.size(); i++) {
			setParameters.get(i).ParamsSetted();
		}
	}

	public BurstEngine getEngine() {
		return engine;
	}

	@Override
	public void resetSession() {
		model.resetModel();
	}

	public void setMatrix(MatrixOsservazioni m) {
		model.setMatrix(m);
		if (epochs != -1) {
			engine = new BurstEngine(m.getVariables()[0], epochs, null);
			notifySetParams();
		}
	}

	public void setParameters(int epochs) {
		this.epochs = epochs;
		if (model.getMatrix() != null) {
			engine = new BurstEngine(model.getMatrix().getVariables()[0], epochs, null);
			notifySetParams();
		}
	}

	@Override
	public void appendXMLResults(Document doc, Element root, ZipOutputStream zos) {

	}

	@Override
	public void saveResultsFile(Document doc, Element root, ZipOutputStream zos) throws IOException {

	}

	@Override
	public void copySession(JwatSession newSession) {
		model.setMatrix(newSession.getDataModel().getMatrix());
	}
}
