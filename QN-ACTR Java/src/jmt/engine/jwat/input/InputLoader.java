package jmt.engine.jwat.input;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.ArrayList;

import jmt.engine.jwat.MatrixOsservazioni;
import jmt.engine.jwat.Observation;
import jmt.engine.jwat.TimeConsumingWorker;

public abstract class InputLoader extends TimeConsumingWorker {

	protected Parameter param;
	protected BufferedReader reader;
	protected VariableMapping[] map;
	protected ArrayList<Observation> valori;
	protected int countObs, totalRaw;
	protected String msg;

	public InputLoader(Parameter param, String fileName, VariableMapping[] map, ProgressShow prg) throws FileNotFoundException {
		super(prg);
		this.param = param;
		this.map = map;
		valori = new ArrayList<Observation>();
		reader = new BufferedReader(new FileReader(fileName));
	}

	@Override
	public void finished() {
		if (this.get() != null) {
			fireEventStatus(new EventFinishLoad((MatrixOsservazioni) this.get(), totalRaw, countObs));
		} else {
			fireEventStatus(new EventFinishAbort(msg));
		}
	}
}
