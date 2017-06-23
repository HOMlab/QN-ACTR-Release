package jmt.engine.jwat.workloadAnalysis.clustering.fuzzyKMean;

import java.lang.reflect.InvocationTargetException;

import jmt.engine.jwat.MatrixOsservazioni;
import jmt.engine.jwat.TimeConsumingWorker;
import jmt.engine.jwat.VariableNumber;
import jmt.engine.jwat.input.EventFinishAbort;
import jmt.engine.jwat.input.ProgressShow;
import jmt.engine.jwat.workloadAnalysis.clustering.EventClusteringDone;

public class MainFuzzyKMean extends TimeConsumingWorker {

	private MatrixOsservazioni matrix;
	private int maxClust;
	private int maxIter;
	private int fuzzyLevel;
	private int[] varSel;
	private short trasf;
	private FuzzyKMean clustering = null;
	private FuzzyKMeanClusteringEngine cluster = null;
	private String msg = null;

	public MainFuzzyKMean(ProgressShow prg, MatrixOsservazioni m, int[] varSel, int numClust, int iteration, int fuzzyL, short trasf) {
		super(prg);
		this.varSel = varSel;
		this.matrix = m;
		this.maxClust = numClust;
		this.maxIter = iteration;
		this.fuzzyLevel = fuzzyL;
		this.trasf = trasf;
		clustering = new FuzzyKMean(maxClust, varSel);
		cluster = new FuzzyKMeanClusteringEngine(clustering, this);
	}

	@Override
	public Object construct() {
		boolean anti = false;
		try {
			initShow((maxIter * maxClust) + 3);
			updateInfos(1, "Initializing FuzzyKMeans Clustering", true);
		} catch (InterruptedException e) {
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			e.printStackTrace();
		}
		//Applicazione trasformazione alle variabili coinvolte nel clustering
		if (trasf != VariableNumber.NONE) {
			for (int element : varSel) {
				matrix.getVariables()[element].doClusteringTrasformation(trasf);
			}
		}
		if (isCanceled()) {
			msg = "CLUSTERING ABORTED BY USER";
			if (trasf != VariableNumber.NONE) {
				for (int element : varSel) {
					matrix.getVariables()[element].undoClueringTrasformation();
				}
			}
			return null;
		}
		try {
			/* Preparazione infos */
			cluster.PrepFClustering(matrix, varSel, maxClust, fuzzyLevel, maxIter);
			if (isCanceled()) {
				msg = "CLUSTERING ABORTED BY USER";
				if (trasf != VariableNumber.NONE) {
					for (int element : varSel) {
						matrix.getVariables()[element].undoClueringTrasformation();
					}
				}
				return null;
			}
			cluster.DoFClustering();
			if (isCanceled()) {
				msg = "CLUSTERING ABORTED BY USER";
				if (trasf != VariableNumber.NONE) {
					for (int element : varSel) {
						matrix.getVariables()[element].undoClueringTrasformation();
					}
				}
				return null;
			}
			if (trasf != VariableNumber.NONE) {
				anti = true;
				for (int element : varSel) {
					matrix.getVariables()[element].undoClueringTrasformation();
				}
			}
			updateInfos((maxIter * maxClust) + 2, "Saving Results", true);
			//Calcolo delle statistiche clustering eseguito
			//for(int i = 0; i < clustering.getNumCluster();i++)
			//clustering.getClusteringInfos(i).DOStat(varSel,clustering.getAsseg()[i],matrix);
			updateInfos((maxIter * maxClust) + 3, "END", true);

		} catch (OutOfMemoryError err) {
			updateInfos((maxIter * maxClust) + 3, "errore", false);
			msg = "Out of Memory. Try with more memory (1Gb JMT Version)";
			if (trasf != VariableNumber.NONE && !anti) {
				for (int element : varSel) {
					matrix.getVariables()[element].undoClueringTrasformation();
				}
			}
			return null;
		}
		return clustering;
	}

	@Override
	public void finished() {
		if (this.get() != null) {
			fireEventStatus(new EventClusteringDone(clustering));
		} else {
			fireEventStatus(new EventFinishAbort(msg));
		}
	}

}
