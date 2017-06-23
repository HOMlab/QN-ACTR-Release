package jmt.engine.jwat.workloadAnalysis.clustering.kMean;

import java.lang.reflect.InvocationTargetException;

import jmt.engine.jwat.MatrixOsservazioni;
import jmt.engine.jwat.TimeConsumingWorker;
import jmt.engine.jwat.VariableNumber;
import jmt.engine.jwat.input.EventFinishAbort;
import jmt.engine.jwat.input.ProgressShow;
import jmt.engine.jwat.workloadAnalysis.clustering.EventClusteringDone;

//UPDATE 29/10/2006: + aggiunto passaggio parametro costruttore del tipo di trasformazione da applicare a variabili selezionate
//+ aggiunta trasformazione e anti a variabili selezionate per clustering			
//UPDATE 31/10/2006: + aggiunto calcolo delle statistiche clustering su variabili non trasformate

public class MainKMean extends TimeConsumingWorker {

	private short trasf;
	private int[] varSel;
	private int numClust;
	private int iteration;
	private MatrixOsservazioni m;
	private KMean clustering;
	private String msg = null;
	private KMeanClusteringEngine Cluster;

	public MainKMean(ProgressShow prg, MatrixOsservazioni m, int[] varSel, int numClust, int iteration, short trasf) {
		super(prg);
		this.m = m;
		this.iteration = iteration;
		this.numClust = numClust;
		this.varSel = varSel;
		clustering = new KMean(numClust, varSel);
		Cluster = new KMeanClusteringEngine(clustering, this);
		this.trasf = trasf;
	}

	@Override
	public Object construct() {
		boolean anti = false;
		try {
			initShow((iteration * numClust) + 3);
			updateInfos(1, "Initializing KMeans Clustering", true);
		} catch (InterruptedException e) {
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			e.printStackTrace();
		}
		//Applicazione trasformazione alle variabili coinvolte nel clustering
		if (trasf != VariableNumber.NONE) {
			for (int element : varSel) {
				m.getVariables()[element].doClusteringTrasformation(trasf);
			}
		}
		if (isCanceled()) {
			msg = "CLUSTERING ABORTED BY USER";
			if (trasf != VariableNumber.NONE) {
				for (int element : varSel) {
					m.getVariables()[element].undoClueringTrasformation();
				}
			}
			return null;
		}
		try {
			// Variabili | numero variabili totale | numero di variaibli coinvolte | trasformazione | num Max Cluster | num Max Iterazioni | Elenco indici delle variabili coinvolte
			Cluster.PrepClustering(m, numClust, iteration, varSel);
			if (isCanceled()) {
				msg = "CLUSTERING ABORTED BY USER";
				if (trasf != VariableNumber.NONE) {
					for (int element : varSel) {
						m.getVariables()[element].undoClueringTrasformation();
					}
				}
				return null;
			}
			if (!Cluster.DoClustering()) {
				msg = "CLUSTERING ABORTED BY USER";
				if (trasf != VariableNumber.NONE) {
					for (int element : varSel) {
						m.getVariables()[element].undoClueringTrasformation();
					}
				}
				return null;
			}
			//Antitrasformazione delle variabili coinvolte

			if (trasf != VariableNumber.NONE) {
				anti = true;
				for (int element : varSel) {
					m.getVariables()[element].undoClueringTrasformation();
				}
			}
			updateInfos((iteration * numClust) + 2, "Saving Results", true);
			//Calcolo delle statistiche clustering eseguito
			for (int i = 0; i < clustering.getNumCluster(); i++) {
				((ClusteringInfosKMean) clustering.getClusteringInfos(i)).DOStat(varSel, clustering.getAsseg()[i], m);
			}
			updateInfos((iteration * numClust) + 3, "END", true);

		} catch (OutOfMemoryError err) {
			updateInfos((iteration * numClust) + 3, "errore", false);
			msg = "Out of Memory. Try with more memory (1Gb JMT Version)";
			if (trasf != VariableNumber.NONE && !anti) {
				for (int element : varSel) {
					m.getVariables()[element].undoClueringTrasformation();
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
