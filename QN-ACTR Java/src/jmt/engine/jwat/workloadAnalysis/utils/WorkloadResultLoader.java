package jmt.engine.jwat.workloadAnalysis.utils;

import java.io.DataInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.StringTokenizer;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import jmt.engine.jwat.JwatSession;
import jmt.engine.jwat.MatrixOsservazioni;
import jmt.engine.jwat.input.ResultLoader;
import jmt.engine.jwat.workloadAnalysis.WorkloadAnalysisSession;
import jmt.engine.jwat.workloadAnalysis.clustering.Clustering;
import jmt.engine.jwat.workloadAnalysis.clustering.fuzzyKMean.ClusterInfoFuzzy;
import jmt.engine.jwat.workloadAnalysis.clustering.fuzzyKMean.ClusteringInfosFuzzy;
import jmt.engine.jwat.workloadAnalysis.clustering.fuzzyKMean.FuzzyKMean;
import jmt.engine.jwat.workloadAnalysis.clustering.kMean.ClusterInfoKMean;
import jmt.engine.jwat.workloadAnalysis.clustering.kMean.ClusteringInfosKMean;
import jmt.engine.jwat.workloadAnalysis.clustering.kMean.KMean;
import jmt.gui.jwat.JWATConstants;

import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class WorkloadResultLoader implements ResultLoader {
	private WorkloadAnalysisSession waSession;

	public int loadResult(ZipFile zf, NodeList resultNodeList, JwatSession session) throws IOException {
		int i, nclustLoaded = 0, numSave;
		Node tmpNode;
		Clustering tmpClust = null;
		String name, algo, numCluster, varSel;
		int varSelLst[];
		waSession = (WorkloadAnalysisSession) session;

		ArrayList resultList = new ArrayList();
		tmpNode = resultNodeList.item(0);
		numSave = Integer.parseInt(tmpNode.getAttributes().getNamedItem("num").getNodeValue());
		resultNodeList = tmpNode.getChildNodes();

		for (i = 0; i < resultNodeList.getLength(); i++) {
			tmpNode = resultNodeList.item(i);
			//System.out.println("Clustering:");
			if (tmpNode.getNodeType() == Node.ELEMENT_NODE) {
				algo = tmpNode.getAttributes().getNamedItem("algo").getNodeValue();
				//System.out.println("TIPO " + algo);

				name = tmpNode.getAttributes().getNamedItem("name").getNodeValue();
				//System.out.println("NOME " + name);

				numCluster = tmpNode.getAttributes().getNamedItem("numcluster").getNodeValue();
				//System.out.println("NUMCLUSTER " + numCluster);

				varSel = tmpNode.getAttributes().getNamedItem("varsel").getNodeValue();
				//System.out.println("VARSEL " + varSel);

				varSelLst = parseVarsSel(varSel);
				switch (Integer.parseInt(algo)) {
					case JWATConstants.KMEANS:
						tmpClust = loadKmeanResult(Integer.parseInt(numCluster), varSelLst, name + "_" + nclustLoaded + JwatSession.BINext, zf);
						break;
					case JWATConstants.FUZZYK:
						tmpClust = loadFuzzyResult(Integer.parseInt(numCluster), varSelLst, name + "_" + nclustLoaded + JwatSession.BINext, zf);
						break;

					default:
						System.out.println("ALGORITHM NOT DEFINED");
						break;
				}

				waSession.addClustering(tmpClust);
				nclustLoaded++;
			}

		}
		return nclustLoaded;
	}

	private int[] parseVarsSel(String varSel) {
		int varSelLst[];
		StringTokenizer st = new StringTokenizer(varSel, ",");
		varSelLst = new int[st.countTokens()];

		for (int i = 0; i < st.countTokens(); i++) {
			varSelLst[i] = Integer.parseInt(st.nextToken());
		}
		return varSelLst;
	}

	private KMean loadKmeanResult(int numCluster, int varSelLst[], String fileName, ZipFile zf) throws IOException {
		ClusteringInfosKMean curInfo[];
		int i, j, k;
		int curNumCluster;
		MatrixOsservazioni m = waSession.getDataModel().getMatrix();
		int numvars = m.getNumVariables();
		int numoss = m.getNumOfObs();
		DataInputStream dis = new DataInputStream(zf.getInputStream(new ZipEntry(fileName)));
		short asseg[][];

		//init result vector
		curInfo = new ClusteringInfosKMean[numCluster];
		asseg = new short[numCluster][numoss];

		//System.out.println("Loading kMeans Results "+fileName);
		for (i = 0; i < numCluster; i++) {
			curNumCluster = dis.readInt();
			curInfo[i] = new ClusteringInfosKMean(curNumCluster - 1, numvars);
			curInfo[i].omsr = dis.readDouble();
			curInfo[i].ratio = dis.readDouble();
			for (k = 0; k < curNumCluster; k++) {
				curInfo[i].infoCluster[k] = new ClusterInfoKMean(numvars);
				curInfo[i].infoCluster[k].numOss = dis.readInt();
				for (j = 0; j < numvars; j++) {
					curInfo[i].infoCluster[k].percVar[j] = dis.readDouble();
					curInfo[i].infoCluster[k].statClust[j].iNotZr = dis.readInt();
					curInfo[i].infoCluster[k].statClust[j].dMedia = dis.readDouble();
					curInfo[i].infoCluster[k].statClust[j].dStdEr = dis.readDouble();
					curInfo[i].infoCluster[k].statClust[j].dStdDv = dis.readDouble();
					curInfo[i].infoCluster[k].statClust[j].dVarnz = dis.readDouble();
					curInfo[i].infoCluster[k].statClust[j].dKurto = dis.readDouble();
					curInfo[i].infoCluster[k].statClust[j].dSkewn = dis.readDouble();
					curInfo[i].infoCluster[k].statClust[j].dRange = dis.readDouble();
					curInfo[i].infoCluster[k].statClust[j].dMaxOs = dis.readDouble();
					curInfo[i].infoCluster[k].statClust[j].dMinOs = dis.readDouble();
				}
			}
			for (k = 0; k < numoss; k++) {
				asseg[i][k] = dis.readShort();
			}
		}
		return new KMean(curInfo, varSelLst, asseg);
	}

	private FuzzyKMean loadFuzzyResult(int numCluster, int varSelLst[], String fileName, ZipFile zf) throws IOException {
		ClusteringInfosFuzzy curInfo[];
		int i, j, k;
		int curNumCluster;
		MatrixOsservazioni m = waSession.getDataModel().getMatrix();
		int numvars = m.getNumVariables();
		int numoss = m.getNumOfObs();
		DataInputStream dis = new DataInputStream(zf.getInputStream(new ZipEntry(fileName)));
		double asseg[][];
		double entropy[];
		double error;
		ClusterInfoFuzzy infoCluster[];
		FuzzyKMean fkm = new FuzzyKMean(numCluster + 1, varSelLst);

		//init result vector
		curInfo = new ClusteringInfosFuzzy[numCluster];
		entropy = new double[numCluster];

		//System.out.println("Loading Fuzzy kMeans Results "+fileName);
		for (i = 0; i < numCluster; i++) {
			curNumCluster = dis.readInt();
			entropy[i] = dis.readDouble();
			error = dis.readDouble();
			infoCluster = new ClusterInfoFuzzy[curNumCluster];
			if (error != -1) {
				for (k = 0; k < curNumCluster; k++) {
					infoCluster[k] = new ClusterInfoFuzzy(numvars);
					infoCluster[k].numOss = dis.readInt();
					for (j = 0; j < numvars; j++) {
						infoCluster[k].percVar[j] = dis.readDouble();
						infoCluster[k].statClust[j].iNotZr = dis.readInt();
						infoCluster[k].statClust[j].dMedia = dis.readDouble();
						infoCluster[k].statClust[j].dStdEr = dis.readDouble();
						infoCluster[k].statClust[j].dStdDv = dis.readDouble();
						infoCluster[k].statClust[j].dVarnz = dis.readDouble();
						infoCluster[k].statClust[j].dKurto = dis.readDouble();
						infoCluster[k].statClust[j].dSkewn = dis.readDouble();
						infoCluster[k].statClust[j].dRange = dis.readDouble();
						infoCluster[k].statClust[j].dMaxOs = dis.readDouble();
						infoCluster[k].statClust[j].dMinOs = dis.readDouble();
					}
				}
			}
			int nel;
			asseg = new double[curNumCluster][numoss];
			for (k = 0; k < curNumCluster; k++) {
				//nel=dis.readInt();
				for (j = 0; j < numoss; j++) {
					asseg[k][j] = dis.readDouble();
				}
			}

			fkm.setAssign(i, asseg);
			curInfo[i] = (ClusteringInfosFuzzy) fkm.getClusteringInfos(i);
			curInfo[i].infoCluster = infoCluster;
			curInfo[i].setError(m, error);

		}

		fkm.setEntropy(entropy);
		return fkm;
	}

}
