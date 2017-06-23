package jmt.engine.jwat;

import java.io.DataOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import jmt.engine.jwat.input.Mapping;
import jmt.gui.jwat.JWATConstants;
import jmt.gui.jwat.JWatModel;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

public abstract class JwatSession {

	protected String filepath;
	protected String filename;
	protected JWatModel model = null;

	public static String XMLext = ".xml";
	public static String ZIPext = ".jwat";
	public static String BINext = ".bin";
	public static String ROOT = "JWat_Save";
	public static String WORKLOAD_SAVE = "Workload Analysis";
	public static String TRAFFIC_SAVE = "Traffic Analysis";
	public static String DATA = "Data_File";

	protected JwatSession(JWatModel model) {
		this.model = model;
		this.filepath = null;
		this.filename = null;
	}

	protected JwatSession(JWatModel model, String filepath, String filename) {
		this.model = model;
		this.filepath = filepath;
		this.filename = filename;
	}

	public JWatModel getDataModel() {
		return model;
	}

	public void saveSession(String filepath, String filename, String type) {
		MatrixOsservazioni matrix = model.getMatrix();
		this.filepath = filepath;
		this.filename = filename;
		//System.out.println("PATH " + filepath);
		//System.out.println("NAME " + filename);
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		try {
			ZipOutputStream zos = new ZipOutputStream((new FileOutputStream(filepath + filename + ZIPext)));
			DocumentBuilder db = dbf.newDocumentBuilder();
			Document doc = db.newDocument();
			//Init root
			Element root = doc.createElement(ROOT);
			doc.appendChild(root);
			root.setAttribute("type", type);
			root.setAttribute("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance");
			root.setAttribute("xsi:noNamespaceSchemaLocation", "jwatsave.xsd");

			Observation[] obs = matrix.getListOss();
			VariableNumber[] var = matrix.getVariables();

			saveVariablesInfo(var, doc, root, zos);
			saveDataInfo(obs, doc, root, zos);

			appendXMLResults(doc, root, zos);

			Transformer tr = TransformerFactory.newInstance().newTransformer();
			tr.setOutputProperty(OutputKeys.INDENT, "yes");
			tr.setOutputProperty(OutputKeys.METHOD, "xml");
			tr.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "3");

			//Write XML file into zip
			zos.putNextEntry(new ZipEntry(filename + XMLext));
			tr.transform(new DOMSource(doc), new StreamResult(zos));
			zos.closeEntry();

			//Save matrix data in a separate binary file
			saveMatrixData(obs, var, zos);
			saveResultsFile(doc, root, zos);
			zos.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private void saveVariablesInfo(VariableNumber[] var, Document doc, Element root, ZipOutputStream zos) {
		Element varEl = doc.createElement("Variables");
		Element tmp;
		int numVar = var.length;

		varEl.setAttribute("num", String.valueOf(numVar));

		for (int i = 0; i < numVar; i++) {
			tmp = doc.createElement("Variable");
			tmp.setAttribute("name", var[i].getName());
			tmp.setAttribute("type", String.valueOf(var[i].getType()));
			varEl.appendChild(tmp);
		}
		root.appendChild(varEl);
	}

	private void saveDataInfo(Observation[] obs, Document doc, Element root, ZipOutputStream zos) {

		Element dataEl = doc.createElement("Data");
		dataEl.setAttribute("size", String.valueOf(obs.length));
		dataEl.setAttribute("filename", filename + BINext);

		root.appendChild(dataEl);
	}

	private void saveMatrixData(Observation[] obs, VariableNumber[] var, ZipOutputStream zos) {
		//System.out.println("Saving matrix");
		try {
			int i, j;
			int size;

			size = obs[0].getSize();

			//save varaible mapping in a separate file
			for (j = 0; j < size; j++) {
				if (var[j].getType() == JWATConstants.STRING) {
					saveVarMapping(var[j], zos);
				}
			}
			//save data
			zos.putNextEntry(new ZipEntry(filename + BINext));
			DataOutputStream dos = new DataOutputStream(zos);
			for (i = 0; i < obs.length; i++) {
				dos.writeInt(obs[i].getID());
				for (j = 0; j < size; j++) {
					dos.writeDouble(obs[i].getIndex(j));
				}
			}
			dos.flush();
			zos.closeEntry();

			//System.out.println("Save done");
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private void saveVarMapping(VariableNumber var, ZipOutputStream zos) throws IOException {
		//System.out.println("VAR "+ filepath + var.getName()+"_Map"+BINext);

		zos.putNextEntry(new ZipEntry(var.getName() + "_Map" + BINext));
		DataOutputStream dos = new DataOutputStream(zos);

		Mapping[] map = var.getMapping().getMappingValue();

		dos.write(map.length);
		for (Mapping element : map) {
			dos.writeDouble(element.getConversion());
			dos.writeUTF(element.getValue().toString());
		}
		dos.flush();
		zos.closeEntry();

	}

	public abstract void appendXMLResults(Document doc, Element root, ZipOutputStream zos);

	public abstract void saveResultsFile(Document doc, Element root, ZipOutputStream zos) throws IOException;

	public abstract void resetSession();

	public abstract void copySession(JwatSession newSession);
}
