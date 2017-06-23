package jmt.engine.jwat.workloadAnalysis.utils;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Vector;

public class FormatFileWriter {

	public FormatFileWriter(Vector<Object> names, Vector comments, Vector delimiters, Vector regExprs, int[] types, String name, Vector defualts,
			Vector replaces) {
		File fileN = new File(name);
		try {
			BufferedWriter writer = new BufferedWriter(new FileWriter(fileN));
			writer.write(new String(names.size() + "\n"));
			for (int i = 0; i < names.size(); i++) {
				writer.write((String) names.get(i) + "\n");
				writer.write(Integer.toString(types[i]) + "\n");
				writer.write("1\n");
				writer.write((String) comments.get(i) + "\n");
				writer.write((String) delimiters.get(i) + "\n");
				writer.write((String) regExprs.get(i) + "\n");
				writer.write((String) defualts.get(i) + "\n");
				writer.write((String) replaces.get(i) + "\n");
			}
			writer.close();
		} catch (IOException e) {
			System.err.println("Errore nella creazione del file di salvataggio del formato");
			e.printStackTrace();
		}
	}
}
