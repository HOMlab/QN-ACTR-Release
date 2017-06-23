package jmt.engine.jwat.input;

import java.io.IOException;
import java.util.zip.ZipFile;

import jmt.engine.jwat.JwatSession;

import org.w3c.dom.NodeList;

public interface ResultLoader {

	public int loadResult(ZipFile zf, NodeList resultNodeList, JwatSession session) throws IOException;

}
