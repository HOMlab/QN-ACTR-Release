/**    
  * Copyright (C) 2006, Laboratorio di Valutazione delle Prestazioni - Politecnico di Milano

  * This program is free software; you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation; either version 2 of the License, or
  * (at your option) any later version.

  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.

  * You should have received a copy of the GNU General Public License
  * along with this program; if not, write to the Free Software
  * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
  */
package jmt.engine.jwat.workloadAnalysis.utils;

import java.io.File;
import java.io.IOException;

import javax.swing.filechooser.FileSystemView;

public class SingleDirFileSystem extends FileSystemView {
	private File singleDir;

	public SingleDirFileSystem(File r) {
		singleDir = r;
	}

	@Override
	public File createNewFolder(File containingDir) throws IOException {
		return null;
	}

	@Override
	public File getParentDirectory(File dir) {
		return singleDir;
	}

	@Override
	public Boolean isTraversable(File f) {
		if (f.isDirectory()) {
			return Boolean.FALSE;
		}
		return Boolean.TRUE;
	}
}
