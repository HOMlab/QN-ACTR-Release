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

package jmt.gui.common.xml;

import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

/**

 * @author Max
 * Date: 17-set-2003
 * Time: 19.43.45

 */
public class JmtErrorHandler implements ErrorHandler {
	/**
	 * Receive notification of a warning.
	 *
	 * <p>SAX parsers will use this method to report conditions that
	 * are not errors or fatal errors as defined by the XML 1.0
	 * recommendation.  The default behaviour is to take no action.</p>
	 *
	 * <p>The SAX parser must continue to provide normal parsing events
	 * after invoking this method: it should still be possible for the
	 * application to process the document through to the end.</p>
	 *
	 * <p>Filters may use this method to report other, non-XML warnings
	 * as well.</p>
	 *
	 * @param exception The warning information encapsulated in a
	 *                  SAX parse exception.
	 * @exception org.xml.sax.SAXException Any SAX exception, possibly
	 *            wrapping another exception.
	 * @see org.xml.sax.SAXParseException
	 */
	public void warning(SAXParseException exception) throws SAXException {
		System.err.println("Warning: " + exception.getMessage());
		//		XMLLoader.success = false;
		throw exception;
	}

	/**
	 * Receive notification of a recoverable error.
	 *
	 * <p>This corresponds to the definition of "error" in section 1.2
	 * of the W3C XML 1.0 Recommendation.  For example, a validating
	 * parser would use this callback to report the violation of a
	 * validity constraint.  The default behaviour is to take no
	 * action.</p>
	 *
	 * <p>The SAX parser must continue to provide normal parsing events
	 * after invoking this method: it should still be possible for the
	 * application to process the document through to the end.  If the
	 * application cannot do so, then the parser should report a fatal
	 * error even if the XML 1.0 recommendation does not require it to
	 * do so.</p>
	 *
	 * <p>Filters may use this method to report other, non-XML errors
	 * as well.</p>
	 *
	 * @param exception The error information encapsulated in a
	 *                  SAX parse exception.
	 * @exception org.xml.sax.SAXException Any SAX exception, possibly
	 *            wrapping another exception.
	 * @see org.xml.sax.SAXParseException
	 */
	public void error(SAXParseException exception) throws SAXException {
		//		System.err.println(exception.getColumnNumber());
		//		throw new SAXException(exception.getSystemId());
		System.err.println("Error while parsing: " + exception.getMessage());
		throw exception;
	}

	/**
	 * Receive notification of a non-recoverable error.
	 *
	 * <p>This corresponds to the definition of "fatal error" in
	 * section 1.2 of the W3C XML 1.0 Recommendation.  For example, a
	 * parser would use this callback to report the violation of a
	 * well-formedness constraint.</p>
	 *
	 * <p>The application must assume that the document is unusable
	 * after the parser has invoked this method, and should continue
	 * (if at all) only for the sake of collecting addition error
	 * messages: in fact, SAX parsers are free to stop reporting any
	 * other events once this method has been invoked.</p>
	 *
	 * @param exception The error information encapsulated in a
	 *                  SAX parse exception.
	 * @exception org.xml.sax.SAXException Any SAX exception, possibly
	 *            wrapping another exception.
	 * @see org.xml.sax.SAXParseException
	 */
	public void fatalError(SAXParseException exception) throws SAXException {
		System.err.println("Fatal error while parsing: " + exception.getMessage());
		throw exception;
	}
}
