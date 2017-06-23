/*
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */
package jmt.jmarkov.utils;

import java.text.DateFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.StringTokenizer;

public class Formatter {

	//--- Constant(s) ---

	public static final int LEFT = 0;
	public static final int CENTER = 1;
	public static final int RIGHT = 2;

	public static final String[] SIZES = { "B", "KB", "MB", "GB", "TB" };

	//--- Data field(s) ---

	//--- Constructor(s) ---

	//--- Method(s) ---

	public static String formatNumber(Object value, int decimal) {
		if ((value != null) && (value instanceof Number)) {
			double d = ((Number) value).doubleValue();
			return formatNumber(d, decimal);
		} else {
			return null;
		}
	}

	public static String formatNumber(double number, int decimal) {
		if (number >= 0) {
			NumberFormat n = NumberFormat.getNumberInstance();
			if (decimal > 0) {
				n.setMinimumFractionDigits(decimal);
				n.setMaximumFractionDigits(decimal);
			}
			return n.format(number);
		} else {
			return "";
		}
	}

	public static String formatNumber(double number, int decimal, boolean grouping) {
		if (number >= 0) {
			NumberFormat n = NumberFormat.getNumberInstance();
			n.setGroupingUsed(grouping);
			if (decimal > 0) {
				n.setMinimumFractionDigits(decimal);
				n.setMaximumFractionDigits(decimal);
			}
			return n.format(number);
		} else {
			return "";
		}
	}

	public static String formatNumber(double number) {
		return formatNumber(number, 0);
	}

	/**
	 * Returns <code>value</code> (byte) formatted as a file size.
	 *
	 * For example value=2048 returns "2 kb".
	 */
	public static String formatSize(Object value) {
		if ((value != null) && (value instanceof Number)) {
			double d = ((Number) value).doubleValue();
			return formatSize(d);
		} else {
			return null;
		}
	}

	public static String formatSize(double size) {
		int i = 0;
		for (; i < SIZES.length - 1 && size >= 1024; i++) {
			size /= 1024;
		}

		return formatNumber(size, 1) + " " + SIZES[i];
	}

	public static String formatLength(long i) {
		StringBuffer s = new StringBuffer();

		long x = (i / 1000);
		if (x > 0) {
			s.append(x);
			s.append(":");
		}
		x = (i % 1000) / 60;
		if (x < 10) {
			s.append("0");
		}
		s.append(x);
		s.append(":");
		x = (i % 60);
		if (x < 10) {
			s.append("0");
		}
		s.append(x);

		return s.toString();
	}

	public static String formatTime(long time) {
		return DateFormat.getDateInstance().format(new Date(time));
	}

	public static String formatTable(String[] rows, int[] colAlignment) {
		if (rows.length == 0) {
			return "(list is empty)";
		}

		int[] widths = new int[colAlignment.length];

		for (String row : rows) {
			StringTokenizer t = new StringTokenizer(row, "|");
			for (int col = 0; t.hasMoreTokens(); col++) {
				int l = t.nextToken().length();
				if (l > widths[col]) {
					widths[col] = l;
				}
			}
		}

		StringBuffer sb = new StringBuffer();
		for (String row : rows) {
			StringTokenizer t = new StringTokenizer(row, "|");
			for (int col = 0; col < colAlignment.length; col++) {
				String s = (t.hasMoreTokens()) ? t.nextToken() : "";
				int appendCount = widths[col] - s.length();

				switch (colAlignment[col]) {
					case LEFT:
						sb.append(s);
						sb.append(fill(" ", appendCount));
						break;
					case CENTER:
						int a = appendCount / 2;
						sb.append(fill(" ", a));
						sb.append(s);
						sb.append(fill(" ", appendCount - a));
						break;
					case RIGHT:
						sb.append(fill(" ", appendCount));
						sb.append(s);
						break;
				}

				if (col < colAlignment.length - 1) {
					sb.append(" | ");
				}
			}
			sb.append("\n");
		}

		return sb.toString();
	}

	public static String fill(String s, int count) {
		StringBuffer sb = new StringBuffer(count);
		for (int i = 0; i < count; i++) {
			sb.append(s);
		}
		return sb.toString();
	}

	/**
	 * Returns the current date in the format needed for CLF
	 *
	 * Example: 15/Dec/2002:23:46:16
	 */
	public static String getCLFDate() {
		String pattern = "dd/MMM/yyyy:HH:mm:ss";
		SimpleDateFormat df = new SimpleDateFormat(pattern);
		return df.format(new Date());
	}

	public static String shortDate() {
		return DateFormat.getTimeInstance(DateFormat.SHORT).format(new Date());
	}
}
