package bxt.unilectures.vorkurs.visitenkarte;

import org.apache.commons.lang.StringUtils;

/**
 * Class for building a ASCII box string
 * @author bxt
 *
 */
public class BoxBuilder {

	/**
	 * Inner width of the box, without padding and border
	 */
	int width;
	/**
	 * Holding the result
	 */
	StringBuilder outStr=null;
	
	/**
	 * Constants defining appearance of the box border
	 * @author bxt
	 */
	protected static class Border {
			public static final char vertical='|';
			public static final char horizontal='-';
			public static final char edge='*';
			public static final int verticalPadding=3;
			public static final char space=' ';
	}
	
	/**
	 * Render some text left aligned
	 * @param line input text, keep shorter than width
	 */
	protected void renderLineLeftAligned(String line) {
		renderLine(StringUtils.rightPad(line, width));
	}

	/**
	 * Render some text right aligned
	 * @param line input text, keep shorter than width
	 */
	protected void renderLineRightAligned(String line) {
		renderLine(StringUtils.leftPad(line, width));
	}

	/**
	 * Render top/bottom frame
	 */
	protected void renderLineBorder() {
		outStr.append(Border.edge);
		for(int i=0;i<width+Border.verticalPadding*2;i++) {
			outStr.append(Border.horizontal);
		}
		outStr.append(Border.edge);
		outStr.append("\n");
	}
	
	/**
	 * Render a line
	 * @param line input text, length shortened to width
	 */
	private void renderLine(String line) {
		outStr.append(Border.vertical);
		for(int i=0;i<Border.verticalPadding;i++) {
			outStr.append(Border.space);
		}
		outStr.append(StringUtils.abbreviate(line, width));
		for(int i=0;i<Border.verticalPadding;i++) {
			outStr.append(Border.space);
		}
		outStr.append(Border.vertical);
		outStr.append("\n");
	}

}