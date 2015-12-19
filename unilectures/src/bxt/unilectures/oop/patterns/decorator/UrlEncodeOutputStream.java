package bxt.unilectures.oop.patterns.decorator;

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URLEncoder;

public class UrlEncodeOutputStream extends FilterOutputStream {

	public UrlEncodeOutputStream(OutputStream out) {
		super(out);
	}
	
	public void writeString(String s) {
		try {
			write(URLEncoder.encode(s, "UTF-8").getBytes("UTF-8"));
		} catch (IOException e) {
			throw new IllegalStateException(e);
		}
	}

}
