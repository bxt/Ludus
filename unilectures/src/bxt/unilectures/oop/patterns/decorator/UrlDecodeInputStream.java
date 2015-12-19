package bxt.unilectures.oop.patterns.decorator;

import java.io.BufferedReader;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;

public class UrlDecodeInputStream extends FilterInputStream {
	
	private BufferedReader br;
	
	public UrlDecodeInputStream(InputStream in) {
		super(in);
		try {
			br = new BufferedReader(new InputStreamReader(in, "UTF-8"));
		} catch (UnsupportedEncodingException e) {
			throw new IllegalStateException(e);
		}
	}
	
	public String readString() {
		try {
			String line = br.readLine();
			if (line != null) {
				return URLDecoder.decode(line, "UTF-8");
			} else {
				return null;
			}
		} catch (IOException e) {
			throw new IllegalStateException(e);
		}
	}
}
