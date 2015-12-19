package bxt.unilectures.oop.patterns.decorator;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

public class DecoratorApp {
	public static void main(String[] args) throws IOException, IOException {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		UrlEncodeOutputStream ueos = new UrlEncodeOutputStream(baos);
		
		ueos.writeString("Hallo, Welt!\n");
		ueos.writeString("Dies soll ein url-encodierter String werden!\n");
		ueos.writeString("Es sollen, unter anderem, / ersetzt werden.\n");
		
		ueos.close();
		
		String result = baos.toString("UTF-8").replace("%0A", "\n");
		
		// The assignment is not very clear about how
		// to handle URL-encoded newlines, so we get rid of them:
		result = result.replace("%0A", "\n");
		
		System.out.println(result);
		
		ByteArrayInputStream bais = new ByteArrayInputStream(result.getBytes("UTF-8"));
		UrlDecodeInputStream udis = new UrlDecodeInputStream(bais);
		
		String line;
		while((line = udis.readString()) != null) {
			System.out.println(line);
		}
		
		udis.close();
	}
}
