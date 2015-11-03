package bxt.util;

import java.util.function.Supplier;

import processing.core.PApplet;

public class TextPrinter implements Drawable {
	
	private Supplier<String> label;
	
	private String previousOutput = null;
	
	public TextPrinter(Supplier<String> label) {
		this.label = label;
	}

	@Override
	public void draw() {
		String output = label.get();
		if(!output.equals(previousOutput)) {
			PApplet.println(output);
			previousOutput = output;
		}
	}
	

}
