package bxt.util;

import java.util.function.Supplier;

import processing.core.PApplet;

/**
 * Prints a changing string.
 */
public class TextDrawer implements Drawable {
	
	private PApplet p;
	private Supplier<String> label;
	private int color;
	private int xOffset;
	private int yOffset;
	
	public TextDrawer(PApplet p, Supplier<String> label) {
		this(p, label, 0xff999999, 20, 20);
	}
	
	public TextDrawer(PApplet p, Supplier<String> label, int color, int xOffset, int yOffset) {
		this.p = p;
		this.label = label;
		this.color = color;
		this.xOffset = xOffset;
		this.yOffset = yOffset;
	}
	
	@Override
	public void draw() {
		p.fill(color);
		String output = label.get();
		if(output != null) {
			p.text(output, xOffset, yOffset);
		}
	}

}
