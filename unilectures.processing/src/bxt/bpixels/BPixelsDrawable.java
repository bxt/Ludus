package bxt.bpixels;

import bxt.util.Drawable;
import processing.core.PApplet;

public class BPixelsDrawable implements Drawable {
	
	private static final int[] source = {0, 0x77775777, 0x54555555, 0x77657765, 0x54555555, 0x77555557};
	
	private static final int WHITE = 0xffffdd77;
	private static final int BLACK = 0xff003300;
	
	private PApplet p;

	public BPixelsDrawable(PApplet p) {
		this.p = p;
	}

	@Override
	public void draw() {
		p.loadPixels();
		for (int y = 0; y < p.pixelHeight; y++) {
			int offs = y * p.pixelWidth;
			for (int x = 0; x < p.pixelWidth; x++) {
				int offs2 = y/6;
				int a = y - offs2*6;
				int b = 32 - ((x+(p.frameCount/32+offs2)*16) % 32);
				p.pixels[offs + x] = ((source[a]>>b)&1) > 0 ? WHITE : BLACK;
			}
		}
		p.updatePixels();
	}
	
	
	
}
