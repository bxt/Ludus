package bxt.video.sampler.drawers;

import bxt.util.Drawable;
import bxt.video.sampler.colorfilters.ColorFilter;
import bxt.video.sampler.samplers.Sampler;
import processing.core.PApplet;

public class PixelSampleDrawer extends SampleDrawer implements Drawable {
	
	public PixelSampleDrawer(PApplet p, Sampler s, int offsetX, int offsetY, ColorFilter cf) {
		super(p, s, offsetX, offsetY, cf);
	}

	@Override
	protected void drawStart(PApplet p, float size) {
		p.loadPixels();
	}
	
	@Override
	protected void drawEach(PApplet p, float x, float y, int color, float size) {
		if(p.pixelDensity == 2) {
			int loc = 2 * ((int)x + (int)y * p.pixelWidth);
			p.pixels[loc] = color;
			p.pixels[loc+1] = color;
			p.pixels[loc+p.pixelWidth] = color;
			p.pixels[loc+1+p.pixelWidth] = color;
		} else {
			p.pixels[(int)x + (int)y * p.pixelWidth] = color;
		}
	}		

	@Override
	protected void drawEnd(PApplet p, float size) {
		p.updatePixels();
	}
	
}
