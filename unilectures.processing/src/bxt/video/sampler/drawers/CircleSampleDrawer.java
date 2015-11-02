package bxt.video.sampler.drawers;

import bxt.util.Drawable;
import bxt.video.sampler.ColorFilter;
import bxt.video.sampler.samplers.Sampler;
import processing.core.PApplet;

public class CircleSampleDrawer extends SampleDrawer implements Drawable {
	
	public CircleSampleDrawer(PApplet p, Sampler s, int offsetX, int offsetY, ColorFilter cf) {
		super(p, s, offsetX, offsetY, cf);
	}

	@Override
	protected void drawStart(PApplet p, float size) {
		p.ellipseMode(PApplet.CORNER);
	}
	
	@Override
	protected void drawEach(PApplet p, float x, float y, int color, float size) {
		p.fill(color);
		p.ellipse(x, y, size, size);
	}

}
