package bxt.video.sampler.drawers;

import bxt.util.Drawable;
import bxt.video.sampler.ColorFilter;
import bxt.video.sampler.samplers.Sampler;
import processing.core.PApplet;

public class RectSampleDrawer extends SampleDrawer implements Drawable {
	
	public RectSampleDrawer(PApplet p, Sampler s, int offsetX, int offsetY, ColorFilter cf) {
		super(p, s, offsetX, offsetY, cf);
	}

	@Override
	protected void drawEach(PApplet p, float x, float y, int color, float size) {
		p.fill(color);
		p.rect(x, y, size, size);
	}

}
