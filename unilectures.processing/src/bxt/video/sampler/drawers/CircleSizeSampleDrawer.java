package bxt.video.sampler.drawers;

import bxt.util.Drawable;
import bxt.video.sampler.ColorFilter;
import bxt.video.sampler.samplers.Sampler;
import processing.core.PApplet;

public class CircleSizeSampleDrawer extends SampleDrawer implements Drawable {
	
	private ColorFilter circleCf;
	
	public CircleSizeSampleDrawer(PApplet p, Sampler s, int offsetX, int offsetY, ColorFilter cf, ColorFilter circleCf) {
		super(p, s, offsetX, offsetY, cf);
		this.circleCf = circleCf;
	}

	@Override
	protected void drawStart(PApplet p, float size) {
		p.fill(0xffffffff);
	}
	
	@Override
	protected void drawEach(PApplet p, float x, float y, int color, float size) {
		float grey = ColorFilter.grey(color);
		p.fill(circleCf.map(color));
		p.ellipse(x + size*.5f, y + size*.5f, size * grey, size * grey);
	}

}
