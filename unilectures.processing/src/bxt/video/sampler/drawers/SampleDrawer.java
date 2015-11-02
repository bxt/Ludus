package bxt.video.sampler.drawers;

import bxt.util.Drawable;
import bxt.video.sampler.ColorFilter;
import bxt.video.sampler.samplers.Sampler;
import processing.core.PApplet;

public abstract class SampleDrawer implements Drawable {
	
	private PApplet p;
	private Sampler s;
	private int offsetX;
	private int offsetY;
	private ColorFilter cf;
	
	public SampleDrawer(PApplet p, Sampler s, int offsetX, int offsetY, ColorFilter cf) {
		this.p = p;
		this.s = s;
		this.offsetX = offsetX;
		this.offsetY = offsetY;
		this.cf = cf;
	}

	@Override
	public void draw() {
		drawStart(p, s.getSize());
		s.forEach((point, color) -> {
			drawEach(p, (float)point.getX() + offsetX, (float)point.getY() + offsetY, cf.map(color), (float)s.getSize());
		});
	}
	
	protected void drawStart(PApplet p, float size) {
		
	}
	protected abstract void drawEach(PApplet p, float x, float y, int color, float size);

}
