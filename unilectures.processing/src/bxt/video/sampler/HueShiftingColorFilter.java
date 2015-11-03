package bxt.video.sampler;

import processing.core.PApplet;

public class HueShiftingColorFilter extends HsbColorFilter implements ColorFilter {
	
	private int cycleLength;
	private PApplet p;
	
	public HueShiftingColorFilter(int cycleLength, PApplet p) {
		super();
		this.cycleLength = cycleLength;
		this.p = p;
	}

	@Override
	protected float[] map(float[] color) {
		color[0] = (color[0] + (float)p.frameCount/cycleLength) % 1;
		//color[2] = 1f - color[2];
		return color;
	}

}
