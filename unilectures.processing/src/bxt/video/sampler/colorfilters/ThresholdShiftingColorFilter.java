package bxt.video.sampler.colorfilters;

import processing.core.PApplet;

public class ThresholdShiftingColorFilter implements ColorFilter {
	
	private int cycleLength;
	private PApplet p;
	private int darkColor;
	private int lightColor;
	
	public ThresholdShiftingColorFilter(int cycleLength, PApplet p) {
		this(cycleLength, p,  0xff000000, 0xffffffff);
	}

	public ThresholdShiftingColorFilter(int cycleLength, PApplet p, int darkColor, int lightColor) {
		super();
		this.cycleLength = cycleLength;
		this.p = p;
		this.darkColor = darkColor;
		this.lightColor = lightColor;
	}

	@Override
	public int map(int color) {
		float threshold = ((float)p.frameCount/cycleLength) % 1;
		float grey = ColorFilters.grey(color);
		return easing(threshold) > grey ? darkColor : lightColor;
	}
	
	private float easing(float input) {
		if(input < 0.8) {
			return input * input * input * 1.953125f;
		} else {
			return 1 - (input-0.8f)*5;
		}
	}

}
