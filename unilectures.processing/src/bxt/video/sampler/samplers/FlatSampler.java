package bxt.video.sampler.samplers;

import processing.core.PImage;

public class FlatSampler extends Sampler {
	
	public FlatSampler(int size, PImage input) {
		super(size, input);
	}
	
	@Override
	protected int getPixelColor(int x, int y) {
		return getInput().get(x, y);
	}

}
