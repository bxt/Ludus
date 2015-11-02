package bxt.video.sampler.samplers;

import processing.core.PImage;

public class FullAverageSampler extends Sampler {
	
	public FullAverageSampler(int size, PImage input) {
		super(size, input);
	}
	
	@Override
	protected int getPixelColor(int x, int y) {
		ColorAverage ca = new ColorAverage();
		for(int i = 0; i < getSize(); i++) {
			for(int k = 0; k < getSize(); k++) {
				ca.add(getInput().get(x+k, y+i));
			}
		}
		return ca.getAverage();
	}

}
