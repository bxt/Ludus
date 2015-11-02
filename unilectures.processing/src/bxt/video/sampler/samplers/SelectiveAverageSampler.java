package bxt.video.sampler.samplers;

import processing.core.PImage;

public class SelectiveAverageSampler extends Sampler {
	
	private int steps;
	
	public SelectiveAverageSampler(int size, PImage input, int steps) {
		super(size, input);
		this.steps = steps;
	}
	
	@Override
	protected int getPixelColor(int x, int y) {
		ColorAverage ca = new ColorAverage();
		int stepSize = getSize()/steps;
		for(int i = 0; i < stepSize; i++) {
			for(int k = 0; k < stepSize; k++) {
				ca.add(getInput().get(x+k, y+i));
			}
		}
		return ca.getAverage();
	}

}
