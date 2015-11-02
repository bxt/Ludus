package bxt.video.sampler.samplers;

import java.awt.geom.Point2D;
import java.util.function.BiConsumer;

import processing.core.PImage;

public abstract class Sampler {
	
	private int size;
	private PImage input;
	
	public Sampler(int size, PImage input) {
		this.size = size;
		this.input = input;
	}
	
	public int getSize() {
		return size;
	}

	public void forEach(BiConsumer<Point2D, Integer> consumer) {
		for (int y = 0; y < input.pixelHeight; y += size) {
			for (int x = 0; x < input.pixelWidth; x += size) {
				consumer.accept(new Point2D.Double(x, y), getPixelColor(x, y));
			}
		}
		
	}
	
	protected abstract int getPixelColor(int x, int y);

	protected PImage getInput() {
		return input;
	}

}
