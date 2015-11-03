package bxt.util;

import java.util.List;
import java.util.function.Supplier;

import processing.core.PApplet;

public class Cycler<T> implements Supplier<T> {

	private PApplet p;
	private int framesPerChioce;
	private List<? extends T> choices;
	
	public Cycler(PApplet p, int framesPerChioce, List<? extends T> choices) {
		this.p = p;
		this.framesPerChioce = framesPerChioce;
		this.choices = choices;
	}
	
	@Override
	public T get() {
		int i = (p.frameCount / framesPerChioce) % choices.size();
		return choices.get(i);
	}

}
