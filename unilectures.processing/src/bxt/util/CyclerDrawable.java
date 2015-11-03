package bxt.util;

import java.util.Arrays;
import java.util.List;

import processing.core.PApplet;

public class CyclerDrawable extends Cycler<Drawable> implements Drawable {

	public CyclerDrawable(PApplet p, int framesPerChioce, List<? extends Drawable> choices) {
		super(p, framesPerChioce, choices);
	}

	public CyclerDrawable(PApplet p, int framesPerChioce, Drawable... drawables) {
		super(p, framesPerChioce, Arrays.asList(drawables));
	}

	@Override
	public void draw() {
		get().draw();
	}

}
