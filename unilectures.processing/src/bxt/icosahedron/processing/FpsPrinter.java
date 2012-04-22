package bxt.icosahedron.processing;

import java.text.DecimalFormat;
import java.text.NumberFormat;

import processing.core.PApplet;

public class FpsPrinter extends FpsCounter implements Drawable {
	
	private int every = 40;
	private final NumberFormat format = new DecimalFormat("###.#");
	
	private PApplet p;
	
	public FpsPrinter(PApplet p) {
		super();
		this.p = p;
	}
	
	public FpsPrinter(int every, PApplet p) {
		super();
		this.every = every;
		this.p = p;
	}

	@Override
	public void draw() {
		if(p.frameCount % every == 0) {
			PApplet.println(format.format(getAvgFps(p.frameCount)) + " fps");
		}
	}

}
