package bxt.icosahedron.processing;

import java.text.DecimalFormat;
import java.text.NumberFormat;

import processing.core.PApplet;

/**
 * Prints the average fps. 
 */
public class FpsPrinter extends FpsCounter implements Drawable {
	
	private int every = 40;
	private final NumberFormat format = new DecimalFormat("###.#");
	private String previousOutput = null;
	
	private PApplet p;
	
	/**
	 * @see #FpsPrinter(int, PApplet)
	 * @param p Processing object. 
	 */
	public FpsPrinter(PApplet p) {
		super();
		this.p = p;
	}
	
	/**
	 * Construct new instance. 
	 * @param every How often to print fps at most.  
	 * @param p Processing object. 
	 */
	public FpsPrinter(int every, PApplet p) {
		super();
		this.every = every;
		this.p = p;
	}

	@Override
	public void draw() {
		if(p.frameCount % every == 0) {
			String output = format.format(getAvgFps(p.frameCount)) + " fps";
			if(!output.equals(previousOutput)) {
				PApplet.println(output);
				previousOutput = output;
			}
		}
	}

}
