package bxt.icosahedron.geometry;

import bxt.util.Drawable;
import processing.core.PVector;

/**
 * Animates between positions. 
 */
public class PositionSwitch implements Drawable {
	
	private int position = 0;
	private float fade = 1.0f;
	
	private final static float STEP_MIN = 0.0f;
	private final static float STEP_MAX = 1.0f;
	private final static float STEP_SIZE = 0.05f;
	
	public PVector getPosition(PVector[] positions) {
		if(fade < STEP_MAX) {
			
			PVector old = positions[(position+positions.length-1) % positions.length];
			PVector nev = positions[ position % positions.length];
			
			PVector returnValue = PVector.sub(nev, old);
			returnValue.mult(fade);
			returnValue.add(old);
			return returnValue;
			
		} else {
			return positions[ position % positions.length];
		}
	}
	
	public void nextPosition() {
		if(!(fade < STEP_MAX)) {
			position++;
			fade = STEP_MIN;
		}
	}
	
	public void step() {
		if(fade < 1.0f) {
			fade += STEP_SIZE;
		}
		if(!(fade < STEP_MAX)) {
			fade = STEP_MAX;
		}
	}

	@Override
	public void draw() {
		step();
	}
}
