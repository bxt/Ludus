package bxt.icosahedron.geometry;

import processing.core.PVector;

public class PositionSwitch {
	
	private int position = 0;
	private float fade = 1.0f;
	
	public PVector getPosition(PVector[] positions) {
		if(fade < 1.0f) {
			
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
		if(!(fade < 1.0f)) {
			position++;
			fade = 0.0f;
		} else {
			System.out.println("ig!");
		}
	}
	
	public void step() {
		if(fade < 1.0f) {
			fade += 0.05;
		}
		if(!(fade < 1.0f)) {
			fade = 1.0f;
		}
	}
}
