package bxt.icosahedron.geometry;

import processing.core.PVector;

public class Vertex {
	
	private String name;
	private PVector[] positions;
	private PositionSwitch positionSwitch;
	
	public Vertex(String name, PVector[] positions, PositionSwitch positionSwitch) {
		this.name = name;
		this.positions = positions;
		this.positionSwitch = positionSwitch;
	}
	public String getName() {
		return name;
	}
	public PVector getPosition() {
		return positionSwitch.getPosition(positions);
	}
	
	
}
