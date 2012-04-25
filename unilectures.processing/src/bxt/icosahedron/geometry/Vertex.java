package bxt.icosahedron.geometry;

import processing.core.PVector;

/**
 * One 3d point. 
 */
public class Vertex {
	
	private String name;
	private PVector[] positions;
	private PositionSwitch positionSwitch;
	
	/**
	 * New immutable Vertex. 
	 * @param name Label. 
	 * @param positions List of positions. 
	 * @param positionSwitch Provides animations through the positions. 
	 */
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
