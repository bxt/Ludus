package bxt.icosahedron.geometry;

import processing.core.PVector;

public class Vertex {
	
	private String name;
	private PVector position;
	
	public Vertex(String name, PVector position) {
		this.name = name;
		this.position = position;
	}
	public String getName() {
		return name;
	}
	public PVector getPosition() {
		return position;
	}
	
}
