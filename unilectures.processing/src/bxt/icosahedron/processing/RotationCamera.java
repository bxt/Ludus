package bxt.icosahedron.processing;

import processing.core.PApplet;

public class RotationCamera implements Drawable {
	
	private PApplet p;
	
	private float radius = 600f;
	
	public RotationCamera(PApplet p) {
		this.p = p;
	}
	
	public RotationCamera(float radius, PApplet p) {
		this.radius = radius;
		this.p = p;
	}
	
	@Override
	public void draw() {
		
		float winkelPlane = (p.mouseX/(float)p.width ) * PApplet.TWO_PI;
		float winkelUp    = (p.mouseY/(float)p.height) * PApplet.PI;
		float xpos = PApplet.cos(winkelPlane) * PApplet.sin(winkelUp);
		float ypos = PApplet.sin(winkelPlane) * PApplet.sin(winkelUp);
		
		float zpos = PApplet.cos(winkelUp);
		
		p.camera(xpos*radius, zpos*radius, ypos*radius, // eyeX, eyeY, eyeZ
				0f, 50f, 0f, // centerX, centerY, centerZ
				0f, 1f, 0f); // upX, upY, upZ
		
	}
	
}
