package bxt.icosahedron.processing;

import processing.core.PApplet;

/**
 * Rotate the camera around the origin, facing the origin,
 * controlled by mouse movement. 
 */
public class RotationCamera implements Drawable {
	
	private PApplet p;
	private float radius;
	
	/**
	 * Construct a new Camera. 
	 * @param radius Camera's distance to the origin. 
	 * @param p Processing object. 
	 */
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
				0f, 0f, 0f, // centerX, centerY, centerZ (origin)
				0f, 1f, 0f); // upX, upY, upZ
		
	}
	
}
