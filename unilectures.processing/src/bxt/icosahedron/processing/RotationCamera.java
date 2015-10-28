package bxt.icosahedron.processing;

import bxt.util.Drawable;
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
		
		float anglePlane = (p.mouseX/(float)p.width ) * PApplet.TWO_PI;
		float angleUp    = (p.mouseY/(float)p.height) * PApplet.PI;
		float xpos = PApplet.cos(anglePlane) * PApplet.sin(angleUp);
		float ypos = PApplet.sin(anglePlane) * PApplet.sin(angleUp);
		
		float zpos = PApplet.cos(angleUp);
		
		p.camera(xpos*radius, zpos*radius, ypos*radius, // eyeX, eyeY, eyeZ
				0f, 0f, 0f, // centerX, centerY, centerZ (origin)
				0f, 1f, 0f); // upX, upY, upZ
		
	}
	
}
