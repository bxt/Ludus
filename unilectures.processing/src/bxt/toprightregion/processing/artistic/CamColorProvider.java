package bxt.toprightregion.processing.artistic;

import java.awt.geom.Point2D;

import bxt.util.Drawable;
import processing.core.PApplet;
import processing.video.Capture;

public class CamColorProvider implements ColorProvider, Drawable {
	
	Capture cam;
	PApplet p;
	
	public CamColorProvider (PApplet p) {
		String[] cameras = Capture.list();
		cam = new Capture(p, cameras[0]);
		cam.start();
		this.p = p;
	}
	
	@Override
	public int getColor(Point2D point) {
		return cam.get(((int)point.getX() * cam.width) / p.width, ((int)point.getY() * cam.height) / p.height);
	}

	@Override
	public int getBackgroundColor() {
		return 0x99;
	}

	@Override
	public void draw() {
		if (cam.available()) {
			cam.read();
		}
	}

}
