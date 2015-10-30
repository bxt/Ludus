package bxt.util;

import java.awt.geom.Line2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

import processing.core.PApplet;

public class GeomDrawer {
	
	private PApplet p;
	
	public GeomDrawer(PApplet p) {
		super();
		this.p = p;
	}

	public void draw(Rectangle2D rect) {
		p.rect((float)rect.getX(), (float)rect.getY(), (float)rect.getWidth(), (float)rect.getHeight());
	}

	public void draw(Rectangle2D rect, float borderRadius) {
		p.rect((float)rect.getX(), (float)rect.getY(), (float)rect.getWidth(), (float)rect.getHeight(), borderRadius);
	}

	public void draw(Point2D point) {
		p.point((float)point.getX(), (float)point.getY());
	}


	public void draw(Point2D a, Point2D b) {
		p.line((float)a.getX(), (float)a.getY(), (float)b.getX(), (float)b.getY());
	}

	public void draw(Line2D line) {
		draw(line.getP1(), line.getP2());
	}

}
