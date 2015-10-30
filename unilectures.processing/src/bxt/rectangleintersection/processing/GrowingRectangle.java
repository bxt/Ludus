package bxt.rectangleintersection.processing;

import java.awt.geom.Rectangle2D;

import bxt.util.Drawable;
import bxt.util.GeomDrawer;
import processing.core.PApplet;

public class GrowingRectangle implements Drawable {
	
	private static float PADDING = 2;
	
	private Rectangle2D base;
	private int color;
	private PApplet p;
	private boolean isGrowing = true;
	
	private GrowingRectangle(Rectangle2D base, int color, PApplet p) {
		this.base = base;
		this.color = color;
		this.p = p;
	}

	public GrowingRectangle(PApplet p, double x, double y) {
		this.p = p;
		color = p.color((float)Math.random()*256, (float)Math.random()*256, (float)Math.random()*256);
		base = new Rectangle2D.Double(x, y, PADDING + Math.random()*PADDING, PADDING + Math.random()*PADDING);
	}

	@Override
	public void draw() {
		p.fill(color);
		new GeomDrawer(p).draw(base, PADDING*2);
	}
	
	public Rectangle2D getSpace() {
		//return base;
		return new Rectangle2D.Double(base.getX()-PADDING, base.getY()-PADDING, base.getWidth()+2*PADDING, base.getHeight()+2*PADDING);
	}
	
	public GrowingRectangle grow() {
		return isGrowing ? new GrowingRectangle(getGrownBase(), color, p) : this;
	}
	
	public void stopGrowing() {
		isGrowing = false;
	}
	
	public boolean isGrowing() {
		return isGrowing;
	}
	
	private Rectangle2D getGrownBase() {
		double growX = PADDING * Math.random() * 0.25;
		double growY = PADDING * Math.random() * 0.25;
		return new Rectangle2D.Double(base.getX()-growX, base.getY()-growY, base.getWidth()+2*growX, base.getHeight()+2*growY);
	}
	
}
