package bxt.rectangleintersection.processing;

import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.List;

import bxt.unilectures.algogeo.fun.rectintersect.RectangleIntersectionFinder;
import processing.core.PApplet;

public class RectangleIntersectionSketch extends PApplet {
	
	List<Rectangle2D> rectangles = new ArrayList<Rectangle2D>();
	Point2D danglingStart = null;
	Rectangle2D danglingRect = null;
	
	/**
	 * Main-method for direct invocation, dispatches to 
	 * {@link PApplet#main(String[])}. 
	 * @param args
	 */
	public static void main(String args[]) {
		PApplet.main(new String[]{RectangleIntersectionSketch.class.getCanonicalName()});
	}
	
	public void settings() {
		size(700, 400);
	}
	
	public void setup() {
	}
	
	public void draw() {
		background(20);
		noFill();
		
		boolean intersecting = RectangleIntersectionFinder.findIntersections(rectangles);
		if (intersecting) {
			stroke(color(255,0,0));
		} else {
			stroke(color(0,100,255));
		}
		
		for (Rectangle2D rect : rectangles) {
			rect(rect);
		}
	}
	
	public void mouseClicked() {
		if(mouseButton == LEFT) {
			if (danglingStart == null) {
				danglingStart = new Point2D.Double(mouseX, mouseY);
				danglingRect = new Rectangle2D.Double(mouseX, mouseY, 0, 0);
				rectangles.add(danglingRect);
			} else {
				danglingStart = null;
				danglingRect = null;
			}
		} else {
			rectangles.clear();
		}
	}
	
	@Override
	public void mouseMoved() {
		if(danglingStart != null) {
			double x1 = danglingStart.getX();
			double y1 = danglingStart.getY();
			double x2 = mouseX;
			double y2 = mouseY;
			danglingRect.setRect(Math.min(x1, x2), Math.min(y1, y2), Math.abs(x1-x2), Math.abs(y1-y2));
		}
	}
	
	private void rect(Rectangle2D rect) {
		rect((float)rect.getX(), (float)rect.getY(), (float)rect.getWidth(), (float)rect.getHeight());
	}

}
