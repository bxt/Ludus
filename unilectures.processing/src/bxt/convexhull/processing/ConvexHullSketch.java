package bxt.convexhull.processing;

import java.awt.geom.Point2D;
import java.util.LinkedList;
import java.util.List;

import processing.core.PApplet;
import bxt.unilectures.algogeo.fun.convexhull.ConvexHullBuilder;
import bxt.unilectures.algogeo.fun.convexhull.FirstConvexHullBuilder;
import bxt.unilectures.algogeo.fun.convexhull.StableConvexHullBuilder;

public class ConvexHullSketch extends PApplet {

	private static final long serialVersionUID = 1L;
	
	private List<Point2D> points = new LinkedList<Point2D>();
	
	/**
	 * Main-method for direct invocation, dispatches to 
	 * {@link PApplet#main(String[])}. 
	 * @param args
	 */
	public static void main(String args[]) {
		PApplet.main(new String[]{ConvexHullSketch.class.getCanonicalName()});
	}
	public void setup() {
		size(700, 400);	
	}
	
	public void draw() {
		boolean fast = false;
		
		background(20);
		
		stroke(color(0,0,255));
		ConvexHullBuilder chb = fast ? new StableConvexHullBuilder() : new FirstConvexHullBuilder();
		List<Point2D> complexHull = chb.build(points);
		for (int i = 1; i < complexHull.size(); i++) {
			line(complexHull.get(i-1), complexHull.get(i));
		}
		if(complexHull.size() >= 2)
			line(complexHull.get(complexHull.size()-1), complexHull.get(0));
		
		stroke(255);
		for(Point2D point : points) {
			point((float)point.getX(), (float)point.getY());
		}
		
	}
	
	public void mouseClicked() {
		if(mouseButton == LEFT) {
			points.add(new Point2D.Double(mouseX, mouseY));
		}
	}
	
	public void line(Point2D a, Point2D b) {
		line((float)a.getX(), (float)a.getY(), (float)b.getX(), (float)b.getY());
	}

}
