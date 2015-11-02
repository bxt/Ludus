package bxt.toprightregion.processing;

import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import bxt.unilectures.algogeo.fun.lagresttopright.TopRightRegionFinder;
import bxt.util.FpsPrinter;
import bxt.util.GeomDrawer;
import processing.core.PApplet;

public class TopRightRegionSketchScientific extends PApplet {

	private List<Point2D> points = new LinkedList<Point2D>();
	private GeomDrawer d = new GeomDrawer(this);
	
	FpsPrinter f = new FpsPrinter(this);
	
	/**
	 * Main-method for direct invocation, dispatches to 
	 * {@link PApplet#main(String[])}. 
	 * @param args
	 */
	public static void main(String args[]) {
		PApplet.main(new String[]{TopRightRegionSketchScientific.class.getCanonicalName()});
	}
	
	@Override
	public void settings() {
	  size(700, 400);
	  pixelDensity(displayDensity());
	}
	
	@Override
	public void setup() {
		//randomPoints(8000);
	}
	
	@Override
	public void draw() {
		Map<Point2D, Rectangle2D> regions = TopRightRegionFinder.findLargestTopRightRegions(points);
		
		background(50);
		fill(255,255,255,100);
		
		stroke(color(0,0,255));
		regions.values().forEach(d::draw);
		
		stroke(255);
		points.forEach(d::draw);
		
		f.draw();
	}
	
	@Override
	public void mouseClicked() {
		if(mouseButton == LEFT) {
			points.add(new Point2D.Double(mouseX, mouseY));
		} else {
			points.clear();
		}
	}
	
	@SuppressWarnings("unused")
	private void randomPoints(int count) {
		for (int i = 0; i < count; i++) {
			Point2D p = new Point2D.Double(Math.random()*width, Math.random()*height);
			points.add(p);
		}
	}
}
