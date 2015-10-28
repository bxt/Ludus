package bxt.rectangleintersection.processing;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import bxt.unilectures.algogeo.fun.rectintersect.RectangleIntersectionFinder;
import processing.core.PApplet;

public class RectangleFillSketch extends PApplet {
	
	List<GrowingRectangle> rectangles = new ArrayList<GrowingRectangle>();
	/**
	 * Main-method for direct invocation, dispatches to 
	 * {@link PApplet#main(String[])}. 
	 * @param args
	 */
	public static void main(String args[]) {
		PApplet.main(new String[]{RectangleFillSketch.class.getCanonicalName()});
	}
	
	public void settings() {
		size(700, 400);
		pixelDensity(2);
	}
	
	public void setup() {
		background(255);
		noStroke();
	}
	
	public void draw() {
		
		tryPlaceNewRect();
		
		tryGrowRects();
		
		for (GrowingRectangle rect : rectangles) {
			rect.draw();
		}
	}
		
	private void tryPlaceNewRect() {
		List<GrowingRectangle> newRectangles = new ArrayList<GrowingRectangle>(rectangles);
		newRectangles.add(new GrowingRectangle(this, Math.random() * width, Math.random() * height));
		if (!RectangleIntersectionFinder.findIntersections(newRectangles.stream().map(GrowingRectangle::getSpace).collect(Collectors.toList()))) {
			rectangles = newRectangles;
		}
	}

	private void tryGrowRects() {
		int tries = 10000;
		int rects = 20;
		while(rectangles.size() > 0 && tries-->0 && rects > 0) {
			int position = (int) (Math.random() * rectangles.size());
			GrowingRectangle rect = rectangles.get(position);
			if(rect.isGrowing()) {
				rects--;
				List<GrowingRectangle> newRectangles = new ArrayList<>(rectangles);
				newRectangles.set(position, rect.grow());
				if (!RectangleIntersectionFinder.findIntersections(newRectangles.stream().map(GrowingRectangle::getSpace).collect(Collectors.toList()))) {
					rectangles = newRectangles;
				} else {
					rect.stopGrowing();
				}
			}
		}
	}


}
