package bxt.toprightregion.processing;

import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import bxt.toprightregion.processing.artistic.ColorProvider;
import bxt.toprightregion.processing.artistic.ColorScheme;
import bxt.toprightregion.processing.artistic.ColorSchemeProvider;
import bxt.unilectures.algogeo.fun.lagresttopright.TopRightRegionFinder;
import bxt.util.Drawable;
import bxt.util.FpsStringSupplier;
import bxt.util.GeomDrawer;
import bxt.util.TextPrinter;
import processing.core.PApplet;

public class TopRightRegionSketchArtistic extends PApplet {
	
	private static final int BATCH_SIZE  =    300;
	private static final int CLICK_SIZE  =  6_000;
	private static final int UPPER_LIMIT = 30_000;
	private static final int LOWER_LIMIT =  3_000;
	
	private ColorProvider c = new ColorSchemeProvider(ColorScheme.BLACK);

	private List<Point2D> points = new LinkedList<Point2D>();
	private Map<Point2D, Integer> colors = new HashMap<>();
	private GeomDrawer d = new GeomDrawer(this);
	private boolean add = true;
	
	Drawable f = new TextPrinter(new FpsStringSupplier());
	
	/**
	 * Main-method for direct invocation, dispatches to 
	 * {@link PApplet#main(String[])}. 
	 * @param args
	 */
	public static void main(String args[]) {
		PApplet.main(new String[]{TopRightRegionSketchArtistic.class.getCanonicalName()});
	}
	
	@Override
	public void settings() {
	  size(700, 400);
	  pixelDensity(displayDensity());
	  fullScreen();
	}
	
	@Override
	public void setup() {
	}
	
	@Override
	public void draw() {
		if (c instanceof Drawable) ((Drawable) c).draw();
		
		if (add) {
			randomPoints(BATCH_SIZE);
			add = points.size() < UPPER_LIMIT;
		} else {
			removePoints(BATCH_SIZE);
			add = points.size() < LOWER_LIMIT;
		}
		Map<Point2D, Rectangle2D> regions = TopRightRegionFinder.findLargestTopRightRegions(points);
		
		background(c.getBackgroundColor());
		noStroke();
		
		regions.forEach((p, r) -> {
			fill(colors.get(p));
			d.draw(r);
		});
		
		f.draw();
	}
	
	@Override
	public void mouseClicked() {
		if(mouseButton == LEFT) {
			points.clear();
			colors.clear();
		} else {
			randomPoints(CLICK_SIZE);
		}
	}
	
	private void randomPoints(int count) {
		for (int i = 0; i < count; i++) {
			Point2D p = new Point2D.Double(Math.random()*width, Math.random()*height);
			points.add(p);
			colors.put(p, c.getColor(p));
		}
	}
	
	private void removePoints(int count) {
		for(int i = 0; i < count; i++) {
			Point2D p = points.remove(0);
			colors.remove(p);
		}
	}

}
