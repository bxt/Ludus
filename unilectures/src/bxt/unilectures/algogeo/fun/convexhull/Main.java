package bxt.unilectures.algogeo.fun.convexhull;

import java.awt.geom.Point2D;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

/**
 * Displays a welcome message and runs one convex hull algorithm
 * on a simple set of points.
 * 
 * If you want to run the convex hull algorithms with a graphical
 * output check the unilectures.processing project.
 */
public class Main {
	public static void main(String[] args) {
		System.out.println("\n    «Mix, mix, swirl... mix!»");
		System.out.println("\n                   - Singed\n");
		
		Collection<Point2D> points = new LinkedList<>();
		points.add(new Point2D.Double(0.00, 0.00));
		points.add(new Point2D.Double(1.00, 1.00));
		points.add(new Point2D.Double(0.00, 1.00));
		points.add(new Point2D.Double(1.00, 0.00));
		points.add(new Point2D.Double(0.50, 0.50));
		points.add(new Point2D.Double(0.25, 0.75));
		
		ConvexHullBuilder[] chbs = new ConvexHullBuilder[]{
				new FirstConvexHullBuilder(),
				new StableConvexHullBuilder(),
				new GiftWrapppingConvexHullBuilder(),
				};
		for (ConvexHullBuilder chb : chbs) {
			System.out.println(chb.getClass().getSimpleName());
			List<Point2D> result = chb.build(points);
			System.out.println(result);
		}
		
	}
}
