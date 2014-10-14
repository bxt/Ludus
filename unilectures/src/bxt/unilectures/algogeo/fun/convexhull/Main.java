package bxt.unilectures.algogeo.fun.convexhull;

import java.awt.geom.Point2D;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

public class Main {
	public static void main(String[] args) {
		System.out.println("\n    «Mix, mix, swirl... mix!»\n");
		System.out.println("\n                   - Singed\n");
		
		Collection<Point2D> points = new LinkedList<>();
		points.add(new Point2D.Double(0.00, 0.00));
		points.add(new Point2D.Double(1.00, 1.00));
		points.add(new Point2D.Double(0.00, 1.00));
		points.add(new Point2D.Double(1.00, 0.00));
		points.add(new Point2D.Double(0.50, 0.50));
		points.add(new Point2D.Double(0.25, 0.75));
		
		ConvexHullBuilder chb = new FirstConvexHullBuilder();
		List<Point2D> result = chb.build(points);
		System.out.println(result);
		
	}
}
