package bxt.unilectures.algogeo.fun.convexhull;

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

/**
 * Jarvis' gift wrapping convex hull algorithm.
 * 
 * This algorithm computes the convex hull by starting at the leftmost point
 * and then for each new hull point looking for the point that lies most
 * to the left of the last hull point, wraping the point cloud like a gift.
 */
public class GiftWrapppingConvexHullBuilder implements ConvexHullBuilder {

	@Override
	public List<Point2D> build(Collection<Point2D> points) {
		
		if (points.size() <= 2) return new ArrayList<Point2D>(points);
		
		LinkedList<Point2D> hull = new LinkedList<Point2D>();
		
		Point2D startPoint = Collections.min(points, new Util.Point2DComparator());
		
		Point2D current = startPoint;
		do {
			hull.add(current);
			
			Point2D next = null;
			for (Point2D point : points) {
				if (next == null || (point != current && !Util.strictlyRight(point, current, next))) {
					next = point;
				}
			}
			current = next;
		} while (current != startPoint);
		
		return hull;
	}
	
}
