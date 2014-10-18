package bxt.unilectures.algogeo.fun.convexhull;

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Very naive convex hull algorithm.
 * 
 * This is a really bad algorithm for finding the convex hull, however it is
 * easily proven to be correct. It checks all pairs of points like a possible
 * edge of the convex hull polygon, by checking for all other points if they
 * are on the right side of the edge. It as O(n^3) running time. It might lead
 * to gaps and errors in the hull due to shortcomings of floating point
 * arithmetics.
 */
public class FirstConvexHullBuilder implements ConvexHullBuilder {

	@Override
	public List<Point2D> build(Collection<Point2D> points) {
		List<Point2D> borderA = new ArrayList<>();
		List<Point2D> borderB = new ArrayList<>();
		
		for (Point2D p : points) {
			for (Point2D q : points) {
				if (p == q) continue;
				
				boolean valid = true;
				
				for (Point2D r : points) {
					if(!(Util.strictlyRight(r, p, q) || Util.onLine(r, p, q) )) {
						valid = false;
					}
				}
				
				if (valid) {
					borderA.add(p);
					borderB.add(q);
				}
			}
		}
		
		return sortedListFromPairs(borderA, borderB);
	}
	
	private <T> List<T> sortedListFromPairs(List<? extends T> borderA,
			List<? extends T> borderB) {
		List<T> result = new ArrayList<>();
		
		if (borderA.isEmpty()) return result;
		T end = borderA.get(0);
		
		int i = 0;
		T current;
		do {
			current = borderB.get(i);
			result.add(current);
			i = borderA.indexOf(current);
		} while (current != end);
		
		return result;
	}


}
