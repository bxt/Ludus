package bxt.unilectures.algogeo.fun.convexhull;

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

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
					if(!(strictlyRight(r, p, q) || onLine(r, p, q) )) {
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

	private boolean strictlyRight(Point2D r, Point2D p, Point2D q) {
		double det = determinant3(
				r.getX(), r.getY(), 1.0,
				p.getX(), p.getY(), 1.0,
				q.getX(), q.getY(), 1.0);
		return det < 0;
	}

	private static double determinant3(
			double a11, double a12, double a13,
			double a21,	double a22, double a23,
			double a31, double a32, double a33 ) {
		return a11*a22*a33 + a12*a23*a31 + a13*a21*a32
		     - a13*a22*a31 - a12*a21*a33 - a11*a23*a32;
	}

	private boolean onLine(Point2D r, Point2D p, Point2D q) {
		return r == p || r == q; // TODO
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
