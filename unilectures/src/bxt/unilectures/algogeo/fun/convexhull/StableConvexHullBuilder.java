package bxt.unilectures.algogeo.fun.convexhull;

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

public class StableConvexHullBuilder implements ConvexHullBuilder {

	@Override
	public List<Point2D> build(Collection<Point2D> points) {
		List<Point2D> sortedPoints = new ArrayList<>(points);
		Collections.sort(sortedPoints, new Comparator<Point2D>() {
			@Override
			public int compare(Point2D o1, Point2D o2) {
				int xCmp = Double.compare(o1.getX(), o2.getX());
				if (xCmp == 0) return Double.compare(o1.getY(), o2.getY());
				return xCmp;
			}
		});
		
		if (sortedPoints.size() <= 2) return sortedPoints;
		
		LinkedList<Point2D> hull = new LinkedList<Point2D>();
		
		for (int i = 0; i < sortedPoints.size(); i++) {
			hull.add(sortedPoints.get(i));
			while (hull.size() > 2 && lastThreePointsNotRightTurn(hull)) {
				removeSecondLastElement(hull);
			}
		}
		
		int upperHullSize = hull.size();
		
		for (int i = sortedPoints.size()-2; i >= 0; i--) {
			hull.add(sortedPoints.get(i));
			while (hull.size() > 1 + upperHullSize && lastThreePointsNotRightTurn(hull)) {
				removeSecondLastElement(hull);
			}
		}
		
		hull.removeLast();
		
		return hull;
	}

	private void removeSecondLastElement(LinkedList<Point2D> halfHull) {
		Iterator<Point2D> it = halfHull.descendingIterator();
		it.next();
		it.next();
		it.remove();
	}
	
	private boolean lastThreePointsNotRightTurn(LinkedList<Point2D> halfHull) {
		Iterator<Point2D> it = halfHull.descendingIterator();
		Point2D c = it.next();
		Point2D b = it.next();
		Point2D a = it.next();
		return ! Util.strictlyRight(c, a, b);
	}


}
