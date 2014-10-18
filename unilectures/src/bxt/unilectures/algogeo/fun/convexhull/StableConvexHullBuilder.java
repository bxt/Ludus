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
		
		LinkedList<Point2D> upperHalf = new LinkedList<Point2D>();
		
		for (int i = 0; i < sortedPoints.size(); i++) {
			upperHalf.add(sortedPoints.get(i));
			while (upperHalf.size() > 2 && lastThreePointsNotRightTurn(upperHalf)) {
				removeSecondLastElement(upperHalf);
			}
		}
		
		LinkedList<Point2D> lowerHalf = new LinkedList<Point2D>();
		
		for (int i = sortedPoints.size()-1; i >= 0; i--) {
			lowerHalf.add(sortedPoints.get(i));
			while (lowerHalf.size() > 2 && lastThreePointsNotRightTurn(lowerHalf)) {
				removeSecondLastElement(lowerHalf);
			}
		}
		
		upperHalf.removeLast();
		lowerHalf.removeLast();
		
		LinkedList<Point2D> result = upperHalf;
		result.addAll(lowerHalf);
		
		return result;
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
