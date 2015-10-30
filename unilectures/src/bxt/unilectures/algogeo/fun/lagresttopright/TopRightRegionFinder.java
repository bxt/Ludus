package bxt.unilectures.algogeo.fun.lagresttopright;

import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NavigableSet;
import java.util.TreeSet;
import java.util.stream.Collectors;

public class TopRightRegionFinder {
	
	public static Map<Point2D, Rectangle2D> findLargestTopRightRegions(Collection<Point2D> points) {
		return findLargestTopRightRegionSizes(points).entrySet().stream()
				.collect(Collectors.toMap(Map.Entry::getKey, (e) -> {
					Point2D p = e.getKey();
					return new Rectangle2D.Double(p.getX(), p.getY(), e.getValue(), e.getValue());
				}));
	}

	public static Map<Point2D, Double> findLargestTopRightRegionSizes(Collection<Point2D> points) {
		Map<Point2D, Double> result = new HashMap<>();

		Map<Point2D, Point2D> leftmost = new HashMap<>();
		{
			List<Point2D> events = new ArrayList<>(points);
			NavigableSet<Point2D> scanline = new TreeSet<>(Comparator.comparingDouble(Point2D::getY));
			events.sort(Comparator.comparingDouble(Point2D::getX).thenComparingDouble(Point2D::getY));
			for (Point2D p : events) {
				Point2D q;
				while ((q = leftOctant(scanline, p, false)) != null) {
					leftmost.put(q, p);
					scanline.remove(q);
				}
				scanline.add(p);
			}
		}
		leftmost.forEach((q, p) -> {
			result.merge(q, p.getX() - q.getX(), Math::min);
		});
		
		Map<Point2D, Point2D> lowest = new HashMap<>();
		{
			List<Point2D> events = new ArrayList<>(points);
			NavigableSet<Point2D> scanline = new TreeSet<>(Comparator.comparingDouble(Point2D::getX));
			events.sort(Comparator.comparingDouble(Point2D::getY).thenComparingDouble(Point2D::getX));
			for (Point2D p : events) {
				Point2D q;
				while ((q = leftOctant(scanline, p, true)) != null) {
					lowest.put(q, p);
					scanline.remove(q);
				}
				scanline.add(p);
			}
		}	
		lowest.forEach((q, p) -> {
			result.merge(q, p.getY() - q.getY(), Math::min);
		});
		
		return result;
	}

	private static Point2D leftOctant(NavigableSet<Point2D> scanline, Point2D p, boolean invert) {
		Point2D q = scanline.floor(p);
		if (q != null && invert != p.getX() - q.getX() > p.getY() - q.getY()) {
			return q;
		} else {
			return null;
		}
	}
	
}
