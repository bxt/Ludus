package bxt.unilectures.algogeo.fun.rectintersect;

import java.awt.geom.Rectangle2D;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.NavigableSet;
import java.util.Set;
import java.util.TreeSet;

public class RectangleIntersectionFinder {

	public static void main(String[] args) {
		Collection<Rectangle2D> rects = Arrays.asList(
				new Rectangle2D.Double(10, 20, 5, 7),
				new Rectangle2D.Double(11, 23, 1, 2));
		
		System.out.println(findIntersections(rects));
	}

	public static boolean findIntersections(Collection<Rectangle2D> rects) {
		return !findIntersectingRectangles(rects).isEmpty();
	}
	
	public static Set<Rectangle2D> findIntersectingRectangles(Collection<Rectangle2D> rects) {
		Set<Rectangle2D> intersecting = new HashSet<>();
		
		NavigableSet<Rectangle2D> scanline = new TreeSet<>((r1, r2) -> Double.compare(r1.getY(), r2.getY()));
		scanline.add(new Rectangle2D.Double(Double.NEGATIVE_INFINITY, Double.NEGATIVE_INFINITY, 0, 0));
		scanline.add(new Rectangle2D.Double(Double.POSITIVE_INFINITY, Double.POSITIVE_INFINITY, 0, 0));
		
		Event[] events = Arrays.stream(Mark.values()).flatMap(mark -> rects.stream().map(r -> new Event(r, mark))).toArray(n -> new Event[n]);
		Arrays.sort(events, null);
		
		for (Event e : events) {
			Rectangle2D r = e.getRectangle();
			if (e.getMark() == Mark.START) {
				Rectangle2D prev = scanline.floor(r);
				Rectangle2D next = scanline.ceiling(r);
				
				if(startsBeforeEndY(r, prev)) {
					intersecting.add(r);
					intersecting.add(prev);
				}
				if(startsBeforeEndY(next, r)) {
					intersecting.add(r);
					intersecting.add(next);
				}
				
				scanline.add(r);
			} else {
				scanline.remove(r);
			}
			
		}
	
		return intersecting;
	}
	
	private static boolean startsBeforeEndY(Rectangle2D startRectangle, Rectangle2D endRectangle) {
		return startRectangle.getY() <= endRectangle.getY() + endRectangle.getHeight();
	}

}
