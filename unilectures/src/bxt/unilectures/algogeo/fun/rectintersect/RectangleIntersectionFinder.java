package bxt.unilectures.algogeo.fun.rectintersect;

import java.awt.geom.Rectangle2D;
import java.util.Arrays;
import java.util.Collection;
import java.util.NavigableSet;
import java.util.TreeSet;

public class RectangleIntersectionFinder {

	public static void main(String[] args) {
		Collection<Rectangle2D> rects = Arrays.asList(
				new Rectangle2D.Double(10, 20, 5, 7),
				new Rectangle2D.Double(11, 23, 1, 2));
		
		System.out.println(findIntersections(rects));
	}

	public static boolean findIntersections(Collection<Rectangle2D> rects) {
		NavigableSet<Rectangle2D> scanline = new TreeSet<>((r1, r2) -> Double.compare(r1.getY(), r2.getY()));
		scanline.add(new Rectangle2D.Double(Double.NEGATIVE_INFINITY, Double.NEGATIVE_INFINITY, 0, 0));
		scanline.add(new Rectangle2D.Double(Double.POSITIVE_INFINITY, Double.POSITIVE_INFINITY, 0, 0));
		
		Event[] events = Arrays.stream(Mark.values()).flatMap(mark -> rects.stream().map(r -> new Event(r, mark))).toArray(n -> new Event[n]);
		Arrays.sort(events, null);
		
		for (Event e : events) {
			Rectangle2D r = e.getRectangle();
			if (e.getMark() == Mark.START) {
				if(startsBeforeEndY(r, scanline.floor(r))
						|| startsBeforeEndY(scanline.ceiling(r), r)) {
					return true;
				}
				
				scanline.add(r);
			} else {
				scanline.remove(r);
			}
			
		}
	
		return false;
	}
	
	private static boolean startsBeforeEndY(Rectangle2D startRectangle, Rectangle2D endRectangle) {
		return startRectangle.getY() <= endRectangle.getY() + endRectangle.getHeight();
	}

}
