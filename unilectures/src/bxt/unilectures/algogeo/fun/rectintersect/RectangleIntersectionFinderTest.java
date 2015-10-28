package bxt.unilectures.algogeo.fun.rectintersect;

import static org.junit.Assert.*;

import java.awt.geom.Rectangle2D;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import org.junit.Test;

public class RectangleIntersectionFinderTest {

	@Test
	public void testFindIntersectionsNone() {
		assertFalse(RectangleIntersectionFinder.findIntersections(Arrays.asList(
				new Rectangle2D.Double(10, 20, 5, 7),
				new Rectangle2D.Double(10, 28, 1, 2))));
		assertFalse(RectangleIntersectionFinder.findIntersections(Arrays.asList(
				new Rectangle2D.Double(10, 20, 5, 7),
				new Rectangle2D.Double(16, 20, 1, 2))));
		assertFalse(RectangleIntersectionFinder.findIntersections(Arrays.asList(
				new Rectangle2D.Double(10, 20, 5, 7),
				new Rectangle2D.Double(16, 18, 1, 2))));
		assertFalse(RectangleIntersectionFinder.findIntersections(Arrays.asList(
				new Rectangle2D.Double(10, 20, 5, 7),
				new Rectangle2D.Double(8, 17, 1, 2))));
	}

	@Test
	public void testFindIntersectionsEasy() {
		assertTrue(RectangleIntersectionFinder.findIntersections(Arrays.asList(
				new Rectangle2D.Double(10, 20, 5, 7),
				new Rectangle2D.Double(11, 23, 1, 2))));
		assertTrue(RectangleIntersectionFinder.findIntersections(Arrays.asList(
				new Rectangle2D.Double(10, 20, 5, 7),
				new Rectangle2D.Double(11, 20, 1, 2))));
		assertTrue(RectangleIntersectionFinder.findIntersections(Arrays.asList(
				new Rectangle2D.Double(10, 20, 5, 7),
				new Rectangle2D.Double(9, 20, 2, 2))));
		assertTrue(RectangleIntersectionFinder.findIntersections(Arrays.asList(
				new Rectangle2D.Double(10, 20, 5, 7),
				new Rectangle2D.Double(9, 20, 1, 2))));
		assertTrue(RectangleIntersectionFinder.findIntersections(Arrays.asList(
				new Rectangle2D.Double(10, 20, 5, 7),
				new Rectangle2D.Double(15, 23, 1, 2))));
	}

	@Test
	public void testFindIntersectionsBoundary() {
		assertTrue(RectangleIntersectionFinder.findIntersections(Arrays.asList(
				new Rectangle2D.Double(10, 20, 5, 7),
				new Rectangle2D.Double(11, 27, 1, 2))));
		assertTrue(RectangleIntersectionFinder.findIntersections(Arrays.asList(
				new Rectangle2D.Double(10, 20, 5, 7),
				new Rectangle2D.Double(15, 22, 1, 2))));
		assertTrue(RectangleIntersectionFinder.findIntersections(Arrays.asList(
				new Rectangle2D.Double(10, 20, 5, 7),
				new Rectangle2D.Double(11, 18, 1, 2))));
		assertTrue(RectangleIntersectionFinder.findIntersections(Arrays.asList(
				new Rectangle2D.Double(10, 20, 5, 7),
				new Rectangle2D.Double(9, 22, 1, 2))));
		assertTrue(RectangleIntersectionFinder.findIntersections(Arrays.asList(
				new Rectangle2D.Double(10, 20, 5, 7),
				new Rectangle2D.Double(15, 27, 1, 2))));
		assertTrue(RectangleIntersectionFinder.findIntersections(Arrays.asList(
				new Rectangle2D.Double(10, 20, 5, 7),
				new Rectangle2D.Double(15, 18, 1, 2))));
		assertTrue(RectangleIntersectionFinder.findIntersections(Arrays.asList(
				new Rectangle2D.Double(10, 20, 5, 7),
				new Rectangle2D.Double(9, 18, 1, 2))));
		assertTrue(RectangleIntersectionFinder.findIntersections(Arrays.asList(
				new Rectangle2D.Double(10, 20, 5, 7),
				new Rectangle2D.Double(9, 27, 1, 2))));
	}

	@Test
	public void testFindIntersectionsMultiple() {
		assertFalse(RectangleIntersectionFinder.findIntersections(Arrays.asList(
				new Rectangle2D.Double(10, 20, 5, 7),
				new Rectangle2D.Double(20, 20, 5, 7),
				new Rectangle2D.Double(30, 20, 5, 7),
				new Rectangle2D.Double(40, 20, 5, 7),
				new Rectangle2D.Double(10, 30, 5, 7),
				new Rectangle2D.Double(20, 30, 5, 7),
				new Rectangle2D.Double(30, 30, 5, 7),
				new Rectangle2D.Double(40, 30, 5, 7))));
		assertTrue(RectangleIntersectionFinder.findIntersections(Arrays.asList(
				new Rectangle2D.Double(10, 20, 5, 7),
				new Rectangle2D.Double(20, 20, 5, 7),
				new Rectangle2D.Double(30, 20, 5, 7),
				new Rectangle2D.Double(40, 20, 5, 7),
				new Rectangle2D.Double(10, 30, 5, 7),
				new Rectangle2D.Double(20, 30, 5, 7),
				new Rectangle2D.Double(30, 30, 5, 7),
				new Rectangle2D.Double(40, 30, 5, 7),
				new Rectangle2D.Double(12, 23, 15, 15))));
	}

	@Test
	public void testFindIntersectingRectangles() {
		List<Rectangle2D> rects = Arrays.asList(
				new Rectangle2D.Double(10, 10,  5,  7),
				new Rectangle2D.Double(20, 20, 40, 40),
				new Rectangle2D.Double(40, 40, 20, 10),
				new Rectangle2D.Double(30, 25, 10, 10));
		
		Set<Rectangle2D> intersecting = RectangleIntersectionFinder.findIntersectingRectangles(rects);
		
		assertFalse(intersecting.contains(rects.get(0)));
		assertTrue(intersecting.contains(rects.get(1)));
		assertTrue(intersecting.contains(rects.get(2)));
		assertTrue(intersecting.contains(rects.get(3)));
	}

}
