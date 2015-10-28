package bxt.unilectures.algogeo.fun.rectintersect;

import static org.junit.Assert.*;

import java.awt.geom.Rectangle2D;
import java.util.Arrays;
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

}
