package bxt.unilectures.algogeo.fun.convexhull;

import static bxt.unilectures.algogeo.fun.convexhull.Util.*;

import static org.junit.Assert.*;

import java.awt.geom.Point2D;

import org.junit.Test;

public class UtilTest {

	@Test
	public void testOnLine() {
		Point2D a = new Point2D.Double(1,0);
		Point2D b = new Point2D.Double(2,0);
		Point2D c = new Point2D.Double(3,0);
		Point2D d = new Point2D.Double(2,1);
		
		assertTrue(onLine(b, a, c));
		assertFalse(onLine(a, b, c));
		assertFalse(onLine(c, b, a));
		assertFalse(onLine(a, b, c));
		assertFalse(onLine(d, a, c));
	}

	@Test
	public void testStrictlyRight() {
		Point2D a = new Point2D.Double(1,0);
		Point2D b = new Point2D.Double(2,0);
		Point2D c = new Point2D.Double(3,0);
		Point2D d = new Point2D.Double(2,1);
		
		assertTrue(strictlyRight(d, c, a));
		assertFalse(strictlyRight(d, a, c));
		assertFalse(strictlyRight(b, a, c));
		assertFalse(strictlyRight(b, c, a));
	}

	@Test
	public void testDeterminant3() {
		assertEquals(111, determinant3(2, 5, 2, 3, -3, 1, 1, 4, -4), 0.0001);
		assertEquals(-270, determinant3(5, -1, 9, -1, 6, -1, 9, -1, 7), 0.0001);
	}

}
