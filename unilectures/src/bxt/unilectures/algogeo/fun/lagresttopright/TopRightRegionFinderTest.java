package bxt.unilectures.algogeo.fun.lagresttopright;

import static org.junit.Assert.*;

import java.awt.geom.Point2D;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.junit.Test;

public class TopRightRegionFinderTest {

	@Test
	public void test1() {
		List<Point2D> points = Arrays.asList(
				new Point2D.Double(10,10),
				new Point2D.Double(15,13)
		);
		
		Map<Point2D, Double> result = TopRightRegionFinder.findLargestTopRightRegionSizes(points);
		
		assertEquals(Double.valueOf(5), result.get(points.get(0)));
		assertEquals(null, result.get(points.get(1)));
	}

	@Test
	public void test2() {
		List<Point2D> points = Arrays.asList(
				new Point2D.Double(1,1),
				new Point2D.Double(5,6),
				new Point2D.Double(6,4),
				new Point2D.Double(7,7),
				new Point2D.Double(3,5)
		);
		
		Map<Point2D, Double> result = TopRightRegionFinder.findLargestTopRightRegionSizes(points);
		
		assertEquals(Double.valueOf(4), result.get(points.get(0)));
		assertEquals(Double.valueOf(2), result.get(points.get(1)));
		assertEquals(Double.valueOf(3), result.get(points.get(2)));
		assertEquals(null, result.get(points.get(3)));
		assertEquals(Double.valueOf(2), result.get(points.get(4)));
	}

	@Test
	public void test3() {
		List<Point2D> points = Arrays.asList(
				new Point2D.Double(3,1),
				new Point2D.Double(1,3),
				new Point2D.Double(4,4)
		);
		
		Map<Point2D, Double> result = TopRightRegionFinder.findLargestTopRightRegionSizes(points);
		
		assertEquals(Double.valueOf(3), result.get(points.get(0)));
		assertEquals(Double.valueOf(3), result.get(points.get(1)));
		assertEquals(null, result.get(points.get(2)));
	}

}
