package bxt.unilectures.algogis.leastsquares;

import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.stream.IntStream;

import org.junit.Test;

import Jama.Matrix;

public class LeastSquaresAdjustmentTest {
	
	private static final double DELTA = 0.00001;
	
	@Test
	public void testMinimalExample() {	
		LeastSquaresAdjustment l = new LeastSquaresAdjustment(column(1), column(1));
		assertMatrixEquals(column(1), l.getUnknowns());
		assertMatrixEquals(column(1), l.getTrueObservations());
		assertMatrixEquals(column(0), l.getError());
	}
	
	@Test
	public void testSingleDistance() {
		LeastSquaresAdjustment l = new LeastSquaresAdjustment(column(1,2,3,4,5), column(1,1,1,1,1));
		assertMatrixEquals(column(3), l.getUnknowns());
		assertMatrixEquals(column(3,3,3,3,3), l.getTrueObservations());
		assertMatrixEquals(column(-2,-1,0,1,2), l.getError());
	}
	
	@Test
	public void testSingleDistanceWithCovariance() {
		LeastSquaresAdjustment l = new LeastSquaresAdjustment(column(1,2,3,4,5), column(1,1,1,1,1), new double[]{6,1,1,1,1});
		assertMatrixEquals(column(2), l.getUnknowns());
		assertMatrixEquals(column(2,2,2,2,2), l.getTrueObservations());
		assertMatrixEquals(column(-1,0,1,2,3), l.getError());
	}
	
	/*
	 * These are the values form the lecture slides "Beispiel 2"
	 */
	@Test
	public void testHeightDifferences() {
		double[][] a = {{1,0,0},{-1,1,0},{0,-1,1},{0,0,-1},{1,0,-1}};
		
		LeastSquaresAdjustment l = new LeastSquaresAdjustment(column(4.1,-7,1.1,1.2,5.4), new Matrix(a));
		
		assertMatrixEquals(column(4.2,-2.6,-1.3), l.getUnknowns());
		assertMatrixEquals(column(4.2,-6.8,1.3,1.3,5.5), l.getTrueObservations());
		assertMatrixEquals(column(-0.1,-0.2,-0.2,-0.1,-0.1), l.getError());
		
		assertEquals(0.055, l.getVariance(), DELTA);
		
		assertEquals(0.0344, l.getUnknownVariance().get(0, 0), 0.0005);
		assertEquals(0.0344, l.getUnknownVariance().get(2, 2), 0.0005);
		assertEquals(0.055, l.getUnknownVariance().get(1, 1), 0.0005);
		
		IntStream.range(0, 4).forEach(i -> {
			assertEquals(0.0344, l.getObservationVariance().get(i, i), 0.0005);
		});
		assertEquals(0.0275, l.getObservationVariance().get(4, 4), 0.0005);
	}
	
	private void assertMatrixEquals(Matrix expected, Matrix actual) {
		for(int i = 0; i < expected.getArray().length; i++) {
			assertArrayEquals(expected.getArray()[i], actual.getArray()[i], DELTA);
		}
	}
	
	private static Matrix column(double... values) {
		return new Matrix(Arrays.stream(values)
				.mapToObj(v -> new double[]{v})
				.toArray(size -> new double[size][]));
	}
	
}
