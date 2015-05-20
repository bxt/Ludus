package bxt.unilectures.algogis.leastsquares;

import static org.junit.Assert.*;

import java.util.Arrays;

import org.junit.Test;

import Jama.Matrix;

public class LeastSuqaresAdjustmentTest {
	
	private static double DELTA = 0.00001;
	
	@Test
	public void testMinimalExample() {	
		LeastSuqaresAdjustment l = new LeastSuqaresAdjustment(column(1), column(1));
		assertMatrixEquals(column(1), l.getUnknowns());
		assertMatrixEquals(column(1), l.getTrueObservations());
		assertMatrixEquals(column(0), l.getError());
	}
	
	@Test
	public void testSingleDistance() {
		LeastSuqaresAdjustment l = new LeastSuqaresAdjustment(column(1,2,3,4,5), column(1,1,1,1,1));
		assertMatrixEquals(column(3), l.getUnknowns());
		assertMatrixEquals(column(3,3,3,3,3), l.getTrueObservations());
		assertMatrixEquals(column(-2,-1,0,1,2), l.getError());
	}
	
	@Test
	public void testSingleDistanceWithCovariance() {
		LeastSuqaresAdjustment l = new LeastSuqaresAdjustment(column(1,2,3,4,5), column(1,1,1,1,1), new double[]{6,1,1,1,1});
		assertMatrixEquals(column(2), l.getUnknowns());
		assertMatrixEquals(column(2,2,2,2,2), l.getTrueObservations());
		assertMatrixEquals(column(-1,0,1,2,3), l.getError());
	}
	
	@Test
	public void testHeightDifferences() {
		double[][] a = {{1,0,0},{-1,1,0},{0,-1,1},{0,0,-1},{1,0,-1}};
		
		LeastSuqaresAdjustment l = new LeastSuqaresAdjustment(column(4.1,-7,1.1,1.2,5.4), new Matrix(a));
		assertMatrixEquals(column(4.2,-2.6,-1.3), l.getUnknowns());
		assertMatrixEquals(column(4.2,-6.8,1.3,1.3,5.5), l.getTrueObservations());
		assertMatrixEquals(column(-0.1,-0.2,-0.2,-0.1,-0.1), l.getError());
	}
	
	private void assertMatrixEquals(Matrix expected, Matrix actual) {
		for(int i = 0; i < expected.getArray().length; i++) {
			assertArrayEquals(expected.getArray()[i], actual.getArray()[i], DELTA);
		}
	}
	
	private static Matrix column(double... values) {
		return new Matrix(Arrays.stream(values).mapToObj(v -> new double[]{v}).toArray(size -> new double[size][]));
	}
	
}
