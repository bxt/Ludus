package bxt.unilectures.algogis.leastsquares;

import java.util.stream.IntStream;

import Jama.Matrix;

public class LeastSuqaresAdjustment {
	
	private Matrix unknowns;
	private Matrix trueObservations;
	private Matrix error;
	private double variance;
	private Matrix observationVariance;
	private Matrix unknownVariance;
	
	public LeastSuqaresAdjustment(Matrix observations, Matrix phi) {
		this(observations, phi, Matrix.identity(observations.getRowDimension(), observations.getRowDimension()));
	}
	
	public LeastSuqaresAdjustment(Matrix observations, Matrix phi, double[] covariance) {
		this(observations, phi, diagonal(covariance));
	}
	
	public LeastSuqaresAdjustment(Matrix observations, Matrix phi, Matrix covariance) {
		if(phi.getRowDimension() != observations.getRowDimension())
			throw new IllegalArgumentException("Observartion and Phi dimenstions must agree!");
		if(covariance.getRowDimension() != covariance.getColumnDimension())
			throw new IllegalArgumentException("Covatiance matrix must be square!");
		if(covariance.getRowDimension() != observations.getRowDimension())
			throw new IllegalArgumentException("Observartion and covatiance dimenstions must agree!");
		
		Matrix atp = phi.transpose().times(covariance);
		Matrix atpa = atp.times(phi);
		Matrix atpl = atp.times(observations);
		
		unknowns = atpa.solve(atpl);
		trueObservations = phi.times(unknowns);
		error = observations.minus(trueObservations);
		variance = error.transpose().times(covariance).times(error).get(0, 0) / (observations.getRowDimension()-unknowns.getRowDimension());
		unknownVariance = atpa.inverse().times(variance);
		observationVariance = phi.times(unknownVariance).times(phi.transpose());
	}

	public Matrix getUnknowns() {
		return unknowns;
	}

	public Matrix getTrueObservations() {
		return trueObservations;
	}

	public Matrix getError() {
		return error;
	}
	
	public double getVariance() {
		return variance;
	}

	public Matrix getObservationVariance() {
		return observationVariance;
	}

	public Matrix getUnknownVariance() {
		return unknownVariance;
	}

	private static Matrix diagonal(double[] covariance) {
		Matrix result = new Matrix(covariance.length, covariance.length);
		IntStream.range(0, covariance.length)
			.forEach(i -> result.set(i, i, covariance[i]));
		return result;
	}

}
