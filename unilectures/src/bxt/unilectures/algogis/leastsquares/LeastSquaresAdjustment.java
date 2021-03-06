package bxt.unilectures.algogis.leastsquares;

import java.util.stream.IntStream;

import Jama.Matrix;

/**
 * Performs Least Squares Adjustments which allows to estimate a set of unknowns
 * from a set of observations and a function mapping from observations to unknowns.
 */
public class LeastSquaresAdjustment {
	
	private Matrix unknowns;
	private Matrix trueObservations;
	private Matrix error;
	private double variance;
	private Matrix observationVariance;
	private Matrix unknownVariance;
	
	/**
	 * Calculate a least square adjustment for the given values.
	 * The variance/reliability values are assumed to be all 1.
	 * @param observations Column vector of observations, aka L
	 * @param phi Linear mapping from unknowns/estimations to observations/measurements, aka A
	 */
	public LeastSquaresAdjustment(Matrix observations, Matrix phi) {
		this(observations, phi, Matrix.identity(observations.getRowDimension(), observations.getRowDimension()));
	}
	
	/**
	 * Calculate a least square adjustment for the given values.
	 * @param observations Column vector of observations, aka L
	 * @param phi Linear mapping from unknowns/estimations to observations/measurements, aka A
	 * @param covariance List containing variance/reliability values for observations
	 */
	public LeastSquaresAdjustment(Matrix observations, Matrix phi, double[] covariance) {
		this(observations, phi, diagonal(covariance));
	}
	
	/**
	 * Calculate a least square adjustment for the given values.
	 * @param observations Column vector of observations, aka L
	 * @param phi Linear mapping from unknowns/estimations to observations/measurements, aka A
	 * @param covariance Diagonal matrix containing variance/reliability values for observations, aka P
	 */
	public LeastSquaresAdjustment(Matrix observations, Matrix phi, Matrix covariance) {
		if(phi.getRowDimension() != observations.getRowDimension())
			throw new IllegalArgumentException("Observartion and Phi dimenstions must agree!");
		if(covariance.getRowDimension() != covariance.getColumnDimension())
			throw new IllegalArgumentException("Covatiance matrix must be square!");
		if(covariance.getRowDimension() != observations.getRowDimension())
			throw new IllegalArgumentException("Observartion and covatiance dimenstions must agree!");
		
		Matrix at = phi.transpose(); // Aᵀ
		Matrix atp = at.times(covariance); // AᵀP
		Matrix atpa = atp.times(phi); // AᵀPA
		Matrix atpl = atp.times(observations); // AᵀPL
		
		unknowns = atpa.solve(atpl); // Solve AᵀPA X = AᵀPL for X
		trueObservations = phi.times(unknowns); // L̂ = AX
		error = observations.minus(trueObservations); // v = L-L̂
		
		variance = error.transpose().times(covariance).times(error).get(0, 0) // σ₀² = vᵀPv /
		         / (observations.getRowDimension()-unknowns.getRowDimension()); //      n-u
		unknownVariance = atpa.inverse().times(variance); // Σxx = (AᵀPA)⁻¹ * σ₀²
		observationVariance = phi.times(unknownVariance).times(at); // Σll = AΣxxAᵀ
	}
	
	/**
	 * Get the estimated adjusted unknowns.
	 * @return Column vector of estimated unknowns.
	 */
	public Matrix getUnknowns() {
		return unknowns;
	}
	
	/**
	 * Get the observation values that would have resulted in the estimated unknowns.
	 * @return Column vector of ture observation values.
	 */
	public Matrix getTrueObservations() {
		return trueObservations;
	}
	
	/**
	 * Get the estimated error in the original observation, calulated from the difference to {@link #getTrueObservations()}.
	 * @return Column vector of error values.
	 */
	public Matrix getError() {
		return error;
	}
	
	/**
	 * Get the estimated total variance of the observations.
	 * @return Value of the standard deviation squared.
	 */
	public double getVariance() {
		return variance;
	}
	
	/**
	 * Get the estimated variances of the single observations. 
	 * @return Matrix containing the variances on its diagonal.
	 */
	public Matrix getObservationVariance() {
		return observationVariance;
	}
	
	/**
	 * Get the estimated variance for each of the unknowns, i.e. the accuracy of our estimation.
	 * @return Matrix containing the variances on its diagonal.
	 */
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
