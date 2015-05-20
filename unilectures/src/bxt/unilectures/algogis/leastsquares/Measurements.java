package bxt.unilectures.algogis.leastsquares;

import java.util.ArrayList;
import java.util.List;
import java.util.function.ToIntFunction;

import Jama.Matrix;

/**
 * Represents a set of measurements of height differences to estimate relative hights of points.
 */
public class Measurements {
	
	private List<Measurement> measurements = new ArrayList<Measurement>();
	private int pointsSize;
	
	public Measurements(int pointsSize) {
		super();
		this.pointsSize = pointsSize;
	}
	
	/**
	 * Add a height difference measurement between two points.
	 * @param from Number of the first point of the measurement (1-n)
	 * @param to Number of the second point of the measurement (1-n)
	 * @param value Measured height difference.
	 * @param variance Assumed variance/reliability of this measurement.
	 * @return <tt>true</tt>
	 */
	public boolean addMeasurement(int from, int to, double value, double variance) {
		return add(new Measurement(from, to, value, variance));
	}
	
	public boolean add(Measurement measurement) {
		checkBounds(measurement.getFrom());
		checkBounds(measurement.getTo());
		return measurements.add(measurement);
	}

	public List<Measurement> getMeasurements() {
		return measurements;
	}
	
	public int getPointsSize() {
		return pointsSize;
	}
	
	public int getMeasurementsSize() {
		return measurements.size();
	}
	
	/**
	 * Calculate the least squares adjustment for the measurements.
	 * @return
	 */
	public LeastSquaresAdjustment getLeastSquaresAdjustment() {
		Matrix observations = buildObservations();
		Matrix phi = buildPhi();
		double[] variance = buildVariance();
		return new LeastSquaresAdjustment(observations, phi, variance);
	}

	private Matrix buildObservations() {
		return new Matrix(getMeasurements().stream()
				.map(v -> new double[]{v.getValue()})
				.toArray(size -> new double[size][]));
	}
	
	private double[] buildVariance() {
		return getMeasurements().stream()
				.mapToDouble(Measurement::getVariance).toArray();
	}
	
	private Matrix buildPhi() {
		Matrix result = new Matrix(getMeasurementsSize(), getPointsSize()-1);
		setPhiValues(result, Measurement::getFrom, -1);
		setPhiValues(result, Measurement::getTo  ,  1);
		return result;
	}
	
	private void setPhiValues(Matrix result, ToIntFunction<Measurement> f, double value) {
		for(int i = 0; i < getMeasurementsSize(); i++) {
			int point = f.applyAsInt(getMeasurements().get(i));
			if(point != 1)
				result.set(i, point - 2, value);
		}
	}
	
	private void checkBounds(int point) {
		if(point < 1) throw new IllegalArgumentException("Point numbers must be postive.");
		if(point > pointsSize) throw new IllegalArgumentException("Point number exceeds point count.");
	}
	
	@Override
	public String toString() {
		return "Maseurements [measurements=" + measurements + ", pointsSize="
				+ pointsSize + "]";
	}
	
}
