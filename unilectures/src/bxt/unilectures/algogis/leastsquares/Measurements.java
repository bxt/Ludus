package bxt.unilectures.algogis.leastsquares;

import java.util.Collection;
import java.util.LinkedList;

public class Measurements {
	
	private Collection<Measurement> measurements = new LinkedList<Measurements.Measurement>();
	private int pointsSize;
	
	public Measurements(int pointsSize) {
		super();
		this.pointsSize = pointsSize;
	}
	
	public boolean addMeasurement(int from, int to, double value, double variance) {
		return add(new Measurement(from, to, value, variance));
	}
	
	public boolean add(Measurement measurement) {
		checkBounds(measurement.getFrom());
		checkBounds(measurement.getTo());
		return measurements.add(measurement);
	}

	public int getPointsSize() {
		return pointsSize;
	}
	
	public int getMeasurementsSize() {
		return measurements.size();
	}

	private void checkBounds(int point) {
		if(point < 1) throw new IllegalArgumentException("Point numbers must be postive.");
		if(point > pointsSize) throw new IllegalArgumentException("Point number exceeds point count.");
	}
	
	public Collection<Measurement> getMeasurements() {
		return measurements;
	}

	@Override
	public String toString() {
		return "Maseurements [measurements=" + measurements + ", pointsSize="
				+ pointsSize + "]";
	}

	public static class Measurement {
		private int from;
		private int to;
		private double value;
		private double variance;
		
		public Measurement(int from, int to, double value, double variance) {
			if(variance < 0 ) throw new IllegalArgumentException("Variance must be positive.");
			this.from = from;
			this.to = to;
			this.value = value;
			this.variance = variance;
		}
		
		public int getFrom() {
			return from;
		}
		public int getTo() {
			return to;
		}
		public double getValue() {
			return value;
		}
		public double getVariance() {
			return variance;
		}
		
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + from;
			result = prime * result + to;
			long temp;
			temp = Double.doubleToLongBits(value);
			result = prime * result + (int) (temp ^ (temp >>> 32));
			temp = Double.doubleToLongBits(variance);
			result = prime * result + (int) (temp ^ (temp >>> 32));
			return result;
		}
		
		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			Measurement other = (Measurement) obj;
			if (from != other.from)
				return false;
			if (to != other.to)
				return false;
			if (Double.doubleToLongBits(value) != Double
					.doubleToLongBits(other.value))
				return false;
			if (Double.doubleToLongBits(variance) != Double
					.doubleToLongBits(other.variance))
				return false;
			return true;
		}
		
		@Override
		public String toString() {
			return "Measurement [from=" + from + ", to=" + to + ", value="
					+ value + ", variance=" + variance + "]";
		}
		
	}
	
}
