package bxt.unilectures.vorkurs.pi;

/**
 * Calculate Pi via Monte Carlo integration over a circle using pseudo random points
 * @author bxt
 */
abstract public class MonteCarloPi implements PiStrategy {
	/**
	 * Run monte carlo
	 */
	@Override
	public double getPi(int iterations) {
		int countInside=0;
		for(int i=0;i<iterations;i++) {
			Point p=generatePoint(i,iterations);
			boolean isInsideCirle= 1> Math.sqrt(Math.pow(p.x, 2)+Math.pow(p.y, 2));
			if(isInsideCirle) countInside++;
		}
		return (double)countInside/iterations*4.0;
	}
	/**
	 * Provide access to a list of Points
	 * @param i Index of current point
	 * @param total Number of points we will need
	 * @return A Point to use in MC
	 */
	abstract protected Point generatePoint(int i,int total);
	/**
	 * Stores x and y coordinates for a MC sample on a 2d plain
	 * @author bxt
	 */
	protected class Point {
		public double x;
		public double y;
		Point() {}
		Point(double nx,double ny) {
			x=nx;
			y=ny;
		}
	}
}
