package bxt.unilectures.vorkurs.pi;

/**
 * Calculate Pi via Monte Carlo integration over a circle using pseudo random points
 * @author bxt
 */
public class MonteCarloPi implements PiStrategy {
	@Override
	public double getPi(int iterations) {
		int countInside=0;
		for(int i=0;i<iterations;i++) {
			double x=Math.random();
			double y=Math.random();
			boolean isInsideCirle= 1> Math.sqrt(Math.pow(x, 2)+Math.pow(y, 2));
			if(isInsideCirle) countInside++;
		}
		return (double)countInside/iterations*4.0;
	}
}
