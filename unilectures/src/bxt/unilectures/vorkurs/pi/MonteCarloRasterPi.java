package bxt.unilectures.vorkurs.pi;

/**
 * Calculate Pi via Monte Carlo integration over a circle using a raster of points
 * @author bxt
 */
public class MonteCarloRasterPi extends MonteCarloPi {

	@Override
	public double getPi(int iterations) {
		iterations= (int) Math.pow(Math.floor(Math.sqrt(iterations)),2);
		return super.getPi(iterations);
	}
	
	@Override
	protected Point generatePoint(int i, int total) {
		int size=(int) Math.sqrt(total);
		Point p=new Point((double)(i%size)/(size-1),(double)(i/size)/(size-1));
		//System.out.printf("x: %f, y: %f%n",p.x,p.y);
		return p;
	}

}
