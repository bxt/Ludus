package bxt.unilectures.vorkurs.pi;

/**
 * An algorithm calculating the mathematical constant Pi
 * @author bxt
 *
 */
public interface PiStrategy {
	
	/**
	 * The method returning Pi
	 * @param iterations Number to indicate desired precision (bigger means better)
	 * @return Pi approximation
	 */
	public abstract double getPi(int iterations);

}