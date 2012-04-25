package bxt.unilectures.fun.proxies;

/**
 * Proxies two implementations of the product function into a timer
 * proxy and executes their mult() methods, logging the duration of the 
 * calculation. 
 * @author Bernhard Häussner
 */
public class Main {
	
	public static void main(String... args) {
		
		// Dummy values
		long a = 321321313l;
		long b = -456456456l;
		
		// Build the proxies:
		Product loopProduct = 
				TimingProxy.newInstance(new LoopProduct(),Product.class);
		Product normalProduct = 
				TimingProxy.newInstance(new NormalProduct(),Product.class);
		
		// This will output something like "$Proxy0" for the class name:
		System.out.println("loopProduct has class "+
				loopProduct.getClass().getCanonicalName());
		
		// Do the calculations: 
		System.out.println(loopProduct.mult(a, b));
		System.out.println(normalProduct.mult(a, b));
	}
	
}
