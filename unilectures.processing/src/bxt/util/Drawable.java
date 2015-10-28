package bxt.util;

/**
 * Should be redrawn in Processing's render loop. 
 */
public interface Drawable {
	
	/**
	 * Do the redraw. The actual Processing object is usually passed in
	 * the constructor. 
	 */
	public void draw();
	
}
