package bxt.icosahedron.processing;

/**
 * FPS calculation. 
 */
public class FpsCounter {
	
	private long start;
	
	private final static float MS_PER_S = 1000f;
	
	public FpsCounter() {
		start = System.currentTimeMillis();
	}
	
	/**
	 * Compute the average FPS from the construction of this object on, 
	 * given the number of frames rendered. 
	 * @param frameCount Current number of frames. 
	 * @return Frames per second. 
	 */
	public float getAvgFps(int frameCount) {
		return MS_PER_S * (float)frameCount / (System.currentTimeMillis()-start);
	}

}
