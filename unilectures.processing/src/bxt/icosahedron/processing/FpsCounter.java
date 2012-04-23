package bxt.icosahedron.processing;


public class FpsCounter {
	
	private long start;
	
	private final static float MS_PER_S = 1000f;
	
	public FpsCounter() {
		start = System.currentTimeMillis();
	}
	
	public float getAvgFps(int frameCount) {
		return MS_PER_S * (float)frameCount / (System.currentTimeMillis()-start);
	}

}
