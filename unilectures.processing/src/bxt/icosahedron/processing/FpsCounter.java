package bxt.icosahedron.processing;


public class FpsCounter {
	
	private long start;
	
	public FpsCounter() {
		start = System.currentTimeMillis();
	}
	
	public float getAvgFps(int frameCount) {
		return 1000f * (float)frameCount / (System.currentTimeMillis()-start);
	}

}
