package bxt.util;

import processing.core.PApplet;
import processing.video.Capture;

public class CaptureRefresher implements Drawable {
	
	private Capture capture;
	
	public CaptureRefresher(PApplet p, int cameraIndex) {
		String[] cameras = Capture.list();
		capture = new Capture(p, cameras[cameraIndex]);
		capture.start();
	}
	
	@Override
	public void draw() {
		if (capture.available()) {
			capture.read();
		}
	}

	public Capture getCapture() {
		return capture;
	}

}
