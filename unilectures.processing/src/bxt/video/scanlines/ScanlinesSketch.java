package bxt.video.scanlines;

import bxt.util.FpsPrinter;
import processing.core.PApplet;
import processing.video.Capture;

public class ScanlinesSketch extends PApplet {
	
	FpsPrinter f = new FpsPrinter(this);
	Capture cam;
	Buffer[] buffers;
	
	/**
	 * Main-method for direct invocation, dispatches to 
	 * {@link PApplet#main(String[])}. 
	 * @param args
	 */
	public static void main(String args[]) {
		PApplet.main(new String[]{ScanlinesSketch.class.getCanonicalName()});
	}
	
	public void settings() {
	  size(640, 360);
	  pixelDensity(displayDensity());
	}
	
	public void setup() {
		String[] cameras = Capture.list();
		cam = new Capture(this, cameras[0]);
		cam.start();
		
		buffers = new Buffer[pixelHeight];
		for (int i = 0; i < buffers.length; i++) {
			buffers[i] = new Buffer(pixelWidth, (i>>0)+1);
		}
	}
	
	public void draw() {
		if (cam.available()) {
			cam.read();
		}
		
		if(cam.width == pixelWidth && cam.height == pixelHeight) {
			loadPixels();
			for (int y = 0; y < pixelHeight; y++) {
				for (int x = 0; x < pixelWidth; x++) {
					int locReversed = (y + 1) * pixelWidth - 1 - x;
					buffers[y].put(x, cam.pixels[locReversed]);
				}
			}
			for (int y = 0; y < pixelHeight; y++) {
				buffers[y].next();
				buffers[y].get(pixels, y * pixelWidth);
			}
			updatePixels();
		} else {
			System.out.println("Skipped frame with width " + cam.width + " instead " + pixelWidth + " and " + cam.height + "instead" + pixelHeight);
		}
		
		f.draw();
	}
	
}
