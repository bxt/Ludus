package bxt.video.sampler;

import java.util.Arrays;
import java.util.Date;

import bxt.util.Drawable;
import bxt.util.FpsPrinter;
import bxt.video.sampler.drawers.CircleSampleDrawer;
import bxt.video.sampler.drawers.CircleSizeSampleDrawer;
import bxt.video.sampler.drawers.RectSampleDrawer;
import bxt.video.sampler.drawers.SampleDrawer;
import bxt.video.sampler.samplers.FlatSampler;
import bxt.video.sampler.samplers.Sampler;
import bxt.video.sampler.samplers.SelectiveAverageSampler;
import processing.core.PApplet;
import processing.video.Capture;

public class SamplerSketch extends PApplet {
	
	private FpsPrinter f = new FpsPrinter(this);
	private Capture cam;
	
	/**
	 * Main-method for direct invocation, dispatches to 
	 * {@link PApplet#main(String[])}. 
	 * @param args
	 */
	public static void main(String args[]) {
		PApplet.main(new String[]{SamplerSketch.class.getCanonicalName()});
	}
	
	@Override
	public void settings() {
	  size(640, 360);
	  pixelDensity(displayDensity());
	  fullScreen();
	}
	
	@Override
	public void setup() {
		String[] cameras = Capture.list();
		cam = new Capture(this, cameras[0]);
		cam.start();
		
		noStroke();
	}
	
	@Override
	public void draw() {
		if (cam.available()) {
			cam.read();
		}
		
		background(0);
		SampleDrawer sd = circlesSampleDrawer();

		Arrays.stream(new Drawable[]{sd, f}).forEach(Drawable::draw);
	}
	
	private SampleDrawer circlesSampleDrawer() {
		Sampler s = new FlatSampler(10, cam);
		return new CircleSizeSampleDrawer(this, s, 0, 40, ColorFilters.NONE, new ExtractHsColorFilter());
	}

	@SuppressWarnings("unused")
	private SampleDrawer boxySampleDrawer() {
		Sampler s = new SelectiveAverageSampler(20, cam, 10);
		return new RectSampleDrawer(this, s, 0, 40, ColorFilters.INVERT);
	}

	@SuppressWarnings("unused")
	private SampleDrawer bwCirclesSampleDrawer() {
		Sampler s = new FlatSampler(5, cam);
		return new CircleSizeSampleDrawer(this, s, 0, 40, ColorFilters.NONE, (c) -> 0xffffffff);
	}

	@Override
	public void keyPressed() {
		if (key == ' ' ) {
			String path = "/Users/bernhardhaussner/Documents/pic_"+(new Date().getTime())+".png";
			save(path);
			System.out.println("Saved " + path);
		}
	}
	
}
