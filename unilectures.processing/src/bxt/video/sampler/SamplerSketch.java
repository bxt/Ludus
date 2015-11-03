package bxt.video.sampler;

import java.util.Arrays;
import java.util.Date;

import bxt.util.CaptureRefresher;
import bxt.util.Cycler;
import bxt.util.CyclerDrawable;
import bxt.util.Drawable;
import bxt.util.FpsStringSupplier;
import bxt.util.TextDrawer;
import bxt.video.sampler.colorfilters.ColorFilters;
import bxt.video.sampler.colorfilters.ExtractHsColorFilter;
import bxt.video.sampler.colorfilters.HueShiftingColorFilter;
import bxt.video.sampler.colorfilters.ThresholdShiftingColorFilter;
import bxt.video.sampler.drawers.CircleSampleDrawer;
import bxt.video.sampler.drawers.CircleSizeSampleDrawer;
import bxt.video.sampler.drawers.PixelSampleDrawer;
import bxt.video.sampler.drawers.RectSampleDrawer;
import bxt.video.sampler.drawers.SampleDrawer;
import bxt.video.sampler.samplers.FlatSampler;
import bxt.video.sampler.samplers.Sampler;
import bxt.video.sampler.samplers.SelectiveAverageSampler;
import processing.core.PApplet;
import processing.video.Capture;

public class SamplerSketch extends PApplet {
	
	private Drawable[] drawables;
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
		
		TextDrawer fps = new TextDrawer(this, new FpsStringSupplier(5));
		TextDrawer lol = new TextDrawer(this, new Cycler<>(this, 10, Arrays.asList("a", "b", "c")), 0xffff8800, 100, 20);
		
		CaptureRefresher cr = new CaptureRefresher(this, 0);
		cam = cr.getCapture();
		
		Drawable cycle = new CyclerDrawable(this, 100,
				camThresholdDrawer(),
				circlesSampleDrawer(),
				boxySampleDrawer(),
				bwCirclesSampleDrawer(),
				colorCirclesSampleDrawer()
				);
		
		drawables = new Drawable[]{fps, lol, cr, cycle};
		
		noStroke();
	}
	
	@Override
	public void draw() {
		background(0);
		Arrays.stream(drawables).forEach(Drawable::draw);
	}
	
	private Drawable camThresholdDrawer() {
		Sampler s = new FlatSampler(1, cam);
		return new PixelSampleDrawer(this, s, 0, 40, new ThresholdShiftingColorFilter(100, this, 0xff330066, 0xffffdd88));
	}

	private SampleDrawer circlesSampleDrawer() {
		Sampler s = new FlatSampler(10, cam);
		return new CircleSizeSampleDrawer(this, s, 0, 40, ColorFilters.NONE, new ExtractHsColorFilter());
	}

	private SampleDrawer boxySampleDrawer() {
		Sampler s = new SelectiveAverageSampler(20, cam, 10);
		return new RectSampleDrawer(this, s, 0, 40, new HueShiftingColorFilter(100, this));
	}

	private SampleDrawer bwCirclesSampleDrawer() {
		Sampler s = new FlatSampler(5, cam);
		return new CircleSizeSampleDrawer(this, s, 0, 40, ColorFilters.NONE, (c) -> 0xffffffff);
	}

	private SampleDrawer colorCirclesSampleDrawer() {
		Sampler s = new SelectiveAverageSampler(20, cam, 10);
		return new CircleSampleDrawer(this, s, 0, 40, ColorFilters.NONE);
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
