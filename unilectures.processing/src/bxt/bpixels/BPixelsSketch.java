package bxt.bpixels;

import java.util.Arrays;
import java.util.Date;

import bxt.util.Drawable;
import bxt.util.FpsStringSupplier;
import bxt.util.TextDrawer;
import processing.core.PApplet;

public class BPixelsSketch extends PApplet {
	
	private Drawable[] drawables;
	
	/**
	 * Main-method for direct invocation, dispatches to 
	 * {@link PApplet#main(String[])}. 
	 * @param args
	 */
	public static void main(String args[]) {
		PApplet.main(new String[]{BPixelsSketch.class.getCanonicalName()});
	}
	
	@Override
	public void settings() {
	  size(640, 360);
	  pixelDensity(displayDensity());
	  fullScreen();
	}
	
	@Override
	public void setup() {
		
		TextDrawer fps = new TextDrawer(this, new FpsStringSupplier(5), 0xffff8800, 20, 20);
		
		Drawable main = new BPixelsDrawable(this);
		
		drawables = new Drawable[]{main, fps};
		
		noStroke();
	}
	
	@Override
	public void draw() {
		background(0);
		Arrays.stream(drawables).forEach(Drawable::draw);
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
