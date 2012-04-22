package bxt.icosahedron.processing;
import processing.core.PApplet;
import processing.core.PFont;


public class IcosahedronSketch extends PApplet {

	private static final long serialVersionUID = 1L;
	
	private PFont font;
	
	private Drawable[] drawables;
	
	public static void main(String args[]) {
		PApplet.main(new String[]{
				"--present",IcosahedronSketch.class.getCanonicalName()});
	}
	
	public void setup() {
		
		size(1920, 1200, P3D);
		//frameRate(100);
		
		font = loadFont("BitstreamVeraSans-Roman-48.vlw");
		textFont(font);
		
		drawables = new Drawable[]{
				new IcosahedronRenderer(100, this),
				new RotationCamera(this),
				new FpsPrinter(this),
		};
		
	}
	
	public void draw() {
		background(10);
		lights();
		
		for (Drawable drawable : drawables) {
			drawable.draw();
		}
		
	}
}
