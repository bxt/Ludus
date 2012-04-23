package bxt.icosahedron.processing;
import bxt.icosahedron.geometry.PositionSwitch;
import processing.core.PApplet;
import processing.core.PFont;


public class IcosahedronSketch extends PApplet {

	private static final long serialVersionUID = 1L;
	
	private PFont font;
	
	private Drawable[] drawables;
	
	PositionSwitch positionSwitch;
	
	public static void main(String args[]) {
		PApplet.main(new String[]{
				"--present",IcosahedronSketch.class.getCanonicalName()});
	}
	
	public void setup() {
		
		size(1920, 1200, P3D);
		//frameRate(100);
		
		font = loadFont("BitstreamVeraSans-Roman-48.vlw");
		textFont(font);
		
		positionSwitch = new PositionSwitch();
		
		drawables = new Drawable[]{
				new IcosahedronRenderer(100, positionSwitch, this),
				new RotationCamera(this),
				new FpsPrinter(this),
		};
		
	}
	
	public void draw() {
		background(10);
		lights();
		
		positionSwitch.step();
		
		for (Drawable drawable : drawables) {
			drawable.draw();
		}
		
	}
	
	public void mouseClicked() {
		if(mouseButton == LEFT) {
			positionSwitch.nextPosition();
		}
	}
	
}
