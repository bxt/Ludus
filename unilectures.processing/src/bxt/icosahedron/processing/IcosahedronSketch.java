package bxt.icosahedron.processing;
import bxt.icosahedron.geometry.PositionSwitch;
import processing.core.PApplet;
import processing.core.PFont;


public class IcosahedronSketch extends PApplet {

	private static final long serialVersionUID = 1L;
	
	private PFont font;
	
	private Drawable[] drawables;
	
	PositionSwitch positionSwitch;
	
	private final static int SCALE = 100;
	
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
				positionSwitch,
				new IcosahedronRenderer(SCALE, positionSwitch, this),
				new RotationCamera(SCALE * 6.0f, this),
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
	
	public void mouseClicked() {
		if(mouseButton == LEFT) {
			positionSwitch.nextPosition();
		}
	}
	
}
