package bxt.icosahedron.processing;
import bxt.icosahedron.geometry.PositionSwitch;
import bxt.util.Drawable;
import bxt.util.FpsStringSupplier;
import bxt.util.TextPrinter;
import processing.core.PApplet;
import processing.core.PFont;

/**
 * The main sketch class, called when executing the application. 
 */
public class IcosahedronSketch extends PApplet {

	private PFont font;
	
	private Drawable[] drawables;
	
	PositionSwitch positionSwitch;
	
	private final static int SCALE = 100;
	
	/**
	 * Main-method for direct invocation, mostly dispatches to 
	 * {@link PApplet#main(String[])}. 
	 * @param args
	 */
	public static void main(String args[]) {
		PApplet.main(new String[]{
				"--present",IcosahedronSketch.class.getCanonicalName()});
	}
	
	@Override
	public void settings() {
		size(1280, 800, P3D);
		pixelDensity(2);
		smooth(4);
	}
	
	@Override
	public void setup() {
		
		hint(DISABLE_DEPTH_TEST); // make transparency kinda work
		//frameRate(100);
		
		font = loadFont("BitstreamVeraSans-Roman-48.vlw");
		textFont(font);
		
		positionSwitch = new PositionSwitch();
		
		drawables = new Drawable[]{
				positionSwitch,
				new IcosahedronRenderer(SCALE, positionSwitch, this),
				new RotationCamera(SCALE * 6.0f, this),
				new TextPrinter(new FpsStringSupplier()),
		};
		
	}
	
	@Override
	public void draw() {
		background(10);
		lights();
		
		for (Drawable drawable : drawables) {
			drawable.draw();
		}
		
	}
	
	@Override
	public void mouseClicked() {
		if(mouseButton == LEFT) {
			positionSwitch.nextPosition();
		}
	}
	
}
