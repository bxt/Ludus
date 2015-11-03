package bxt.util;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.function.Supplier;

public class FpsStringSupplier extends FpsCounter implements Supplier<String> {

	private static final NumberFormat format = new DecimalFormat("###.##");
	
	public static String format(float fps) {
		return format.format(fps) + " fps";
	}

	private int every;
	
	private int frameCount = 0;
	private String output = "";
	
	public FpsStringSupplier() {
		this(40);
	}

	public FpsStringSupplier(int every) {
		super();
		this.every = every;
	}

	@Override
	public String get() {
		frameCount++;
		if(frameCount>every) {
			output = format(getAvgFps(every));
			reset();
			frameCount = 0;
			
		}
		return output;
	}
	
}
