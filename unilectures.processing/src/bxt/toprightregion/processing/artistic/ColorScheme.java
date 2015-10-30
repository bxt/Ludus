package bxt.toprightregion.processing.artistic;

import java.util.function.Supplier;

public enum ColorScheme {
	WHITE(255,
			() -> ((0x88<<24) + ((int)Math.round(Math.random()*0xff)<<16) + ((int)(0xff-Math.round(Math.random()*0x99)<<8))),
			() -> ((0x88<<24) + (0x55<<8) + (0x55<<16) + ((int)Math.round(Math.random()*0x55)<<16) + ((int)(0xff-Math.round(Math.random()*0x99)))) ),
	BLACK(0, 
			() -> ((0x88<<24) + ((int)Math.round(Math.random()*0xff)<<16) + ((int)(0xff-Math.round(Math.random()*0x99)<<8))),
			() -> ((0x88<<24) + (0x55<<8) + (0x55) + ((int)Math.round(Math.random()*0x55)) + (((int)(0xff-Math.round(Math.random()*0x99))<<16))) );
	
	int backgroundColor;
	private ColorScheme(int backgroundColor, Supplier<Integer> randomColorA, Supplier<Integer> randomColorB) {
		this.backgroundColor = backgroundColor;
		this.randomColorA = randomColorA;
		this.randomColorB = randomColorB;
	}
	Supplier<Integer> randomColorA;
	Supplier<Integer> randomColorB;
}