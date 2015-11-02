package bxt.video.sampler;

import java.awt.Color;

public class ExtractHsColorFilter implements ColorFilter {
	
	@Override
	public int map(int color) {
		float[] hsv = Color.RGBtoHSB(color>>16 & 0xff, color>>8 & 0xff, color & 0xff, null);
		return Color.HSBtoRGB(hsv[0], hsv[1], 1);
	}

}
