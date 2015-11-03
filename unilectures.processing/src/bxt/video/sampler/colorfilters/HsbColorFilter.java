package bxt.video.sampler.colorfilters;

import java.awt.Color;

public abstract class HsbColorFilter implements ColorFilter {
	
	@Override
	public int map(int color) {
		float[] hsv = Color.RGBtoHSB(color>>16 & 0xff, color>>8 & 0xff, color & 0xff, null);
		float[] result = map(hsv);
		return Color.HSBtoRGB(result[0], result[1], result[2]);
	}
	
	protected abstract float[] map(float[] color);

}
