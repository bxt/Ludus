package bxt.video.sampler.colorfilters;

public class ExtractHsColorFilter extends HsbColorFilter implements ColorFilter {
	
	@Override
	protected float[] map(float[] color) {
		color[2] = 1;
		return color;
	}

}
