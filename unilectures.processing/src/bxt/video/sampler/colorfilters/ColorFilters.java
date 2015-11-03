package bxt.video.sampler.colorfilters;

public interface ColorFilters {

	public static ColorFilter NONE = (c) -> c;
	public static ColorFilter GRYSCALE = (c) -> grey(grey(c));
	public static ColorFilter INVERT = (c) -> 0xffffff ^ c;
	
	public static float grey(int c) {
		return ((c & 0xff) + (c>>8 & 0xff) + (c>>16 & 0xff)) / 768f;
	}

	public static int grey(float percentage) {
		int value = (int)(percentage * 255);
		return 0xff<<24 | value<<16 | value<<8 | value;
	}

}
