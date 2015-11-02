package bxt.video.sampler.samplers;

import java.util.ArrayList;
import java.util.List;

public class ColorAverage {
	
	private List<Integer> colors = new ArrayList<>();
	
	public void add(int color) {
		colors.add(color);
	}
	
	public int getAverage() {
		float rSum = 0;
		float gSum = 0;
		float bSum = 0;
		for (int color: colors) {
			rSum += (color >> 16) & 0xff;
			gSum += (color >>  8) & 0xff;
			bSum +=  color        & 0xff;
		}
		rSum /= colors.size();
		gSum /= colors.size();
		bSum /= colors.size();
		return (0xff << 24) + ((int)rSum<<16) + ((int)gSum<<8) + (int)bSum;
	} 
	
}
