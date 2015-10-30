package bxt.toprightregion.processing.artistic;

import java.awt.geom.Point2D;

public class ColorSchemeProvider implements ColorProvider {
	private ColorScheme scheme;
	
	public ColorSchemeProvider(ColorScheme scheme) {
		this.scheme = scheme;
	}

	@Override
	public int getColor(Point2D p) {
		if(Math.random() < 0.8) {
			return scheme.randomColorA.get();
		} else {
			return scheme.randomColorB.get();
		}
	}

	@Override
	public int getBackgroundColor() {
		return scheme.backgroundColor;
	}
}