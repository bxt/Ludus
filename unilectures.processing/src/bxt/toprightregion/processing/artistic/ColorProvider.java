package bxt.toprightregion.processing.artistic;

import java.awt.geom.Point2D;

public interface ColorProvider {
	public int getColor(Point2D p);
	public int getBackgroundColor();
}