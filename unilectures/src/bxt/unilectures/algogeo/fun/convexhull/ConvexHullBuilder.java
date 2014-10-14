package bxt.unilectures.algogeo.fun.convexhull;

import java.awt.geom.Point2D;
import java.util.Collection;
import java.util.List;

public interface ConvexHullBuilder {
	
	public abstract List<Point2D> build(Collection<Point2D> points); 
	
}
