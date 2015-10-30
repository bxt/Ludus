package bxt.unilectures.algogeo.fun.rectintersect;

import java.awt.geom.Rectangle2D;
import java.util.Comparator;

public class Event implements Comparable<Event>{
	
	private static final Comparator<Event> NATURAL_COMPARATOR =
			Comparator.comparingDouble(Event::getPosition).thenComparing(Event::getMark);
	
	private Rectangle2D rectangle;
	private Mark mark;
	
	public Event(Rectangle2D rectangle, Mark mark) {
		super();
		this.rectangle = rectangle;
		this.mark = mark;
	}

	public Rectangle2D getRectangle() {
		return rectangle;
	}

	public Mark getMark() {
		return mark;
	}
	
	public double getPosition() {
		return getRectangle().getX() + (mark == Mark.START ? 0 : getRectangle().getWidth());
	}

	@Override
	public String toString() {
		return "Event [rectangle=" + rectangle + ", mark=" + mark + "]";
	}

	@Override
	public int compareTo(Event o) {
		return NATURAL_COMPARATOR.compare(this, o);
	}
	
}
