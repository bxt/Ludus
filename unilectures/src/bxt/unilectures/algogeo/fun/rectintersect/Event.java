package bxt.unilectures.algogeo.fun.rectintersect;

import java.awt.geom.Rectangle2D;

public class Event implements Comparable<Event>{
	
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
		int positionComparison = Double.compare(getPosition(), o.getPosition());
		if(positionComparison == 0) {
			return getMark().compareTo(o.getMark());
		} else {
			return positionComparison;
		}
	}
	
}
