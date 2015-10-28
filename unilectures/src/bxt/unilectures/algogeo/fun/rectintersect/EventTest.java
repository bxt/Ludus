package bxt.unilectures.algogeo.fun.rectintersect;

import static org.junit.Assert.*;

import java.awt.geom.Rectangle2D;
import org.junit.Test;

public class EventTest {

	@Test
	public void testGetPosition() {
		Event e1 = new Event(new Rectangle2D.Double(10, 20, 5, 7), Mark.START);
		Event e2 = new Event(new Rectangle2D.Double(10, 20, 5, 7), Mark.END);
		Event e3 = new Event(new Rectangle2D.Double( 9, 20, 1, 2), Mark.START);
		Event e4 = new Event(new Rectangle2D.Double( 9, 20, 1, 2), Mark.END);
		assertEquals(10, e1.getPosition(), 0.00001);
		assertEquals(15, e2.getPosition(), 0.00001);
		assertEquals( 9, e3.getPosition(), 0.00001);
		assertEquals(10, e4.getPosition(), 0.00001);
	}

	@Test
	public void testCompareTo() {
		Event e1 = new Event(new Rectangle2D.Double(10, 20, 5, 7), Mark.START);
		Event e2 = new Event(new Rectangle2D.Double(10, 20, 5, 7), Mark.END);
		Event e3 = new Event(new Rectangle2D.Double( 9, 20, 1, 2), Mark.START);
		Event e4 = new Event(new Rectangle2D.Double( 9, 20, 1, 2), Mark.END);
		assertTrue(e3.compareTo(e2) < 0);
		assertTrue(e1.compareTo(e4) < 0);
		assertTrue(e4.compareTo(e2) < 0);
		assertTrue(e3.compareTo(e4) < 0);
		assertTrue(e1.compareTo(e2) < 0);
		assertTrue(e3.compareTo(e2) < 0);
	}

}
