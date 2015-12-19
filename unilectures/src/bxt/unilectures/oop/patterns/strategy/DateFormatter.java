package bxt.unilectures.oop.patterns.strategy;

public class DateFormatter {
	
	public static String format(String format, Date date) {
		DateFormatterStrategy d;
		switch (format) {
		case "DE": d = new GermanDateFormatterStrategy(); break;
		case "US": d = new UsDateFormatterStrategy(); break;
		case "ISO": d = new IsoDateFormatterStrategy(); break;
		default: throw new IllegalArgumentException("Invalid date format: " + format);
		}
		return d.performDateFormatting(date);
	}
	
}
