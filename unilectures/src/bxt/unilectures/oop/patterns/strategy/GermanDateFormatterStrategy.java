package bxt.unilectures.oop.patterns.strategy;

public class GermanDateFormatterStrategy extends DateFormatterStrategy {
	
	public GermanDateFormatterStrategy() {
		dateFormatString = "%02d.%02d.%04d";
	}
	
}
