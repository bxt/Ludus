package bxt.unilectures.oop.patterns.strategy;

public class IsoDateFormatterStrategy extends DateFormatterStrategy {
	
	public IsoDateFormatterStrategy() {
		dateFormatString = "%3$04d-%2$02d-%1$02d";
	}
	
}
