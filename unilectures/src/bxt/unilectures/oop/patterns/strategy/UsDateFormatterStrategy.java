package bxt.unilectures.oop.patterns.strategy;

public class UsDateFormatterStrategy extends DateFormatterStrategy {
	
	public UsDateFormatterStrategy() {
		dateFormatString = "%2$02d/%1$02d/%3$04d";
	}
	
}
