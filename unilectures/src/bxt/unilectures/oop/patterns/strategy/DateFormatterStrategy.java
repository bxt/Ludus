package bxt.unilectures.oop.patterns.strategy;

public abstract class DateFormatterStrategy {
	
	String dateFormatString;
	
	public String performDateFormatting(Date date) {
		return String.format(dateFormatString, date.getDay(), date.getMonth(), date.getYear());
	}
	
}
