package bxt.unilectures.oop.patterns.strategy;

import java.util.Arrays;
import java.util.Collection;

public class DateFormatterDemo {
				
	public static void main(String[] args) {
		Collection<String> formats = Arrays.asList("DE", "US", "ISO");
		Collection<Date> dates = Arrays.asList(new Date(19, 11, 1990), new Date(3, 2, 1991), Date.today());
		for (Date date : dates) {
			for (String format : formats) {
				System.out.println(DateFormatter.format(format, date));
			}
		}
	}
	
}
