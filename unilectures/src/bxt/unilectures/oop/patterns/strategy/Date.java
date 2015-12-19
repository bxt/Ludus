package bxt.unilectures.oop.patterns.strategy;

public class Date {
	private int day;
	private int month;
	private int year;
	
	public Date(int day, int month, int year) {
		super();
		this.day = day;
		this.month = month;
		this.year = year;
	}

	public int getDay() {
		return day;
	}

	public int getMonth() {
		return month;
	}

	public int getYear() {
		return year;
	}
	
	@SuppressWarnings("deprecation")
	public static Date today() {
		java.util.Date d = new java.util.Date();
		return new Date(d.getDate(), d.getMonth()+1, d.getYear() + 1900);
	}
}
