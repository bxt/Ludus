package bxt.unilectures.algogis.leastsquares;

import java.util.Arrays;
import java.util.stream.Collectors;

import Jama.Matrix;

public class Util {

	public static String matrixToString(Matrix m) {
		return Arrays
				.stream(m.getArray())
				.map(row -> Arrays
						.stream(row)
						.mapToObj(Double::toString)
						.collect(Collectors.joining(" ")))
				.collect(Collectors.joining("\n"));
	}

}
