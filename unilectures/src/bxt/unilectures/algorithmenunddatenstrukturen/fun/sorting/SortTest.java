package bxt.unilectures.algorithmenunddatenstrukturen.fun.sorting;

import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.Collection;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

@RunWith(value = Parameterized.class)
public class SortTest {
	
	private Sort sort;

	public SortTest(Sort sort) {
		super();
		this.sort = sort;
	}

	@Test
	public void testRandomArrays() {
		for (int i = 0; i < 100; i++) {
			long[] a = new long[i];
			for (int k = 0; k < i; k++) {
				a[k] = Math.round(Math.random()*100);
			}
			long[] b = a.clone();
			Arrays.sort(b);
			sort.sort(a);
			assertArrayEquals(b, a);
		}
	}
	
	@Parameters(name="{0}")
	public static Collection<Sort> implementation() {
		return Arrays.asList(
				new QuickSort(),
				new HeapSort(),
				new InsertionSort(),
				new BubbleSort(),
				new MergeSort(),
				new SelectionSort(),
				new CombinedQuickSort(3),
				new PatienceSort()
				);
	}

}
