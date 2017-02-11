package bxt.unilectures.algorithmenunddatenstrukturen.fun.sorting;

import java.util.Arrays;

/**
 * Main class to call some sorting algos
 * @author Burny
 */
public class SortMain {

	/**
	 * Main method to call some sorting algos
	 * @param args
	 */
	public static void main(String[] args) {
		
		Sort[] sortList={
				new QuickSort(),
				new HeapSort(),
				new InsertionSort(),
				new BubbleSort(),
				new MergeSort(),
				new SelectionSort(),
				new CombinedQuickSort(3),
				new PatienceSort(),
				};
		for (Sort sort : sortList) {
			System.out.println(String.format(
					"Sortiere mit %s:",sort.getClass().getSimpleName()));
			long[] a = {4,6,3,5,8,9,7,2,1};
			System.out.println(Arrays.toString(a));
			sort.sort(a);
			System.out.println(Arrays.toString(a));
		}
	}

}
