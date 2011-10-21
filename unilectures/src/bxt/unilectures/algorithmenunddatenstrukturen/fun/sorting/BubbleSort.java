package bxt.unilectures.algorithmenunddatenstrukturen.fun.sorting;

public class BubbleSort implements Sort {

	@Override
	public void sort(long[] a) {
		boolean didSwitch = true;
		while (didSwitch) {
			didSwitch  = false;
			for (int j = 1; j < a.length; j++) {
				if(a[j]<a[j-1]) {
					long key=a[j];
					a[j]=a[j-1];
					a[j-1]=key;
					didSwitch = true;
				}
			}
		}
	}

}
