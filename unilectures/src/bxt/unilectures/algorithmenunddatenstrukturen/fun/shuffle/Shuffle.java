package bxt.unilectures.algorithmenunddatenstrukturen.fun.shuffle;

import java.util.Arrays;

public class Shuffle {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		long[] a={1,2,3,4,5,6,7,8,9};
		System.out.println(Arrays.toString(a));
		shuffle(a);
		System.out.println(Arrays.toString(a));

	}
	
	public static void shuffle(long[] a) {
		int n1=a.length/2;
		int n2=a.length-n1;
		long[] l=new long[n1];
		long[] r=new long[n2];
		for (int i=0;i<a.length;i++) {
			if(i<n1) {
				l[i]=a[i];
			} else {
				r[i-n1]=a[i];
			}
		}
		int i=0,j=0;
		for(int k=0;k<a.length;k++) {
			int b=(int) Math.floor(Math.random()+0.5d);
			if((b==0 && i<n1) || j>=n2) {
				a[k]=l[i];
				i++;
			} else {
				a[k]=r[j];
				j++;
			}
		}
	}
	
}
