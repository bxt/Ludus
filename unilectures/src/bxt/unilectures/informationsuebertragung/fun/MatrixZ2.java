package bxt.unilectures.informationsuebertragung.fun;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.Scanner;
import java.util.regex.Pattern;

public class MatrixZ2 {
	
	private boolean[][] entries;
	
	private MatrixZ2(boolean[][] entries) {
		this.entries=entries;
	}
	
	public static MatrixZ2 fromString(String s) {
		Pattern digits=Pattern.compile("[01]+");
		Scanner sc=new Scanner(s);
		Queue<boolean[]> entiresList=new LinkedList<boolean[]>();
		int preColumns=-1;
		
		while(sc.hasNext(digits)) {
			String digit=sc.next(digits);
			
			if(preColumns>=0 && preColumns!=digit.length())
				throw new IllegalArgumentException();
			preColumns=digit.length();
			
			boolean[] row=new boolean[digit.length()];
			int col=0;
			for (char c: digit.toCharArray()) {
				row[col]=c=='1'?true:false;
				col++;
			}
			entiresList.add(row);
		}
		
		boolean[][] entries=new boolean[entiresList.size()][];
		for(int i=0;!entiresList.isEmpty();i++) {
				entries[i]=entiresList.remove();
		}
		return new MatrixZ2(entries);
	}
	
	public static MatrixZ2 fromVector(List<Boolean> v) {
		boolean[][] entries=new boolean[v.size()][1];
		for(int i=0;i<v.size();i++) {
			entries[i][0]=v.get(i);
		}
		return new MatrixZ2(entries);
	}
	
	public static MatrixZ2 mult(MatrixZ2 a, MatrixZ2 b) {
		if(a.columns()!=b.rows()) throw new IllegalArgumentException();
		boolean[][] entries=new boolean[a.rows()][b.columns()];
		for(int i=0;i<a.rows();i++) {
			for(int k=0;k<b.columns();k++) {
				boolean entry=false;
				for(int l=0;l<a.columns();l++) {
					entry=( entry!=(a.entries[i][l] && b.entries[l][k]) );
				}
				entries[i][k]=entry;
			}
		}
		return new MatrixZ2(entries);
	}
	
	public static MatrixZ2 add(MatrixZ2 a, MatrixZ2 b) {
		if(a.columns()!=b.columns()) throw new IllegalArgumentException();
		if(a.rows()!=b.rows()) throw new IllegalArgumentException();
		boolean[][] entries=new boolean[a.rows()][a.columns()];
		for(int i=0;i<a.rows();i++) {
			for(int k=0;k<a.columns();k++) {
				entries[i][k]=(a.entries[i][k] != b.entries[i][k]);
			}
		}
		return new MatrixZ2(entries);
	}
	
	public static MatrixZ2 transpose(MatrixZ2 a) {
		boolean[][] entries=new boolean[a.columns()][a.rows()];
		for(int i=0;i<a.rows();i++) {
			for(int k=0;k<a.columns();k++) {
				entries[k][i]=a.entries[i][k];
			}
		}
		return new MatrixZ2(entries);
	}
	
	public static MatrixZ2 unit(int size) {
		boolean[][] entries=new boolean[size][size];
		for(int i=0;i<size;i++) entries[i][i]=true;
		return new MatrixZ2(entries);
	}
	
	public static MatrixZ2 zero(int rows,int columns) {
		boolean[][] entries=new boolean[rows][columns];
		return new MatrixZ2(entries);
	}
	
	public static MatrixZ2 catRows(MatrixZ2 a, MatrixZ2 b) {
		if(a.columns()!=b.columns()) throw new IllegalArgumentException();
		boolean[][] entries=new boolean[a.rows()+b.rows()][a.columns()];
		for(int i=0;i<a.rows()+b.rows();i++) {
			for(int k=0;k<a.columns();k++) {
				if(i<a.rows())
					entries[i][k]=a.entries[i][k];
				else
					entries[i][k]=b.entries[i-a.rows()][k];
			}
		}
		return new MatrixZ2(entries);
	}
	
	public static MatrixZ2 catColumns(MatrixZ2 a, MatrixZ2 b) {
		if(a.rows()!=b.rows()) throw new IllegalArgumentException();
		boolean[][] entries=new boolean[a.rows()][a.columns()+b.columns()];
		for(int i=0;i<a.rows();i++) {
			for(int k=0;k<a.columns();k++) {
				entries[i][k]=a.entries[i][k];
			}
			for(int k=0;k<b.columns();k++) {
				entries[i][k+a.columns()]=b.entries[i][k];
			}
		}
		return new MatrixZ2(entries);
	}
	
	public int rows() {
		return entries.length;
	}
	
	public int columns() {
		return entries.length < 1 ? 0 : entries[0].length;
	}
	
	public void inc() {
		for(int i=0;i<this.rows();i++) {
			for(int k=0;k<this.columns();k++) {
				this.entries[i][k]=!this.entries[i][k];
			}
		}
	}
	
	public void mult(boolean x) {
		for(int i=0;i<this.rows();i++) {
			for(int k=0;k<this.columns();k++) {
				this.entries[i][k]=this.entries[i][k]&&x;
			}
		}
	}
	
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		for(int i=0;i<this.rows();i++) {
			sb.append(i==this.rows()-1?'\\':(i==0?'/':'|'));
			for(int k=0;k<this.columns();k++) {
				sb.append(this.entries[i][k]?'1':'0');
			}
			sb.append(i==this.rows()-1?'/':(i==0?'\\':'|'));
			sb.append('\n');
		}
		return sb.toString();
	}
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		MatrixZ2 a= MatrixZ2.fromString("11 00 01");
		
		MatrixZ2 b= new MatrixZ2(new boolean[][]{{true},{false}});
		
		System.out.println(a);
		System.out.println(b);
		System.out.println(MatrixZ2.mult(a, b));
		MatrixZ2 c = MatrixZ2.mult(a, MatrixZ2.transpose(a));
		System.out.println(c);
		System.out.println(MatrixZ2.catRows(b, MatrixZ2.mult(a, b)));
		System.out.println(MatrixZ2.catRows(
				MatrixZ2.catColumns(c, a),
				MatrixZ2.zero(3, 5)));
		List<Boolean> v = Arrays.asList(new Boolean[]{false,true});
		MatrixZ2 d = MatrixZ2.fromVector(v);
		System.out.println(MatrixZ2.add(b, d));
			}

}
