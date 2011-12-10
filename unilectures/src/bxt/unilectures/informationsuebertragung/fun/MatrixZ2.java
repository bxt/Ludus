package bxt.unilectures.informationsuebertragung.fun;

import java.util.ArrayList;
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
	
	public void gaussJordan() {
		boolean[][] a=this.entries;
		
		int row=0,col=0;
		while (row<this.rows()&&col<this.columns()) {
			
			int row1=row;
			while(row1<this.rows() && !a[row1][col]) row1++;
			if(!(row1<this.rows())) {
				col++;
				continue;
			}
			swapRows(row1, row);
			
			for(int i=0;i<this.rows();i++) {
				if(i!=row && a[i][col]) {
					for(int j=col;j<this.columns();j++) {
						a[i][j]=a[i][j]!=a[row][j];
					}
				}
			}
			
			row++;col++;
		}
	}
	
	private void swapRows(int a, int b) {
		boolean[] tmp=this.entries[a];
		this.entries[a]=this.entries[b];
		this.entries[b]=tmp;
	}
	
	public MatrixZ2 grepColumn(int col) {
		boolean[][] entries=new boolean[this.rows()][1];
		for(int i=0;i<this.rows();i++) {
				entries[i][0]=this.entries[i][col];
		}
		return new MatrixZ2(entries);
	}
	
	public MatrixZ2 grepRow(int row) {
		boolean[][] entries=new boolean[1][this.columns()];
		for(int i=0;i<this.columns();i++) {
				entries[0][i]=this.entries[row][i];
		}
		return new MatrixZ2(entries);
	}
	
	public MatrixZ2 grepColumns(int from, int to) {
		boolean[][] entries=new boolean[this.rows()][to-from];
		for(int col=from;col<to;col++) {
			for(int i=0;i<this.rows();i++) {
					entries[i][col-from]=this.entries[i][col];
			}
		}
		return new MatrixZ2(entries);
	}
	
	public MatrixZ2 grepRows(int from, int to) {
		boolean[][] entries=new boolean[to-from][this.columns()];
		for(int row=from;row<to;row++) {
			for(int i=0;i<this.columns();i++) {
					entries[row-from][i]=this.entries[row][i];
			}
		}
		return new MatrixZ2(entries);
	}
	
	public List<Boolean> toBitvector() {
		if(this.rows()==1) {
			List<Boolean> vector=new ArrayList<Boolean>(this.columns());
			for(int i=0;i<this.columns();i++)
			{
				vector.add(this.entries[0][i]);
			}
			return vector;
		}
		if(this.columns()==1) {
			List<Boolean> vector=new ArrayList<Boolean>(this.rows());
			for(int i=0;i<this.rows();i++)
				vector.add(this.entries[i][0]);
			return vector;
		}
		return null;
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
	
	public boolean equals(MatrixZ2 other) {
		if(!(other.columns()==this.columns())) return false;
		if(!(other.rows()==this.rows())) return false;		
		for(int i=0;i<this.rows();i++) {
			for(int k=0;k<this.columns();k++) {
				if(!this.entries[i][k]==other.entries[i][k])
					return false;
			}
		}
		return true;
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
		
		MatrixZ2 i= MatrixZ2.fromString("1111 0100 1101");
		System.out.println(i);
		i.gaussJordan();
		System.out.println(i);
		
		System.out.println(i.grepColumn(1));
		System.out.println(i.grepRow(2));
	}

}
