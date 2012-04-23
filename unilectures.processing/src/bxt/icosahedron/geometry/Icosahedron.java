package bxt.icosahedron.geometry;

import java.util.Arrays;

import processing.core.PVector;

public class Icosahedron {

	protected Vertex[] vertices;
	protected int[] edges;
	protected int[] roundtrip;

	public Icosahedron(float size, PositionSwitch positionSwitch) {
		
		float phi = (float) (1.0+Math.sqrt(5)) / 2.0f;
		float sizeBig = size * phi;
		float sizeSqrt = size * (float) Math.sqrt(phi);
		
		//float[] vertexData = new float[]{};
		
		vertices = new Vertex[]{
				new Vertex("00",new PVector[]{
						new PVector( sizeBig, 0, size),
						new PVector( 0, 0, size),
						new PVector( sizeSqrt, 0, sizeSqrt),
						},positionSwitch),
				new Vertex("01",new PVector[]{
						new PVector(-sizeBig, 0, size),
						new PVector(-size/2, 0, size*3),
						new PVector(-sizeSqrt, 0, sizeSqrt),
						},positionSwitch),
				new Vertex("02",new PVector[]{
						new PVector(-sizeBig, 0,-size),
						new PVector(-size/2, 0,-size*3),
						new PVector(-sizeSqrt, 0,-sizeSqrt),
						},positionSwitch),
				new Vertex("03",new PVector[]{
						new PVector( sizeBig, 0,-size),
						new PVector( 0, 0,-size),
						new PVector( sizeSqrt, 0,-sizeSqrt),
						},positionSwitch),
				
				new Vertex("04",new PVector[]{
						new PVector( size, sizeBig, 0),
						new PVector( 0, size, 0),
						new PVector( sizeSqrt, sizeSqrt, 0),
						},positionSwitch),
				new Vertex("05",new PVector[]{
						new PVector( size,-sizeBig, 0),
						new PVector( 0,-size, 0),
						new PVector( sizeSqrt,-sizeSqrt, 0),
						},positionSwitch),
				new Vertex("06",new PVector[]{
						new PVector(-size,-sizeBig, 0),
						new PVector(-size/2,-sizeBig-size, 0),
						new PVector(-sizeSqrt,-sizeSqrt, 0),
						},positionSwitch),
				new Vertex("07",new PVector[]{
						new PVector(-size, sizeBig, 0),
						new PVector(-size/2, sizeBig+size, 0),
						new PVector(-sizeSqrt, sizeSqrt, 0),
						},positionSwitch),
				
				new Vertex("08",new PVector[]{
						new PVector( 0, size, sizeBig),
						new PVector(-size/4, size, size),
						new PVector( 0, sizeSqrt, sizeSqrt),
						},positionSwitch),
				new Vertex("09",new PVector[]{
						new PVector( 0, size,-sizeBig),
						new PVector(-size/4, size,-size),
						new PVector( 0, sizeSqrt,-sizeSqrt),
						},positionSwitch),
				new Vertex("10",new PVector[]{
						new PVector( 0,-size,-sizeBig),
						new PVector(-size/4,-size,-size),
						new PVector( 0,-sizeSqrt,-sizeSqrt),
						},positionSwitch),
				new Vertex("11",new PVector[]{
						new PVector( 0,-size, sizeBig),
						new PVector(-size/4,-size, size),
						new PVector( 0,-sizeSqrt, sizeSqrt),
						},positionSwitch),
		};
		
		// zeile: vertex, spalten 0,1 und 2,3, von/nach vtxs
		edges = new int[]{
				4, 8,  5,11, // 00
				6,11,  7, 8, // 01
				6,10,  7, 9, // 02
				4, 9,  5,10, // 03
				0, 8,  3, 9, // 04
				0,11,  3,10, // 05
				1,11,  2,10, // 06
				1, 8,  2, 9, // 07
				0, 4,  1, 7, // 08
				2, 7,  3, 4, // 09
				2, 6,  3, 5, // 10
				0, 5,  1, 6, // 11
		};
		
		findRoundtrip(4,0);
		
	}

	private void findRoundtrip(int startFrom, int start) {
		
		int matches = 0;
		int space = 1 << (vertices.length+1);
		int[] positions = new int[0];
		
		searching:for(int s = 0; s < space; s++) {
			
			boolean[] secondary = new boolean[vertices.length];
			for(int i=0;i<vertices.length;i++) {
				secondary[i] = (s & (1<<i)) > 0;
			}
			
			int progress = 0;
			positions = new int[vertices.length*2 + 2];
			positions[0] = startFrom;
			positions[1] = start;
			int[] visitCounts = new int[vertices.length];
			
			while(progress < positions.length - 2) {
				
				int pre = progress;
				int cur = progress + 1;
				int next= progress + 2;
				
				positions[next] = walkEdge(positions[pre],
						positions[cur],
						secondary[positions[cur]]);
				
				if(visitCounts[positions[next]] > 2) {
					continue searching;
				} else {
					visitCounts[positions[next]]++;
					progress++;
				}
			}
			for(int i = 0; i < visitCounts.length; i++) {
				if(visitCounts[i] != 2) {
					continue searching;
				}
			}
			
			/*
			for(int i = 0; i < secondary.length; i++) {
				System.out.print(secondary[i] ? '1' : '0');
			}
			System.out.print(": ");
			
			System.out.println(Arrays.toString(positions));
			*/
			
			matches++;
			if(matches > 200)
				break searching;
		}
		
		roundtrip = Arrays.copyOf(positions, vertices.length * 2);
		//System.out.println("Roundtrips: " + matches + "/" + space);
	}

	private int walkEdge(int from, int over, boolean secondary) {
		if(!secondary) {
			if(from == edges[over*4 + 0]) return edges[over*4 + 2];
			if(from == edges[over*4 + 1]) return edges[over*4 + 3];
			if(from == edges[over*4 + 2]) return edges[over*4 + 0];
			if(from == edges[over*4 + 3]) return edges[over*4 + 1];
			throw new IllegalStateException(
					"From edge " + from + " not in adj. list of " + over + ".");
		} else {
			if(from == edges[over*4 + 0]) return edges[over*4 + 3];
			if(from == edges[over*4 + 1]) return edges[over*4 + 2];
			if(from == edges[over*4 + 2]) return edges[over*4 + 1];
			if(from == edges[over*4 + 3]) return edges[over*4 + 0];
			throw new IllegalStateException(
					"From edge " + from + " not in adj. list of " + over + ".");
		}
	}

}