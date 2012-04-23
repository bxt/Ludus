package bxt.icosahedron.processing;


import bxt.icosahedron.geometry.Icosahedron;
import bxt.icosahedron.geometry.PositionSwitch;
import bxt.icosahedron.geometry.Vertex;
import processing.core.PApplet;
import processing.core.PVector;

public class IcosahedronRenderer extends Icosahedron implements Drawable {
	
	private int C_VERTEX_X;
	private int C_VERTEX_Y;
	private int C_VERTEX_Z;
	private int C_VERTEX_TEXT;
	private int C_RECT_FILL;
	private int C_RECT_STROKE;
	private int C_EDGE;
	private int C_INACTIVE_EDGE;
	
	private PApplet p;
	
	public IcosahedronRenderer(float size, 
			PositionSwitch positionSwitch, PApplet p) {
		
		super(size, positionSwitch);
		this.p = p;
		
		C_VERTEX_X = p.color(255, 0, 0, 170);
		C_VERTEX_Y = p.color(0, 0, 255, 170);
		C_VERTEX_Z = p.color(0, 255, 0, 170);
		C_VERTEX_TEXT = p.color(255, 255, 255, 30);
		C_RECT_FILL = p.color(255, 255, 255, 30);
		C_RECT_STROKE = p.color(255, 255, 255);
		C_EDGE = p.color(255, 170, 0);
		C_INACTIVE_EDGE = p.color(0, 170, 255, 50);
	}
	
	@Override
	public void draw() {
		
		drawVertices();
		drawGoldenRects();
		//drawEdges();
		drawRoundtrip();
		
	}
	
	private void drawVertices() {
		
		for(Vertex v : vertices) {
			p.stroke(C_VERTEX_X);
			p.line(v.getPosition().x+10, v.getPosition().y, v.getPosition().z,
					v.getPosition().x-10, v.getPosition().y, v.getPosition().z);
			
			p.stroke(C_VERTEX_Y);
			p.line(v.getPosition().x ,v.getPosition().y, v.getPosition().z+10,
					v.getPosition().x, v.getPosition().y, v.getPosition().z-10);
			
			p.stroke(C_VERTEX_Z);
			p.line(v.getPosition().x, v.getPosition().y+10, v.getPosition().z,
					v.getPosition().x, v.getPosition().y-10, v.getPosition().z);
			
			
		}
		
		float textScale = 0.3f;
		p.fill(C_VERTEX_TEXT);
		p.pushMatrix();
		p.scale(textScale);
		for(Vertex v : vertices) {
			p.text(v.getName(),
					(v.getPosition().x + 10) / textScale,
					(v.getPosition().y - 10) / textScale,
					v.getPosition().z / textScale);			
		}
		p.popMatrix();
		
	}
	
	private void drawGoldenRects() {
		p.stroke(C_RECT_STROKE);
		p.fill(C_RECT_FILL);
		for(int i = 0; i < 3; i++) {
			p.beginShape();
			for(int k = 0; k < 4; k++) {
				drawVertex(vertices[i * 4 + k]);
			}
			p.endShape(PApplet.CLOSE);
		}
	}
	
	private void drawVertex(Vertex v) {
		p.vertex(v.getPosition().x, v.getPosition().y, v.getPosition().z);
	}
	
	@SuppressWarnings("unused")
	private void drawEdges() { 
		p.stroke(C_EDGE);
		for(int i = 4; i < 8; i++) {
			for(int k = 0; k < 4; k++) {
				drawEdge(vertices[i],vertices[edges[i*4 + k]]);
			}
		}
		for(int i = 8; i < 12; i++) {
			for(int k = 0; k < 4; k += 2) {
				drawEdge(vertices[i],vertices[edges[i*4 + k]]);
			}
		}
	}
	
	private void drawRoundtrip() {
		
		int framesPerEdge = 20;
		int progress =   (p.frameCount / framesPerEdge) % roundtrip.length;
		int edgeProcess = p.frameCount % framesPerEdge;
		
		p.stroke(C_EDGE);
		for(int i = 0; i < progress; i++) {
			drawEdge(vertices[roundtrip[i]],
					vertices[roundtrip[(i+1) % roundtrip.length]]);
		}
				
		drawRoundtripEdge(vertices[roundtrip[progress]], 
				vertices[roundtrip[(progress+1) % roundtrip.length]], 
				(float)edgeProcess / framesPerEdge);
		
		p.stroke(C_INACTIVE_EDGE);
		for(int i = progress + 1; i < roundtrip.length; i++) {
			drawEdge(vertices[roundtrip[i]],
					vertices[roundtrip[(i+1) % roundtrip.length]]);
		}
		
	}
	
	private void drawEdge(Vertex a, Vertex b) {
		p.line( a.getPosition().x, a.getPosition().y, a.getPosition().z,
				b.getPosition().x, b.getPosition().y, b.getPosition().z );
	}

	private void drawRoundtripEdge(Vertex a, Vertex b, float percentage) {
		PVector end = PVector.sub(b.getPosition(), a.getPosition());
		end.mult(percentage);
		end.add(a.getPosition());
		
		p.line( a.getPosition().x, a.getPosition().y, a.getPosition().z,
				end.x, end.y, end.z );
	}

}
