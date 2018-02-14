//author: Will Smith

class MethodOverriding {
	public static void main (String[] args) {
		System.out.println(new TestSuite().start());
	}
}

class TestSuite {
	public int start() {
		Rectangle A;
		Square B;
		int aArea;
		int aPerim;
		int bArea;
		int bPerim;
		
		A = new Rectangle();
		B= new Square();
		
		sidef(A.setHeight(5));
		sidef(A.setLength(10));
		sidef(B.setHeight(5));
		sidef(B.setLength(5));
		
		aArea = A.area();
		aPerim = A.perimeter();
		bArea = B.area();
		bPerim = B.perimeter();
		
		System.out.println("A Area: " + aArea);
		System.out.println("A Perimeter: " + aPerim);
		System.out.println("B Area: " + bArea);
		System.out.println("B Perimeter: " + bPerim);
		
		return 0;
	}
}

class Rectangle {
	int h;
	int l;
	
	public int setHeight(int h2) {
		h = h2;
		return 0;
	}
	
	public int setLength(int l1) {
		l = l1;
		return 0;
	}
	
	public int area() {
		return l * h;
	}
	
	public int perimeter() {
		return l * 2 + h * 2;
	}
}

class Square extends Rectangle {
	public int area() {
		System.out.println("I'm a square!");
		return l * h;
	}
	
	public int perimeter() {
		System.out.println("My sides are all equal");
		return l * 4;
	}
}