class Main {
	// Main function
	// Serves as a unit test to evaluate fib (int n)
	public static void main (String[] args) {{
		sidef (new fibanacci ().fib_test ());
	}}
}

class fibanacci {
	public int fib (int n) {
		// An array to store the fibanacci numbers
		// Rather than repeatedly calculating them
		int[] f;
		int i;
		
		f = new int[n+1];
		i = 2;

		// Start the sequence
		f[0] = 0;
		f[1] = 1;

		while (i < n-1) {
			// Add the previous two numbers and store them in the series
			f[i] = f[i-1] + f[i-2];
			i = i + 1;
		}
		return f[n];
	}

	public int fib_test () {
		int res;
		String outStr;
		boolean passed;
		passed = true;

		// Begin testing
		res = this.fib (2);
		if (!(res == 1)) {
			passed = false;
			outStr = "Error: fib (2) Returned: " + res + " Expected: 1";
			System.out.println (outStr);
		}

		res = this.fib (3);
		if (!(res == 2)) {
			passed = false;
			outStr = "Error: fib (3) Returned: " + res + " Expected: 2";
			System.out.println (outStr);
		}

		res = this.fib (4);
		if (!(res == 3)) {
			passed = false;
			outStr = "Error: fib (4) Returned: " + res + " Expected: 3";
			System.out.println (outStr);
		}

		res = this.fib (5);
		if (!(res == 5)) {
			passed = false;
			outStr = "Error: fib (5) Returned: " + res + " Expected: 5";
			System.out.println (outStr);
		}

		res = this.fib (6);
		if (!(res == 8)) {
			passed = false;
			outStr = "Error: fib (6) Returned: " + res + " Expected: 8";
			System.out.println (outStr);
		}

		res = this.fib (7);
		if (!(res == 13)) {
			passed = false;
			outStr = "Error: fib (7) Returned: " + res + " Expected: 13";
			System.out.println (outStr);
		}

		if (passed == true) {
			System.out.println ("Fibanacci Unit Test Passed");
		}
		return 0;
	}
}
