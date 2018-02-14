//author: Justin Gottshall & Dhanasekar Elangovan

class PrimesMain {

    public static void main(String[] a) {
        {
            System.out.println(new Primes().getPrimesSlow());
            System.out.println(new Primes().getPrimesFast());
        }
    }
}

class Primes {
    // generate the first 1000 primes really inefficiently
    public String getPrimesSlow() {
        int[] primes; int index; String s;
        index = 0;
        s = "";
        primes = new PrimeGen().generatePrimesSlow(1000);
        while(!(primes[index] == 0)) {
            s = s + primes[index] + " ";
            index = index + 1;
        }
        return s;
    }

    // generate the first 1000 primes slightly more efficiently
    public String getPrimesFast() {
        int[] primes; int index; String s;
        index = 0;
        s = "";
        primes = new PrimeGen().generatePrimesFast(1000);
        // report the result
        while(!(primes[index] == 0)) {
            s = s + primes[index] + " ";
            index = index + 1;
        }
        return s;
    }
}

class PrimeGen {

    // generates primes up to num really inefficiently
    public int[] generatePrimesSlow(int num) {
        int[] primes;
        int primesIndex;
        int i;
        primes = new int[num];
        primes[0] = 1;
        primesIndex = 1;
        i = 2;
        while (i < num) {
            if(this.isPrimeSlow(i)) {
                primes[primesIndex] = i;
                primesIndex = primesIndex + 1;
            }
            i = i + 1;
        }
        return primes;
    }

    // generates primes up to num faster than generatePrimesSlow
    public int[] generatePrimesFast(int num) {
        int[] primes;
        int primesIndex;
        int i;
        primes = new int[num];
        primes[0] = 1;
        primesIndex = 1;
        i = 3;
        while (i < num) {
            if (this.isPrimeFast(i)) {
                primes[primesIndex] = i;
                primesIndex = primesIndex + 1;
            }
            i = i + 2;
        }
        return primes;
    }

    // checks primality of a number very inefficiently
    public boolean isPrimeSlow(int num) {
        boolean isPrime;
        int i;
        isPrime = true;
        if(new Math().isDivisible(num, 2)) isPrime = false;
        i = 2;
        while (i < num) {
            if(new Math().isDivisible(num, i)) isPrime = false;
            i = i + 1;
        }
        return isPrime;
    }

    // checks primality of a number faster than isPrimeSlow
    public boolean isPrimeFast(int num) {
        boolean returnValue;
        int sqrt;
        int i;
        returnValue = true;
        if(new Math().isDivisible(num, 2)) returnValue = false;
        else {
            sqrt = new Math().sqrtFloor(num);
            i = 3;
            while((i < sqrt + 1) && returnValue) {
                if(new Math().isDivisible(num, i)) returnValue = false;
                else i = i + 2;
            }
        }
        return returnValue;
    }
}

class Math {

    // checks if a number if divisible by another number
    public boolean isDivisible(int dividend, int divisor) {
        int quotient;
        quotient = dividend / divisor;
        return quotient * divisor == dividend;
    }

    // returns the floor of the square root of an integer
    public int sqrtFloor(int num) {
        int i;
        i = 2;
        while ((i * i) < num) {
            i = i + 1;
        }

        if(num < (i*i)) {
            i = i-1;
        }
        return i;
    }
}
