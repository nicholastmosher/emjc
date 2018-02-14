//author: Will Smith

class QuickSort {
	public static void main(String[] args) {
		System.out.println(new TestSuite().start());
	}

}

class TestSuite {
	public int start() {
		SortClass sort;
		int[] input;
		int[] results;
		int i;
		
		input = new int[10];
		sort = new SortClass();
		
		input[0] = 25;
		input[1] = 14;
		input[2] = 57;
		input[3] = 23;
		input[4] = 0;
		input[5] = 24;
		input[6] = 6;
		input[7] = 24;
		input[8] = 45;
		input[9] = 130;
		
		results = sort.sort(input);
		
		i = 0;
		while (i < 10) {
			System.out.println("I = " + i);
			System.out.println("X = " + results[i]);
			i = i + 1;
		}
		
		return 0;
	}
}

class SortClass {
	int[] array;
	int size;
	
	public int[] sort(int[] inputArray) {
		array = inputArray;
		size = inputArray.length;
		sidef(this.quickSort(0, size - 1));
		return array;
	}
	
	public int quickSort(int lowerIndex, int higherIndex) {
		int i;
		int j;
		int pivot;
		
		i = lowerIndex;
		j = higherIndex;
		
		pivot = array[lowerIndex + (higherIndex-lowerIndex)/2];
		
		while (i < (j+1)) {
			while (array[i] < pivot) {
				i = i + 1;
			}
			while (pivot < array[j]) {
				j = j - 1;
			}
			if (i < (j + 1)) {
				sidef(this.exchangeNumbers(i, j));
				i = i + 1;
				j = j - 1;
			}
		}
		
		if (lowerIndex < j) {
			sidef(this.quickSort(lowerIndex, j));
		}
		if (i < higherIndex) {
			sidef(this.quickSort(i, higherIndex));
		}
		return 0;
	}
	
	public int exchangeNumbers(int i, int j) {
		int temp;
		temp = array[i];
		array[i] = array[j];
		array[j] = temp;
		return 0;
	}
}
