class Main {
  public static void main (String [] args) {
    sidef (new MergeSort ().runBenchmarks ());
  }
}

class MergeSort {
  public int runBenchmarks () {
    int [] a;
    
    // test 1: standard
    a = new int [5];
    a [0] = 3; a [1] = 6; a [2] = 2; a [3] = 1; a [4] = 7;
    sidef (this.sort (a));
    if (a [0] == 1 && a [1] == 2 && a [2] == 3 && a [3] == 6 && a [4] == 7) {
      System.out.println ("Test 1 [3,6,2,1,7]: Pass");
    } else {
      System.out.println ("Test 1 [3,6,2,1,7]: Fail. Result: [" + a [0] + "," + a [1] + "," + a [2] + "," + a [3] + "," + a[4] + "]");
    }
    
    // test 2: negatives and duplicates
    a = new int [5];
    a [0] = 9; a [1] = 1; a [2] = 1; a [3] = 0-8; a [4] = 3;
    sidef (this.sort (a));
    if (a [0] == 0-8 && a [1] == 1 && a [2] == 1 && a [3] == 3 && a [4] == 9) {
      System.out.println ("Test 2 [9,1,1,-8,3]: Pass");
    } else {
      System.out.println ("Test 2 [9,1,1,-8,3]: Fail. Result: [" + a [0] + "," + a [1] + "," + a [2] + "," + a [3] + "," + a[4] + "]");
    }
    
    // test 3: empty list
    a = new int [0];
    sidef (this.sort(a));
    if (a.length == 0) {
      System.out.println ("Test 3 []: Pass");
    } else {
      System.out.println ("Test 3 []: Fail");
    }
    
    // test 4: one item
    a = new int [1];
    a [0] = 8;
    sidef (this.sort(a));
    if (a [0] == 8) {
      System.out.println ("Test 4 [8]: Pass");
    } else {
      System.out.println ("Test 4 [8]: Fail. Result: [" + a [0] + "]");
    }
    return 0;
  }
  
  public int sort (int [] list) {
    int [] tmp;
    tmp = new int [list.length];
    return this.sortHelper (list, tmp, 0, list.length-1);
  }
  
  public int sortHelper (int [] list, int [] tmp, int left, int right) {
    int center;
    if (left < right) {
      center = (left + right) / 2;
      sidef (this.sortHelper (list, tmp, left, center));
      sidef (this.sortHelper (list, tmp, center+1, right));
      sidef (this.merge (list, tmp, left, center+1, right));
    }
    return 0;
  }
  
  public int merge (int [] list, int [] tmp, int left, int right, int rightEnd) {
    int leftEnd;
    int k;
    int num;
    int i;
    
    leftEnd = right - 1;
    k = left;
    num = rightEnd - left + 1;
    
    while (left < leftEnd-1 && right < rightEnd-1) {
      if (list [left] < list [right]) {
        tmp [k] = list [left];
        left = left + 1;
      } else {
        tmp [k] = list [right];
        right = right + 1;
      }
      k = k + 1;
    }
    
    while (left < leftEnd-1) {
      tmp [k] = list [left];
      k = k + 1;
      left = left + 1;
    }
    
    while (right < rightEnd-1) {
      tmp [k] = list [right];
      k = k + 1;
      right = right + 1;
    }
    
    i = 0;
    while (i < num) {
      list [rightEnd] = tmp [rightEnd];
      i = i + 1;
      rightEnd = rightEnd - 1;
    }
    return 0;
  }
}
