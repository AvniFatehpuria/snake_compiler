#include <stdio.h>

#include "printer.h"

void printValueHelper(int x) {
  if ((x & 0x00000001) == 0) {
    // Then this is an integer.
    x /= 2;
    printf("%d", x);
  } else if ((x & 0x00000003) == 3) {
    // Then this is a boolean.
    if (x == 0x7FFFFFFF) {
      printf("false");
    } else if (x == 0xFFFFFFFF) {
      printf("true");
    } else {
      // This should never happen!  Print the hex of the value so we can debug.
      printf("UNINTERPRETABLE VALUE: %08x", x);
    }
  } else {
    // Then this is a heap pointer.
    x &= 0xFFFFFFFC;
    int* p = (int*)x;
    int size = *p;
    if ((size & 0x80000000) == 0) {
      // Then it points to a tuple.
      printf("(");
      for (int i=0;i<size;i++) {
        if (i!=0) {
          printf(", ");
        }
        printValueHelper(p[i+2]);
      }
      printf(")");
    } else {
      // Then it points to a closure
      size ^= 0x80000000;
      int max = *(p+2);
      int addr = *(p+3);
      printf("<closure@%08x>[%d/%d]", addr, size, max);
      printf("(");
      for (int i=0;i<max;i++) {
        if (i!=0) {
          printf(", ");
        }
        if (i < size) {
          printValueHelper(p[i+4]);
        } else {
          printf("?");
        }
      }
      printf(")");
    }
  }
}

void printValue(int x) {
  printValueHelper(x);
  printf("\n");
}
