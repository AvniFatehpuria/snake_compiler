#include <stdio.h>
#include "printer.h"

int snake_main(int* ptr_to_heap, int* end_of_heap) asm("snake_main");

int main(int argc, char** argv) {
  int* ptr_to_heap = malloc(sizeof(int)*1000000);
  int* end_of_heap = (ptr_to_heap + 1000000 );
  int result = snake_main(ptr_to_heap, end_of_heap);
  printValue(result);
  return 0;
}
