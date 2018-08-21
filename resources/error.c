#include <stdlib.h>

#include "error.h"

void stopWithError(int type) {
  switch (type) {
    case 1:
    printf("Expected an int.\n");
    break;
    /* TODO: put your other error cases here */
    case 2:
    printf("Expected a bool.\n");
    break;
    case 3:
    printf("Operation caused overflow.\n");
    break;
    case 4:
    printf("Accessed a non-tuple value by index\n");
    break;
    case 5:
    printf("Tuple index out of range\n");
    break;
    case 6:
    printf("Expected a closure\n");
    case 7:
    printf("out of memory\n");
    default:
    printf("Unknown error %d occurred.\n", type);
    break;
  }
  exit(type);
}
