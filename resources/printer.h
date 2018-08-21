#ifndef _PRINTER_H_
#define _PRINTER_H_

/**
 * This function interprets a value by examining its bit layout.  It then prints
 * an appropriate representation of it.
 */
void printValue(int x) asm("printValue");

#endif
