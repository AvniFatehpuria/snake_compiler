#ifndef _GC_H_
#define _GC_H_

/**
 * This function performs a mark-and-compact garbage collection pass on a heap.
 * Upon completion, the heap cursor is updated to point to the new start of free
 * heap memory.
 *
 * @param desired_free The minimum number of bytes which should be free after
 *                     garbage collection.  If the number of free bytes after
 *                     garbage collection is less than this number the program
 *                     will halt.  When this function returns, at least this
 *                     many bytes are guaranteed to be free.
 */
void gc(int desired_free) asm("gc");


//bool is_heap_pointer(int possible_heap_pointer);
#endif
