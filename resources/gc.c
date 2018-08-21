

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"

// These extern declarations allow us to refer to the variables you made global
// in your generated assembly file.

extern int* start_of_stack;
extern int* end_of_stack;
extern int* start_of_heap;
extern int* end_of_heap;
extern int* heap_cursor;

/*
  The following macros allow you to use "debugf" in place of "printf" for
  formatted debugging.  For instance, you can write
      debugf("Pointer %p changed to pointer %p.\n", old, new);
  to print in the same way you would use printf.  The advantage is that,
  depending upon which of the two macros you are using, the debugf call either
  turns into a printf call or is erased entirely.  Use debugf to print-debug
  your garbage collector and then disable printing when you need to perform your
  unit tests.
*/

// This macro disables all debugf statements.  (They become no-ops.)
#define debugf(fmt, ...) ;

// This macro enables all debugf statements.  (They become printf statements.)
// #define debugf(fmt, ...) printf(fmt, ##__VA_ARGS__); fflush(stdout)


/**
 * This helper function will show the entire contents of your heap.  This output
 * can get very large if your heap is too big!
 */
void dump_heap() {
  debugf("HEAP:\n");
  int c = 0;
  for (int* p = (int*)((int)start_of_heap & 0xFFFFFFF0);
       p < end_of_heap; p += 1) {
    if (c==0) {
      debugf("%08x:", p);
    }
    if (p >= start_of_heap) {
      debugf("    %08x", *p);
    } else {
      debugf("            ");
    }
    c++;
    if (c==8) {
      debugf("\n");
      c=0;
    }
  }
  if (c!=0) {
    debugf("\n");
  }
}


 int get_size( int strt_pt) {
  //debugf("strt_pt = %08x\n", strt_pt);
  int size = 0;
  if ((strt_pt & 0x80000000) == 0x80000000) {

    size =  ((strt_pt - 0x80000000) + 4);
  }
  else {
    size = (strt_pt +  2);
  }
  //debugf("size = %08x\n", size);
  return size;
}

/* This helper function will take in an int and tell you if it's a heap pointer*/
 int is_heap_pointer ( int possible_heap_pointer){
   int poss_heap_ptr = possible_heap_pointer;
   int poss_heap_ptr_anded_with_3 = (poss_heap_ptr&0x00000003);
   int  possible_heap_pointer_anded_with_3 = /*(int*)*/ poss_heap_ptr_anded_with_3;
  //debugf("THING: %08x  DEREF THING: %08x\n", possible_heap_pointer, *possible_heap_pointer);
// debugf("trying to see if %08x is a heap pointer, mates. I anded the thing and it's %08x\n", possible_heap_pointer, possible_heap_pointer_anded_with_3);
  if (poss_heap_ptr_anded_with_3==1) {
    //debugf("I have found a pointer of some sort\n");
    //debugf("possible_heap_pointer: %08x start_of_heap: %08x heap_cursor: %08x\n", possible_heap_pointer, start_of_heap, heap_cursor);
    if (poss_heap_ptr >= start_of_heap) {

    if (poss_heap_ptr < heap_cursor){
      //debugf("it's a heap pointer, names\n");
      return 1;
    }
    }
  }
  return 0;
}

void mark_heap( int* start_point) {
  //debugf("I made it to mark_heap\n");
  if(*(start_point+1) == 0x00000001) {
    //we've been here, yo! dont do that shit! it's an evil cycle u will loop forever and maybe definitely seg fault :OOOOOOOOOOOOOO000000000000000
    //we're doing u a favour, promise. you'll thank us. or you wont, bc u'll never know what could have gone wrong, bc we saved u. ok bye.
    return;
  }
  else{
   int strt_pt = *start_point;

  //debugf("start_point = %08x, strt_pt = %08x \n", start_point, strt_pt);
   int size = get_size(strt_pt);
   //debugf("I think this object is %08x big", size);


//  debugf("Start point: %08x Start point + 4: %08x Start point deref: %08x Start point + 4 deref %08x \n", start_point, start_point+4, *start_point, *(start_point+4));
  *(start_point+1) = 0x00000001;
  //*gc_word = 1;
   int* current_word;
//  debugf("strt_pt = %08x, size = %08x\n", strt_pt, size);
  for(int i = 0; i<size; i++){
    current_word = (start_point+i);
    //debugf("current word = %08x\n", current_word);
    if(is_heap_pointer(*current_word)){
    //  debugf("GUYS IM GONNA RECURSIVELY CALL MARK HEAP!!!!\n");
     mark_heap((*current_word)-1);
    }
  }
}
}

void mark(){
  //debugf("marking things on the stack, buds\n");
  //debugf("Here's what I think the start of stack is:%08x\n", start_of_stack);
   int* current_pos_on_stack = start_of_stack;
  while(current_pos_on_stack>=end_of_stack){
    // int current_pos = ( int) current_pos_on_stack;
  //  current_pos -= 1;
    // int * current_pos_ptr = ( int *) current_pos;
    //debugf("i'm gonna loop now, pals\n");
    //debugf("current_pos_on_stack: %08x *cpos: %08x, cpos-1: %08x, *cpos-1: %08x\n", current_pos_on_stack, *current_pos_on_stack, current_pos_on_stack-1, (*current_pos_on_stack)-1);
    if(is_heap_pointer((*current_pos_on_stack))){
    // debugf("found a heap pointer, dudes\n");
      //debugf("Found a pointer to the heap on the stack, friends\n");
      mark_heap((*current_pos_on_stack)-1);
    }

    current_pos_on_stack-=1;
  }
}

void forward() {
   int* next_heap_object = start_of_heap;
   int* next_live_destination = start_of_heap;
  while (next_heap_object<heap_cursor){
     int size = get_size(*next_heap_object);
    //debugf("I think this object is %d big\n", size);
    if (*(next_heap_object + 1) == 1) {
      *(next_heap_object + 1) = next_live_destination;
      next_live_destination += size;
    }
    next_heap_object += size;
  }
}
/*this helper function takes in snake pointers and updates them to where they will ened to point to I promise I'll make a better comment someday*/
void update_pointer(int * ptr_location) {

  int * machine_ptr = (*ptr_location) -1;
  *ptr_location = *(machine_ptr + 1) + 1;
}
void update_stack() {
  int * current_pos_on_stack = start_of_stack;
  while(current_pos_on_stack>=end_of_stack){
    if(is_heap_pointer(*current_pos_on_stack)) {
      update_pointer(current_pos_on_stack);
    }
    current_pos_on_stack-=1;
  }
}
void update_heap() {
  int* next_heap_object = start_of_heap;
  while (next_heap_object<heap_cursor){
    if(is_heap_pointer(*next_heap_object)) {
      update_pointer(next_heap_object);
    }
    next_heap_object+=1;
  }
}
void update() {
  update_stack();
  update_heap();
}
int is_reachable(int * start_object) {
  if(*(start_object+1)==0){
    return 0;
  }
  return 1;
}
void compact() {
  int* current_pos_on_heap = start_of_heap;
  int* new_heap_cursor = start_of_heap;
  while (current_pos_on_heap<heap_cursor){
     int size = get_size(*current_pos_on_heap);
     if (is_reachable(current_pos_on_heap)){
       memmove(*(current_pos_on_heap+1), current_pos_on_heap, sizeof(unsigned int)*size);
       new_heap_cursor = new_heap_cursor + size;
     }
     current_pos_on_heap = current_pos_on_heap + size;
  }
  heap_cursor = new_heap_cursor;
  while(new_heap_cursor < end_of_heap){
    *new_heap_cursor = 0xba1ddad5;
    new_heap_cursor++;
  }
}
void remark() {
  int * current_pos_on_heap = start_of_heap;
  while (current_pos_on_heap<heap_cursor){
     int size = get_size(*current_pos_on_heap);
     *(current_pos_on_heap+1)=0;
     current_pos_on_heap = current_pos_on_heap + size;
  }
}

void gc(int desired_free) {
  // TODO: replace the following line with an implementation of the mark-compact
  //       garbage collection algorithm.  (You *will* need helper functions!)
  debugf("og heap_cursor: %08x\n", heap_cursor);
  debugf("before gc:\n");
  dump_heap();
  mark();
  debugf("after mark\n");
  dump_heap();
  forward();
  debugf("after forward\n");
  dump_heap();
  update();
  debugf("after update\n");
  dump_heap();
  compact();
  debugf("after compact\n");
  dump_heap();
  remark();
  debugf("after remark\n");
  dump_heap();
  debugf("desired free: %08x, heap_cursor: %08x, free space: %08x, end_of_heap: %08x\n", desired_free, heap_cursor, (end_of_heap-heap_cursor), end_of_heap);
  if (((int)end_of_heap) - ((int)heap_cursor) < desired_free) {
    stopWithError(7);
  }
}
