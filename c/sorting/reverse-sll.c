
//
// Copyright (c) 2010, Justin Grant <justin at imagine27 dot com>
// All rights reserved.

// Redistribution and use in source and binary forms, with or without modification
// are permitted provided that the following conditions are met:

// Redistributions of source code must retain the above copyright notice, this list
// of conditions and the following disclaimer.
// Redistributions in binary form must reproduce the above copyright notice, this
// list of conditions and the following disclaimer in the documentation and/or
// other materials provided with the distribution.
// Neither the name of the <ORGANIZATION> nor the names of its contributors may be
// used to endorse or promote products derived from this software without specific
// prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
// ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE
// EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//


#include <stdio.h>
#include <stdlib.h>
#include <time.h>


#define LIST_SIZE 3
#define SHOW_SIZE 5


typedef struct node {
  struct node* next;
  int   val;
} node; 


int main(int argc, char* argv[]) 
{
  int i;
  long start, end;
  node* head = malloc(sizeof(node));
  node* cur = head; node* cur2 = head;
  node* prev = NULL; node* next = NULL;

  // init linked list
  printf("         linked list : ");
  for (i=1; i <= LIST_SIZE; i++)
  {
    cur->val = i;
    if (i <= SHOW_SIZE) printf("%d -> ", cur->val);
    if (i != LIST_SIZE) cur->next = malloc(sizeof(node));
    cur = cur->next;
  }
  cur = NULL;
  printf(" ... (%d more)\n", LIST_SIZE - SHOW_SIZE);

  // reverse linked list
  start = clock();
  /* while (cur2 != NULL) */
  /* { */
  /*   next = cur2->next; */
  /*   cur2->next = prev; */
  /*   prev = cur2; */
  /*   cur2 = next; */
  /* } */
  /* node* head2 = prev; */

  /* node* previous = head; */
  /* node* current = head->next; */
  /* previous->next = NULL; */
  /* next = current->next; */
    
  /* while(current != NULL){ */
  /*   current->next = previous; */
  /*   previous = current; */
  /*   current = next; */
  /*   if(current != NULL) */
  /*     next = current>next; */
  /* } */
  /* head = previous; */

  /* prev = head; */
  /* node* curr = head->next; */
  /* head->next = NULL; */
  /* next = NULL; */
  /* while(curr != NULL) { */
  /*   next = curr->next; */
  /*   curr->next = prev; */
  /*   prev = curr; */
  /*   curr = next; */
  /* } */
  /* head = prev; */

  prev = NULL;
  while(head!= NULL)
  {
    next = head->next;
    //switch
    head->next = prev;
    // move pointer
    prev = head;
    head = next;
  }
  head = prev;

  end = clock();

  printf("reversed linked list : ");
  for (i=1; i <= LIST_SIZE; i++)
  {
    if (i <= SHOW_SIZE) printf("%d -> ", head->val);
    head = head->next;
  }
  printf(" ... (%d more)\n", LIST_SIZE - SHOW_SIZE);

  printf("Elapsed time: %.3lf msecs\n", 
	 (((double)(end - start)) / CLOCKS_PER_SEC) * 1000);
}
