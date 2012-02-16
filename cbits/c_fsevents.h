#ifndef CBITS_C_FSEVENTS_H
#define CBITS_C_FSEVENTS_H 1

#include <CoreServices/CoreServices.h>

typedef struct {
  FSEventStreamRef eventStream;
  CFRunLoopRef runLoop;
  int writefd;
} watch;


int createWatch ( char** folders
                , int n
                , UInt32 createFlags
                , UInt64 since
                , double latency
                , int* fd
                , void** wp
                );

int destroyWatch(watch* w);

#endif

