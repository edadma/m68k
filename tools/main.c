#include <stdio.h>
#include <stdlib.h>
#include "services.h"


void
main() {
	printf( "asdf\n" );
}



    int close(int file) {
      return -1;
    }


    #include <sys/stat.h>
    int fstat(int file, struct stat *st) {
      st->st_mode = S_IFCHR;
      return 0;
    }


int isatty(int file) {
  return 1;
}


    int lseek(int file, int ptr, int dir) {
      return 0;
    }



    int read(int file, char *ptr, int len) {
      return 0;
    }



    int write(int file, char *ptr, int len) {
      int todo;

      for (todo = 0; todo < len; todo++) {
        outc (*ptr++);
      }
      return len;
    }



    caddr_t sbrk(int incr) {
      extern char _end;		/* Defined by the linker */
      static char *heap_end;
      char *prev_heap_end;
        register char *stack_ptr __asm ("sp");

      if (heap_end == 0) {
        heap_end = &_end;
      }
      prev_heap_end = heap_end;
      if (heap_end + incr > stack_ptr) {
        write (1, "Heap and stack collision\n", 25);
        abort ();
      }

      heap_end += incr;
      return (caddr_t) prev_heap_end;
    }

void
_exit( int status ) {
    halt();
}

int getpid(void) {
  return 1;
}



    #include <errno.h>
    #undef errno
    extern int errno;
    int kill(int pid, int sig) {
      errno = EINVAL;
      return -1;
    }



