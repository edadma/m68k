#include <stdint.h>

extern void outln();
extern void outc( char c );
extern void outf( double a );
extern void outs( char* s );
extern void outsln( char* s );
extern void outn( int n );
extern void outnln( int n );
extern void outl( int64_t n );
extern void halt() __attribute__ ((noreturn));
extern int64_t currentTime();
