#include <stdio.h>
#include <sys/time.h>
#include <setjmp.h>
#include <unistd.h>
#include <signal.h>
#include "laconic.h"

jmp_buf lac_error_jmp;
char *lac_errmsg;
lreg_t lac_errlreg;

void lac_error(const char *arg, lreg_t errlr)
{
  lac_errmsg = arg;
  lac_errlreg = errlr;
  longjmp(lac_error_jmp, 1);
}

void lac_error_print(FILE *f)
{
  fprintf(f, "(*LAC-ERROR* \"%s", lac_errmsg);
  if (lac_errlreg != NIL) 
    {
      fprintf(f, ": ");
      sexpr_print(f, lac_errlreg);
    }
  fprintf(f, "\")\n");
}

void
sigint(int sig)
{
  lac_error("Interrupted", NIL);
}

#define TDIFF_SEC(_a, _b)				\
    ((_b)->tv_usec >= (_a)->tv_usec			\
     ? (_b)->tv_sec - (_a)->tv_sec			\
     : (_b)->tv_sec - (_a)->tv_sec - 1)

#define TDIFF_USEC(_a, _b)						\
    ((_b)->tv_usec >= t1.tv_usec					\
     ? (_b)->tv_usec - (_a)->tv_usec					\
     : (_b)->tv_usec + 1000000L - (_a)->tv_usec)


int repl(FILE *infd, FILE *outfd, FILE *errfd)
{
  int r;
  void *scan;
  struct timeval t1, t2;
  lreg_t res = NIL;

  if ( setjmp(lac_error_jmp) != 0 ) {
    lac_error_print(errfd);
    res = NIL;
  }

  sexpr_read_start(infd, &scan);
  do {
    if ( isatty(fileno(outfd)) )    
      fprintf(outfd, "LAC>");
    r = sexpr_read(&res, scan);

    gettimeofday(&t1, NULL);    
    res = eval(res, lac_null_env);
    gettimeofday(&t2, NULL);

    if ( isatty(fileno(outfd)) ) {
	fprintf(outfd, "=> "); 
	sexpr_print(outfd, res); 
	fprintf(outfd, "\n");
	fprintf(outfd, "Evaluation took %ld seconds and %ld microseconds.\n",
		TDIFF_SEC(&t1, &t2), TDIFF_USEC(&t1, &t2));
    }
  } while(r);
  sexpr_read_stop(scan);

  return r;
}

int main()
{

  signal(SIGINT, sigint);
  if ( setjmp(lac_error_jmp) != 0 ) {
      fprintf(stderr, "(LAC-SYSTEM \"ERROR IN SYSTEM LIBRARY: %s\")\n",
	      lac_errmsg);
      return -1;
  }
  lac_init();
  repl(stdin, stdout, stderr);
  fprintf(stdout, "\ngoodbye!\n");
  return 0;
}
