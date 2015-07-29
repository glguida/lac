#include <stdio.h>
#include <sys/time.h>
#include <stdlib.h>
#include <setjmp.h>
#include <unistd.h>
#include <signal.h>
#include "laconic.h"

lenv_t *null_env;

void lac_error_print(FILE *f)
{
  fprintf(f, "(*LAC-ERROR* \"%s", lac_errmsg());
  if (lac_errlreg() != NIL) 
    {
      fprintf(f, ": ");
      sexpr_print(f, lac_errlreg());
    }
  fprintf(f, "\")\n");
}

void
sigint(int sig)
{
  raise_exception("Interrupted", NIL);
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
  lreg_t res = NIL;
  struct timeval t1, t2;

 restart:
  lac_on_error({
    lac_error_print(errfd);
    sexpr_read_stop(scan);
    res = NIL;
    goto restart;
  });

  sexpr_read_start(infd, &scan);
  do {
    if ( isatty(fileno(outfd)) )    
      fprintf(outfd, "LAC>");
    r = sexpr_read(&res, scan);

    gettimeofday(&t1, NULL);
    res = eval(res, null_env);
    gettimeofday(&t2, NULL);

    if ( isatty(fileno(outfd)) ) {
	fprintf(outfd, "=> "); 
	sexpr_print(outfd, res); 
	fprintf(outfd, "\n");
	fprintf(outfd,
		"Evaluation took %ld seconds and %ld microseconds.\n",
		TDIFF_SEC(&t1, &t2), TDIFF_USEC(&t1, &t2));
    }
  } while(r);
  sexpr_read_stop(scan);

  lac_off_error();
  return r;
}

int main()
{

  signal(SIGINT, sigint);

  null_env = lac_init();
  repl(stdin, stdout, stderr);
  fprintf(stdout, "\ngoodbye!\n");
  return 0;
}
