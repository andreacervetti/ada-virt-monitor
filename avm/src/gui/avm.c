/*
 *               Copyright (C) 2018 Homebrew Internet s.r.l.
 *                  Copyright (C) 2018 Andrea Cervetti
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/* We have to detach the program from the controlling terminal in order
   to have openSSH to call ssh-askpass, but calling fork() after the Ada
   runtime performed initialition can have side effect on a multitasking
   program, so we call fork() from C and then we call the main Ada program */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>

extern void adainit (void);
extern void adafinal (void);
extern void adamain (void);

int main (int argc, char **argv, char **envp)
{
  extern int    gnat_argc;
  extern char **gnat_argv;
  extern char **gnat_envp;
  gnat_argc = argc;
  gnat_argv = argv;
  gnat_envp = envp;

  int i,n = getdtablesize();

  for (i=0; i<n; i++)
    /* close all files except standard error */
    if (i!=2)
      (void) close(i);

  switch (fork()) {
    case -1:
      perror("detach fork");
      exit(EXIT_FAILURE);
    case 0:
      if(setsid() == -1) {
	perror("setsid");
	exit(EXIT_FAILURE);
      }
      adainit();
      adamain();
      adafinal();
      break;
    default:
      exit(EXIT_SUCCESS);
  }
}
