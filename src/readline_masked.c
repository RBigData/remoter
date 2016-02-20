/*  Copyright (c) 2016, Schmidt
    All rights reserved.
    
    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are met:
    
    1. Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
    
    2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
    
    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
    TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
    EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
    PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
    PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


#include <R.h>
#include <Rinternals.h>

// if you have passwords more than 200 chars, you have mental problems
#define MAXLEN 200
char pw[MAXLEN];
int ctrlc;


#define CHARPT(x,i)	((char*)CHAR(STRING_ELT(x,i)))



#define OS_WINDOWS (defined(__WIN32) || defined(__WIN32__) || defined(_WIN64) || defined(__WIN64) || defined(__WIN64__) || defined(__TOS_WIN__) || defined(__WINNT) || defined(__WINNT__))
#define OS_LINUX (defined(__gnu_linux__) || defined(__linux__) || defined(__linux))

#if OS_WINDOWS
#include <windows.h>
#include <conio.h>
#else
#include <stdio.h>
#include <stdlib.h>
#include <termios.h>
#include <unistd.h>
#include <signal.h>

static void ctrlc_handler(int signal)
{
  ctrlc = 1;
}

#endif


SEXP readline_masked(SEXP msg)
{
  SEXP ret;
  int i=0;
  char c;
  ctrlc = 0;
  
  Rprintf(CHARPT(msg, 0));
  
#if !(OS_WINDOWS)
  struct termios tp, old;
  tcgetattr(STDIN_FILENO, &tp);
  old = tp;
  tp.c_lflag &= ~ECHO;
  tcsetattr(0, TCSAFLUSH, &tp);
  #if OS_LINUX
  signal(SIGINT, ctrlc_handler); 
  #else
  struct sigaction sa;
  sa.sa_handler = ctrlc_handler;
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = 0;
  sigaction(SIGINT, &sa, NULL);
  #endif
#endif
  
  for (i=0; i<MAXLEN; i++)
  {
#if OS_WINDOWS
    c = _getch();
#else
    c = fgetc(stdin);
#endif
    
    // newline
    if (c == '\n' || c == '\r')
      break;
    // backspace
    else if (c == '\b')
    {
      if (i == 0)
      {
        i--;
        continue;
      }
      else
      {
        pw[--i] = '\0';
        i--;
      }
    }
    // C-c
    else if (ctrlc == 1 || c == 3 || c == '\xff')
    {
#if !(OS_WINDOWS)
      tcsetattr(0, TCSANOW, &old);
#endif
      Rprintf("\n");
      return R_NilValue;
    }
    // store value
    else
      pw[i] = c;
  }
  
#if !(OS_WINDOWS)
  tcsetattr(0, TCSANOW, &old);
#endif
  
  Rprintf("\n");
  PROTECT(ret = allocVector(STRSXP, 1));
  SET_STRING_ELT(ret, 0, mkCharLen(pw, i));
  UNPROTECT(1);
  
  return ret;
}
