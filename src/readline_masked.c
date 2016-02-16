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


#define CHARPT(x,i)	((char*)CHAR(STRING_ELT(x,i)))

static void chkIntFn(void *dummy)
{
  R_CheckUserInterrupt();
}

static int checkInterrupt()
{
  return (R_ToplevelExec(chkIntFn, NULL) == FALSE);
}



#define OS_WINDOWS (defined(__WIN32) || defined(__WIN32__) || defined(_WIN64) || defined(__WIN64) || defined(__WIN64__) || defined(__TOS_WIN__) || defined(__WINNT) || defined(__WINNT__))

#if OS_WINDOWS
#include <windows.h>
#include <conio.h>
#else
#include <termios.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#endif

SEXP readline_masked(SEXP msg)
{
  SEXP ret;
  int i=0;
  char c;
  
  Rprintf(CHARPT(msg, 0));
  
#if !(OS_WINDOWS)
  struct termios tp, old;
  tcgetattr(STDIN_FILENO, &tp);
  old = tp;
  tp.c_lflag &= ~ECHO;
  tcsetattr(0, TCSAFLUSH, &tp);
#endif
  
  for (i=0; i<MAXLEN; i++)
  {
#if OS_WINDOWS
    c = _getch();
#else
    c = fgetc(stdin);
#endif
    
    if (c == '\n' || c == '\r')
      break;
    
    if (c == '\b')
    {
      if (i == 0)
        continue;
      else
      {
        pw[i-1] = '\0';
        i -= 2;
      }
    }
    
    //FIXME not actually working...
    if (checkInterrupt())
    {
#if !(OS_WINDOWS)
      tcsetattr(0, TCSANOW, &old);
#endif
      return R_NilValue;
    }
    
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
