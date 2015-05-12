#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <ifaddrs.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>


#define HOME "127.0.0.1"

SEXP pbdcs_getip()
{
  SEXP ip;
  struct ifaddrs *tmp, *ifap;
  struct sockaddr_in *pAddr;
  char *addr;
  
  getifaddrs(&ifap);
  tmp = ifap;
  
  while (tmp)
  {
    if (tmp->ifa_addr && tmp->ifa_addr->sa_family == AF_INET)
    {
      pAddr = (struct sockaddr_in *) tmp->ifa_addr;
      
      addr = inet_ntoa(pAddr->sin_addr);
      
      if (strncmp(addr, HOME, strlen(HOME)) != 0)
      {
        PROTECT(ip = allocVector(STRSXP, 1));
        SET_STRING_ELT(ip, 0, mkChar(addr));
        free(ifap);
        UNPROTECT(1);
        return ip;
      }
    }
    
    tmp = tmp->ifa_next;
  }
  
  freeifaddrs(ifap);
  
  return R_NilValue;
}


