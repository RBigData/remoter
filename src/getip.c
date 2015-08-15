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
#include <sys/ioctl.h>
#include <net/if.h>


#define LOCALHOST "127."

// FIXME this SHOULD be in net/if.h, but doesn't get included for some insane reason
#ifndef IFF_LOOPBACK
#define IFF_LOOPBACK 0 // skip if undefined
#endif

// hope they don't do something weird lol
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
      
      if (strcmp(tmp->ifa_name, "lo") != 0  && 
          strcmp(addr, LOCALHOST)     != 0  && 
          !(tmp->ifa_flags & IFF_LOOPBACK)  )
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


