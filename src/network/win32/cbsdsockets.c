#include <winsock2.h>

int WinSetNonBlocking(int Socket)
{
   unsigned long v=1;
   return ioctlsocket(Socket,FIONBIO,&v);
}
