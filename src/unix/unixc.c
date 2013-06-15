#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

int geterrno()
{
  return errno;
}

int argumentcount(const char * arguments)
{
  if(arguments==0)
    {
      return 0;
    }

  const char * p = arguments;
  int Empty      = 1;
  int Count = 0;

  while(*p!=0)
    {
      if(*p==32)
	{
	  Empty=1;
	}
      else
	{
	  if(Empty)
	    {
	      Count++;
	      Empty=0;
	    }
	}
      p++;
    }

//  printf("StringCount: %i\n",Count);

  return Count;

}

char ** argumentsplit(const char * arguments)
{
  int count=argumentcount(arguments);

  char ** result=malloc((count+1)*sizeof(char *));
//  printf("ARGUMENTARRAY %p\n",result);

  const char * p     = arguments;
  const char * start = arguments;
  char ** entry=result;
  int Empty=1;

  while(*p!=0) // As long as there are any characters available, read
    {
      if(*p==32) // Handle "Space"
	{
	  if(!Empty)
	    {
	      char * part=malloc((p-start+1)*sizeof(char));
//	      printf("Length of String Part : %i\n",(p-start));
	      strncpy(part,start,(p-start));
	      part[p-start]='\0';
//	      printf("Part %s\n",part);

	      *entry = part;
//	      printf("Addr -> %p , %p\n",*entry,entry);
	      entry += 1;

	      Empty = 1;
	    }
	  start=p+1;
	}
      else
	{
	  Empty=0;
	}
      p++;
    }
  if(!Empty)
    {
      char * part=malloc((p-start+1)*sizeof(char));
      strncpy(part,start,(p-start));
      part[p-start]='\0';
      *entry=part;
      entry+=1;
//      printf("Length of String Part : %i\n",(p-start));
//      printf("Part %s\n",part);
    }

  *entry=0; // Terminate argument list

  return result;

}

void freearguments(char ** splitargs)
{
  char ** partp=splitargs;
  while(*partp!=0)
    {
//      printf("Free part %p\n",*partp);
      free(*partp);
      partp+=1;
    }
//  printf("Free %p\n",splitargs);
  free(splitargs);
}

void debugarguments(char ** splitargs)
{
  printf("Split Arguments %p:\n",splitargs);
  while(*splitargs!=0)
    {
      printf("[%s]\n",*splitargs);
      splitargs+=1;
    }
  printf("//////////////////////////\n");
}

int _exec(const char * program,const char * arguments)
{
  printf("_exec %p %p\n",program,arguments);
  char ** splitargs=argumentsplit(arguments);
//  debugarguments(splitargs);
  printf("Program %s\n",program);
  int result=execv(program,splitargs);
  printf("Failed..");
  freearguments(splitargs);
  return result;
}

void SetNonBlocking(int file)
{
  int flags=fcntl(file,F_GETFL,0);
  if (flags=-1)
  {
    flags=0;
  }
  fcntl(file,F_SETFL,flags | O_NONBLOCK);
}
