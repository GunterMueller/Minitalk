
/*
 * MINITALK.C	main program
 */


#include "minitalk.h"


#include <unistd.h>		/* to get prototype for access() */

#ifdef DOS_STANDARD
#include <alloc.h>	/* to get prototypes for coreleft(), farcoreleft() */
#endif


void error(char *msg)
{
  printf("\n**** ERROR: %s ****\n", msg);
  exit(2);
}


void usage(void)
{
  printf("\nUsage: minitalk [-dm] [-di] [-ds] [-dp] [-dt] [file]\n");
  exit(1);
}


void copyFile(char *destName, char *progPath, char *extension)
{
  char copyCommand[100];
  char *cp;

  strcpy(copyCommand, "cp ");
  strcat(copyCommand, progPath);
  cp = copyCommand;
  while (*cp != '\0') {
    cp++;
  }
  while (*cp != '/') {
    cp--;
  }
  *++cp = '\0';
  strcat(copyCommand, ORIGINAL_NAME);
  strcat(copyCommand, ".");
  strcat(copyCommand, extension);
  strcat(copyCommand, " ");
  strcat(copyCommand, destName);
  if (system(copyCommand) != 0) {
    error("cannot copy original image file");
  }
}


int main(int argc, char *argv[])
{
  int i;
  char *argp;
  char *fileName;
  char imageFileName[50];
  char sourcesFileName[50];
#ifdef DOS_STANDARD
  unsigned long nearCoreBefore, nearCoreAfter;
  unsigned long farCoreBefore, farCoreAfter;
#endif

  /* analyze command line arguments */
  fileName = NULL;
  for (i = 1; i < argc; i++) {
    argp = argv[i];
    if (*argp == '-') {
      /* option */
      argp++;
      if (strcmp(argp, "dm") == 0) {
	debugMemory = TRUE;
      } else
      if (strcmp(argp, "di") == 0) {
	debugInterpreter = TRUE;
      } else
      if (strcmp(argp, "ds") == 0) {
	debugScanner = TRUE;
      } else
      if (strcmp(argp, "dp") == 0) {
	debugParser = TRUE;
      } else
      if (strcmp(argp, "dt") == 0) {
	debugTree = TRUE;
      } else {
	usage();
      }
    } else {
      /* file */
      if (fileName == NULL) {
	fileName = argp;
      } else {
	usage();
      }
    }
  }
  /* set image and sources file names */
  if (fileName == NULL) {
    strcpy(imageFileName, DEFAULT_NAME);
    strcpy(sourcesFileName, DEFAULT_NAME);
  } else {
    strcpy(imageFileName, fileName);
    strcpy(sourcesFileName, fileName);
  }
  if (strchr(imageFileName, '.') == NULL) {
    strcat(imageFileName, ".");
    strcat(imageFileName, DEFAULT_IMAGE_EXT);
    strcat(sourcesFileName, ".");
    strcat(sourcesFileName, DEFAULT_SOURCE_EXT);
  }
  /* ensure access to image and sources files */
  if (access(imageFileName, 0) != 0 ||
      access(sourcesFileName, 0) != 0) {
    /* image or sources file not present, must copy originals */
    copyFile(imageFileName, argv[0], DEFAULT_IMAGE_EXT);
    copyFile(sourcesFileName, argv[0], DEFAULT_SOURCE_EXT);
  }
  /* write greeting */
  printf(GREETING);
  /* load image file and run bytecode interpreter */
#ifdef DOS_STANDARD
  nearCoreBefore = coreleft();
  farCoreBefore = farcoreleft();
#endif
  initMemory(imageFileName);
  runInterpreter();
  exitMemory(imageFileName);
#ifdef DOS_STANDARD
  nearCoreAfter = coreleft();
  farCoreAfter = farcoreleft();
  if (nearCoreBefore != nearCoreAfter) {
    printf("near core: %lu vs. %lu", nearCoreBefore, nearCoreAfter);
    error("memory leakage");
  }
  if (farCoreBefore != farCoreAfter) {
    printf("far core: %lu vs. %lu", farCoreBefore, farCoreAfter);
    error("memory leakage");
  }
#endif
  /* write completion message */
  printf(BYEBYE);
  /* return to OS */
  return 0;
}
