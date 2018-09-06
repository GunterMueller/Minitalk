/*
 * main.c -- main program
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>

#include "common.h"
#include "utils.h"
#include "memory.h"
#include "virtproc.h"
#include "compiler.h"
#include "tree.h"
#include "scanner.h"
#include "parser.h"


/*------------------------------------*/
/* Signal Handling                    */
/*------------------------------------*/


static void (*oldSigIntHandler)(int signum);


static void newSigIntHandler(int signum) {
  signal(SIGINT, newSigIntHandler);
  //cpuHalt();
}


static void initSigInt(void) {
  oldSigIntHandler = signal(SIGINT, newSigIntHandler);
}


static void exitSigInt(void) {
  signal(SIGINT, oldSigIntHandler);
}


/*------------------------------------*/
/* Main Program                       */
/*------------------------------------*/


static void version(char *myself) {
  printf("%s version %d.%d (compiled %s)\n",
         myself, MAJOR_VNUM, MINOR_VNUM, __DATE__);
}


static void help(char *myself) {
  printf("Usage: %s [options] [image file]\n", myself);
  printf("Options:\n");
  printf("  --memory         debug memory\n");
  printf("  --processor      debug virtual processor\n");
  printf("  --scanner        debug scanner\n");
  printf("  --parser         debug parser\n");
  printf("  --tree           show syntax tree\n");
  printf("  --version        show version\n");
  printf("  --help           show this help\n");
}


static void copyFile(char *srcPath, char *srcName, char *dstName) {
  char copyCommand[LINE_SIZE];

  strcpy(copyCommand, "cp ");
  strcat(copyCommand, srcPath);
  strcat(copyCommand, "/");
  strcat(copyCommand, srcName);
  strcat(copyCommand, " ");
  strcat(copyCommand, dstName);
  printf("\nCopying file\n");
  printf("  from '%s/%s'\n", srcPath, srcName);
  printf("  to   '%s'...\n", dstName);
  if (system(copyCommand) != 0) {
    error("cannot execute copy command");
  }
  printf("Done.\n");
}


int main(int argc, char *argv[]) {
  char *imageFileName;
  char sourcesFileName[LINE_SIZE];
  int i;

  /* analyze command line */
  imageFileName = NULL;
  for (i = 1; i < argc; i++) {
    if (argv[i][0] == '-') {
      /* option */
      if (strcmp(argv[i], "--memory") == 0) {
        debugMemory = true;
      } else
      if (strcmp(argv[i], "--processor") == 0) {
        debugProcessor = true;
      } else
      if (strcmp(argv[i], "--scanner") == 0) {
        debugScanner = true;
      } else
      if (strcmp(argv[i], "--parser") == 0) {
        debugParser = true;
      } else
      if (strcmp(argv[i], "--tree") == 0) {
        debugTree = true;
      } else
      if (strcmp(argv[i], "--version") == 0) {
        version(argv[0]);
        exit(0);
      } else
      if (strcmp(argv[i], "--help") == 0) {
        help(argv[0]);
        exit(0);
      } else {
        error("unrecognized option '%s'; try '%s --help'",
              argv[i], argv[0]);
      }
    } else {
      /* file */
      if (imageFileName != NULL) {
        error("more than one file name not allowed");
      }
      imageFileName = argv[i];
    }
  }
  if (imageFileName == NULL) {
    imageFileName = DFLT_IMG_NAME;
  }
  /* construct sources file name from image file name */
  strcpy(sourcesFileName, imageFileName);
  i = strlen(sourcesFileName);
  if (i >= 4 && strcmp(sourcesFileName + i - 4, ".img") == 0) {
    i -= 4;
  }
  strcpy(sourcesFileName + i, ".src");
  /* ensure access to image and sources files */
  if (access(imageFileName, 0) != 0 ||
      access(sourcesFileName, 0) != 0) {
    /* image or sources file not present, must copy originals */
    copyFile(STD_IMG_PATH, STD_IMG_NAME, imageFileName);
    copyFile(STD_SRC_PATH, STD_SRC_NAME, sourcesFileName);
  }
  /* write greeting message */
  printf(GREETING);
  /* load image file and run virtual processor */
  initSigInt();
  initMemory(imageFileName);
  runProcessor();
  exitMemory(imageFileName);
  exitSigInt();
  /* write byebye message */
  printf(BYEBYE);
  /* done */
  return 0;
}
