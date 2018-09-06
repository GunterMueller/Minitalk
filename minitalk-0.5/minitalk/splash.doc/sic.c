/*
 * sic.c -- splash image converter
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>


#define BACKGROUND	0
#define FOREGROUND	1


typedef struct {
  char signature[2];
  int fileSize;
  int reserved;
  int dataOffset;
} __attribute__((packed)) Header;


typedef struct {
  int size;
  int width;
  int height;
  short planes;
  short bitCount;
  int compression;
  int imageSize;
  int xPixPerM;
  int yPixPerM;
  int colorsUsed;
  int colorsImportant;
} __attribute__((packed)) Info;


void error(char *fmt, ...) {
  va_list ap;

  va_start(ap, fmt);
  printf("Error: ");
  vprintf(fmt, ap);
  printf("\n");
  va_end(ap);
  exit(1);
}


void usage(char *myself) {
  printf("Usage: %s <bmp file> <splash file> [threshold (0..100)]\n",
         myself);
  exit(1);
}


int main(int argc, char *argv[]) {
  FILE *bmp;
  Header header;
  Info info;
  unsigned char *data;
  int row, col;
  FILE *splash;
  int threshold;
  int grayVal;
  int current;
  int runlength;
  int count;

  if (argc != 3 && argc != 4) {
    usage(argv[0]);
  }
  bmp = fopen(argv[1], "r");
  if (bmp == NULL) {
    error("cannot open bmp file '%s'", argv[1]);
  }
  if (fread(&header, sizeof(Header), 1, bmp) != 1) {
    error("cannot read bmp header");
  }
  if (header.signature[0] != 'B' ||
      header.signature[1] != 'M') {
    error("'%s' is not a proper bmp file", argv[1]);
  }
  printf("file size      %d\n", header.fileSize);
  printf("data offset    %d\n", header.dataOffset);
  if (fread(&info, sizeof(Info), 1, bmp) != 1) {
    error("cannot read bmp info");
  }
  if (info.size != 40) {
    error("size of bmp info is not 40");
  }
  printf("bitmap width   %d\n", info.width);
  printf("bitmap height  %d\n", info.height);
  if (info.planes != 1) {
    error("number of planes is not 1");
  }
  printf("bit count      %d\n", info.bitCount);
  if (info.bitCount != 24) {
    error("this image does not have 24 bpp");
  }
  printf("compression    %d\n", info.compression);
  if (info.compression != 0) {
    error("this image is compressed");
  }
  data = malloc(info.width * info.height * 3);
  if (data == NULL) {
    error("cannot allocate memory for raster data");
  }
  fseek(bmp, header.dataOffset, SEEK_SET);
  for (row = info.height - 1; row > -1; row--) {
    for (col = 0; col < info.width; col++) {
      if (fread(&data[row * info.width + col + 0], 1, 1, bmp) != 1) {
        error("cannot read raster data");
      }
      if (fread(&data[row * info.width + col + 1], 1, 1, bmp) != 1) {
        error("cannot read raster data");
      }
      if (fread(&data[row * info.width + col + 2], 1, 1, bmp) != 1) {
        error("cannot read raster data");
      }
    }
  }
  if (ftell(bmp) != header.fileSize) {
    error("junk data at end of file");
  }
  fclose(bmp);
  splash = fopen(argv[2], "w");
  if (splash == NULL) {
    error("cannot open splash file '%s'", argv[2]);
  }
  if (argc == 4) {
    threshold = atoi(argv[3]);
  } else {
    threshold = 50;
  }
  printf("converting with a threshold value of %d\n", threshold);
  if (threshold < 0 || threshold > 100) {
    error("illegal threshold value");
  }
  threshold *= 3 * 256;
  current = BACKGROUND;
  runlength = 0;
  count = 0;
  for (row = 0; row < info.height; row++) {
    for (col = 0; col < info.width; col++) {
      grayVal = (data[row * info.width + col + 0] +
                 data[row * info.width + col + 1] +
                 data[row * info.width + col + 2]) * 100;
      if ((grayVal > threshold && current == BACKGROUND) ||
          (grayVal <= threshold && current == FOREGROUND)) {
        runlength++;
      } else {
        fprintf(splash, "0x%05X,", runlength);
        if (++count == 8) {
          fprintf(splash, "\n");
          count = 0;
        } else {
          fprintf(splash, " ");
        }
        current = 1 - current;
        runlength = 1;
      }
    }
  }
  fprintf(splash, "0x%05X\n", runlength);
  fclose(splash);
  return 0;
}
