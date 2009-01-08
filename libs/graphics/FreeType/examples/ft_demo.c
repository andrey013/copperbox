/* gcc -o ft_demo ft_demo.c -I/usr/local/include/freetype2 -I/usr/local/include -lfreetype */

#include <ft2build.h>
#include FT_FREETYPE_H
#include <stdio.h>

int main (void)
{
  FT_Error err;
  FT_Library  * h;
  FT_Int mj, mn, ph;
  
  
  printf("init...\n");
  
  err = FT_Init_FreeType(h);
  printf("err_no %d \n", err);
  
  FT_Library_Version(*h, &mj, &mn, &ph);
  printf("Version %d.%d.%d \n", mj, mn, ph);
  
  
  err = FT_Done_FreeType(*h);
  printf("err_no %d \n", err);
  
  return 0;  
}  


