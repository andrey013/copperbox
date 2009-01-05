/* cygwin... */
/* gcc -o CGetString CGetString.c -lopengl32 -lglu32 -lglut32 -lopenvg32 */

/* From Cygwin this program may silently fail if openvg32.dll and glut32.dll
 * cannot be found. Either copy them into the same directory as the *.exe,
 * or make sure you have registered them in WINDOWS/system32 */


#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>
#include <vg/openvg.h>
#include <stdio.h>

int main(int argc, char **argv)
{
  VGboolean okb;

  /* Create a glutWindow first! */
  glutInit(&argc, argv);

  glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE | GLUT_ALPHA |
                      GLUT_STENCIL | GLUT_MULTISAMPLE);

  glutInitWindowPosition(0,0);
  glutInitWindowSize(100,100);
  glutCreateWindow("glutWin");


  /* vgCreateContextSH - needs a `glutWindow` to exist 
   * before it can be called 
   */
  okb = vgCreateContextSH(10,10);

  if (!okb)
  {
    printf("vgCreateContextSH failed - bad news!\n");
  }
  else
  {
    printf("Excellent - OpenVG (and OpenGL and Glut) work!\n");
    printf("OpenVG - Vendor %s\n",  (char *) vgGetString(VG_VENDOR));
    printf("OpenVG - Version %s\n", (char *) vgGetString(VG_VERSION));
  }
  vgDestroyContextSH();

  return 0;
}

