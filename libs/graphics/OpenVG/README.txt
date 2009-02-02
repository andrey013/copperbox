Haskell bindings to ShivaVG (OpenVG implementation).

I've tested the bindings on both Windows XP (MinGW/Msys) 
and MacOSX leopard.

KNOWN PROBLEMS:

MacOSX - runhaskell / GHCi freeze the shell when you try to run
the example TestVgu.hs. You will have to compile it first.

Windows - running the test through GHCi kills the GHCi session
when you close the display window. Its better to run through 
runhaskell.

Shiva-VG (the C Library) should install quite easily on MacOSX - 
I installed it with the usual `configure`, `make` % `make install`.
I would imagine Linux is easy too. Windows isn't at all easy - but
there are instructions in the file `InstallWindows.txt`.


On all platforms you will need OpenGL and GLUT and the Haskell
bindings to OpenGL and GLUT installed and working.

