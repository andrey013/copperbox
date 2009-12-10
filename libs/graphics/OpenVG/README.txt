Haskell bindings to ShivaVG (OpenVG implementation).

Haskell OpenVG 0.1 binds to ShivaVG-0.2.0
Haskell OpenVG 0.2 binds to ShivaVG-0.2.1
Haskell OpenVG 0.2.1 binds to ShivaVG-0.2.1
Haskell OpenVG 0.3.0 binds to ShiaVG-0.2.1

I've tested the bindings on both Windows XP (MinGW/Msys) 
and MacOSX Leopard.

On MacOSX both the 0.2 and 0.1 bindings seem work on 
their respective libraries.

On Windows I've not been able to get the 0.3.0 bindings to 
work (yet), 0.2.1 is preferred...



OTHER PROBLEMS:

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

