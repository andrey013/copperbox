0dbfs = 32767
sr = 44100
ksmps = 32
nchnls = 2


instr 3
i1      = 0.006667
i2      = cpspch(p5)
i3      = octpch(p5)
k1      linseg      0, p3/3, 1, p3/3, 1, p3/3, 0
k2      linseg      0, p3/3, 5, p3/3, 3, p3/3, 0
k3      linseg      p6, p3, p7
a1      = k2*(k3-1/k3)/2
a2      = abs(a1*2/20)
a3      = k2*(k3+1/k3)/2
a4      tablei      a2, 3, 1, 0, 0
a5      oscil       a1, i2, 2
a6      = exp(-0.5*a4+a5)
a7      oscil       a3*i2, i2, 2
a8      oscil       1000*k1*a6, a7+cpsoct(i3+i1), 1
a9      oscil       1000*k1*a6, a7+cpsoct(i3-i1), 1
        outs        a8, a9
endin