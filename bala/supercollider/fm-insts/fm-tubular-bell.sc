// FM tubular bell from Nicky Hind's
// CLM tutorials:
//
// https://ccrma.stanford.edu/software/clm/compmus/clm-tutorials/fm2.html
//

// Run this first
s.recHeaderFormat_("WAV");

// 3 modulators, 3 carriers

SynthDef("fm-tubular-bell", {
	arg freq, dr = 1.0, amp = 1.0, amp1 = 0.5, amp2 = 0.25, amp3 = 0.25,
		c1rat = 2.0, m1rat = 5.0, 
		c2rat = 0.6, m2rat = 4.8, 
		c3rat = 0.22, m3rat = 0.83, 
		idxmin = 0.15,
		idx1scl = 3.0, idx2scl = 2.0, idx3scl = 1.0;

	var amp1level = amp * amp1;
	var amp2level = amp * amp2;
	var amp3level = amp * amp3;
	var idx1max = amp * idx1scl * ((261.5 / freq) pow: 3);
	var idx2max = amp * idx2scl * ((261.5 / freq) pow: 3);
	var idx3max = amp * idx3scl * ((261.5 / freq) pow: 3);
	var mod1scaler = (idx1max - idxmin) * m1rat * freq;
	var mod2scaler = (idx3max - idxmin) * m2rat * freq;
	var mod3scaler = (idx3max - idxmin) * m3rat * freq;
	var mod1offset = idxmin * m1rat * freq;
	var mod2offset = idxmin * m2rat * freq;
	var mod3offset = idxmin * m3rat * freq;

	var mod1sig, car1sig, mod2sig, car2sig, mod3sig, car3sig;
	var idxenv, ampenv;
	

	// index envelope
	idxenv = EnvGen.kr(
		Env(levels: [ 1, 0.01], times: [dr], curve: \exp),
		doneAction: 2);
	
	// amp envelope
	ampenv = EnvGen.kr(
		Env(levels: [ 0, 1, 0.01], times: [0.002, dr - 0.002], curve: \exp),
		doneAction: 2);

	// modulator 1
	mod1sig = SinOsc.ar(freq: freq * m1rat);
	// envelope the signal, afterwards
	mod1sig = (mod1offset + (mod1scaler * idxenv)) * mod1sig;
	
	// carrier 1
	car1sig = SinOsc.ar(freq: (freq * c1rat) + mod1sig );
	car1sig = amp1level * ampenv * car1sig;

	// modulator 2
	mod2sig = SinOsc.ar(freq: freq * m2rat);
	// envelope the signal, afterwards
	mod2sig = (mod2offset + (mod2scaler * idxenv)) * mod2sig;

	// carrier 2
	car2sig = SinOsc.ar(freq: (freq * c2rat) + mod2sig );
	car2sig = amp2level * ampenv * car2sig;

	// modulator 3
	mod3sig = SinOsc.ar(freq: freq * m3rat);
	// envelope the signal, afterwards
	mod3sig = (mod3offset + (mod3scaler * idxenv)) * mod3sig;

	// carrier 3
	car3sig = SinOsc.ar(freq: (freq * c3rat) + mod3sig );
	car3sig = amp3level * ampenv * car3sig;

	Out.ar(0, car1sig + car2sig + car3sig);	

}).store;      

Synth("fm-tubular-bell", [\freq, 261.61, \dr, 3.0, \amp, 0.7]);



// pow syntax:
// 2 ^ 6

(2 pow: 6).value;                         