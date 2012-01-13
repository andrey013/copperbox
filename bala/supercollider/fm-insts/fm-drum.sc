// This is fm-drum written by Jan Mattox and distributed with
// CLM (Common Lisp Music). The CLM original is in the file 
// fmex.ins. 


// Run this first
s.recHeaderFormat_("WAV");

// Be careful not to use functions inside conditionals
// on the server...

SynthDef("fm-drum", { 
	arg freq, dr = 1.0, amp = 1.0, idx = 5, hi = \hi;
	var noiseamp = 1000.0;
	var casrat = if (hi == \hi, 8.525, 3.515);
	var modrat = if (hi == \hi, 3.414, 1.414);
	var amprise = 0.015;
	var amp1decay = if (dr < 0.18, dr * 0.5, 0.18);
	var amp2decay    = dr - (amprise + amp1decay);
	var cascadehz    = freq * casrat;
	var modhz        = freq * modrat;
	var carrhz       = freq;
	var devscaler    = 7000;
	var cascscaler   = idx * cascadehz;
	var modscaler    = idx * modhz;
	var glsscaler    = if (hi == \hi, 66.0, 0.0);

	var idxenv, noiseenv, ampenv, glsenv;
	var dev1, devsig, modsig, cascsig, carrsig;
	
	// modulator indices
	idxenv = EnvGen.kr(
		Env.new(levels: [0.01, 1, 0.333, 0.12], 
					times: [0.015, 0.085, dr - 0.1]),
		doneAction: 2);
   
	// Noise..
	noiseenv = EnvGen.kr(
		Env(levels: [0.01, 1, 1, 0.1, 0.002, 0], 
			times: [0.015, 0.005, 0.010, 0.040, dr - 0.070], 
			curve: \exp),
		doneAction: 2);

	// Amplitude envlope
	ampenv = EnvGen.kr(
		Env(levels: [ 0.01, 1.0, 0.75, 0.001], 
			times: [amprise, amp1decay, amp2decay]),
		doneAction: 2);

	// gls envelope
	glsenv = EnvGen.kr(
		Env(levels: [0,0,1,1], 
			times: [dr * 0.25, dr*0.50, dr * 0.25]),
		doneAction: 2);

	// deviation
	dev1 = LFNoise0.kr(mul: noiseamp);
	devsig = devscaler * noiseenv * dev1;
	devsig = (casrat * glsscaler * glsenv) + devsig;


	// cascade
	cascsig = SinOsc.ar(freq: cascadehz + devsig);
	cascsig = cascscaler * idxenv * cascsig;
	cascsig = (modrat * glsscaler * glsenv) + cascsig;

	// modulator
	modsig = SinOsc.ar(freq: modhz + cascsig);
	modsig = modscaler * idxenv * modsig;
	modsig = (glsscaler * glsenv) + modsig;

	// carrier
	carrsig = SinOsc.ar(freq: carrhz + modsig);
   
	Out.ar(0, amp * carrsig * ampenv);

}).store

Synth("fm-drum", [\freq, 55, \dr, 1.5]);
Synth("fm-drum", [\freq, 66, \dr, 1.5, \hi, \no]);

 