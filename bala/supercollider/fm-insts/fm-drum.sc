// This is fm-drum written by Jan Mattox and distributed with
// CLM (Common Lisp Music). The CLM original is in the file 
// fmex.ins. 


// Run this first
s.recHeaderFormat_("WAV");

// Be careful not to use functions inside conditionals
// on the server...

// The noise signal adds more excitation at the start
// of a sound...

SynthDef("fm-drum", { 
	arg freq, dr = 1.0, amp = 1.0, idx = 5, hi = 1.0;
	// var noiseamp = 1000.0;
	var noiseamp = 20000.0;
	var casrat = if (hi > 0, 8.525, 3.515);
	var modrat = if (hi > 0 , 3.414, 1.414);
	var amprise = 0.015;
	var amp1decay = if (dr < 0.18, dr * 0.5, 0.18);
	var amp2decay = dr - (amprise + amp1decay);
	var cascadehz = freq * casrat;
	var modhz = freq * modrat;
	var carrhz = freq;
	var devscaler = 7000;
	var cascscaler = idx * cascadehz;
	var modscaler = idx * modhz;
	var glsscaler = if (hi > 0, 66.0, 0.0);

	var idxenv, noiseenv, ampenv, glsenv;
	var dev1, devsig, modsig, cascsig, carrsig;
	
	// modulator indices
	idxenv = EnvGen.kr(
		Env(levels: [0.01, 1, 0.33, 0.012],
			times: [0.015, 0.058, dr - 0.010],
			curve: [2,-1,-1]),
		doneAction: 2);
   
	// Noise..
	noiseenv = EnvGen.kr(
		Env(levels: [0.01, 1, 1, 0.1, 0.002, 0.001], 
			times: [0.015, 0.005, 0.010, 0.040, dr - 0.070], 
			curve: \exp),
		doneAction: 2);

	// Amplitude envlope
	ampenv = EnvGen.kr(
		Env(levels: [ 0.01, 1.0, 0.75, 0.001], 
			times: [amprise, amp1decay, amp2decay],
			curve: [-2,2,-6]),
		doneAction: 2);

	// gls envelope
	glsenv = EnvGen.kr(
		Env(levels: [0,0,1,1], 
			times: [dr * 0.25, dr*0.50, dr * 0.25]),
		doneAction: 2);

	// deviation
	dev1 = noiseamp * WhiteNoise.ar(1.0);
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

// Windows paths use backslash:
// hi
s.prepareForRecord("d:/coding/supercollider/sc-hi-fm-drum.wav");
s.bind( Synth("fm-drum", [\freq, 66, \dr, 1.5, \amp, 0.75, \idx, 4, \hi, 1.0]); s.record; );
s.stopRecording;

s.prepareForRecord("d:/coding/supercollider/sc-lo-fm-drum.wav");
s.bind( Synth("fm-drum", [\freq, 55, \dr, 1.5, \amp, 0.75, \idx, 5, \hi, 0.0]); s.record; );
s.stopRecording;

Synth("fm-drum", [\freq, 55, \dr, 1.5]);
Synth("fm-drum", [\freq, 66, \dr, 1.5, \hi, \no, \idx, 4]);

    
(
var dr = 1.5, amprise = 0.015, amp1decay = 0.18, 
	amp2decay = dr - (amprise + amp1decay);
Env(levels: [ 0.01, 1.0, 0.75, 0.01], 
	times: [amprise, amp1decay, amp2decay],
	curve: [-2,2,-6] ).plot;
)
	                             

(
var dr = 1.5;
Env(levels: [0.01, 1, 0.33, 0.012],
	times: [0.015, 0.058, dr - 0.010],
	curve: [2,-1,-1]).plot;
)

(
SynthDef("help-Dust", { arg out=0;
Out.ar(out, 
Dust.ar(200, 1)
)
}).play;
)

(
SynthDef("help-LFClipNoise", { arg out=0;
Out.ar(out, 
LFClipNoise.ar(1000, 0.25)
)
}).play;
)
                                 