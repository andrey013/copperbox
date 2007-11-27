
all: midiprint demotrack

midiprint: MidiPrint.hs ./Sound/Midi/ReadFile.hs ./Sound/Midi/Datatypes.hs
	ghc --make -i./Sound/Midi MidiPrint.hs

demotrack: DemoTrack.hs ./Sound/Midi/WriteFile.hs ./Sound/Midi/Datatypes.hs
	ghc --make -i./Sound/Midi DemoTrack.hs
		