#!/bin/bash

hs_files=$(find Bala -name "*.hs" )

haddock -o doc -h $hs_files

