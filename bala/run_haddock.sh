#!/bin/bash

hs_files=$(find Bala/Base Bala/Format -name "*.hs" )

haddock -o doc -h $hs_files

