#!/bin/bash


mono ../../../bin/git2prov.exe --path ../../../bin/testrepo --showhistory --includeworkingarea --since all --output .
touch ../../../bin/testrepo/workingareafile.md
mono ../../../bin/git2prov.exe --path ../../../bin/testrepo --includeworkingarea > workingarea.ttl
