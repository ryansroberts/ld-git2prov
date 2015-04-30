#!/bin/bash


mono ../../../bin/git2prov.exe --path ../../../bin/testrepo --showhistory --since HEAD~1 > HEADtobe3.ttl
mono ../../../bin/git2prov.exe --path ../../../bin/testrepo --showhistory --since HEAD~1 > HEADtobe3563.ttl
mono ../../../bin/git2prov.exe --path ../../../bin/testrepo --showhistory --since all > AllHistory.ttl
mono ../../../bin/git2prov.exe --path ../../../bin/testrepo --showhistory --since HEAD~1 > HEADtoHEAD-1.ttl
mono ../../../bin/git2prov.exe --path ../../../bin/testrepo --showhistory --since all  > withcontent.ttl
mono ../../../bin/git2prov.exe --path ../../../bin/testrepo --showhistory --since all --showcompilation > withcompilation.ttl
