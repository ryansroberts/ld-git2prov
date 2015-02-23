#!/bin/bash


mono ../../../bin/git2prov.exe --path ../../../bin/testrepo --showhistory --since HEAD~1 > HEADtobe3.ttl
mono ../../../bin/git2prov.exe --path ../../../bin/testrepo --showhistory --since HEAD~1 > HEADtobe3563.ttl
mono ../../../bin/git2prov.exe --path ../../../bin/testrepo --showhistory --since 8496071 > AllHistory.ttl
mono ../../../bin/git2prov.exe --path ../../../bin/testrepo --showhistory --since HEAD~1 > HEADtoHEAD-1.ttl
mono ../../../bin/git2prov.exe --path ../../../bin/testrepo --showhistory --since HEAD~2 --showcontent > withcontent.ttl
mono ../../../bin/git2prov.exe --path ../../../bin/testrepo --showhistory --since HEAD~2 --showcontent --showcompilation > withcompilation.ttl
