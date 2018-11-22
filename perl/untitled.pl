#!/usr/bin/perl
# $Id: xref.perl,v 1.1 2014-10-03 16:57:20-07 - - $

map { $hash{lc $_} .= " $." } m/(\w+)/g while <>; #while box, diamond operated iterates over the next line form the input file.
map { print "$_$hash{$_}\n" } sort keys %hash;


#read a lot of words and for each words print out in lower case as well as list of line numbers where they occur. 
# each one of the lines, \w+ /g globally grab all of them. 

#map is apply that black for every word. 