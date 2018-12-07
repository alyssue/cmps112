#!/usr/bin/perl -w 
use strict; # no worries for short programs
use warnings; # use warnings will warn about use of uninitialized variables. will be zero if uninitialized.
my $RCSID = '$Id: hello.perl,v 1.1 2014-10-03 16:57:20-07 - - $'; # used to enclose string.
#
# NAME
#    hello - hello world program
#
# SYNOPSIS
#    hello
#
# DESCRIPTION
#    Prints either message ``Hello, World!''.
#

print "Hello, world!\n"; # with " " black slash \ will do something 
# with single quotes ' ' the \ is read and printed as is. 


# first two characters of a script must be #! followed by path to script.


# chmod +x will make it an excecutable.