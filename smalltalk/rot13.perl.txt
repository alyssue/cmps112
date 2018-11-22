#!/usr/bin/perl -p
# $RCSfile: rot13.perl,v $$Revision: 19981001.160536 $
#
# Usage:
#    rot13 [filename...]
#
# Synopsis:
#    Performs rotate-13 encoding (decoding) on all files specified
#    in @ARGV.  If there is no @ARGV, uses STDIN.  Output is to 
#    the standard output.
#
# Encoding:
#    Each upper case letter is shifted down the alphabet by 13
#    letters, wrapping around from Z to A if necessary.  Lower
#    case letters are treated in the same way.  Other characters
#    are passed through unchanged.  Since there are 26 letters in
#    alphabet, the same program can be used as both an encoder and
#    a decoder.
#
# Perl:
#    Note that Perl is a really great language.  The program itself
#    consists only of the option -p and one line of code at the end
#    of the file.  The rest of the program (lines beginning with #)
#    consists of comments.
#
tr/a-zA-Z/n-za-mN-ZA-M/;
