#!/usr/bin/perl
# $Id: mtime.perl,v 1.1 2018-11-06 18:50:44-08 - - $
#
# NAME
#    older.perl - check whether a pair of files are older or newer
#
# SYNOPSIS
#    older.perl filename...
#
# DESCRIPTION
#    The two files' modification times are compared and a
#    relationship is printed.
#

use strict;
use warnings; #will tell us if we didn't declare a variable. 
use POSIX qw(strftime);
$0 =~ s|.*/||; #include all but last / in the name. if none, this match fails.

sub mtime ($) {         #subroutine / function. 
   my ($filename) = @_; # @_ is the list of arguments passed into function. 
   my @stat = stat $filename; #function built into perl and pass filename in. returns array with all statics about the file. 
   return @stat ? $stat[9] : undef; #if stat is length 0 it couldn't. failure. otherwise will be 9 things.  
            #length of array stat not equal to zero? if not equal to zero lets get element number 9. if not there let it equal undef (null pointer)
}

sub fileinfo ($) {
   my ($filename) = @_;
   my $mtime = mtime $filename;
   print "$filename: ";
   if (defined $mtime) {print strftime "%c\n", localtime $mtime}
                  else {print "$!\n"}
   return $mtime;
}

for my $filename (@ARGV) { #my declares as local variable. will go through all arguements.
   unless (-e $filename) { #check to see if it exists. same as if not 
      printf STDERR "$0: $filename: $!\n"; #if it doesn't print an error message. 
   }else { #otherwise find out time. 
      my $mtime = mtime ($filename); #you can put $filename to make it look like call in c
      my $ctime = strftime "%c", localtime $mtime; # two parameters, string and localtime of mtime
      printf "%-20s %12d %s\n", $filename, $mtime, $ctime; #then print the filename, -20 character field, mtime and ctime. 
   }
}


#^^^^ this is for getting timestamp on a program. 