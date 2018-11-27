#!/usr/bin/perl
use strict;
use warnings;
my $RCSID = '$Id: cid,v 1.3 2018-08-09 16:56:40-07 - - $';
$0 =~ s|/*$||; $0 =~ s|.*/||;

my $USAGE = <<__END_USAGE__;
#
# NAME
#    $0 - check-in file to RCS subdirectory archive unlocked
#
# SYNOPSIS
#    $0 [options] filename...
#
# DESCRIPTION
#    The options given are ci(1) options, ignored by $0 except to
#    consider them not files if they begin with a dash.  The time
#    zone options is set to local time and locking is non-strict.
#
# OPTIONS
#    +  Causes $0 to run silently without excess chatter.
#       Must be the first option and in a word by itself.
#
# $RCSID
#
__END_USAGE__

$USAGE =~ s/^#[ ]?//mg;
print $USAGE and exit unless @ARGV;

use POSIX qw (strftime);

my $silent = @ARGV && ($ARGV[0] eq "+" ? shift @ARGV : 0);

sub echo_run(@) {
   my (@command) = @_;
   print "$0: % @command\n" unless $silent;
   my $output = `@command 2>&1`;
   $output =~ s/done\n//g;
   $output =~ s{file\s+is\s+unchanged;\s+
                reverting\s+to\s+previous\s+revision\s+\d+\.?\d+\s*
               }{}x if $silent;
   $output =~ s{RCS/.*,v\s+<--\s+.*\s*}{} if $silent;
   print $output unless $silent;
};

my (%mode, @files, %rcsdirs, @rcsdirs, @rcsfiles);
for my $file (@ARGV) {
   next unless $mode{$file} = (stat $file)[2];
   push @files, $file;
   my $rcsdir = $file;
   $rcsdir =~ s{[^/]*$}{RCS};
   next if $rcsdirs{$rcsdir} or -d $rcsdir;
   $rcsdirs{$rcsdir} = push @rcsdirs, $rcsdir;
};

map {echo_run "mkdir $_"} @rcsdirs;

$ENV{RCSINIT} = '-zLT -s- -t-';
$ENV{LOGNAME} = '-';

echo_run "ci -m- -u @ARGV" if @ARGV;

if (@files) {
   @rcsfiles = map {my $x = $_; $x =~ s|.*|RCS/$&,v|; $x} @files;
   print "$0: [@files]\n" unless $silent;
   echo_run "rcs -q -U @files";
   map{ chmod $mode{$_} & 07777, $_ or warn "$0: $_: $!" } @files;
   echo_run "ls -goaFd @files @rcsfiles | cut -c1-80" unless $silent;
   echo_run "ident -q @files" unless $silent;
};
