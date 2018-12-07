#!/usr/bin/perl -w
# $Id: calc,v 1.3 2013-05-21 12:57:20-07 - - $
#
# NAME
#    calc
#
# SYNOPSIS
#    calc [exprs...]
#
# DESCRIPTION
#    Read each line, evaluate it as a Perl expression, and
#    print the value.  If @ARGV is given, treat each element
#    of @ARGV as a line.  Each input line or @ARGV element is
#    a Perl expression, except that something matching the
#    pattern /\#(\d+)/ refers to the value of line $1 and a
#    match of /\#/ is the value of the most recent line.
#

use constant pi => 4 * atan2 (1, 1);
sub log2 ($) {log ($_[0]) / log (2)}
sub log10 ($) {log ($_[0]) / log (10)}
sub rad ($) {$_[0] * 2 * PI / 360}

$INPUT = @ARGV ? 'shift @ARGV' : '<>';

while ($CALC = eval ($INPUT)) {
   $CALC =~ s{\#(\d+)?}
             {"\$CALC[" . (defined ($1) ? $1 : $#CALC) . "]"}xge;
   push @CALC, $CALC = eval ($CALC) || 1e1000 / 1e1000;
   printf "#%d=\t%.15g\n%s", $#CALC, $CALC, $@;
}
