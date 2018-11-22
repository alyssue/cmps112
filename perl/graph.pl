#!/usr/bin/perl
# $Id: graph.perl,v 1.1 2018-11-06 18:50:43-08 - - $

use strict;
use warnings;
$0 =~ s|.*/||;

# Example setting up a directed graph.

my @inputs = (
   "all : hello",
   "hello : main.o hello.o",
   "main.o : main.c hello.h",
   "hello.o : hello.c hello.h",
   "ci : Makefile main.c hello.c hello.h",
   "test : hello",
   "clean : ",
   "spotless : clean",
);

sub parse_dep ($) {
   my ($line) = @_;
   return undef unless $line =~ m/^(\S+)\s*:\s*(.*?)\s*$/; #.* means greedy match. (.*?) match as little as you can. 
   my ($target, $dependency) = ($1, $2); #target is first paren. $dependecny is second paren. 
   my @dependencies = split m/\s+/, $dependency; #dependcy returns an array 
   return $target, \@dependencies; 
}


#\s is whitespace
#\S is anythign thats NOT whitespace
#\d is a digit.
#\D is NOT a digit
#\w is character in a word
#\W is NOT a digit that should appear in a word.

my %graph;
for my $input (@inputs) {
   my ($target, $deps) = parse_dep ($input); #target is string scalar. deps is reference to array. scalar can be number string or reference.
   print "$0: syntax error: $input\n" and next unless defined $target;
   $graph{$target} = $deps;
}

for my $target (keys %graph) {
   print "\"$target\"";
   my $deps = $graph{$target};
   if (not @$deps) {
      print " has no dependencies";
   }else {
      print " depends on";
      print " \"$_\"" for @$deps; #gets the array from the pointer.
   }
   print "\n";
}
