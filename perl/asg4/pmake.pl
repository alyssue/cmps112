#!/usr/bin/perl
# Alyssa Melton@ucsc.edu
# amelton@ucsc.edu
# CMPS 112 -- Comparative Programming Languages
# Assignment 4 - pmake.pl
# Perl
#

use strict;
use warnings;
use Getopt::Std;
use Data::Dumper;
use File::stat;

my $file;
my $target;
my @targets = ();
my %targetPreReqs = ();
my %commands = ();
my %macros = ();
my %OPTS;
my %targetcomplete = ();

# -----------------------------------------------------------------------------
getopts ("d", \%OPTS);
print "Debug turned on!\n" if $OPTS{'d'};
if (!exists $ARGV[0]) { $file = "Makefile" }
else { $file = $ARGV[0]; }

if ($OPTS{'d'}){
  if (!exists $ARGV[0]) {
    print "No target specified, defaulting to 'Makefile'. \n"
  } else {
    print "$0: ARGV[$_]= $ARGV[$_]\n" for 0 .. $#ARGV;
  }
}

# -----------------------------------------------------------------------------
open(my $infile, '<:encoding(UTF-8)', $file)
  or die "Could not open file '$file' $!";

if ($OPTS{'d'}){
  print "Target contents between dividers: \n";
  print "---------------------------------------------\n";
  while (my $row = <$infile>) {
    chomp $row;
    print "$row\n";
  }
  seek $infile, 0, 0;
  print "---------------------------------------------\n";
}

# -----------------------------------------------------------------------------
# READ MAKEFILE
while (my $row = <$infile>) {
  my $commentIndex = index($row, '#');
  my $commandIndex = index($row, "\t");
  my $macroIndex = index($row, '=');
  my $targetPrereq = index($row, ':');
  chomp $row;

  while ($row =~ /\${[^}]+}/){
      my $openBrace = index($row, '${');
      my $closeBrace = index($row, '}');
      my $macroKey = substr($row, $openBrace+2, $closeBrace-$openBrace-2);
      my $macroVal = getMacroValue($macroKey);
      substr($row, $openBrace, $closeBrace+1-$openBrace) = $macroVal;
  }

  # skip comments
  if ($commentIndex >= 0){
    next;
  }
  my $offset = 0;
  my $dollarIndex = index($row, '$', $offset);
    while ($dollarIndex != -1) {
      my $macroKey = substr($row, $dollarIndex, 2);
      my $macroVal = getMacroValue($macroKey);
      substr($row, $dollarIndex, 2) = $macroVal;
      $offset = $dollarIndex + 1;
      $dollarIndex = index($row, '$', $offset);
  }
  # get commands     push @{ $hash{"a key"} }, $value;
  if ($commandIndex >= 0){
    my $command = $row;
    $command =~ s/^\s+|\s+$//g;
    push @{$commands{$target}}, $command;
  }
  # define macros
  elsif ($macroIndex >= 0){
    my @keyVal = split(/=/, $row); #keyVal[0]=key, keyVal[1]=val
    $keyVal[0] =~ s/\s+//g; #get rid of whitespace
    $macros{$keyVal[0]} = $keyVal[1];
  }
  #if target
  elsif ($targetPrereq >= 0){
      my @split = split(/:/, $row);
      $split[0] =~ s/\s+//g; #get rid of whitespace
      $target = $split[0];
      push @targets, $target;
      $targetcomplete{$target} = 0;
      if (exists $split[1]){
        $split[1] =~ s/^\s+//; #get rid of preceding whitespace
        $targetPreReqs{$split[0]} = $split[1];
      }
  }

}

# -----------------------------------------------------------------------------
# SOME DEBUGGING
# -----------------------------------------------------------------------------
if ($OPTS{'d'}){
  print "\n";
  print "The macros: \n";
  print Dumper(\%macros);

  print "\n";
  print "The targets: \n";
  print Dumper(\@targets);

  print "\n";
  print "The targetsPreReqs: \n";
  print Dumper(\%targetPreReqs);

  print "\n";
  print "The commands: \n";
  print Dumper(\%commands);
}

#do the makefile stuff
make($targets[0]);

# -----------------------------------------------------------------------------
# COMMAND
# -----------------------------------------------------------------------------
# The line is echoed to STDOUT before executing the command.
# The line is then passed to the system function call for execution by the shell.
# The resulting exit status and signal is then tested. If either is non-zero,
# pmake exits at that point.

# Use the function 'system' to run the command. $? is the 'wait(2)'' exit status.
# The notation wait(2) refers to the manual page in section 2 of the manual. The
# command may be read with the command
# man -s 2 wait

sub handlecommands {
  my $target = shift;

  if ($OPTS{'d'}){
    print "inside handlecommand \n";
    print "the target: $target. its commands:  \n";
    print Dumper(\@{$commands{$target}});
  }

  foreach my $command (@{$commands{$target}}){
    #print "The command in foreach is '$command' \n";
    my $noExit = index($command, '-');
    my $noEcho = index($command, '@');

    if ($noExit == 0){
      $command = substr($command, $noExit+2);
    }

    if ($noEcho == 0){
      $command = substr($command, $noEcho+2);
    }
    #print "The updated command in foreach is '$command' \n";

    # @ command
    # Behaves like command, except that the command is not echoed to STDOUT
    # before being executed.
    if ($noEcho < 0){ # only print if noEcho character '-' is not present.
      print "$command \n";
    }

    system($command); #do command
    my $term_signal = $? & 0x7F;
    my $core_dumped = $? & 0x80;
    my $exit_status = ($? >> 8) & 0xFF;

    # - command
    # Behaves like command, except that a non-zero exit status or signal does
    # not cause pmake to exit at that point.
    if ($noExit < 0){
      if ($term_signal != "0") {
        print "termination signal $term_signal: %strsignal[$term_signal]";
        exit 1;
      }
    }


    if ($OPTS{'d'}){
      print "Core dumped = ";
      if ($core_dumped == 0) {
        print "false. \n"
      } else {
        print "true. \n"
      }
    }
  }
  $targetcomplete{$target} = 1;
  return;
}



#-----------------------------------------------------------------------------
#MACROS
sub getMacroValue {
  my $macroKey = shift;
  my $prevTarget = shift;
  if (defined ($macros{$macroKey})){
    return $macros{$macroKey};
  } elsif ($macroKey eq '$$'){
    # $$ Represents the dollar sign itself.
    return '$';
  } elsif ($macroKey eq '$<'){
    # $< Represents the first file specified as a prerequisite.
    my @preRequisites = split(/ /,$targetPreReqs{$target});
    return $preRequisites[0];
  } elsif ($macroKey eq '$@'){
    # $@ Represents the first file specified as a target.
    foreach my $thisTarget (@targets){
      if (isFile($thisTarget)){
        return $thisTarget;
      }
    }
  }
}

# -----------------------------------------------------------------------------

sub isTarget{
  my $inQuestion = shift;
  for my $tValue (@targets) {
    if ($tValue eq $inQuestion) { #inQuestions is a target
      return 1;
    }
  }
  return 0;
}

sub isFile{
  my $inQuestion = shift;
  return 1 if -e $inQuestion;
  return 0;
}

# -----------------------------------------------------------------------------
#target... : prerequisite ...
sub make {
  my $target = shift;
  if ($targetcomplete{$target}) { return; }
  $targetcomplete{$target} = 1;

  my @preRequisites;
  if (defined($targetPreReqs{$target})) {
    @preRequisites = $targetPreReqs{$target};
    @preRequisites = split(/ /,$preRequisites[0]);
  }

  if ($OPTS{'d'}){
    print "\n";
    print "The prereqs of $target: \n";
    print Dumper(\@preRequisites);
    print "\n";
  }

  if (isFile($target)){ #target is not a file (yet or ever)
    foreach my $preReq (@preRequisites){
      if (isTarget($preReq)) {
        if ($OPTS{'d'}){
          print "\n";
          print "inside target is file, prereq is target: \n";
        }
        #the target to which is refers is made recursively.
        make($preReq);
      } elsif (isFile($preReq)) {

        if (-C $target < -C $preReq){
          if ($OPTS{'d'}){
            print "\n";
            print "inside target is file, prereq is target, targettime < prereqtime: \n";
          }
          handlecommands($target);
        } else {
          next;
        }
        } else {
          print "bad makefile buddy!";
        }
    }
  } else { #target is not a file (yet or ever)
    foreach my $preReq (@preRequisites){
      if ($OPTS{'d'}){
        print "\n";
        print "$target prereq: $preReq. \n";
      }
      if (isTarget($preReq)) {
        #Prereq is a target
        make($preReq);
      } else {
        #Prereq is not a target
        handlecommands($target);
      }
    }
    handlecommands($target);
  }
}


# -----------------------------------------------------------------------------
my %strsignal = (
    1 => "Hangup",
    2 => "Interrupt",
    3 => "Quit",
    4 => "Illegal instruction",
    5 => "Trace/breakpoint trap",
    6 => "Aborted",
    7 => "Bus error",
    8 => "Floating point exception",
    9 => "Killed",
   11 => "Segmentation fault",
   13 => "Broken pipe",
   14 => "Alarm clock",
   15 => "Terminated",
   16 => "Stack fault",
   17 => "Child exited",
   18 => "Continued",
   19 => "Stopped (signal)",
   20 => "Stopped",
   21 => "Stopped (tty input)",
   22 => "Stopped (tty output)",
   24 => "CPU time limit exceeded",
   25 => "File size limit exceeded",
   26 => "Virtual timer expired",
   27 => "Profiling timer expired",
   28 => "Window changed",
   29 => "I/O possible",
   30 => "Power failure",
   31 => "Bad system call",
);
