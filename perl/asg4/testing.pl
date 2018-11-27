use strict;
use warnings;
use Getopt::Std;
use Data::Dumper;
use File::stat;

my $command = "@ here's the command.";

my $noExit = index($command, '-');
my $noEcho = index($command, '@');

if ($noExit >= 0){
  $command = substr($command, $noExit+2);
}

if ($noEcho >= 0){
  $command = substr($command, $noEcho+2);
}

print $command;
print "\n";
print "--------------------------------------------------------------------\n";
print "--------------------------------------------------------------------\n";


#'${[brackets] [brackets]}';
