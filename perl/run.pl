#!/usr/bin/perl -w
# $Id: run.perl,v 1.1 2014-10-03 16:57:20-07 - - $
#
# NAME
#    run.perl - run a Unix command and print exit status and signal
#
# SYNOPSIS
#    run.perl command operands...
#
# DESCRIPTION
#    Runs a command and prints the resulting exit status, etc.
#
# EXAMPLE
#    run.perl echo foo bar
#

$0 =~ s{.*/}{};
$EXITCODE = 0;
sub setexit($){
   my( $newexit ) = @_;
   $newexit &= 0xFF;
   $EXITCODE = $newexit if $EXITCODE < $newexit;
};
$SIG{'__WARN__'} = sub{ print STDERR "$0: @_"; setexit 1; };
$SIG{'__DIE__'} = sub{ warn @_; exit; };
END{ exit $EXITCODE; }

# Source: signal(3HEAD)
$SIG[ 1]="SIGHUP: Hangup (see termio(7I))";
$SIG[ 2]="SIGINT: Interrupt (see termio(7I))";
$SIG[ 3]="SIGQUIT: Quit (see termio(7I))";
$SIG[ 4]="SIGILL: Illegal Instruction";
$SIG[ 5]="SIGTRAP: Trace or Breakpoint Trap";
$SIG[ 6]="SIGABRT: Abort";
$SIG[ 7]="SIGEMT: Emulation Trap";
$SIG[ 8]="SIGFPE: Arithmetic Exception";
$SIG[ 9]="SIGKILL: Killed";
$SIG[10]="SIGBUS: Bus Error";
$SIG[11]="SIGSEGV: Segmentation Fault";
$SIG[12]="SIGSYS: Bad System Call";
$SIG[13]="SIGPIPE: Broken Pipe";
$SIG[14]="SIGALRM: Alarm Clock";
$SIG[15]="SIGTERM: Terminated";
$SIG[16]="SIGUSR1: User Signal 1";
$SIG[17]="SIGUSR2: User Signal 2";
$SIG[18]="SIGCHLD: Child Status Changed";
$SIG[19]="SIGPWR: Power Fail or Restart";
$SIG[20]="SIGWINCH: Window Size Change";
$SIG[21]="SIGURG: Urgent Socket Condition";
$SIG[22]="SIGPOLL: Pollable Event (see streamio(7I))";
$SIG[23]="SIGSTOP: Stopped (signal)";
$SIG[24]="SIGTSTP: Stopped (user) (see termio(7I))";
$SIG[25]="SIGCONT: Continued";
$SIG[26]="SIGTTIN: Stopped (tty input) (see termio(7I))";
$SIG[27]="SIGTTOU: Stopped (tty output) (see termio(7I))";
$SIG[28]="SIGVTALRM: Virtual Timer Expired";
$SIG[29]="SIGPROF: Profiling Timer Expired";
$SIG[30]="SIGXCPU: CPU time limit exceeded (see getrlimit(2))";
$SIG[31]="SIGXFSZ: File size limit exceeded (see getrlimit(2))";
$SIG[32]="SIGWAITING: Concurrency signal reserved by threads library";
$SIG[33]="SIGLWP: Inter-LWP signal reserved by threads library";
$SIG[34]="SIGFREEZE: Check point Freeze";
$SIG[35]="SIGTHAW: Check point Thaw";
$SIG[36]="SIGCANCEL: Cancellation signal reserved by threads library";

push @ARGV, '2>&1';
print `@ARGV`;

$exit_value  = ($? >> 8) & 0xFF;
$signal_num  = $? & 0x7F;
$dumped_core = $? & 0x80;
$signal_msg  = $SIG[$signal_num];

$message = "=> exit $exit_value, status $signal_num";
$message .= ", $signal_msg" if $signal_msg;
$message .= " (core dumped)" if $dumped_core;

print "\n$0: @ARGV\n$message\n";

$EXITCODE = $signal_num if $signal_num;
$EXITCODE = $exit_value if $exit_value;
