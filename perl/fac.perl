#!/usr/bin/perl
sub fac($){
	my ($n) = @_;
	my $f = 1;
	my $f = 1;
	return undef if $n < 0;
	while ($n > 1) { $f *= $n; --$n }
	return $f;
}

for my $n (15, 10, 5, 0, -1) {
	my $f = fac $n;
	print "fac $n =";
	print defined $f ? $f: "undef"; 
	print "\n";
}

