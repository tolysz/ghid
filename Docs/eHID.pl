#!/usr/bin/env perl
use LWP::UserAgent;
my $lwp = new LWP::UserAgent;
print STDERR "Please consider using your local copy:\n\thttps://code.google.com/p/ghid/\n\n";
undef $/;
print $lwp->post('http://gem4.tolysz.co.uk/parse/', { from => shift,  to => shift,  comment => <STDIN>,})->content;
