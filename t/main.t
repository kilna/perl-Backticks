#!perl

use strict;
use warnings;

use Scalar::Util qw(blessed);
use Test::More 'tests' => 14;

BEGIN {

    # $Backticks::filter_debug = 1;
    use_ok('Backticks') || print "Bail out!\n";
}

diag("Testing Backticks $Backticks::VERSION, Perl $], $^X");

my $cmd = `perl -e 'print "foo\n"'`;
isa_ok( $cmd, 'Backticks' );
is( $cmd->stdout, "foo\n", 'output captured' );
is( $cmd . '',    "foo\n", 'stringification works' );

$cmd
    = Backticks->new(
    q^perl -e 'print STDERR "foo!\n"; print "bar!\n"; exit 3'^,
    chomped => 1, );
isa_ok( $cmd, 'Backticks' );
isnt( $cmd->stderr, 'foo!', 'command was not run yet' );
$cmd->run();
is( $cmd->stderr,   'foo!', 'stderr captured, chomped' );
is( $cmd->stdout,   'bar!', 'stdout captured, chomped' );
is( $cmd->exitcode, 3,      'exit code captured' );
is( $cmd->success,  0,      'Success set properly' );

eval { local $Backticks::autodie = 1; `perl -e 'die'`; };
isnt( $@, '', '$Backticks::autodie works' );

eval { `perl -e 'die'`; };
is( $@, '', '$Backticks::autodie local worked' );

`command_that_doesnt_exist`;
is( Backticks->success, 0, 'failure when command does not exist' );

no Backticks;
$cmd = `perl -e 'print "foo\n"'`;
isnt( blessed($cmd), 'Backticks', '"no Backticks" works' );
