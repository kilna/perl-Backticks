#!perl

use strict;
use warnings;

use Scalar::Util qw(blessed);
use Test::More 'tests' => 18;

BEGIN {
    # $Backticks::filter_debug = 1;
    use_ok('Backticks') || print "Bail out!\n";
}

my $n = '\r?\n';

diag("Testing Backticks $Backticks::VERSION, Perl $], $^X");

my $b = `perl -e "print qq{foo\n}"`;
isa_ok( $b, 'Backticks' );
like( $b->stdout, qr/foo\r?\n/, 'output captured' );
like( $b . '',    qr/foo\r?\n/, 'stringification works' );

$b = Backticks->new(
    'perl -e "'
          . 'use Time::HiRes qw(sleep); '
          . 'select (STDERR); '
          . '$| = 1; '
          . 'select (STDOUT); '
          . '$| = 1; '
          . 'print qq{o1\n}; '
          . 'sleep 0.1; '
          . 'print STDERR qq{e1\n}; '
          . 'sleep 0.1; '
          . 'print qq{o2\n}; '
          . 'sleep 0.1; '
          . 'print STDERR qq{e2\n}; '
          . 'sleep 0.1; '
          . 'exit 3;'
          . '"',
    chomped => 1,
);
isa_ok( $b, 'Backticks' );
is( $b->stdout,   '',                            'command was not run yet'  );
$b->run();
like( $b->stdout, qr/^o1\r?\no2$/,               'stdout captured, chomped' );
like( $b->stderr, qr/^e1\r?\ne2$/,               'stderr captured, chomped' );
like( $b->merged, qr/^o1\r?\ne1\r?\no2\r?\ne2$/, 'merged captured, chomped' );
is( $b->exitcode, 3,                             'exit code captured'       );
is( $b->success,  0,                             'success set properly'     );

eval { local $Backticks::autodie = 1; `perl -e "die"`; };
isnt( $@, '', '$Backticks::autodie works' );

eval { `perl -e "die"`; };
is( $@, '', '$Backticks::autodie local worked' );

`command_that_doesnt_exist`;
is( Backticks->success, 0, 'failure when command does not exist' );

my $str = <<EOF;
fake_perl_code();
`fake command`;
EOF
is( $str, "fake_perl_code();\n\x{60}fake command\x{60};\n",
    'heredoc works normally' );

$str = "`command`";
is($str, "\x{60}command\x{60}", 'double quotes work normally' );

$str = '`command`';
is($str, "\x{60}command\x{60}", 'single quotes work normally' );

no Backticks;
$b = `perl -e "print qq{foo\n}"`;
isnt( blessed($b), 'Backticks', '"no Backticks" works' );
