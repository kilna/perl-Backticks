#!perl

use strict;
use warnings;

use Scalar::Util qw(blessed);
use Test::More 'tests' => 26;
use Test::Output;
use Capture::Tiny ':all';

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
          . 'sub z(){select(undef,undef,undef,0.1)}' # Used as a sleep .1 sec here
          . 'select(STDERR);'
          . '$|=1;'
          . 'select(STDOUT);'
          . '$|=1;'
          . 'print qq{o1\n};'
          . 'z;'
          . 'warn qq{e1\n};'
          . 'z;'
          . 'print qq{o2\n};'
          . 'z;'
          . 'warn qq{e2\n};'
          . 'z;'
          . 'exit 3;'
          . '"',
);
isa_ok( $b, 'Backticks' );
is( $b->stdout,   '',                            'command was not run yet'    );
Backticks::config 'chomped';
is( $b->chomped,  1,                             'chomped was set via config' ); 
eval {
    Backticks::config qw(-chomped);
    is($b->chomped,  0,                          'config -param in eval works');
};
is($b->chomped,   1,                             'scoping works'              );
$b->run();
like( $b->stdout, qr/^o1\r?\no2$/,               'stdout captured, chomped'   );
like( $b->stderr, qr/^e1\r?\ne2$/,               'stderr captured, chomped'   );
like( $b->merged, qr/^o1\r?\ne1\r?\no2\r?\ne2$/, 'merged captured, chomped'   );
is( $b->exitcode, 3,                             'exit code captured'         );
is( $b->success,  0,                             'success set properly'       );

eval {
    local $Backticks::autodie = 1;
    `perl -e "die"`;
};
like(
    $@,
    qr/^\QError executing `perl -e "die"`:\E/,
    'local $Backticks::autodie worked'
);

`command_that_doesnt_exist`;
is( Backticks->success, 0, 'failure when command does not exist' );

# Set some stuff up for matching later.
my $fc = "\x{60}fake command\x{60}";
my $ml = "fake_perl_code();\n$fc;\n";

my $str = <<EOF;
fake_perl_code();
`fake command`;
EOF
is( $str, $ml, 'un-quoted heredoc works as expected' );

$str = <<'EOF';
fake_perl_code();
`fake command`;
EOF
is( $str, $ml, 'single-quoted heredoc works as expected' );

$str = <<"EOF";
fake_perl_code();
`fake command`;
EOF
is( $str, $ml, 'qouble-quoted heredoc works as expected' );

$str = <<`EOF`;
perl -e "print qq{foo\n}"
EOF
is( $str, "foo\n", 'backticks-quoted heredoc works as expected' );

$str = "`fake command`";
is($str, $fc, 'double quotes work as expected' );

$str = '`fake command`';
is($str, $fc, 'single quotes work as expected' );

my $obj;
my ($stdout, $stderr, $exit) = capture {
    Backticks::config 'compat';
    $obj = `perl -e 'print STDERR "Ignore this line\n"'`
};
is( $stderr, "Ignore this line\n", 'compat mode output works' );
is( $obj->stdout, '', 'compat mode stdout works' );
is( $obj->stderr, "Ignore this line", 'compat mode stderr works' );

no Backticks;
$b = `perl -e "print qq{foo\n}"`;
isnt( blessed($b), 'Backticks', '"no Backticks" works' );

