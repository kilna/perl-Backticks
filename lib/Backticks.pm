package Backticks;

use 5.006;
use strict;
use warnings;

use Filter::Simple;
use File::Temp qw(tempfile);
use Carp qw(croak);
use Scalar::Util qw(blessed);
use Class::ISA;
use overload '""' => \&stdout;    # Object stringifies to command's stdout

$Carp::Internal{ (__PACKAGE__) }++;

=head1 NAME

Backticks - Use `backticks` like objects!

=cut

our $VERSION = '1.0.4';

=head1 SYNOPSIS

This module turns backticks into full objects which you can
query in interesting ways.

    use Backticks;

    my $results = `ls -a /`; # Assign a Backticks object to $results

    print $results->stdout;  # Get the command's STDOUT
    print $results->stderr;  # Get the command's STDERR
    print $results->success; # Will be true when the command exited clean
    print $results;          # Get the command's STDOUT... the object
                             #   stringifies to the command's output so you 
                             #   can use it most places you use normal
                             #   backticks

You can have failed commands automatically die your perl script
                            
    $Backticks::autodie = 1;
    `perl -e 'print STDERR "OUCH!\n"; exit 1'`; 

Which dies with the following message:

    Error executing `perl -e 'print STDERR "OUCH!\n"; exit 1'`:
    Failed with non-zero exit code 1
    Error output:
    OUCH!
    
You can automatically chomp output:

    $Backticks::chomped = 1;
    my $chomped = `perl -e 'print "Hello!\n"'`;

You can even access parameters instantly in object mode by calling methods
immediately after the backticks!
    
    say `echo foo`->stdout;                              # Shows 'foo'
    say `perl -e 'print STDERR "Hello world!"'`->stderr; # Shows 'Hello world'
    say `perl -e 'exit 1'`->exitcode;                    # Shows '1'
    
You can also use the classical perl object-oriented interface instead of
using the backticks to create objects, the following command is the same as
the first one above:

    my $results = Backticks->run("ls -la /");
    
Alternately, you can create a command and run it later:
    
    my $command = Backticks->new("ls -la /");
    # ... do some stuff
    $command->run();
    
Creating commands as an object affords you the opportunity to override
Backticks package settings, by passing them as hash-style params:

    $Backticks::chomped = 0;
    my $chomped_out = Backticks->run(
        'echo "Hello there!"',
        'chomped' => 1,
    );

=head1 PACKAGE VARIABLES

=head2 $Backticks::autodie

If set to 1, then any command which does not have a true success() will cause
the Perl process to die.  Defaults to 0.

=head2 $Backticks::chomped

If set to 1, then STDOUT and STDERR will remove a trailing newline from the
captured contents, if present.  Defaults to 0.

=head2 $Backticks::debug

If set to 1, then additional debugging information will be output to STDERR.
Defaults to 0.

=cut

# Only class fields are settable (via class method calls)
# ... those are the ones with the 'package' definitions.
my %field_info = (

    # Instance variables
    'command'    => { 'default' => '' },
    'error'      => { 'default' => '' },
    'stdout'     => { 'default' => '' },
    'stderr'     => { 'default' => '' },
    'returncode' => { 'default' => 0 },

    # Instance-overrideable class variables
    'debug' => {
        'default' => 0,
        'package' => 1,
        'check'   => sub {
            return if ( defined $_[0] && ( $_[0] != 1 || $_[0] != 0 ) );
            croak "Value for 'debug' of '$_[0]' is not a 1 or 0";
        },
    },
    'autodie' => {
        'default' => 0,
        'package' => 1,
        'check'   => sub {
            return if ( defined $_[0] && ( $_[0] != 1 || $_[0] != 0 ) );
            croak "Value for 'autodie' of '$_[0]' is not a 1 or 0";
        },
    },
    'chomped' => {
        'default' => 0,
        'package' => 1,
        'check'   => sub {
            return if ( defined $_[0] && ( $_[0] != 1 || $_[0] != 0 ) );
            croak "Value for 'chomped' of '$_[0]' is not a 1 or 0";
        },
    },
);

FILTER_ONLY quotelike => sub {
    s{^`(.*?)`$}
         {
            my $cmd = $1;
            $cmd =~ s|\\|\\\\|gs;
            $cmd =~ s|"|\\"|gs;
            "Backticks->run(\"$cmd\")";
         }egsx;
    },
    all => sub {
    $Backticks::filter_debug
        && print STDERR join '', map {"Backticks: $_\n"} split /\n/, $_;
    };

# Determine if we're being called as a valid
# class or instance method...  if neither then die
sub _class_method (@) {
    my $source = $_[0];
    if ( blessed $source ) {
        return 0 if $source->isa('Backticks');
    }
    elsif ( defined $source && not ref $source ) {
        return 1
            if scalar( grep { $_ eq 'Backticks' }
                Class::ISA::self_and_super_path($source) );
    }
    croak "Must be called as a class or instance method";
}

# Get this object or the last run's object depending on if this
# was called as a class method or instance method
sub _self (@) {
    if ( _class_method(@_) ) {
        defined($Backticks::last_run)
            || croak "No previous Backticks command was run";
        return $Backticks::last_run;
    }
    return $_[0];
}

# Generic accessor
sub _field ($$;$) {
    my $self  = _self( shift @_ );
    my $field = shift @_;
    defined( $field_info{$field} ) || croak "Unrecognized field '$field'";
    my %info = %{ $field_info{$field} };
    $Backticks::default_debug
        && print STDERR eval { use Data::Dumper; Dumper( \%info ) };
    if ( scalar @_ ) {

        # Allow setting of class variables or class-overriding instance
        # variables ( regular instance variables are only set at the time
        # the command is run, by the ->new method )
        unless ( defined $info{'package'} && $info{'package'} ) {
            croak "Field '$field' cannot be set";
        }
        my $newval = shift;
        if ( defined $info{'check'} ) { $info{'check'}->($newval); }
        $self->{$field} = $newval;
    }
    no strict 'refs';
    if ( defined $self->{$field} ) {
        $Backticks::default_debug
            && print STDERR "'$field' from self = '"
            . $self->{$field} . "'\n";
        return $self->{$field};
    }
    elsif (defined $info{'package'}
        && $info{'package'}
        && defined ${ 'Backticks::' . $field } )
    {
        $Backticks::default_debug
            && print STDERR "'$field' from package = '"
            . ${ 'Backticks::' . $field } . "'\n";
        return ${ 'Backticks::' . $field };
    }
    elsif ( defined $info{'default'} ) {
        $Backticks::default_debug
            && print STDERR "'$field' from default = '"
            . $info{'default'} . "'\n";
        return $info{'default'};
    }
    else {
        $Backticks::default_debug
            && print STDERR "No default defined for field '$field'";
        return '';
    }
}

=head1 CLASS METHODS

=head2 Backticks->new( 'command', [ %params ] )

Creates a new Backticks object but does not run it yet.  %params may contain
boolean values for this instance's 'debug', 'autodie' and 'chomped' settings.

=cut

sub new {

    _class_method(@_) || croak "Must be called as a class method!";
    my $class   = shift;
    my $command = shift;
    my %params  = @_;

    my $self = bless { 'command' => $command }, $class;
    $self->_field( $_, $params{$_} ) foreach keys %params;

    return $self;
}

=head2 `command`

=head2 Backticks->run( 'command', %params )

Behaves exactly like Backticks->new(...), but after the object is created it
immediately runs the command before returning the object.

=head1 OBJECT METHODS

=head2 $obj->run()

Runs (or if the command has already been run, re-runs) the $obj's command,
and returns the object.  Note this is the only object method that can't be
called in class context (Backticks->run) to have it work on the last executed
command as described in the "Accessing the Last Run" secion below.  If you
need to re-run the last command, use Backticks->rerun instead.

=cut

sub run {

    # Get a new object if called as a class method or the
    # referenced object if called as an instance method
    my $self = _class_method(@_) ? new(@_) : $_[0];

    $self->reset;

    $self->_debug_print( "Executing command `" . $self->command . "`:" );

    # Redirecting stdout/stderr to files was the most consistent
    # cross-platform way of capturing output.  It's inefficient and
    # kinda kludgy but it works!

    ( undef, my $outfile ) = tempfile();    # Redirect file for STDOUT
    ( undef, my $errfile ) = tempfile();    # Redirect file for STDERR

    # Run in an eval to catch any perl errors
    eval {

        my $null;
        my $cmd = $self->command . " 1>$outfile 2>$errfile";
        $null = qx{$cmd};

        # Prep the results hash
        if ($?) { $self->{'returncode'} = $? }

        # Complain if we got anything to stdout for the command (it should
        # have been captured to a temp file per the open command above)
        if ( $null ne '' ) {
            die "Unexpected output after redirection:\n" . $null;
        }

        # Read in the redirected STDOUT file contents
        if ( open my $OUT_FILE, '<', $outfile ) {
            my $stdout = join '', <$OUT_FILE>;
            if ( $stdout ne '' ) { $self->{'stdout'} = $stdout }
            close $OUT_FILE;
        }
        else {
            die "Unable to open STDOUT file $outfile: $!\n";
        }

        # Read in the redirected STDERR file contents
        if ( open my $ERR_FILE, '<', $errfile ) {
            my $stderr = join '', <$ERR_FILE>;
            if ( $stderr ne '' ) { $self->{'stderr'} = $stderr }
            close $ERR_FILE;
        }
        else {
            die "Unable to open STDERR file $errfile: $!\n";
        }
    };

    if ($@) {

        # If $@ was set then perl had a problem running the command
        $self->_add_error($@);
    }
    elsif ( $self->returncode == -1 ) {

        # If we got a return code of -1 then we weren't able to run the
        # command (the most common cause of this is the command didn't exist
        # or we didn't have permissions to run it)
        $self->_add_error("Failed to execute: $!");
    }
    elsif ( $self->signal ) {

        # If we have a non-zero signal then the command went askew
        my $err = "Died with signal " . $self->signal;
        if ( $self->coredump ) { $err .= " with coredump"; }
        $self->_add_error($err);
    }
    elsif ( $self->exitcode ) {

        # If we have a non-zero exit code then the command went askew
        $self->_add_error(
            "Failed with non-zero exit code " . $self->exitcode );
    }

    # Remote the temp files used for the command's STDOUT and STDERR
    if ( -e $outfile ) { unlink $outfile; }
    if ( -e $errfile ) { unlink $errfile; }

    # Perform a chomp if requested
    if ( $self->chomped ) {
        defined( $self->{'stdout'} ) && chomp $self->{'stdout'};
        defined( $self->{'stderr'} ) && chomp $self->{'stderr'};
    }

    # Print debugging information
    $self->_debug_print( $self->as_table );

    # If we are expected to die unless we have a success, then do so...
    if ( $self->autodie && not $self->success ) { croak $self->error_verbose }

    # Make it so we can get at the last command run through class methods
    $Backticks::last_run = $self;

    return $self;
}

=head2 $obj->rerun()

Re-runs $obj's command, and returns the object.

=cut

sub rerun { _self(@_)->run }

=head2 $obj->reset()

Resets the object back to a state as if the command had never been run

=cut

sub reset {
    my $self = _self(@_);
    delete $self->{$_} foreach qw(error stdout stderr returncode);
}

=head2 $obj->as_table()

Returns a summary text table about the command.

=cut

sub as_table {
    my $self = _self(@_);
    my $p    = sub {
        my $val = shift;
        if ( not defined $val ) { $val = 'undef'; }
        return join( "\n" . ( ' ' x 14 ), split "\n", $val );
    };
    my $out = '';
    $out .= "Command     : " . $p->( $self->command ) . "\n";
    if ( $self->error ) {
        $out .= "Error       : " . $p->( $self->error ) . "\n";
    }
    if ( $self->stdout ne '' ) {
        $out .= "STDOUT      : " . $p->( $self->stdout ) . "\n";
    }
    if ( $self->stderr ne '' ) {
        $out .= "STDERR      : " . $p->( $self->stderr ) . "\n";
    }
    if ( $self->returncode ) {
        $out .= "Return Code : " . $p->( $self->returncode ) . "\n";
        $out .= "Exit Code   : " . $p->( $self->exitcode ) . "\n";
        $out .= "Signal      : " . $p->( $self->signal ) . "\n";
        $out .= "Coredump    : " . $p->( $self->coredump ) . "\n";
    }
    return $out;
}

=head2 $obj->command()

Returns a string containing the command that this object is/was configured to
run.

=head2 $obj->stdout(), $obj->stderr()

Returns a string containing the contents of STDOUT or STDERR of the command
which was run.  If chomped is true, then this value will lack the trailing
newline if one happened in the captured output.

=head2 $obj->returncode(), $obj->exitcode(), $obj->coredump(), $obj->signal()

Returns an integer, indicating a $?-based value at the time the command was
run:

	* returncode = $?
    * exitcode   = $? >> 8
    * coredump   = $? & 128
    * signal     = $? & 127

=head2 $obj->error(), $obj->error_verbose()

Returns a string containing a description of any errors encountered while
running the command.  In the case of error_verbose, it will also contain the
command which was run and STDERR's output.

=cut

sub command    { _field( shift(@_), 'command' ) }
sub error      { _field( shift(@_), 'error' ) }
sub returncode { _field( shift(@_), 'returncode' ) }
sub stdout     { _field( shift(@_), 'stdout' ) }
sub stderr     { _field( shift(@_), 'stderr' ) }
sub coredump { _self(@_)->returncode & 128 }
sub exitcode { _self(@_)->returncode >> 8 }
sub signal   { _self(@_)->returncode & 127 }

sub error_verbose {
    my $self = shift;
    return '' unless $self->error;
    my $err = "Error executing `" . $self->command . "`:\n" . $self->error;
    if ( $self->stderr ne '' ) { $err .= "\nError output:\n" . $self->stderr }
    return $err;
}

=head2 $obj->success()

Returns a 1 or 0, indicating whether or not the command run had an error or
return code.

=cut

sub success {
    my $self = _self(@_);
    return 0 if ( $self->error ne '' );
    return 0 if ( $self->returncode == 0 );
    return 1;
}

=head2 $obj->autodie(), $obj->chomped(), $obj->debug()

Returns a 1 or 0, if the corresponding $Backticks::xxx variable has been
overridden within this object (as passed in as parameters during ->new()).
Otherwise it will return the value of the corresponding $Backticks::xxx field
as default.

=cut

sub autodie    { _field( shift(@_), 'autodie' ) }
sub chomped    { _field( shift(@_), 'chomped' ) }
sub debug      { _field( shift(@_), 'debug' ) }

# Appent to this instance or the last run instance's error field
sub _add_error {
    my $self = _self( shift @_ );
    if ( $self->{'error'} ) { $self->{'error'} .= "\n"; }
    $self->{'error'} .= join "\n", @_;
    chomp $self->{'error'};
}

# Print debugging output to STDERR if debugging is enabled
sub _debug_print {
    _self( shift @_ )->debug || return;
    print STDERR "$_\n" foreach split /\n/, @_;
}

=head1 ACCESSING THE LAST RUN

Any of the instance $obj->method's above can also be called as
Backticks->method and will apply to the last command run through the Backticks
module.  So:

    `run a command`;
    print Backticks->stderr;  # Will show the STDERR for `run a command`!
    print Backticks->success; # Will show success for it...
    
    $foo = Backticks->run('another command');
    print Backticks->stdout; # Output for the above line

If you want to access the last run object more explicitly, you can find it at:
    
    $Backticks::last_run
    
=head1 CAVEATS

We capture output through redirection to temp files, so you can't use
redirection in your backticks or you will get unexpected results.  This also
means there's a speed penatly as compared to plain perl backticks, but it's the
only way to capture both STDOUT and STDERR that worked consistently
cross-platform and cross-perl-versions.
    
The overriding of `backticks` is provided by Filter::Simple.  Source filtering
can be weird sometimes...   if you want to use this module in a purely
traditional Perl OO style, simply turn off the source filtering as soon as you
load the module:

    use Backticks;
    no Backticks; # Module is still loaded, but `backticks` are perl's built-in
                  # ones...  use Backticks->run() or Backticks->new() to create
                  # objects now.
    
If you want to use Perl's normal backticks functionality in conjunction with
this module's `backticks`, simply use qx{...} instead:

    use Backticks;
    `command`;   # Goes through the Backticks modules and returns an object
    qx{command}; # Bypasses the Backticks module, returns a string

The module's variables are shared everywhere it's used within a perl runtime.
If you want to make sure that the setting of a Backticks variable is limited to
the scope you're in, you should use 'local':

    local $Backticks::chomped = 1;
    
This will return $Backticks::chomped to whatever its prior state was once it
leaves the block.
    
=head1 AUTHOR

Anthony Kilna, C<< <anthony at kilna.com> >> - L<http://anthony.kilna.com>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-backticks at rt.cpan.org>,
or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Backticks>.  I will be
notified, and then you'll automatically be notified of progress on your
bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Backticks

You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Backticks>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Backticks>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Backticks>

=item * Search CPAN

L<http://search.cpan.org/dist/Backticks/>

=back

=head1 LICENSE AND COPYRIGHT

Copyright 2012 Kilna Companies.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.

=cut

1;    # End of Backticks
