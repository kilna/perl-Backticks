package Backticks;

use Exporter qw();
@ISA = qw(Exporter);
@EXPORT_OK = qw(run config rerun reset as_table command error returncode stdout stderr merged coredump exitcode signal error_verbose success);

use 5.006;
use strict;
use warnings;

use Filter::Simple;
use File::Temp qw(tempfile);
use Carp qw(croak);
use Scalar::Util qw(blessed);
use Scope::Upper qw(localize context_info HERE UP);
use IPC::Open3;
use overload '""' => \&_stringify; # Stringification shows the command's STDOUT or merged output 
use constant { FUNCTION => 1, CLASS => 2, INSTANCE => 3 }; # used with _call_type()

# Always report errors from a context outside of this package
$Carp::Internal{ (__PACKAGE__) }++;

our $VERSION = '1.9.0_02';

# Some hashes that store information about Backticks object fields
my (
    %f_defaults,     # Default values for fields
    %f_settable,     # Whether the field is writeable on object creation
    %f_causes_reset, # Whether the setting of the field causes object reset
    %f_resets,       # Whether a field is deleted when the object is reset
    %f_config,       # Fields which indicate configuration
);

sub _init_obj ($@) {
    my $self = shift;
    $self->reset;
    foreach my $var (keys %f_config) {
        my $var = ref($self).'::'.$var;
        my $val = eval { no strict 'refs'; ${$var} };
        defined($val) || $self->_warn(
            "Config package variable \$$var no longer valid in "
            . __PACKAGE__ . ' version ' . $VERSION . ".  Use "
            . __PACKAGE__ . '::config(...) instead'
        );
    }
    return unless @_;
    # Set the command
    $self->_set( 'command', shift @_ );
    # Set all of the fields passed in
    my %params = _parse_config_list(@_);
    $self->_set( $_, $params{$_} ) foreach keys %params;
}

BEGIN {
    # Default values for all object fields
    %f_defaults = (
        # These fields have blank defaults
        ( map { $_ => '' } qw(command error stdout stderr merged) ),
        # These fields have zero defaults
        ( map { $_ => 0  } qw(returncode debug autodie chomped compat merge) ),
        'warnings' => 1,
    );
    # These object fields are settable
    %f_settable = map { $_ => 1 } qw(command debug autodie chomped compat warnings);
    # These settable object fields cause the object to be reset when the
    # trigger field is set
    %f_causes_reset = map { $_ => 1 } qw(command);
    # These object fields are deleted when the ->reset method is called
    %f_resets = map { $_ => 1 } qw(error stdout stderr returncode);
    # These object fields determine object behavior
    %f_config = map { $_ => 1 } qw(autodie chomped compat debug merge warnings);
    # This object stores the current scope's config, and its elements are
    # overridden via localized changes in a given scope
    our $self = bless { %f_defaults }, __PACKAGE__;
    $self->_init_obj;
}

# Implement the source filter in Filter::Simple
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
        # The variable $Backticks::filter_debug indicates that we
        # should print the input source lines as the appear after processing
        $Backticks::filter_debug
            && warn join '', map {"Backticks: $_\n"} split /\n/, $_;
    };

# Returns a constant of FUNCTION, CLASS or INSTANCE depending on how the
# passed in @_ was called
sub _call_type ($) {
    my $args = shift @_; # A reference to @_ in our caller
    my $source = $args->[0];
    return FUNCTION unless defined($source) && $source->isa(__PACKAGE__);
    return INSTANCE if blessed($source);
    return CLASS unless ref($source);
    return FUNCTION; # In theory we should never get here
}

# Find the next calling context (as per Scope::Upper) which isn't related
# to this object
sub _calling_context ($) {
    my $context = HERE;
    while ( my $context = UP $context ) {
        next if (context_info($context))[0]->isa(__PACKAGE__);
    }
    croak "Unable to resolve calling context!";
}

# Resolve to an object regardless of whether a sub was called as
# a class method, object method or a function.  This modifies the
# passed in caller's @_ so that only parameters to the call are left
sub _self ($) {

    my $args = shift @_; # A reference to @_ in our caller
    my $call_type = _call_type $args;

    no strict 'refs';
    my $self;
    if    ( $call_type == INSTANCE ) { $self = shift @$args                  }
    elsif ( $call_type == CLASS    ) { $self = ${shift(@$args).'::last_run'} }
    elsif ( $call_type == FUNCTION ) { $self = ${'Backticks::last_run'}      }

    return wantarray ? ($self, $call_type) : $self;
}

# Generic accessor to get the field for the current object (if called
# as an instance method) or the last run's object (if called as a class
# method)
sub _get (@) {

    # Resolve the object being operated upon (class or instance)
    my $self = _self \@_;
    my $field = shift @_; # The field being requested for this object
    
    exists( $f_defaults{$field} ) || croak "Unrecognized field '$field'";

    # Firstly, try to get the value from the object
    return $self->{$field} if defined( $self->{$field} );
    
    # If not found in context params, then get the value from the local config
    # stored in Backticks::last_run
    if ( $f_config{$field} ) {
        my $var = eval { no strict 'refs'; ${ref($self).'::last_run'}->{$field} };
        return $var if defined( $var );
    }

    # Otherwise return the default value for the field
    return $f_defaults{$field};
}

sub _set (@) {
    # Resolve the object being operated upon (class or instance)
    my ($self, $call_type)  = _self \@_;
    my $field = shift @_; # The field being operated upon for this object
    
    exists( $f_defaults{$field} ) || croak "Unrecognized field '$field'";

    if ( scalar @_ ) {
        croak "Field '$field' cannot be set." unless $f_settable{$field};
        if ( $call_type == INSTANCE || not $f_config{$field} ) {
            $self->{$field} = shift @_;
            $self->reset if $f_causes_reset{$field};
        }
        else {
            localize_elem '%$'.ref($self).'::last_run', $field, shift @_;
        }
    }
}

sub _stringify {
    my $self = _self \@_;
    return $self->merge ? $self->merged : $self->stdout; 
}

=head1 NAME

Backticks - Use `backticks` like objects!

=head1 SYNOPSIS

This module turns backticks into full objects which you can
query in interesting ways.

    use Backticks;

    my $results = `ls -a /`; # Assign a Backticks object to $results

    print $results->stdout;  # Get the command's STDOUT
    print $results->stderr;  # Get the command's STDERR
    print $results->merged;  # Get STDOUT and STDERR together
    print $results->success; # Will be true when command exited clean
    print $results;          # Get the command's STDOUT... the object
                             #   stringifies to the command's output
                             #   so you can use it most places you
                             #   use normal backticks

You can have failed commands automatically die your perl script
                            
    Backticks::config '+autodie';
    `perl -e 'print STDERR "OUCH!\n"; exit 1'`;

Which dies with the following message:

    Error executing `perl -e 'warn "OUCH!\n"; exit 1'`:
    Failed with non-zero exit code 1
    Error output:
    OUCH!
    
You can automatically chomp output:

    Backticks::config '+chomped';
    my $chomped = `perl -e "print qq{Hello\n}"`;

You can even access parameters instantly in object mode by calling methods
immediately after the backticks!
    
    say `echo foo`->stdout;                 # Shows 'foo'
    say `perl -e "warn 'Hello!'"`->stderr;  # Shows 'Hello!'
    say `perl -e "exit 1"`->exitcode;       # Shows '1'
    
You can also use a perl object-oriented interface instead of using the
`backticks` to create objects, the following command is the same as the first
one above:

    my $results = Backticks->run("ls -la /");
    
Alternately, you can create a command and run it later:
    
    my $command = Backticks->new("ls -la /");
    # ... do some stuff
    $command->run();
    
Creating commands as an object affords you the opportunity to override
Backticks config settings, by passing them as hash-style params:

    use Backticks; # Not chomped...
    my $chomped_out = Backticks->run(
        'echo "Hello there!"',
        'chomped' => 1,
    );

=head1 CONFIG PARAMETERS

You can combine multiple parameters:

    Backticks::config '+autodie', '+chomped';

You can also explicitly disable parameters which are enabled
by using a minus:

    Backticks::config '-warnings';

Configuration is local to the block that Backticks::config is run in.

=head2 +autodie

If set, then any command which does not have a true success() will cause
the Perl process to die.

This setting was the original onus for this module.  By setting autodie you can
change a script which as a bunch of unchecked system calls in backticks to
having the results all checked using only two lines of code, a just by use'ing this
module and calling a C<config '+autodie'>
If you want similar functionality for calls to system(),
then use the L<autodie> module.

=head2 +chomped

If set, then STDOUT and STDERR will remove a trailing newline from the
captured contents, if present.

It's very rare when you get output from a command and you don't want its
output chomped, or at least it's rare when chomping will cause a problem.

=head2 +compat

If set, then the subprocess's STDERR will be echoed to the perl
process's STDERR in realtime.  Perl's normal backticks operate this way.

By default Backticks assumes you want to manage the errors after the fact, but 
this may hinder error output that used to show up from being displayed.  
Enable this feature to mimick Perl's native behavior more closely.

=head2 +debug

If set, then additional debugging information will be output to STDERR.

If you are running deployment scripts in which the output of every command
needs to be logged, this can be a handy way of showing everything about each
command which was run.

=head2 +merge

If set, then the object stringifies to the merged output including both STDERR 
and STDOUT, rather than the default of only STDOUT

=head2 -warnings

If unset, then warnings about potential misuse of redirection, depricated 
interfaces, etc. will be disabled.

=cut

sub config (@) {
    my $self = _self \@_;
    my %config = _parse_config_list(@_);
    foreach my $field (keys %config) {
        $self->_set( $field, $config{$field} );
    }
}

sub _parse_config_list {
    my %config = ();
    PARAM: while ( my $param = shift @_ ) {
        foreach my $var (keys %f_config) {
            if ( $param =~ m/^(\+|\-)?\Q$var\E$/ ) {
                $config{$var} = ($1 eq '+') ? 1 : 0;
                next PARAM;  
            }
            elsif ( $param eq $var ) {
                $config{$var} = shift @_;
                next PARAM;  
            }
        }
        croak "Unrecognized config param '$_'";
    }
    return %config;
}


=head2 Backticks->new( 'command' [ , @params ] )

Creates a new Backticks object but does not execute the command yet.
The optional @params list can contain config() settings.  For example:

    my $obj = Backticks->new( 'ls -la', '+autodie' );

=cut

sub new ($@) {
    _call_type(\@_) == CLASS
        or croak "Must be called as a class method!";
    my $self = bless {}, shift @_;
    $self->_init_obj(@_); 
    return $self;
}

=head2 Backticks->run( 'command' [ , @params ] )

Behaves like Backticks->new(...), but before it returns the object 
to the user it executes the command.

=head2 `command`

This is a source filter alias for:

    Backticks->run( 'command' ) 

It will create a new Backticks object, run the command, and return the object
complete with results.  Since Backticks objects stringify to the STDOUT from the
command that was run, the default behavior is very similar to Perl's normal
backticks.

=head2 $obj->run( [ 'command' [ , @params ] ] )

Executes $obj's command.  If it is passed parameters, they are applied to the
object as if passed into ->new() before the command is run.

=cut

sub run (@) {

    my ($self, $call_type) = _self \@_;
    $self->_init_obj(@_);

    $self->_debug_warn( "Executing command `" . $self->command . "`:" );

    # Run in an eval to catch any perl errors
    eval {

        local $/ = "\n";
        
        # Open the command via open3, specifying IN/OUT/ERR streams
        my $pid = open3( \*P_STDIN, \*P_STDOUT, \*P_STDERR, $self->command )
          || die $!;
        
        close P_STDIN; # Close the command's STDIN
        while (1) {
            if ( not eof P_STDOUT ) {
                $self->{'stdout'} .= my $out = <P_STDOUT>;
                $self->{'merged'} .= $out;
            }
            if ( not eof P_STDERR ) {
                $self->{'stderr'} .= my $err = <P_STDERR>;
                $self->{'merged'} .= $err;
                $self->compat && print STDERR $err;
            }
            last if eof(P_STDOUT) && eof(P_STDERR);
        }
        
        waitpid( $pid, 0 ) || die $!;

        if ($?) { $self->{'returncode'} = $? }

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

    # Perform a chomp if requested
    if ( $self->chomped ) {
    # Defined checks are here so we don't auto-vivify the fields...
    # We don't actually use chomp here because on Win32, chomp doesn't
    # nix the carriage return.
        defined( $self->{'stdout'} ) && $self->{'stdout'} =~ s/\r?\n$//;
        defined( $self->{'stderr'} ) && $self->{'stderr'} =~ s/\r?\n$//;
        defined( $self->{'merged'} ) && $self->{'merged'} =~ s/\r?\n$//;
    }

    # Print debugging information
    $self->_debug_warn( $self->as_table );

    # If we are expected to die unless we have a success, then do so...
    if ( $self->autodie && not $self->success ) { croak $self->error_verbose }

    # Make it so we can get at the last command run through class methods
    $Backticks::last_run = $self;

    return $self;
}

=head2 $obj->rerun()

Re-execute $obj's command, and returns the object.

=cut

sub rerun (;$) { _self(\@_)->run() }

=head2 $obj->reset()

Resets the object back to a state as if the command had never been run

=cut

sub reset (;$) {
    my $self = _self \@_;
    delete $self->{$_} foreach grep { $f_resets{$_} } keys %$self;
}

=head2 $obj->as_table()

Returns a summary text table about the command.

=cut

sub as_table (;$$) {
    my $self = _self \@_;
    my $verbose = defined($_[0]) ? $_[0] : 0;
    my $out = '';
    # A private sub for this method...
    sub $add = sub {
        my $name = shift; # Name of the field being displayed
        my $val  = shift; # Value of the field being displayed
        my $res  = shift; # Value resolved to (if applicable)
        # Show undefined values as the string "undef"
        if ( not defined $val ) { $val = 'undef'; }
        if ( defined $res ) { $val = "$val => $res"; }
        # Indent multi-line values
        $val = join( "\n" . ( ' ' x 14 ), split "\n", $val );
        # Append the row
        $out .= sprintf "%-11s : %s\n", $name, $val;
    };
    $add->( 'Command', $self->command );
    $self->error  && $add->( 'Error',  $self->error  );
    $self->stdout && $add->( 'STDOUT', $self->stdout );
    $self->stderr && $add->( 'STDERR', $self->stderr );
    $self->merged && $add->( 'Merged', $self->merged );
    if ($verbose) {
        $add->( 'Debug',   $self->{debug}   => $self->debug   );
        $add->( 'Autodie', $self->{autodie} => $self->autodie );
        $add->( 'Chomped', $self->{chomped} => $self->chomped );
        $add->( 'Compat',  $self->{compat}  => $self->compat  );
    }
    if ( $self->returncode ) {
        $add->( 'Return Code', $self->returncode );
        $add->( 'Exit Code',   $self->exitcode   );
        $add->( 'Signal',      $self->signal     );
        $add->( 'Coredump',    $self->coredump   );
    }
    return $out;
}

=head2 $obj->command()

Returns a string containing the command that this object is/was configured to
run.

=head2 $obj->stdout(), $obj->stderr(), $obj->merged()

Returns a string containing the contents of STDOUT or STDERR of the command
which was run.  If +chomped is true, then this value will lack the trailing
newline if one happened in the captured output.  Merged is the combined output
of STDOUT and STDERR.

=head2 $obj->returncode(), $obj->exitcode(), $obj->coredump(), $obj->signal()

Returns an integer, indicating a $?-based value at the time the command was
run:

=over 4

=item returncode = $?

=item exitcode   = $? >> 8

=item coredump   = $? & 128

=item signal     = $? & 127

=back

=head2 $obj->error()

Returns a string containing a description of any errors encountered while
running the command.

=cut

sub command    (;$) { _self(\@_)->_get( 'command'    ) }
sub error      (;$) { _self(\@_)->_get( 'error'      ) }
sub returncode (;$) { _self(\@_)->_get( 'returncode' ) }
sub stdout     (;$) { _self(\@_)->_get( 'stdout'     ) }
sub stderr     (;$) { _self(\@_)->_get( 'stderr'     ) }
sub merged     (;$) { _self(\@_)->_get( 'merged'     ) }
sub coredump   (;$) { _self(\@_)->returncode & 128     }
sub exitcode   (;$) { _self(\@_)->returncode >> 8      }
sub signal     (;$) { _self(\@_)->returncode & 127     }

=head2 $obj->error_verbose()

Returns a string containing a description of any errors encountered while
running the command, along with the command which was run and STDERR's output.

=cut

sub error_verbose (;$) {
    my $self = _self \@_;
    return '' unless $self->error;
    my $err = "Error executing `" . $self->command . "`:\n" . $self->error;
    if ( $self->stderr ne '' ) { $err .= "\nError output:\n" . $self->stderr }
    return $err;
}

=head2 $obj->success()

Returns a 1 or 0, indicating whether or not the command run had an error or
return code.

=cut

sub success (;$) {
    my $self = _self \@_;
    return ( $self->error eq '' ) ? 1 : 0;
}

=head2 $obj->autodie(), $obj->chomped(), $obj->compat, $obj->debug(), $obj->merge, $obj->warnings

Returns a 1 or 0, for the corresponding config for this object.  If these
configurations have not been explicitly set for the object, it will return
the settings as they are currently set using Backticks::config.

=cut

sub autodie   (;$) { _self(\@_)->_get( 'autodie'   ) }
sub chomped   (;$) { _self(\@_)->_get( 'chomped'   ) }
sub compat    (;$) { _self(\@_)->_get( 'compat'    ) }
sub debug     (;$) { _self(\@_)->_get( 'debug'     ) }
sub merge     (;$) { _self(\@_)->_get( 'merge'     ) }
sub warnings  (;$) { _self(\@_)->_get( 'warnings'  ) }

# Append to this instance or the last run instance's error field
sub _add_error ($@) {
    my $self = _self \@_;
    if ( $self->{'error'} ) { $self->{'error'} .= "\n"; }
    $self->{'error'} .= join "\n", @_;
    chomp $self->{'error'};
}

# Print debugging output to STDERR if debugging is enabled
sub _debug_warn ($@) {
    _self(\@_)->debug || return;
    warn "$_\n" foreach split /\n/, @_;
}

# Print a warning if warnings are enabled
sub _warn ($@) {
    _self(\@_)->warnings || return;
    warn "$_\n" foreach split /\n/, @_;
}

=head1 ACCESSING THE LAST COMMAND

Any of the instance $obj->method's above can also be called as
Backticks::method (or Backticks->method) and will apply to the
last command executed through the Backticks module.  Most of these
functions can be called without parentheses.

Example:

    `run a command`;
    print Backticks::stderr;  # Will show the STDERR for `run a command`!
    print Backticks::success; # Will show success for it...
    
    $foo = Backticks::run 'another command';
    print Backticks::stdout; # Output for the above line

If you want to access the last run object more explicitly, you can find it at:
    
    $Backticks::last_run

=head2 IMPORTING FUNCTIONS

The Backtick module exports no functions into your local namespace by default.  
However, nearly all of the object methods are importable as functions,
which will operate on the last run command in the current block as above.
You can use :all to import all of Backtick's object methods like functions.

For example:

    use Backticks ':all';
    config '+autodie'; # Equivalent to Backticks::config '+autodie'
    `command`;
    if (success) { ... } # Equivalent to `command`->success()
    print stderr(); # Print's the STDERR output of `command`
    
=head1 NOTES

=over 4

=item No redirection

Since we're not using the shell to open subprocesses (behind the scenes we're
using L<open3>) you can't redirect input or output.  But that shouldn't be a
problem, since getting the redirected output is likely why you're using this
module in the first place. ;)
 
=item Source filtering

The overriding of `backticks` is provided by Filter::Simple.  This module
uses well-bound well-tested regexes to replace `backticks` with objects in a
reasonably bulletproof manner, but regardless, source filtering is not for
everyone.

If you want to use this module in a purely traditional Perl OO style, simply
turn off the source filtering as soon as you load the module:

    use Backticks;
    no Backticks;

This way the class is loaded, but `backticks` processing is Perl-native.  You
can still use Backticks->run() or Backticks->new() to create objects after the
"no Backticks" statement.

=item Using Perl's native backticks with the Backticks module

If you want to use Perl's normal backticks functionality in conjunction with
this module's `backticks`, simply use qx{...} instead:

    use Backticks;
    `command`;   # Uses the Backticks module, returns an object
    qx{command}; # Bypasses Backticks module, returns a string

=back

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

Copyright 2015 Kilna Companies.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.

=cut

1;    # End of Backticks
