#!/usr/bin/perl -w 
#
# Create Harmonie namelists
#
# -a will add a configuration to working hash
# -r will remove a namelist from the working hash
#
# The result is dependent on the order of the arguments
#

# Init
$default = "empty";
$mode    = "add";

if ( $ENV{HM_LIB} ) { push @INC,"$ENV{HM_LIB}/nam"; }

( @ARGV ) or die "Usage gen_namelist.pl -n namelist-module [-a] config(s) [-r namelists(s)] \n";

LOOP : foreach ( @ARGV ) {

 $testop  = $_ ;

 if ( $testop eq '-r' ) { $mode = 'remove'   ; next LOOP ; } ;
 if ( $testop eq '-a' ) { $mode = 'add'      ; next LOOP ; } ;
 if ( $testop eq '-n' ) { $mode = 'namelist' ; next LOOP ; } ;

 if ( $mode eq 'namelist' ) {
   require $testop or die " Could not find ".$testop." \n";
 }

 if ( $mode eq 'add' ) {

    # Add namelists options

    for $key ( sort keys %${testop} ) {
         for $role ( sort keys %{ ${$testop}{$key} } ) {
             ${$default}{$key}{$role}=${$testop}{$key}{$role};
         } ;
    } ;
 
 } ;

 if ( $mode eq 'remove' ) { 

    # Remove the specified namelist
 
    delete ${$default}{$testop} ; 

 } ;

 # Set default mode to be add
 if ( $mode eq 'namelist' ) {
  $mode='add';
 }
} ;

# Print the final list

for $key ( sort keys %${default} ) {
     print "\&$key\n";
     for $role ( sort keys %{ ${$default}{$key} } ) {
         ${$default}{$key}{$role} =~ s/,$//;
         print "   $role=${$default}{$key}{$role},\n";
     } ;
     print "\/\n";
} ;

exit
