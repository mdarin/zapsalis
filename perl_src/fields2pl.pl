#!/usr/bin/env perl -w 
use warnings;
#use strict;
#use encoding 'utf8';
#use encoding 'cp1251';
use Encode qw(decode encode);
use Getopt::Long;

my @names;
my ($LOOKING,$INSTATE)=(0,1);
my $state=$LOOKING;

# получить аргументы командной строки
my %options;
GetOptions("values=s" => \$options{"values"}) 
	or die "invalid values:$!";

#if defined($options{"values"});
my %values = map { split ':', $_, 2 } split ',',$options{"values"};

while (<>) {
	if (($state==$LOOKING) && m/---/) {
    $state=$INSTATE;
  }
  if (($state==$INSTATE) && m/^FieldName:\s+(.*)$/) {    	
		push(@names,$1);
    $state=$LOOKING;
  }
}


print '$fields={',"\n";
print join(",\n",map {
		my $value = pop @ARGV || $_;
		#sprintf("\t'%s' => q{%s}",$value,$values{$value});
		#sprintf("\t'%s' => q{%s}",$value,encode("utf8", $values{$value}));
		#sprintf("\t'%s' => q{%s}",$value,encode("iso-8859-5", $values{$value}));
		sprintf("\t'%s' => q{%s}",$value,encode("cp1251", $values{$value}));
	} @names);
print "\n};\n";

