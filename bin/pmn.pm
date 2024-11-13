# Charles Hawkins at Rhee Lab, Department of Plant Biology, Carnegie
# Institution for Science
#
# Created: April 2022
#
# Description: This perl module defines the function to read the pgdb-table.txt
# (or to-create-PGDBs.txt as it was formerly known) file and put it all into a
# dictionary mapping from orgids (e.g. "ara" for Aracyc) to a dictionary of the
# fields in the file. The header line is required and defines the order of the
# columns. Some columns are computed if they are missing or blank for a given
# organism.

package pmn;
$VERSION="0.1";
use strict;

# @individual_required_fields are required for each organism, and cannot be
# specified using /default
my @individual_required_fields;
@individual_required_fields = ("Database ID");
# @required_fields are required, but can be specified using /default
my @required_fields;
@required_fields = ("Authors", "Curator", "Version", "Citation Year", "Species Name", "NCBI Taxon ID", "Sequence File", "Seq Source");
my @all_required_fields;
@all_required_fields = (@individual_required_fields, @required_fields);
sub read_pgdb_table
{
	my $table_filename = shift;
	my $uids_filename = shift;
	my %pgdb_table;
	my %req_fields_empty;   # Keys are @required_fields; value is true if at least one db has no value for that field
	my %req_fields_default; # Keys are @required_fields; value is true if /default has a value for that field
	open(TABLE, $table_filename) or die "$table_filename: $!\n";

	# We will first read in the whole file and manually split it into lines so
	# that we can be independent of DOS/Unix/Mac newlines (who knows what kind
	# of newlines the user's spreadsheet program will put in). Also causes us
	# to ignore empty lines
	my $table_file;
	local $/=undef; $table_file=<TABLE>;
	my @table_lines = split /[\r\n]+/, $table_file;

	# The first line is the header. We will go through it, check that required
	# fields are present, and then build a map from header fields to column
	# numbers so we know what column is what later
	my $header = $table_lines[0];
	chomp $header;
	my @hfields = split(/\t/, $header);
	# Remove quote marks from around all the fields (spreadsheets like to put
	# quotes around the cells)
	foreach my $f (@hfields)
	{
		$f =~ s/\A"(.*)"\Z/\1/;
	}

	# Check that all required fields are present
	my %hf_hash = map {$_ => 1} @hfields;
	foreach my $f (@all_required_fields)
	{
		if(!exists($hf_hash{$f}))
		{
			die "Field is required in $table_filename: $f\n"
		}
	}

	# Get the index of each named column
	my %field_index;  # Maps from column names to column numbers
	for my $hi (0 .. $#hfields)
	{
		$field_index{$hfields[$hi]} = $hi;
	}

	# Now iterate through the table
	my $line_n = 2;	# Line number, used purely for error messages
	foreach $_ (@table_lines[1..$#table_lines])
	{
		my @fields = split(/\t/);
		# Remove quotes, as in the header
		foreach my $f (@fields)
		{
			$f =~ s/\A"(.*)"\Z/\1/;
		}

		# The Database ID field is what we'll be indexing by
		my $db_id = $fields[$field_index{"Database ID"}];
		#if ($db_id ne "/default")
		if ($db_id !~ /^\//)
		{
			print "checking $db_id\n";
			# Check that this entry has all individually-required fields
			print "@fields\n";
			foreach my $f (@individual_required_fields)
			{
				print "checking field $f\n";
				if(!$fields[$field_index{$f}])
				{
					die "Line $line_n in $table_filename is missing required field: $f\n"
				}
				print "value is $fields[$field_index{$f}]\n";
			}
		}
		my %entry;  # The hash for this species entry; maps from column names to the value in that column for this species
		foreach my $field (keys %field_index)
		{
			$entry{$field} = $fields[$field_index{$field}];
		}
		$pgdb_table{$db_id} = \%entry;
		$line_n ++;
	}

	# Now go through our table and fill in default values from the /default entry
	my $default_entry = $pgdb_table{"/default"};
	foreach my $db_id (keys %pgdb_table)
	{
		my $entry = $pgdb_table{$db_id};
		foreach my $field (keys %field_index)
		{
			if (!$$entry{$field})
			{
				$$entry{$field} = $$default_entry{$field};	# Will remain empty if default_entry doesn't exist or is empty for $field
			}
		}
	}

	# Now fill in computed values
	foreach my $db_id (keys %pgdb_table)
	{
		my $db_entry = $pgdb_table{$db_id};

		$$db_entry{"Database Name"} ||= $$db_entry{"Database ID"}."Cyc";
		my @species_words = split(/ /, $$db_entry{"Species Name"});
		$$db_entry{"ID/Name"} ||= substr($species_words[0], 0, 1).$species_words[1];
		if (!$$db_entry{"Abbrev Name"})
		{
			# The "Abbrev Name" field is, e.g., "A. thaliana col". Unlike
			# "ID/Name" it gets subspecies included but shouldn't have things
			# like "var.", so e.g. Brassica oleracea var. capitata would
			# translate to "B. oleracea capitata". As a short cut, we just
			# remove any words after the species name that end in a ".". We
			# don't apply this to the species name itself (i.e. the first word
			# after the genus) because of things like "Xenia sp."
			my $species_and_subsp = $species_words[1];
			for my $i (2 .. $#species_words)
			{
				if ($species_words[$i] !~ /\.\z/)
				{
					$species_and_subsp .= " ".$species_words[$i];
				}
			}

			$$db_entry{"Abbrev Name"} = substr($species_words[0], 0, 1).". ".$species_and_subsp;
		}
		$$db_entry{"Initial PF File"} ||= $$db_entry{"Sequence File"}.".e2p2.orxn.pf";
		if (!$$db_entry{"PF File"})
		{
			my $pf_file = $$db_entry{"Initial PF File"};
			$pf_file =~ s/(\.pf)?\z/.revised$&/;
			$$db_entry{"PF File"} = $pf_file;
		}
		my $year = $$db_entry{"Citation Year"};
		$$db_entry{"SAVI Citation"} ||= "|PUB-PMNUPP$year| |PUB-PMNAIPP$year| |PUB-PMNRXN$year| |PUB-PMNRXNTAXON$year| |PUB-PMNTAXON$year| |PUB-PMNIC$year| |PUB-PMNSP$year|";
		$$db_entry{"E2P2 Citation"} ||= "|PUB-E2P2PMN$year|";
		$$db_entry{"Enzrxn Citation"} ||= "E2P2PMN$year:EV-COMP-AINF";
		$$db_entry{"ENZ name file"} ||= "ec_name.map.parsed";
		$$db_entry{"RXN Map"} ||= "metacyc-rxn-name-mapping";
		$$db_entry{"PWY Metacyc"} ||= "all_pwy.meta";
		$$db_entry{"PWY Plantcyc"} ||= "all_pwy.plant";
		$$db_entry{"Reference DB"} ||= "Plant";
	}
	delete $pgdb_table{"/default"};
	return %pgdb_table;
}
