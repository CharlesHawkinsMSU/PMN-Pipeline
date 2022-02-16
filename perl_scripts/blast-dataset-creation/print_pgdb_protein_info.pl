use strict;
#use lib ('/home/achi1/handle_ptools/perlcyc_1.2/');
use lib ('/home/pzhang/perlcyc_1.2/');
use perlcyc;

### modified from /home/achi1/PGDB2BlastDB/scripts/print_aracyc_pgdb_info.pl, to include DBLink-uniprot info. 

### modified again to include accession-1, this is the phytozome sequence id used in E2P2's fasta sequence file. 

###revised on 6/28/2013. previously $enz included complexes, the benefit of including complexes was to retrieve enzrxn name but that was not propagated to monomers in the code. revised the script so that $enz only includes monomers |Polypipetides|. 

my $DB= $ARGV[0];
my $cyc = perlcyc -> new ($DB);
#my $species = "Populus trichocarpa";
#my $species = "Manihot esculenta";
#my $species = "Vitis vinifera";
#my $species = "Glycine max";
#my $species = "Zea mays";
#my $species = "Chlamydomonas reinhardtii";
#my $species = "Physcomitrella patens";
#my $species = "Selaginella moellendorffii";
#my $species = "Carica papaya";
my $species = $ARGV[1];
$species =~ s/_/ /g;

print "Enzyme\tEnzyme_common_name\tEnzrxn_common_name\tSpecies\tGene\tGene_names\tUniProt_id\tAccession\n";

my @ENZ=$cyc->get_class_all_instances("|Polypeptides|");
foreach my $enz (@ENZ){
    my $enz_common_name = $cyc->get_slot_value($enz,"common-name");
    my $g = $cyc ->get_slot_value ($enz,"gene");
    #my @GeneFrames = $cyc ->get_slot_values($enz,"gene");
    my @uniprots;
    my @DBlinks =  $cyc -> get_dblinks($enz);
    if(@DBlinks){
        foreach my $dblink (@DBlinks){
            if($dblink =~ m/UNIPROT/){
                my $uniprot = $dblink;
                $uniprot =~ s/\(UNIPROT "//;
                $uniprot =~ s/".*$//;
		push(@uniprots,$uniprot);
	    }
	}   
    }
    my $uniprot= join("; ",@uniprots);
    
    if(!$enz_common_name){
	#$enz_common_name = "No-protein-common-name";
    }
    $enz_common_name =~ s/\/\//; /g;
    $enz_common_name =~ s/<.*?>//g;
    my @Genes;
    my %Gene_names;
    #my @accession_1;
    #foreach my $g (@GeneFrames){
    my $accession_1 = $cyc->get_slot_value($g, "accession-1");
    #push @accession_1, $accession_1;
    my $cn=$cyc->get_slot_value($g,"common-name");
    #If the common name is an AGI capitalize it
    if($cn =~ m/at\d.\d{5}/i){
	$cn = uc $cn;
    }
    if($Gene_names{$cn}){
	#print "Found Already: $cn\n";
    }
    else {
	$Gene_names{$cn}=1;
    }
    my @Syn= $cyc->get_slot_values($g, "synonyms");
    foreach my $syn (@Syn){
	if($syn =~ m/at\d.\d{5}/i){
	    $syn = uc $syn;
	}
	$Gene_names{$syn}=1;
    }
    
#Get rid of duplicates of gene names
    foreach my $gcn (keys %Gene_names){
	if($gcn){push(@Genes,$gcn);}
    }
    #Get all the unique common-names associated with the enzrxns of a enzyme
    my @Enzrxns = $cyc->get_slot_values($enz, "catalyzes");
    my %Enzrxn_common_name;
    foreach my $enzrxn(@Enzrxns){
	my $cn = $cyc->get_slot_value($enzrxn,"common-name");
#	print "$cn\n";
	$Enzrxn_common_name{$cn}=1;
    }
    
    #Getting rid of duplicates of enrxn-common-names
    my @Enzrxn_cns;
    foreach my $enzrxn_cn (keys %Enzrxn_common_name){
	#clean the tags
	$enzrxn_cn =~ s/<.*?>//g;
	$enzrxn_cn =~ s/&(.*?);/$1/g;
	if($enzrxn_cn) {
#	    print "$enzrxn_cn\n";
	    push(@Enzrxn_cns,$enzrxn_cn);
	}
    }
    
    #my $accession_1 = join ("; ", @accession_1);
    #my $gene_frame = join("; ",@GeneFrames);
    my $gene_cn_str= join("; ",@Genes);
    my $enzrxn_cn_str = join("; ",@Enzrxn_cns);
    #if(!$enzrxn_cn_str){$enzrxn_cn_str  ="No-enzrxn-common-name"}
    print "$enz\t$enz_common_name\t$enzrxn_cn_str\t$species\t$g\t$gene_cn_str\t$uniprot\t$accession_1\n";
}


exit;
