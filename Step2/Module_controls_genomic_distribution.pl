#!/usr/bin/perl

if ($ARGV[1] eq '')
{
    print "Please enter script arguments (e.g., ./Module_controls_genomic_distribution.pl Ce10_oris.ech2.40a.40b.40c.3rep.gff Chromosome_organization.dat). The first argument is the name of a gff file containing the genomic coordinates of (real or random) boxes. The second argument is the name of a CSV file containing the description of C. elegans chromosome centers and arms.\n";
}
else
{
    open(ORGANIZATION,$ARGV[1]);
    while(<ORGANIZATION>)
    {
	chomp;
	if ($_ !~ /^Chromosome,/)
	{
	    @array=split(',',$_);
	    $chr=$array[0];
	    $left_arm_end{$chr}=$array[1];
	    $center_end{$chr}=$array[2];
	    $right_arm_end{$chr}=$array[3];
	}
    }
    close(ORGANIZATION);
    
    open(GFF,$ARGV[0]);
    while(<GFF>)
    {
	chomp;
	@array=split('\t',$_);
	($chr,$start,$end)=($array[0],$array[3],$array[4]);
	$length=$end-$start+1;
	if ($end <= $left_arm_end{$chr})
	{
	    $left_arm{$chr}=$left_arm{$chr}." ".$length;
	}
	else
	{
	    if ($end <= $center_end{$chr})
	    {
		$center{$chr}=$center{$chr}." ".$length;
	    }
	    else
	    {
		$right_arm{$chr}=$right_arm{$chr}." ".$length;
		if ($chr eq 'chrX')
		{
		    print "chr=$chr start=$start end=$end\n";
		}
	    }
	}
    }
    close(GFF);

    foreach $chr (keys %left_arm)
    {
	if ($left_arm{$chr} ne '')
	{
	    open(LEFT,">Left_arm_".$chr."_from_".$ARGV[0].".dat");
	    $_=$left_arm{$chr};
	    s/^ //;
	    s/ /\n/g;
	    print LEFT "$_\n";
	    close(LEFT);
	}
	if ($center{$chr} ne '')
	{
	    open(CENTER,">Center_".$chr."_from_".$ARGV[0].".dat");
	    $_=$center{$chr};
	    s/^ //;
	    s/ /\n/g;
	    print CENTER "$_\n";
	    close(CENTER);
	}
	if ($right_arm{$chr} ne '')
	{
	    open(RIGHT,">Right_arm_".$chr."_from_".$ARGV[0].".dat");
	    $_=$right_arm{$chr};
	    s/^ //;
	    s/ /\n/g;
	    print RIGHT "$_\n";
	    close(RIGHT);
	}
    }
}
