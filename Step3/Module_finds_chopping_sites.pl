#!/usr/bin/perl

$WINDOW=10000;
$MINIMAL_CUT=2800; #minimal number of lines in output WIG files

if ($ARGV[0] eq '')
{
    print "Please enter script argument (input .wig file; e.g., ./Module_finds_chopping_sites.pl Priority_order/01_H4K20ME1_Ab2_EEMB_NormLis_1.wig).\n";
}
else
{
    @ORI_file_list=`ls /mnt/data/home/herve.seitz/Analyses_for_paper_July2015/Maximum_probe_coordinates/maximum* | grep -v coordinates_oris`;
    foreach $ORI_file (@ORI_file_list)
    {
	open(DATA,$ORI_file);
	while(<DATA>)
	{
	    chomp;
	    @array=split(' ',$_);
	    push(@{ $peaks{$array[0]} },$array[3]);
	}
	close(DATA);
    }

#    open(SIZES,'Chromosome_size.dat');
#    while (<SIZES>)
#    {
#	chomp;
#	if ($_ !~ /^Chromosome Size(bp)$/)
#	{
#	    @array=split(' ',$_);
#	    $chr_length{$array[0]}=$array[1];
#	}
#    }
#    close(SIZES);
    
    for $chr (keys %peaks)
    {
	@possible_cuts=();
	%seen = ();
	@uniqu = grep { ! $seen{$_} ++ } @{ $peaks{$chr} };
	@sorted=sort {$a <=> $b} @uniqu;
	$nb_ORIs=push(@sorted);
	for ($i=0;$i<$nb_ORIs-1;++$i)
	{
	    if ($sorted[$i+1]-$sorted[$i] > 2*$WINDOW+1140) # "+120" to leave more than twice the length of the longest probe (there are side effects otherwise, because of the genomic coordinate difference between the middle and the end of a peak probe) # changed to "+1140" on March 10, 2014 (to add more than twice the length of a probe)
	    {
		$x1=$sorted[$i]+$WINDOW+570;# "+60" to leave more than the length of the longest probe (there are side effects otherwise, because of the genomic coordinate difference between the middle and the end of a peak probe) # changed to "+570" on March 10, 2014 (to add more than the length of a probe)
		$x2=$sorted[$i+1]-$WINDOW-570;# "-60" to leave more than the length of the longest probe (there are side effects otherwise, because of the genomic coordinate difference between the middle and the end of a peak probe) # changed to "-570" on March 10, 2014 (to add more than the length of a probe)
		push(@possible_cuts,$x1.'-'.$x2);
	    }
	}
	$cut{$chr}= [ @possible_cuts ];
#	print "chr = $chr\npossible_cuts=\n@possible_cuts\n";
    }
    
    $file=$ARGV[0];
    $file_name=$ARGV[0];
    $file_name=~s/.*\///;
    open(DATA,$file);
    while(<DATA>)
    {
	chomp;
	if (/ chrom=/)
	{
	    if ($_ !~ / chrom=chr/)
	    {
		s/ chrom=/ chrom=chr/;
	    }
	    ($chr,$header)=($_,$_);
	    $chr=~s/.* chrom=//;
	    $chr=~s/ .*//;
	    close(OUT);
	    ++$chunk;
	    open(OUT,">Chunk_$chunk"."_from_$file_name");
	    print OUT "$header\n";
	}
	if (/^[0-9]/)
	{
	    ++$line;
	    print OUT "$_\n";
	    $keep_going=1;
	    if ($line > $MINIMAL_CUT)
	    {
		$x=$_;
		$x=~s/\t.*//;
		foreach $available (@{ $cut{$chr} })
		{
		    ($x1,$x2)=($available,$available);
		    $x1=~s/-.*//;
		    $x2=~s/.*-//;
		    if (($x>=$x1) && ($x<=$x2))
		    {
			$keep_going=0;
		    }
		}
	    }
	    if ($keep_going==0)
	    {
		$line=0;
		close(OUT);
		++$chunk;
		open(OUT,">Chunk_$chunk"."_from_$file_name");
		print OUT "$header\n";
	    }
	}
    }
    close(OUT);
    close(DATA);
}
