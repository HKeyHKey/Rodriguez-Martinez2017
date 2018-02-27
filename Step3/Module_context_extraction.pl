#!/usr/bin/perl
use Math::Round;

$CONTEXT=1200; #sequence context size (on each side of the maximum probe) to be extracted from genome

if ($ARGV[1] eq '')
{
    print "Please give script arguments (file with maximum probe coordinates, and genome file, whose sequences must fit on single lines; e.g., ./Module_context_extraction.pl /mnt/data/home/herve.seitz/Analyses_for_paper_July2015/Maximum_probe_coordinates/maximum_probes_coordinates_Ce10_oris.ech2.40a.40b.40c.3rep.txt /mnt/data/home/herve.seitz/Genomes/Fused_Ce10.fa).\n";
}
else
{
    open(GENOME,$ARGV[1]);
    while (<GENOME>)
    {
	chomp;
        if (/^>/)
        {
            s/^> *//;
            $chr=$_;
        }
        else
        {
            $genome{$chr}=$_;
            $chr_length{$chr}=length($_);
        }
    }
    close(GENOME);

    $name=$ARGV[0];
    $name=~s/.*\///;
    $name=~s/\.txt$//;
    open(OUT,">Context_".$CONTEXT."_bp_around_peaks_in_".$name.".fa");
    open(OUT2,">Homogeneous_context_".$CONTEXT."_bp_around_peaks_in_".$name.".fa");
    open(PEAKS,$ARGV[0]);
    while (<PEAKS>)
    {
        chomp;
        if ($_ !~ /^#/)
        {
            @array=split(' ',$_);
            $peak_mid=round(($array[3]+$array[4])/2);
            $start=$peak_mid-$CONTEXT;
            $end=$peak_mid+$CONTEXT;
            if (($start >= 0) && ($end <= $chr_length{$array[0]}))
            {
                $extract=substr $genome{$array[0]},$start-1,$end-$start+1;
                print OUT2 ">$array[0]_bp_$start"."-$end ($CONTEXT bp around maximum probe at bp $array[3]-$array[4] for origin located at bp $array[1]-$array[2]; maximum array signal: $array[5], measured on $array[6] probe(s)\n$extract\n";
            }
            if ($start < 0)
            {
                $start=0;
            }
            if ($end > $chr_length{$array[0]})
            {
                $end=$chr_length{$array[0]}
            }
            $extract=substr $genome{$array[0]},$start-1,$end-$start+1;
            print OUT ">$array[0]_bp $start"."-$end ($CONTEXT bp around maximum probe at bp $array[3]-$array[4] for origin located at bp $array[1]-$array[2]; maximum array signal: $array[5], measured on $array[6] probe(s)\n$extract\n";
        }
    }
    close(PEAKS);
    close(OUT);
    close(OUT2);
}
