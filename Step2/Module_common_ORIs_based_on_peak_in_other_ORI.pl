#!/usr/bin/perl

if ($ARGV[1] eq '')
{
    print "Please enter script arguments (e.g., ./Module_common_ORIs_based_on_peak_in_other_ORI.pl oris.ech2.40a.40b.40c.3rep.gff maximum_probes_coordinates_Mix.txt).\n";
}
else
{
    open(ORI,$ARGV[0]);
    while (<ORI>)
    {
	chomp;
	@array=split('\t',$_);
	($ORI_chr,$ORI_start,$ORI_end)=($array[0],$array[3],$array[4]);
	push(@ORI_data,$ORI_chr."_".$ORI_start."_".$ORI_end);
    }
    close(ORI);

    $name0=$ARGV[0];
    $name0=~s/.*\///;
    $name0=~s/\.gff$//;
    $name1=$ARGV[1];
    $name1=~s/.*\///;
    $name1=~s/\.gff$//;

    open(OUT,">Peaks_in_$name1"."_falling_in_ORIs_from_$name0".".csv");
    print OUT "Peak coordinates in $name1,ORI coordinates in $name0\n";

    open(PEAK,$ARGV[1]);
    while (<PEAK>)
    {
        if ($_ !~ /^#/)
        {
            chomp;
    	    @array=split(' ',$_);
	    ($peak_chr,$peak_start,$peak_end)=($array[0],$array[3],$array[4]);
	    $d='';
	    for $ORI (@ORI_data)
	    {
	        ($ORI_chr,$ORI_start,$ORI_end)=($ORI,$ORI,$ORI);
	        $ORI_chr=~s/_.*//;
	        $ORI_start=~s/.*_(\d*)_.*/\1/;
	        $ORI_end=~s/.*_//;
	        if (($ORI_chr eq $peak_chr) && ($peak_start>=$ORI_start) && ($peak_start<=$ORI_end) && ($peak_end>=$ORI_start) && ($peak_end<=$ORI_end))
	        {
		    $d=$d." ".$ORI;
	        }
	    }
	    $d=~s/_/ /;
	    print OUT "$peak_start-$peak_end of $peak_chr,$d\n";
        }
    }
    close(PEAK);
    close(OUT);
}
