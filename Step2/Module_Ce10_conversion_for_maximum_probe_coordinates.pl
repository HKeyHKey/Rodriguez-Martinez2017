#!/usr/bin/perl

if ($ARGV[1] eq '')
{
    print "Please enter script arguments (e.g., ./Module_Ce10_conversion_for_maximum_probe_coordinates.pl Ce10_ech2.40a.40b.40c.3rep.bed /mnt/data/home/herve.seitz/Analyses_for_paper_July2015/Aurore_Emmanuelle_scripts_par_Herve/Data_comb/ech2.40a.40b.40c.3rep.txt).\n";
}
else
{
    open(CE10,$ARGV[0]);
    while (<CE10>)
    {
	chomp;
	@array=split('\t',$_);
	($Ce10_start,$probe)=($array[1],$array[3]);
        $converted_start{$probe}=$Ce10_start;
    }
    close(CE10);

    $name1=$ARGV[1];
    $name1=~s/.*\///;

    open(OUT,">Ce10_$name1");

    open(CE6,$ARGV[1]);
    while (<CE6>)
    {
        chomp;
        if (/^PROBE_ID\tCHROMOSOME\tPOSITION\t/)
        {
            print OUT "$_\n";
        }
        else
        {
    	    @array=split('\t',$_);
	    ($probe,$chr,$Ce6_start)=($array[0],$array[1],$array[2]);
            s/^$probe\t$chr\t$Ce6_start/$probe\t$chr\t$converted_start{$probe}/;
            if ($converted_start{$probe})
            {
	       print OUT "$_\n";
           }
        }
    }
    close(CE6);
    close(OUT);
}
