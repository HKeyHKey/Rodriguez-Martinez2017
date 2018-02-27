#!/usr/bin/perl
use Math::Round;

if ($ARGV[1] eq '')
{
    print "Please enter 2 input file name: the first one with origin coordinates (e.g., /mnt/data/home/herve.seitz/Analyses_for_paper_July2015/Aurore_Emmanuelle_scripts_par_Herve/Regions_ORIs_comb/oris.ech2.40a.40b.40c.3rep.gff), the second one with probe signal intensities (e.g., /mnt/data/home/herve.seitz/Analyses_for_paper_July2015/Aurore_Emmanuelle_scripts_par_Herve/Data_comb/ech2.40a.40b.40c.3rep.txt).\n";
}
else
{
#pour prendre les coordonnees 5' et 3' et le chromosome des boites > stocke dans @cr_x_boite
    $boxes_file = $ARGV[0];
    open(INFO,$boxes_file);	
    while ($line=<INFO>)
    {
	chomp $line;
	@data= split (/\t/, $line);
	$cr_boite_5=$data[3];
	push (@cr_5_boite,$cr_boite_5);
	$cr_boite_3=$data[4]; 
	push (@cr_3_boite,$cr_boite_3);
	$chr_boites=$data[0];
	push (@chrs_boite,$chr_boites);
    }
    close (INFO);
    
#pour prendre les coordonnees 5' et 3'et le chromosome des sondes
    $probe_file = $ARGV[1];
    open(INFO2, $probe_file);	
    while ($line_probe=<INFO2>)
    {
	chomp $line_probe;
	@data_probe= split (/\t/, $line_probe);
	$cr_probe_5=$data_probe[2];
	$cr_probe_3=$data_probe[2]+$data_probe[3]-1;
	$intensite=$data_probe[14];
	$chr_probe=$data_probe[1];
	push(@{$probe_data{$chr_probe}},$cr_probe_5.' '.$cr_probe_3.' '.$intensite);
    }
    close (INFO2);
    
#comparer les coordonnees des boites et des sondes
    
    $length=@cr_probe_5;
    $length_boite=@cr_5_boite;

    $name0=$ARGV[0];
    $name0=~s/.*\///;
    $name0=~s/\.gff$//;
    open (OUTPUT, '>maximum_probes_coordinates_'.$name0.'.txt');
    print OUTPUT "#Chromosome ORI_start ORI_end highest_probe_start highest_probe_end highest_probe_intensity number_of_contributing_highest_probes\n";
    for ($i=0; $i<$length_boite ; ++$i)
    {
#	print "Now working on ORI $chrs_boite[$i], $cr_5_boite[$i] to $cr_3_boite[$i]\n";
	$deja_vu = 0;
	for $probe (@{$probe_data{$chrs_boite[$i]}})
	{
	    @array=split(' ',$probe);
	    ($cr_probe_5,$cr_probe_3,$intensite)=($array[0],$array[1],$array[2]);
	    {
		if (($cr_probe_5 >= $cr_5_boite[$i]) && ($cr_probe_3 <= $cr_3_boite[$i]))
		{
		    if ($deja_vu == 0)
		    {
			$record = $intensite;
		        @cr_5_probe_max=($cr_probe_5);
			@cr_3_probe_max=($cr_probe_3);
		    }
		    else
		    {
			if ($intensite == $record)
			{
			    push(@cr_5_probe_max,$cr_probe_5);
			    push(@cr_3_probe_max,$cr_probe_3);
			}
			if ($intensite > $record)
			{
			    $record= $intensite;
			    @cr_5_probe_max=($cr_probe_5);
			    @cr_3_probe_max=($cr_probe_3);
			}
		    }
		    $deja_vu = 1;
		}
	    }
	}
	
	$nb_probes=push(@cr_5_probe_max);
	$sum5=0;
	$sum3=0;
	for $coord (@cr_5_probe_max)
	{
	    $sum5+=$coord;
	}
	for $coord (@cr_3_probe_max)
	{
	    $sum3+=$coord;
	}
	$sum5=round($sum5/$nb_probes);
	$sum3=round($sum3/$nb_probes);

	print OUTPUT "$chrs_boite[$i] $cr_5_boite[$i] $cr_3_boite[$i] $sum5 $sum3 $record $nb_probes\n";

    }
    close OUTPUT;
}
