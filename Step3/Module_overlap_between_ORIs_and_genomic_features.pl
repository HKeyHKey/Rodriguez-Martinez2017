#!/usr/bin/perl

if ($ARGV[2] eq '')
{
    print "Please enter script arguments (e.g., ./Module_overlap_between_ORIs_and_genomic_features.pl Real_ORIs/Ce10_oris.ech2.40a.40b.40c.3rep.gff CpG_coordinates.txt Chromosome_organization.dat).\n";
}
else
{
    open(ORGA,$ARGV[2]);
    while(<ORGA>)
    {
	chomp;
	if ($_ ne 'Chromosome,end left arm (bp),end centre (bp),end right arm (bp)')
	{
	    @array=split(',',$_);
	    ($chr,$lim1,$lim2,$lim3)=($array[0],$array[1],$array[2],$array[3]);
#	    print "array=@array lim1=$lim1 lim2=$lim2 lim3=$lim3)\n";
	    $limit1{$chr}=$lim1;
	    $limit2{$chr}=$lim2;
#	    $limit3{$chr}=$lim3;
	}
    }
    close(ORGA);

    open(IR,$ARGV[1]);
    while (<IR>)
    {
	chomp;
	@array=split(' ',$_);
	($IR_chr,$IR_start,$IR_end)=($array[0],$array[1],$array[2]);
	$nb_IR=push(@IR_data,$IR_chr."_".$IR_start."_".$IR_end);
	$overlapped_IR{$IR}=0;
	$overlapped_left_IR{$IR}=0;
	$overlapped_center_IR{$IR}=0;
	$overlapped_right_IR{$IR}=0;
	if ($IR_end<=$limit1{$IR_chr})
	{
	    ++$left_arm_IR;
	}
	else
	{
	    if ($IR_start>=$limit2{$IR_chr})
	    {
		++$right_arm_IR;
	    }
	    else
	    {
		++$center_IR;
	    }
	}

    }
    close(IR);

    $name0=$ARGV[0];
    $name0=~s/.*\///;
    $name1=$ARGV[1];
    $name1=~s/.*\///;

    open(ORI,$ARGV[0]);
    while (<ORI>)
    {
	chomp;
	@array=split('\t',$_);
	($ORI_chr,$ORI_start,$ORI_end)=($array[0],$array[3],$array[4]);
	$nb_ORI=push(@ORI_data,$ORI_chr."_".$ORI_start."_".$ORI_end);
	$overlapped_ORI{$ORI}=0;
	$overlapped_left_ORI{$ORI}=0;
	$overlapped_center_ORI{$ORI}=0;
	$overlapped_right_ORI{$ORI}=0;
	if ($ORI_end<=$limit1{$ORI_chr})
	{
	    ++$left_arm_ORI;
	}
	else
	{
	    if ($ORI_start>=$limit2{$ORI_chr})
	    {
		++$right_arm_ORI;
	    }
	    else
	    {
		++$center_ORI;
	    }
	}
    }
    close(ORI);

    foreach $IR (@IR_data)
    {
#	print "Now looking at $IR...\n";
	@array=split('_',$IR);
	($IR_chr,$IR_start,$IR_end)=($array[0],$array[1],$array[2]);
	foreach $ORI (@ORI_data)
	{
#	    print "   now looking at $ORI...\n";
	    @array2=split('_',$ORI);
	    ($ORI_chr,$ORI_start,$ORI_end)=($array2[0],$array2[1],$array2[2]);

#	    print "ORI_start=$ORI_start ORI_end=$ORI_end limit1{ORI_chr}=$limit1{$ORI_chr} limit2{ORI_chr}=$limit2{$ORI_chr}";

	    if ($ORI_end<=$limit1{$ORI_chr})
	    {
		$region=0;
	    }
	    else
	    {
		if ($ORI_start>=$limit2{$ORI_chr})
		{
		    $region=2;
		}
		else
		{
		    $region=1;
		}
	    }

#	    print " region=$region\n";

#	    print "IR_chr=$IR_chr ORI_chr=$ORI_chr IR_start=$IR_start IR_end=$IR_end ORI_start=$ORI_start ORI_end=$ORI_end\n";

	    if (($IR_chr eq $ORI_chr) && (((($IR_start<=$ORI_start) && ($IR_end>=$ORI_start)) || (($IR_start<=$ORI_end) && ($IR_end>=$ORI_end))) || ((($ORI_start<=$IR_start) && ($ORI_end>=$IR_start)) || (($ORI_start<=$IR_end) && ($ORI_end>=$IR_end)))))
	    {
		$overlapped_IR{$IR}=1;
		$overlapped_ORI{$ORI}=1;
		if ($region==0)
		{
		    $overlapped_left_IR{$IR}=1;
		    $overlapped_left_ORI{$ORI}=1;
		}
		if ($region==1)
		{
		    $overlapped_center_IR{$IR}=1;
		    $overlapped_center_ORI{$ORI}=1;
		}
		if ($region==2)
		{
		    $overlapped_right_IR{$IR}=1;
		    $overlapped_right_ORI{$ORI}=1;
		}
	    }
	}
    }

    foreach $IR (@IR_data)
    {
	if ($overlapped_IR{$IR})
	{
	    ++$ov_IR;
	}
	if ($overlapped_left_IR{$IR})
	{
	    ++$ov_left_IR;
	}
	if ($overlapped_center_IR{$IR})
	{
	    ++$ov_center_IR;
	}
	if ($overlapped_right_IR{$IR})
	{
	    ++$ov_right_IR;
	}
    }
    foreach $ORI (@ORI_data)
    {
	if ($overlapped_ORI{$ORI})
	{
	    ++$ov_ORI;
	}
	if ($overlapped_left_ORI{$ORI})
	{
	    ++$ov_left_ORI;
	}
	if ($overlapped_center_ORI{$ORI})
	{
	    ++$ov_center_ORI;
	}
	if ($overlapped_right_ORI{$ORI})
	{
	    ++$ov_right_ORI;
	}
    }
    if ($ov_IR eq '')
    {
	$ov_IR=0;
    }
    if ($ov_left_IR eq '')
    {
	$ov_left_IR=0;
    }
    if ($ov_center_IR eq '')
    {
	$ov_center_IR=0;
    }
    if ($ov_right_IR eq '')
    {
	$ov_right_IR=0;
    }
    if ($ov_ORI eq '')
    {
	$ov_ORI=0;
    }
    if ($ov_left_ORI eq '')
    {
	$ov_left_ORI=0;
    }
    if ($ov_center_ORI eq '')
    {
	$ov_center_ORI=0;
    }
    if ($ov_right_ORI eq '')
    {
	$ov_right_ORI=0;
    }

    open(OUT,">Overlaps_between_$name0"."_and_$name1");
    print OUT "Overlapped genomic features: $ov_IR out of $nb_IR (overlapped by left arm ORIs: $ov_left_IR out of $left_arm_IR; by center ORIs: $ov_center_IR out of $center_IR; by right arm ORIs: $ov_right_IR out of $right_arm_IR).\n";
    print OUT "Overlapped ORIs: $ov_ORI out of $nb_ORI (overlapped left arm ORIs: $ov_left_ORI out of $left_arm_ORI; center ORIs: $ov_center_ORI out of $center_ORI; right arm ORIs: $ov_right_ORI out of $right_arm_ORI).\n";
    close(OUT);
}
