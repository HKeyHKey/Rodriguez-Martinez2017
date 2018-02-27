#!/usr/bin/perl

if ($ARGV[1] eq '')
{
    print "Please enter script arguments (e.g., ./Module_detailed_overlap_between_ORIs_and_IRs.pl Filtered_fused_ORIs_2_40_cells.gff All_IRs.gff).\n";
}
else
{
    open(IR,$ARGV[1]);
    while (<IR>)
    {
	chomp;
	@array=split('\t',$_);
	($IR_chr,$IR_start,$IR_end,$loop_start,$loop_end)=($array[0],$array[3],$array[4],$array[8],$array[8]);
	$loop_start=~s/End_of_left_arm://;
	$loop_start=~s/;.*//;
	$loop_start=$loop_start+1;
	$loop_end=~s/.*://;
	$loop_end=$loop_end-1;
	push(@IR_data,$IR_chr."_".$IR_start."_".$IR_end."_".$loop_start."_".$loop_end);
    }
    close(IR);

    $name0=$ARGV[0];
    $name0=~s/.*\///;
    $name1=$ARGV[1];
    $name1=~s/.*\///;

    open(INSIDE,">ORIs_overlapping_loops_from_$name0"."_and_$name1");
    open(INTERM,">ORIs_overlapping_IRs_but_not_loops_from_$name0"."_and_$name1");
    open(OUTSIDE,">ORIs_not_overlapping_IRs_from_$name0"."_and_$name1");

    open(ORI,$ARGV[0]);
    while (<ORI>)
    {
	chomp;
	@array=split('\t',$_);
	($ORI_chr,$ORI_start,$ORI_end)=($array[0],$array[3],$array[4]);
	$outside=1;
	$overlap_loop=0;
	foreach $IR (@IR_data)
	{
	    ($IR_chr,$IR_start,$IR_end,$loop_start,$loop_end)=($IR,$IR,$IR,$IR,$IR);
	    $loop_end=~s/.*_//;
	    $loop_start=~s/.*_(\d*)_\d*$/\1/;
	    $IR_end=~s/.*_(\d*)_\d*_\d*$/\1/;
	    $IR_start=~s/.*_(\d*)_\d*_\d*_\d*$/\1/;
	    $IR_chr=~s/_\d*_\d*_\d*_\d*$//;
	    if (($IR_chr eq $ORI_chr) && (((($IR_start<=$ORI_start) && ($IR_end>=$ORI_start)) || (($IR_start<=$ORI_end) && ($IR_end>=$ORI_end))) || ((($ORI_start<=$IR_start) && ($ORI_end>=$IR_start)) || (($ORI_start<=$IR_end) && ($ORI_end>=$IR_end)))))
	    {
		$outside=0;
		if ((($loop_start<=$ORI_start) && ($loop_end>=$ORI_start)) || (($loop_start<=$ORI_end) && ($loop_end>=$ORI_end)) || (($ORI_start<=$loop_start) && ($ORI_end>=$loop_start)) || (($ORI_start<=$loop_end) && ($ORI_end>=$loop_end)))
		{
		    $overlap_loop=1;
		}
	    }
	}
	if ($outside)
	{
	    print OUTSIDE "$_\n";
	}
	else
	{
	    if ($overlap_loop)
	    {
		print INSIDE "$_\n";
	    }
	    else
	    {
		print INTERM "$_\n";
	    }
	}
    }
    close(ORI);
    close(INSIDE);
    close(INTERM);
    close(OUTSIDE);
}
