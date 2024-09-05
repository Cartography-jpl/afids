# {******** Roads 1 "CBS Loose Surface" (101) ********}
# {From roadl}
Roads1_Loose_Surface_roadl {
"$rst" == "0"
}
Roads1_Loose_Surface_roadl_lowVolume_CANCELLED {
0
}
# {From trackl}
Roads1_Loose_Surface_trackl {
"$f_code" == "AP010"
}
Roads1_Loose_Surface_trackl_lowVolume_CANCELLED {
0
}
# {From traill}
Roads1_Loose_Surface_traill {
"$f_code" == "AP050"
}
Roads1_Loose_Surface_traill_lowVolume_CANCELLED {
0
}
# {From trans_arc}
Roads1_Loose_Surface_trans_arc {
"$F_CODE" == "AP010" ||
"$F_CODE" == "AP050"
}
Roads1_Loose_Surface_trans_arc_lowVolume_CANCELLED {
0
}
# {From vm0_roadl}
Roads1_Loose_Surface_vm0_roadl {
"$ROUTE_INTE" == "0 (Unknown)"
}
Roads1_Loose_Surface_vm0_roadl_lowVolume_CANCELLED {
0
}
# {From vm0_traill}
Roads1_Loose_Surface_vm0_traill {
1
}
Roads1_Loose_Surface_vm0_traill_lowVolume_CANCELLED {
0
}

# {******** Roads 2 "CBS Hard Surface" (102) ********}
# {From roadl}
Roads2_Hard_Surface_roadl {
"$rst" == "2" || 
"$rst" == "3" ||
("$rst" == "1" && 
 ("$rtt" == "14" || 
  "$rtt" == "0"))
}
# {From bridgel}
Roads2_Hard_Surface_bridgel {
"$tuc" == "1" ||
"$tuc" == "4"
}
# {From tunnell}
Roads2_Hard_Surface_tunnell {
"$tuc" == "1" ||
"$tuc" == "4"
}
# {From trans_arc}
Roads2_Hard_Surface_trans_arc {
"$F_CODE" == "AP030" ||
"$F_CODE" == "AQ040" ||
"$F_CODE" == "AQ130"
}
# {From vm0_roadl}
Roads2_Hard_Surface_vm0_roadl {
"$ROUTE_INTE" == "15 (Secondary Route)"
}


# {******** Roads 3 "CBS Dual Highways" (103) ********}
# {From roadl}
Roads3_Dual_Highways_roadl {
"$rst" == "1" && 
("$rtt" == "13" || 
 "$rtt" == "15")
}
# {From vm0_roadl}
Roads3_Dual_Highways_vm0_roadl {
"$ROUTE_INTE" == "14 (Primary Route)"
}

# {******** Rivers 1 "CBS Small River" (201) ********}
# {From watrcrsl}
Rivers1_Small_River_watrcrsl {
(("$f_code" == "BH020" ||
  "$f_code" == "BH030" ||
  "$f_code" == "BH140") &&
 ("$hyc" == "3" ||
  "$hyc" == "6")) ||
(("$f_code" == "BH030" ||
  "$f_code" == "BH140") &&
 "$hyc" == "8")
}
Rivers1_Small_River_watrcrsl_lowVolume {
(("$f_code" == "BH030" ||
  "$f_code" == "BH140") &&
 "$hyc" == "8")
}
# {From aquedctl}
Rivers1_Small_River_aquedctl {
"$f_code" == "BH010"
}
# {From hydro_arc}
Rivers1_Small_River_hydro_arc {
"$F_CODE" == "BH010" ||
"$F_CODE" == "BH030" ||
"$F_CODE" == "BH140"
}
Rivers1_Small_River_hydro_arc_lowVolume {
"$F_CODE" == "BH010"
}
# {From vm0_aquecanl}
Rivers1_Small_River_vm0_aquecanl {
1
}

# {******** Rivers 2 "CBS Medium River" (202) ********}
# {From watrcrsl}
Rivers2_Medium_River_watrcrsl {
"$f_code" == "BH020" &&
"$hyc" == "8"
}
# {From vm0_watrcrsl}
Rivers2_Medium_River_vm0_watrcrsl {
"$HYDROLOGIC" == "6 (Non-Perennial/Intermittent/Fluctuating)"
}

# {******** Rivers 3 "CBS Large River" (203) ********}
# {From watrcrsa}
Rivers3_Large_River_watrcrsa {
("$f_code" == "BH020" ||
 "$f_code" == "BH030" ||
 "$f_code" == "BH140") &&
("$hyc" == "6" ||
 "$hyc" == "8")
}
Rivers3_Large_River_watrcrsa_lowVolume {
("$f_code" == "BH020" ||
 "$f_code" == "BH030" ||
 "$f_code" == "BH140") &&
"$hyc" == "8"
}
# {From vm0_watrcrsl}
Rivers3_Large_River_vm0_watrcrsl {
"$HYDROLOGIC" == "8 (Perennial/Permanent)"
}

# {******** Vegetation "CBS Sparse" (302) ********}
# {From cropa}
Vegetation_Sparse_cropa {
"$f_code" == "EA010" ||
"$f_code" == "EB010" ||
"$f_code" == "BJ110"
}
Vegetation_Sparse_cropa_lowVolume {
"$f_code" == "EA010"
}
# {From orcharda}
Vegetation_Sparse_orcharda {
"$f_code" == "EA050"
}
# {From veg_poly}
Vegetation_Sparse_veg_poly {
"$F_CODE" == "EA050" ||
"$F_CODE" == "EB010"
}
# {From vm0_cropa}
Vegetation_Sparse_vm0_cropa {
1
}
# {From vm0_grassa}
Vegetation_Sparse_vm0_grassa {
1
}
Vegetation_Sparse_vm0_grassa_lowVolume {
0
}
# {From vm0_tundraa}
Vegetation_Sparse_vm0_tundraa {
1
}
Vegetation_Sparse_vm0_tundraa_lowVolume {
0
}

# {******** Vegetation "CBS Moderate" (303) ********}
# {From orcharda}
Vegetation_Moderate_orcharda {
"$f_code" == "EA040"
}
# {From treesa}
Vegetation_Moderate_treesa {
"$f_code" == "EC030"
}
# {From veg_poly}
Vegetation_Moderate_veg_poly {
"$F_CODE" == "EA040" ||
"$F_CODE" == "EC030"
}
# {From vm0_treesa}
Vegetation_Moderate_vm0_treesa {
1
}

# {******** Urbanization "CBS Village" (401) ********}
# {From builtupa}
Urbanization_Village_builtupa {
"$use" == "111"
}
Urbanization_Village_builtupa_lowVolume {
0
}

# {******** Urbanization "CBS Town" (402) ********}
# {From builtupa}
Urbanization_Town_builtupa {
"$use" == "0" ||
"$use" == "31"
}

# {******** Urbanization "CBS City" (403) ********}
# {From builtupa}
Urbanization_City_builtupa {
"$use" == "26" ||
"$use" == "30"
}
# {From pop_poly}
Urbanization_City_pop_poly {
"$F_CODE" == "AL020"
}
# {From vm0_builtupa}
Urbanization_City_vm0_builtupa {
1
}

# {******** Trafficability "CBS Water" (501) ********}
# {From lakeresa}
Trafficability_Water_lakeresa {
"$f_code" == "BH080" ||
"$f_code" == "BH130"
}
# {From coasta}
Trafficability_Water_coasta {
"$f_code" == "BA040"
}
# {From hydro_poly}
Trafficability_Water_hydro_poly {
"$F_CODE" == "BH080" ||
"$F_CODE" == "BH130" ||
"$F_CODE" == "BA040"
}
# {From vm0_inwatera}
Trafficability_Water_vm0_inwatera {
"$FACC_FEATU" == "BH000 (Inland Water)"
}
# {From vm0_oceansea}
Trafficability_Water_vm0_oceansea {
1
}

# {******** Trafficability "CBS Mountainous" (505) ********}
# {From grounda}
Trafficability_Mountainous_grounda {
"$mcc" == "52" ||
"$mcc" == "119"
}
# {From vm0_grounda}
Trafficability_Mountainous_vm0_grounda {
"$SURFACE_MA" == "119 (Distorted Surface)" ||
"$SURFACE_MA" == "52 (Lava)"
}

# {******** Trafficability "CBS Marsh" (506) ********}
# {From swampa}
Trafficability_Marsh_swampa {
1
}
# {From veg_poly}
Trafficability_Marsh_veg_poly {
"$F_CODE" == "BH095" ||
"$F_CODE" == "BH135"
}
# {From vm0_swampa}
Trafficability_Marsh_vm0_swampa {
1
}

# {******** Trafficability from elevation /home/tll/terrain/cbs_traf.shp ********}
Trafficability_FCODE_501 {
"$FCODE" == "501"
}
Trafficability_FCODE_502 {
"$FCODE" == "502"
}
Trafficability_FCODE_503 {
"$FCODE" == "503"
}
Trafficability_FCODE_504 {
"$FCODE" == "504"
}
Trafficability_FCODE_505 {
"$FCODE" == "505"
}
Trafficability_FCODE_506 {
"$FCODE" == "506"
}
