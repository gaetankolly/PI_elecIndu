
# PlanAhead Launch Script for Post-Synthesis pin planning, created by Project Navigator

create_project -name groupe4_cansat -dir "D:/master/PI_elecIndu/gitProject/PI_elecIndu/groupe4_cansat/planAhead_run_1" -part xc6slx9tqg144-3
set_property design_mode GateLvl [get_property srcset [current_run -impl]]
set_property edif_top_file "D:/master/PI_elecIndu/gitProject/PI_elecIndu/groupe4_cansat/top.ngc" [ get_property srcset [ current_run ] ]
add_files -norecurse { {D:/master/PI_elecIndu/gitProject/PI_elecIndu/groupe4_cansat} }
set_param project.pinAheadLayout  yes
set_property target_constrs_file "board.ucf" [current_fileset -constrset]
add_files [list {board.ucf}] -fileset [get_property constrset [current_run]]
link_design
