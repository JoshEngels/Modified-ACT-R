# this file generated when environment is closed
# refresh . to make sure sizes are right

wm deiconify .
update
wm withdraw .
if {[winfo screenwidth .] != 1280 || [winfo screenheight .] != 720 || [lindex [wm maxsize .] 0] != 1284 || [lindex [wm maxsize .] 1] != 701} {
  set size_mismatch 1
} else {
  set size_mismatch 0
}

if $size_mismatch {
  set reset_window_sizes [tk_messageBox -icon warning -title "Screen resolution changed" -type yesno \
                                         -message "The screen resolution is not the same as it was the last time the Environment was used.  Should the window positions reset to the defaults?"]
} else { set reset_window_sizes 0}
if {$reset_window_sizes != "yes"} {
  set window_config(.whynotdm) 200x300+540+210
  set changed_window_list(.whynotdm) 1
  set window_config(.audicon) 870x150+205+260
  set changed_window_list(.audicon) 1
  set window_config(.visicon) 1153x549+105+75
  set changed_window_list(.visicon) 1
  set window_config(.stepper) 849x436+563+450
  set changed_window_list(.stepper) 1
  set window_config(.control_panel) 171x686+1022+0
  set changed_window_list(.control_panel) 1
  set window_config(.declarative) 420x300+430+210
  set changed_window_list(.declarative) 1
  set window_config(.copyright) 400x290+440+215
  set changed_window_list(.copyright) 1
  set window_config(.reload_response) 500x229+390+245
  set changed_window_list(.reload_response) 1
  set window_config(.buffers) 420x240+564+159
  set changed_window_list(.buffers) 1
  set window_config(.procedural) 500x400+757+91
  set changed_window_list(.procedural) 1
  set window_config(.whynot) 333x422+540+210
  set changed_window_list(.whynot) 1
}
