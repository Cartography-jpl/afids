package provide TMASRV 1.0

proc Gui_ColorSelect { w } {
  global gui_ColorWheelSelection env
  toplevel $w -class Dialog
  wm title $w "Select Color"
  set size 151; #Should be odd.
  wm minsize $w $size $size
  wm maxsize $w $size $size

#  wm minsize $w $size [expr $size + 50]
#  wm maxsize $w $size [expr $size + 50]
 
  canvas $w.display -bg black -width $size -height $size
  pack $w.display -fill both -expand true
  bind $w.display <Button-1> \
	{set gui_ColorWheelSelection [colorwheel get %x %y]; set ok 1}
  image create photo colorwheel -file $env(PUB)/toolkit/data/colorwheel.gif
  $w.display create image 0 0 -anchor nw -image colorwheel

#  button $w.close -text "Close" -width 11 -command { exit }
#  pack  $w.close -side bottom

  set oldFocus [focus]
  grab set $w
  focus $w

  # Wait for the user to respond, then restore the focus
  # and return the index of the selected button.

  tkwait variable ok
  destroy $w
  focus $oldFocus
  return $gui_ColorWheelSelection
}

