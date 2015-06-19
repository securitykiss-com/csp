#!/usr/bin/env tclsh
#
# doctools and textutil::expander must be on this path
lappend auto_path [file normalize ../skt/lib/generic]

package require doctools

proc slurp {path} {
    set fd [open $path r]
    fconfigure $fd -encoding utf-8
    set data [read $fd]
    close $fd
    return $data
}

proc spit {path content} {
    set fd [open $path w]
    puts -nonewline $fd $content
    close $fd
}

doctools::new mydoc -format html
set path ./csp.man
set path [file normalize $path]
set dest [file join [file dir $path] [file root [file tail $path]].html]
spit $dest [mydoc format [slurp $path]]

