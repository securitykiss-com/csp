# Copyrite (c) 2015 SecurityKISS Ltd (http://www.securitykiss.com)  
# 
# The MIT License (MIT)
# 
# Yes, Mr patent attorney, you have nothing to do here. Find a decent job instead. 
# Fight intellectual "property".
# 
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
# 
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

package require Tcl 8.6

package provide csp 0.1.0

namespace eval csp {
    # channel as a list/queue for buffered channel or
    # venue container for rendez-vous channel (non-buffered size 0 channel)
    variable Channel
    array set Channel {}
    # channel capacity (buffer size)
    variable ChannelCap
    array set ChannelCap {}
    variable ChannelReadOnly
    array set ChannelReadOnly {}
    variable Routine
    array set Routine {}
    # counter/uid to produce unique Routine and Channel names
    variable Uid 0

    namespace export go channel select timer ticker range range! <- <-! -> ->> forward
    namespace ensemble create
}


proc ::csp::ChannelProxy {ch operator val} {
    if {$operator eq "close"} {
        # delete channel command == channel closed
        rename $ch "" 
        # let the CDrained purge the channel if empty
        CDrained $ch
        SetResume
        return
    }
    CheckOperator $operator
    if {[CReadOnly $ch]} {
        error "Cannot send to read only channel $ch"
    }

    while {![CSendReady $ch]} {
        CheckClosed $ch
        Wait$operator
    }
    CAppend $ch $val
    SetResume
    # post-send extra logic for rendez-vous channels
    if {![CBuffered $ch]} {
        # wait again for container empty (once receiver collected the value)
        while {![CEmpty $ch]} {
            CheckClosed $ch
            Wait$operator
        }
    }
    return
}



proc ::csp::Wait<- {} {
    yield
}

proc ::csp::Wait<-! {} {
    vwait ::csp::resume
}

proc ::csp::IsOperator {op} {
    return [expr {$op in {<- <-!}}]
}

proc ::csp::CheckOperator {op} {
    if {![IsOperator $op]} {
        error "Unrecognized operator $op. Should be <- or <-!"
    }
    if {[info coroutine] eq ""} {
        if {$op eq "<-"} {
            error "<- can only be used in a coroutine"
        }
    } else {
        if {$op eq "<-!"} {
            error "<-! should not be used in a coroutine"
        }
    }
}

# throw error if channel is closed
proc ::csp::CheckClosed {ch} {
    if {[CClosed $ch]} {
        error "Cannot send to the closed channel $ch"
    }
}

# throw error if incorrect channel name
proc ::csp::CheckName {ch} {
    if {![regexp {::csp::Channel#\d+} $ch]} {
        error "Wrong channel name: $ch"
    }
}

proc ::csp::CSendReady {ch} {
    if {[CClosed $ch]} {
        return 0
    }
    if {[CBuffered $ch]} {
        return [expr {! [CFull $ch]}]
    } else {
        return [CEmpty $ch]
    }
}

proc ::csp::CReceiveReady {ch} {
    CheckName $ch 
    return [expr {![CEmpty $ch]}]
}

# remove the channel completely
proc ::csp::CPurge {ch} {
    CheckName $ch
    catch {
        unset Channel($ch)
        unset ChannelCap($ch)
        unset ChannelReadOnly($ch)
        rename $ch ""
        SetResume
    }
}


# channel chlist ?cap? 
# Create channel(s) (with internal name) and place that name in given var
# the default buffer size (capacity) is zero which means rendez-vous channel
proc ::csp::channel {chVars {cap 0}} {
    variable Channel
    variable ChannelCap
    variable CTemplate
    lmap chVar $chVars {
        upvar $chVar ch
        set ch [NewChannel]
        # initialize channel as a list or do nothing if exists
        set Channel($ch) {}
        set ChannelCap($ch) $cap
        # create channel object from the template
        namespace eval ::csp [string map [list %CHANNEL% $ch] {
            proc %CHANNEL% {operator {val ""}} {
                ::csp::ChannelProxy %CHANNEL% $operator $val
            }
        }]
    }
}

# Sending will be possible only by using internal CAppend
proc ::csp::CMakeReadOnly {ch} {
    variable ChannelReadOnly
    set ChannelReadOnly($ch) 1
}

proc ::csp::CReadOnly {ch} {
    variable ChannelReadOnly
    return [info exists ChannelReadOnly($ch)]
}



# A channel is considered closed if no longer exists (but its name is correct) 
proc ::csp::CClosed {ch} {
    CheckName $ch
    # if channel command no longer exists
    return [expr {[info procs $ch] eq ""}]
}


# uconditionally append to the channel - internal only
proc ::csp::CAppend {ch val} {
    variable Channel
    lappend Channel($ch) $val
    #puts "Channel($ch) after CAppend: $Channel($ch)"
    return
}

proc ::csp::CBuffered {ch} {
    variable ChannelCap
    return [expr {$ChannelCap($ch) != 0}]
}

proc ::csp::CEmpty {ch} {
    variable Channel
    CheckName $ch
    if {[info exists Channel($ch)]} {
        return [expr {[llength $Channel($ch)] == 0}]
    } else {
        return 1
    }
}


proc ::csp::CFull {ch} {
    variable Channel
    variable ChannelCap
    set clen [llength $Channel($ch)]
    return [expr {$clen >= $ChannelCap($ch)}]
}

# return contents of the channel
proc ::csp::CGet {ch} {
    variable Channel
    return $Channel($ch)
}



# generate new routine name
proc ::csp::NewRoutine {} {
    return ::csp::Routine#[incr ::csp::Uid]
}

proc ::csp::NewChannel {} {
    return ::csp::Channel#[incr ::csp::Uid]
}



# invoke proc in a routine
# args should be proc name and arguments
proc ::csp::go {args} {
    variable Routine
    # args contain the routine name with arguments
    set rname [::csp::NewRoutine]
    coroutine $rname {*}$args
    set Routine($rname) 1
    SetResume
    return $rname
}

# schedule wake-up
proc ::csp::SetResume {} {
    after idle {after 0 ::csp::Resume}
}

# notify routines to reevaluate resume conditions
# it may be improved by keeping track of the yielding routines (yield [info coroutine])
proc ::csp::Resume {} {
    variable Routine
    foreach r [array names Routine] {
        if {[info commands $r] eq ""} {
            # coroutine must have ended so remove it from the array
            catch {unset Routine($r)}
        } else {
            # cannot run the already running coroutine - catch error when it happens
            # this may regularly throw 'coroutine "::csp::Routine#N" is already running'
            catch $r
            #if {[catch {$r} out err]} 
                #puts stderr "OUT: $out, ERR: $err"
            
        }
    }
    set ::csp::resume 1
}


proc ::csp::CReceive {ch} {
    variable Channel
    set elem [lindex $Channel($ch) 0]
    set Channel($ch) [lreplace $Channel($ch) 0 0]
    return $elem
}


proc ::csp::CDrained {ch} {
    CheckName $ch
    # drained = empty and closed
    set drained [expr {![CReceiveReady $ch] && [CClosed $ch]}]
    if {$drained} {
        # just in case purge every time
        CPurge $ch
        return 1
    } else {
        return 0
    }
}


proc ::csp::ReceiveWith {ch operator} {
    CheckOperator $operator
    # check if ch contains elements, if so return element, yield otherwise
    while {![CReceiveReady $ch]} {
        # trying to receive from empty and closed channel should clean up the channel and throw error
        if {[CDrained $ch]} {
            error "Cannot receive from a drained (empty and closed) channel $ch"
        }
        Wait$operator
    }
    set elem [CReceive $ch]
    SetResume
    return $elem
}

# Receive from channel, wait if channel not ready, throw error if channel is drained
# Can be only used from coroutine
# Uses yield for wait
proc ::csp::<- {ch} {
    return [ReceiveWith $ch <-]
}

# Receive from channel, wait if channel not ready, throw error if channel is drained
# Can be used from non-coroutine
# Uses vwait for wait => nested event loops
# It means that not ready channel in nested vwait 
# may block an upstream channel that become ready
# Use with care. Avoid if you can.
proc ::csp::<-! {ch} {
    return [ReceiveWith $ch <-!]
}


# Create a channel and a callback handler being a coroutine which when called
# will send callback single argument to the newly created channel
# The library user should only receive from that channel
# It is designed for one-time callbacks i.e. the coroutine exits after first call.
# The channel will be automatically destroyed after receiving the first message.
# The limitation is that the callback must be a single- or zero- argument callback
# In case of no-argument callback empty string is sent to the channel
proc ::csp::-> {chVar} {
    upvar $chVar ch
    channel ch
    CMakeReadOnly $ch
    # this coroutine will not be registered in Routine array for unblocking calls
    # it should be called from the userland callback
    set routine [NewRoutine]
    coroutine $routine OneTimeSender $ch
    return $routine
}

# ch should be a newly created channel with exclusive rights for sending
proc ::csp::OneTimeSender {ch} {
    # unconditionally push the callback arguments (returned by yield) to the channel
    set arg [yield]
    if {![CClosed $ch]} {
        CAppend $ch $arg
        $ch close
        SetResume
    }
}


# EXPERIMENTAL - works well with Tk sparse events like mouse clicks or key press buttons
# The problem is that in another typical use case i.e. reading from nonblocking file/socket chan
# the constant hammering with events from fileevent starves other coroutines 
# and overflows the channel buffer here.
# 
# Create a channel and a callback handler being a coroutine which when called
# will send callback single argument to the newly created channel
# The library user should only receive from that channel
# It is designed for multi-time callbacks i.e. the coroutine resumes after every callback. 
# The limitation is that the callback must be a single- or zero- argument callback
# In case of no-argument callback empty string is sent to the channel
proc ::csp::->> {chVar {maxbuffer 1000}} {
    upvar $chVar ch
    channel ch $maxbuffer
    CMakeReadOnly $ch
    # this coroutine will not be registered in Routine array for unblocking calls
    # it should be called from the userland callback
    set routine [NewRoutine]
    coroutine $routine MultiSender $ch
    return $routine
}

proc ::csp::MultiSender {ch} {
    while 1 {
        set arg [yield]
        # terminate the coroutine if channel not ready for send 
        if {![CSendReady $ch]} {
            break
        }
        # push the callback arguments (returned by yield) to the channel
        CAppend $ch $arg
        SetResume
    }
}

proc ::csp::Forward {from to} {
    range msg $from {
        # destination channel may be closed in the meantime so catch error and don't break the loop
        # it is to clear the $from channels 
        catch {$to <- $msg}
    }
}

proc ::csp::forward {froms to} {
    foreach from $froms {
        go ::csp::Forward $from $to
    }
}

# The select command provides a way to handle multiple channels. 
# It is a switch like statement where channels are evaluated for readiness.
# The select command makes the coroutine wait until at least one channel is ready.
# If multiple can proceed, select chooses pseudo-randomly.
# A default clause, if present, executes immediately if no channel is ready.
proc ::csp::select {a} {
    set ready 0
    # ensure that operator and channel are substituted only once
    set substa {}
    foreach {op ch body} $a {
        if {$op eq "default"} {
            lappend substa $op $ch $body
        } else {
            lappend substa [uplevel subst $op] [uplevel subst $ch] $body
        }
    }
    while {$ready == 0} {
        # (op ch body) triples ready for send/receive
        set triples {}
        set default 0
        set defaultbody {}
        set operator {}
        foreach {op ch body} $substa {
            if {[IsOperator $ch]} {
                lassign [list $op $ch] ch op
                if {$op ni $operator} {
                    lappend operator $op
                }
                if {[CSendReady $ch]} {
                    lappend triples s $ch $body
                }
            } elseif {[IsOperator $op]} {
                if {$op ni $operator} {
                    lappend operator $op
                }
                if {[CReceiveReady $ch]} {
                    lappend triples r $ch $body
                }
            } elseif {$op eq "default"} {
                set default 1
                set defaultbody $ch
            } else {
                error "Wrong select arguments: $op $ch"
            }
        }
        if {[llength $operator] == 0} {
            error "<- or <-! operator required in select"
        }
        if {[llength $operator] > 1} {
            error "<- and <-! should not be mixed in a single select"
        }
        CheckOperator $operator
        set ready [expr {[llength $triples] / 3}]
        if {$ready == 0} {
            if {$default == 0} {
                Wait$operator
            } else {
                return [uplevel $defaultbody]
            }
        }
    }

    if {$ready == 1} {
        set triple $triples
    } else {
        set random [expr {round(floor(rand()*$ready))}]
        set triple [lrange $triples [expr {$random * 3}] [expr {$random * 3 + 2}]]
    }
     
    lassign $triple op ch body
    return [uplevel $body]
}

proc ::csp::timer {chVar interval} {
    upvar $chVar ch
    after $interval [-> ch] {[clock microseconds]}
    return $ch
}

proc ::csp::TickerRoutine {ch interval closeafter} {
    if {![CClosed $ch]} {
        if {[regexp {#(\d+)} $closeafter _ ticksleft]} {
            if {$ticksleft <= 0} {
                $ch close
                return
            }
            set closeafter #[incr ticksleft -1]
        }
        $ch <- [clock microseconds]
        after $interval csp::go TickerRoutine $ch $interval $closeafter
     }
}

proc ::csp::ticker {chVar interval {closeafter 0}} {
    upvar $chVar ch
    csp::channel ch
    if {$closeafter != 0 && [string is integer -strict $closeafter]} {
        after $closeafter $ch close
    }
    after $interval csp::go TickerRoutine $ch $interval $closeafter
    return $ch
}


# receive from channel until closed in coroutine
proc ::csp::range {varName ch body} {
    if {[info coroutine] eq ""} {
        error "range can only be used in a coroutine"
    }
    uplevel [subst -nocommands {
        while 1 {
            try {
                set $varName [<- $ch]
                $body
            } on error {out err} {
                break
            }
        }
    }]
}

# receive from channel until closed in main control flow
proc ::csp::range! {varName ch body} {
    if {[info coroutine] ne ""} {
        error "range! should not be used in a coroutine"
    }
    uplevel [subst -nocommands {
        while 1 {
            try {
                set $varName [<-! $ch]
                $body
            } on error {out err} {
                break
            }
        }
    }]
}
