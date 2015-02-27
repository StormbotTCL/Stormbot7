# TAIL!

# ISEGGCORECMD must be first!
proc iseggcorecmd cmd { expr ![string eq "" [info commands $cmd]] && [string eq "" [info procs $cmd]] }

proc validcmd cmd { expr ![string eq "" [info commands $cmd]] }
proc validproc cmd { expr ![string eq "" [info procs $cmd]] }

proc sb7 { args } { # General
	global sb7
     lassign $args cmd 1 2 3 4 5 6 7 8 9      
	switch -exact -- [string tolower $cmd] {

		sec - security {
			foreach var [info vars ::_*] { if [regexp -nocase -- {^(\:\:)?_[a-z]+[1-9]$} $var] { unset $var } }
			return
		}

		log {
			set data [join [join [lrange [split $args] 1 end]]] ; # Double-JOINted.... :p
			set level [info level]
			incr level -1
			putloglev 7 * "\[[effects [lindex [info level $level] 0] up b]\] $data"
			return
		}

		debug {
# Does not work. :( May have to re-write this custom (or send "UPLEVEL 2" to DEBUG below)
			data set @temp:debug $args
			uplevel 1 { debug -log 7 [data get @temp:debug] }
			data set @temp:debug ""
			return
		}

		header {
			# Used by: GMT:FORMAT
			set level [info level]
			incr level -1
			if [isnum -integer $args] { incr level -$args }
			if $level { set header [lindex [info level $level] 0] } { set header global }
			return "\[[string toupper $header]\]"
		}

		command {
			set valid [list cmd source level original flags abbr extra proc]
			switch -exact -- [string tolower $1] {

				add {
					set validflags [list -none -badchan:ok -logout:ok -suspended:ok -auth -chanspecific -locklevel -corecommand -redirect]
					set source [file tail [data get @BEAD]]
					set proc @[string tolower $2]
					set flags [lassign $args - - cmd level]
					#flags -simple $flags [lsearch -all -inline -glob $flags -*] abbr flags
					flags -simple [lrange $args 4 end] $validflags abbr flags
					empty extra; # Will be set later by other commands (SUSPEND, et al)
					if [data array notempty @COMMANDLIST $cmd] { data array unset @COMMANDLIST $cmd }
#debug =COMMAND/ADD cmd source level flags abbr extra proc
					# Level twice: one for "active" and one for "original" (default)
					data array set @COMMANDLIST $cmd [list $cmd $source $level $level $flags $abbr $extra $proc]
					set src [data get @BEAD:SOURCE]
					if [notempty src] { data array set @BEAD:SOURCE $cmd $src }

					# Abbreviations?
					if [notempty abbr] { sb7 abbr add $cmd $abbr }

					# Binds ??
					if [data array value -boolean CONFIG bind:msg] { sb7:bind rename $cmd msg ; bind MSG  - $cmd sb7:dispatch:msg }
					if [data array value -boolean CONFIG bind:dcc] { sb7:bind rename $cmd dcc ; bind DCC  - $cmd sb7:dispatch:dcc }
					if [data array value -boolean CONFIG bind:not] { sb7:bind rename $cmd not ; bind NOTC - *    sb7:dispatch:not }
					return $cmd
				}

				del {
					if ![sb7 command check $2] { error "\[SB7 COMMAND DEL\] No such command: $2" }
					data array unset @COMMANDLIST $2
					if [data array value -normalize CONFIG bind:msg] { catch { unbind MSG - $cmd sb7:dispatch:msg ; sb7:bind restore $cmd } }
					if [data array value -normalize CONFIG bind:dcc] { catch { unbind DCC - $cmd sb7:dispatch:dcc ; sb7:bind restore $cmd } }
					if [data array value -normalize CONFIG bind:not] { catch { unbind NOTC - $cmd sb7:dispatch:not ; sb7:bind restore $cmd } }

					# When we delete commands, we also need to
					# delete the corresponding abbreviations.

					return $2
				}

				list { return [lsort -inc -dict [data array names @COMMANDLIST]] }

				get {
					set data [data array value @COMMANDLIST $2]
					if [isempty data] { error "\[SB7 COMMAND GET\] Unknown command: $2" }
					if [isempty 3] { return $data }
					set l [lsearch -exact $valid [uniquematch $valid $3]]
					if { $l == -1 } { error "\[SB7 COMMAND GET\] Unknown option: $3" }
					return [lindex $data $l]
				}

				set {
					set data [data array value @COMMANDLIST $2]
					if [isempty data] { error "\[SB7 COMMAND SET\] Unknown command: $2" }
					set l [lsearch -exact $valid [uniquematch $valid $3]]
					if { $l == -1 } { error "\[SB7 COMMAND SET\] Unknown option: $3" }
					if [isempty 4] { error "\[SB7 COMMAND SET\] Unknown new value for $3" }
					if ![isnum -integer $4] { error "\[SB7 COMMAND SET\] Illegal value for ${3}: $4" }
					lset data $l $4
					data array set @COMMANDLIST $2 $data
					return $4
				}

				iscmd - isvalid - valid - check {
					set 2 [string tolower $2]
					set cmds [string tolower [data array names @COMMANDLIST]]
					if { [lsearch -exact $cmds $2] != -1 } { return 1 } { return 0 }
				}

				clear { data array unset @COMMANDLIST ; return }

				default { error "\[SB7 COMMAND\] Unknown command option: $1" }

			}
		}

		abbr {
			##if [string eq "" $2] { set abbr "" } { set abbr [data array value ABBR $2] }
			#if ![string eq "" $2 ] { set abbr [data array value ABBR $2] }
#putlog "\00301,00\[SB7 ABBR\] [info level [info level]]\003"
			set abbr [data array get ABBR $2]
			switch -exact -- [string tolower $1] {

				add {
					if ![sb7 command check $2] { error "\[SB7 ABBR ADD\] Unknown command: $2" }
					if [isempty 2] { error "\[SB7 ABBR ADD\] Missing keyword" }
					if [isempty 3] { error "\[SB7 ABBR ADD\] Missing abbreviation to add to $2" }
					data array set ABBR $2 [lsort -inc -uni [concat $abbr [join [lrange $args 3 end]]]]
					return $abbr
				}

				del {
					if ![sb7 command check $2] { error "\[SB7 ABBR DEL\] Unknown command: $2" }
					if [isempty 2] { error "\[SB7 ABBR DEL\] Missing keyword" }
					if [isempty 3] { error "\[SB7 ABBR DEL\] Missing abbreviation to remove from $2" }
					data array set ABBR $2 [ldestroy -nonulls -all -nocase -unique -multiple -increasing -- $abbr [join [lrange $args 3 end]]]
					return $abbr
				}

				get {
					if ![sb7 command check $2] { error "\[SB7 ABBR GET\] Unknown command: $2" }
					return [lsort -inc -uni -dict $abbr]
				}

				check {
					set 2 [string tolower $2]
					foreach a [data array names ABBR] {
						set abbr [data array value -lower ABBR $a]
						if { [lsearch -exact $abbr $2] != -1 } { return $a }
					}
					return $2
				}

				find  {
					set 2 [string tolower $2]
					foreach a [data array names ABBR] {
						if [isempty a] continue ; # This shouldn't trigger: DATA ARRAY for ABBR should have list-enabled nulls; FOREACH would not activate otherwise.
						set abbr [data array value ABBR $a]
						if { [lsearch -exact $abbr $2] != -1 } { return $a }
					}
					return
				}

				clear { data array unset ABBR ; return }

				default { error "\[SB7 ABBR\] Unknown option: $1" }

			}
		}

		register {
			if [isempty 2] return
			set valid [list global user chan block]
			set match [uniquematch $valid $1]

			switch -exact -- $match {

				user - global - chan - block {
					set data [data array value %VARIABLES $match]
					set data [lsort -increasing -unique [concat $data [lrange $args 2 end]]]
					data array set %VARIABLES $match $data
					return $data
				}

				del {
					set match [uniquematch $valid $2]
					if [isempty match] { error "\[SB7 REGISTER DEL\] Unknown data type: $2" }
					set data [data array value %VARIABLES $match]
					set data [ldestroy -multiple -all -nocase -nonulls -unique -increasing $data [lrange $arg 3 end]]
					data array set %VARIABLES $match $data
					return $data
				}

				clear {
					set match [uniquematch $valid $2]
					if [isempty match] { error "\[SB7 REGISTER CLEAR\] Unknown data type: $2" }
					data array set %VARIABLES $match
					return
				}

				default { error "\[SB7 REGISTER\] Unknown option: $1" }

			}
		}

		component {
			if [instr $1 /] { return $1 }
			#set dir [file dirname [file normalize $::config]]/scripts/sb7
			set dir ./scripts/sb7
			foreach dir [list ./scripts/sb7 ./scripts/sb7/[string tolower $::nick]] {
				set list [list %.sb7 sb7_%.tcl sb7%.tcl % sb7_% %.tcl %.txt sb7_%.txt]
				if { [lsearch -exact [list "" core] $1] != -1 } { empty 1 ; set list [ldestroy -all -nonulls $list %] } ; # Core (the FILE EXISTS will match the directory itself! ): )
				foreach element $list {
					regsub % $element $1 temp
					foreach name [glob -nocomplain -directory $dir -tails -- $temp] {
#debug dir element name =EXISTS([file exist ${dir}/$name])
						if [file exists ${dir}/$name] { return ${dir}/$name }
					}
				}
			}
			return $1
		}

		short - shortname {
			set tail [file tail $1]
			set tail [file rootname $tail]
			regsub ^sb7_ $tail "" tail
			if [string eq sb7 $tail] { set tail core }
			if [string eq "" $tail] { set tail core }
			return $tail
		}

		target {
			# Allow "." and "" to return the bot's root directory
			# (because it won't match anything)
			set root [file dirname [file normalize $::config]]
			switch -glob -- [string tolower $2] {

				stormbot*.tar.gz { set target ${root}/scripts }

				*.sb7 - sb7* { set target ${root}/scripts/sb7 }

				*.tcl { set target ${root}/scripts }

				default { set target $root }

			}
			switch -exact -- [string tolower $1] {

				dir { return $target }

				file { return ${target}/$2 }

				default { ? "SB7 TARGET [string toupper $1]" }

			}
			? SB7:TARGET
		}

		error {
			# Optional RETURN-level? Hmmmm....
			if [isempty 3] { swap 2 3 }
			set level [ expr [ info level ] - 1 ]
			if [isempty 2] { set 2 * }
			if [string eq * $2] {
				set 2 [lindex [info level $level] 0]
				# Only works when $2 is "" or *
				#! Don't do this ~ let's reveal the true source! :: if [left $2 8 sb7:cmd_] { set 2 [mid $2 9] }
			}
			#! Don't do this ~ let's reveal the true source! :: if [left $2 8 sb7:cmd_] { set 2 [mid $2 9] }
			print -help $1 "[iff [notempty 2] "\[[string toupper $2]\] "]$3"
			return -code return 0 ; # The old method threw a "RETURN 0"
		}

		killtimer {
			flags -simple [concat $1 $2] -quiet text flags
			zero count
			foreach timer [timers] {
				lassign $timer - do id
				if [string match -nocase $text $do] {
					incr count
					killtimer $id
					if ![validflag -quiet] { putlog "\[SB7 KILLTIMER\] Killed TIMER #$id ($do)" }
				}
			}
			foreach utimer [utimers] {
				lassign $utimer - do id
				if [string match -nocase $text $do] {
					incr count
					killutimer $id
					if ![validflag -quiet] { putlog "\[SB7 KILLTIMER\] Killed UTIMER #$id ($do)" }
				}
			}
			return $count
		}

		addtimer {
			sb7 killtimer -quiet $2
			return [timer $1 $2]
		}

		addutimer {
			sb7 killtimer -quiet $2
			return [utimer $1 $2]
		}

		vars - setvars - setvariables - parseflags {
			# We need to parse for flags from index #1 and after, BUT,
			# we ALSO need to stick the "command keyword" back into
			# index #0. We'll LINSERT it back in after the flag scan.

			upvar 1 arg arg flags flags
#debug arg args =L1E/ARGS([lrange $args 1 end]) flags
##			set flarg  [lrange $arg  1 end]
##			set flargs [lrange $args 1 end]
##			if { ( [llength $flargs] == 1 ) && ( [llength [join $flagss]] != 1 ) } { set flargs [join $flargs] }
##			flags:simple $flarg $flargs ::sb7_parseflags_text flags
			flags:simple [lrange $arg 1 end] [lrange $args 1 end] ::sb7_parseflags_text flags
#debug =0 ::sb7_parseflags_text flags
			set ::sb7_parseflags_text [linsert $::sb7_parseflags_text 0 [lindex $arg 0]]
#debug =1 ::sb7_parseflags_text flags
			uplevel 1 {
				if [isnum -integer $nick] {
					set idx2nick [idx2nick $nick]
					set idx2host \[DCCIDX:${nick}\]
				} {
					set idx2nick $nick
					set idx2host $host
				}
#putcmdlog [effects <parseflags> b u up 4,1]
				foreach _LOOP [list 0 1 2 3 4 5 6 7 8 9] {
#putcmdlog =====0:_LOOP($_LOOP):LINDEX([lindex $::sb7_parseflags_text $_LOOP])
					set ${_LOOP} [lindex $::sb7_parseflags_text $_LOOP]
#putcmdlog =====1
					set ${_LOOP}l [string tolower [lindex $::sb7_parseflags_text $_LOOP]]
#putcmdlog =====2
					set ${_LOOP}u [string toupper [lindex $::sb7_parseflags_text $_LOOP]]
#putcmdlog =====3
					set ${_LOOP}s [string totitle [lindex $::sb7_parseflags_text $_LOOP]]
#putcmdlog =====4
					set ${_LOOP}t [stt [lindex $::sb7_parseflags_text $_LOOP]]
#putcmdlog =====5
# Let the JOIN loop below set this                         set _ERROR [ catch { set ${_LOOP}j [join [lindex $::sb7_parseflags_text $_LOOP]] } _ERROR2]
#putcmdlog =====6
# Let the JOIN loop below set this                         if $_ERROR { set ${_LOOP}j [unescape [lindex $::sb7_parseflags_text $_LOOP]] }
#putcmdlog =====7
					set ${_LOOP}p [split [lindex $::sb7_parseflags_text $_LOOP]]
#putcmdlog =====8
					set ${_LOOP}e [lrange $::sb7_parseflags_text $_LOOP end]
#putcmdlog =====9
					foreach _LOOP2 [lsort -dictionary -increasing [info vars ${_LOOP}*]] {
#putcmdlog =====10:_LOOP2($_LOOP2)
						set _NAMETEMP [left $_LOOP2 1]j[mid $_LOOP2 2]
#putcmdlog =====11:_LOOP2($_LOOP2):_NAMETEMP($_NAMETEMP)
						set _ERROR [ catch { set $_NAMETEMP [join [set ${_LOOP2}]] } _ERROR2]
#putcmdlog =====12:_LOOP2($_LOOP2):_NAMETEMP($_NAMETEMP):_ERROR($_ERROR):_ERROR2($_ERROR2):=SET([set $_LOOP2])
						if $_ERROR { set $_NAMETEMP [unescape [set $_LOOP2]] }
#putcmdlog =====13:_NAMETEMP($_NAMETEMP)
					}
#putcmdlog =====20
				}
#putcmdlog [effects </parseflags> b u lower 3,1]
			}
			return ""
		}

		auth {
#putlog "\[AUTH\] 00:CMD($cmd):ARGS($args)"
			set args [lassign [lrange $args 1 end] cmd user arg1 arg2]
#putlog "\[AUTH\] 01:ARGS($args)"
			set auth [data array value @AUTH $user]
			set outh [data array value @OUTH $user]
			lassign:array authed $auth handle nick host
#putlog "\[AUTH\] 04:AUTH($auth):OUTH($outh)"
			switch -exact -- [string tolower $cmd] {
		
				authed - isauth - isauthed - check {
					set message "\[SB7 AUTH [string toupper $cmd]\] Use \"is authed\" instead! From: [info level [expr [info level] -1]]"
					rawhome $message
					error $message
					# Obviously, the below-listed code is useless. :p
					lassign $auth _handle _nick _host
					if { ![valididx $arg1] && ![string eq -nocase $_nick $arg1] } { return 0 }
					if { [notempty arg2] && [notempty _host] && ![valididx $arg1] } { if ![string eq -nocase $_host $arg2] { return 0 } }
					return 1
				}
		
				outhed - isouth - isouthed {
					set message "\[SB7 AUTH [string toupper $cmd]\] Use \"is outhed\" instead! From: [info level [expr [info level] -1]]"
					rawhome $message
					error $message
					# Obviously, the below-listed code is useless. :p
					lassign $outh _handle _nick _host
					if { ![valididx $arg1] && ![string eq -nocase $_nick $arg1] } { return 0 }
					if { [notempty arg2] && [notempty _host] && ![valididx $arg1] } { if ![string eq -nocase $_host $arg2] { return 0 } }
					return 1
				}
		
				auth -  login -  set { data array set @AUTH $user [list [casehandle $user] $arg1 $arg2] ; return }
		
				outh - ologin - oset { data array set @OUTH $user [list [casehandle $user] $arg1 $arg2] ; return }
		
				get {
					if ![validuser $user] { error "\[SB7 AUTH GET\] Unknown user: $user" }
					set data [data array value @AUTH $user]
					switch -exact -- [string tolower $arg1] {
		
						handle { return [lindex $data 0] }
		
						nick   { return [lindex $data 1] }
		
						host   { return [lindex $data 2] }
		
						default { error "\[SB7 AUTH GET\] Unknown command: [string toupper $user]" }
		
					}
				}
		
				find {
					# Loop through data array get {A b}, match by index, return REQUESTED type!
					# (if NICK is specified, return the matching NICK [presumably matched by
					# the handle])
		
					switch -exact -- [string tolower $user] {
						handle { set l 0 ; set c [list 1 2] }
						nick   { set l 1 ; set c [list 0 2] }
						host   { set l 2 ; set c [list 0 1] }
						default { error "\[SB7 AUTH FIND\] Unknown data type: $user" }
					}
#putlog "\[SB7 AUTH FIND\] 01:ARG1($arg1):L($l):C($c)"
					set who $arg1
					if [isnum -integer $arg1] {
						switch -exact -- $l {
							0 { set who [idx2nick $arg1] }
							1 { #set who [idx2hand $arg1] -- Breaks DCC auto-auths! }
							2 { #set who \[DCCIDX:$arg1\] -- Breaks DCC auto-auths! }
							default ?
						}
					}
					foreach { a b } [data array array:get @AUTH] {
						foreach d $c {
							if [string eq -nocase $who [lindex $b $d]] { return [lindex $b $l] }
						}
					}
					return
				}
		
				logout {
					# Take out both LOGIN & OLOGIN!
					data array set @AUTH $user
					data array set @OUTH $user
					return
				}
		
				default { error "\[SB7 AUTH\] Unknown command: [string toupper $cmd]" }
			}
		}

		fixlevel {
			if ![validuser $1] return
			if [notempty 2] {
				if ![validchan $2] return
				set current [userinfo get $1 ACCESS:$2]
				if [notempty current] return
				set list [list b 0 &n 500 &m 400 &o 100 &h 75 &v 50 &f 1]
				foreach { flag level } $list { if [matchattr $1 $flag $2] { access set $1 $level $2 ; return $level } }
				return
			}
			set current [userinfo get $1 ACCESS:GLOBAL]
			if [notempty current] return
			set list [list b 0 n 900 m 800 t 700 o 600 h 575 v 550 f 501]
			foreach { flag level } $list { if [matchattr $1 $flag] { access set $1 $level ; return $level } }
			return
		}

		default { error "\[SB7\] Unknown command option: $cmd" }

	}
}

# Can NOT be put into SB7 command due to the PUB / MSG / DCC / PUBM BINDs.
proc sb7:dispatch { nick host handle chan arg } {
	# Set execution time
	array set time [list 0 [clock seconds] 1 0 2 0 3 0]

	# Get command
	set original $arg
#debug =0 arg
	#set arg [ldestroy {forgot -all !!} -nonulls [split $arg]]
	set arg [split $arg]
	set arg [lsearch -all -inline -not $arg ""]
#debug =1 arg

	# Handle MACROs (will return a LIST of commands to execute (will process the "||" splitter internally))
#if ![string match -nocase *LOGIN* $cmd] { debug =1 cmd arg }
	if ![string eq "" [info procs @macro:process]] {
		set macro [@macro:process $nick $host $handle $chan $arg]
		if ![string eq "" $macro] {
			foreach new_arg $macro { sb7:dispatch $nick $host $handle $chan $new_arg }
			return 1
		}
	}
#if ![string match -nocase *LOGIN* $cmd] { debug =2 cmd arg }

	set cmd [string tolower [lindex $arg 0]]
#putlog [effects DISPATCH:09 11,12 bold]:CMD($cmd)
	# Check for abbreviations
	set abbr [sb7 abbr check $cmd]
	if ![string eq $abbr $cmd] {
		set cmd $abbr
		set arg [lreplace $arg 0 0 $abbr]
	}

	# Assign attributes
	lassign:array cmdinfo [data array value -lower @COMMANDLIST $cmd] cmd source level original flags abbr extra proc
#debug cmd abbr level =COMMANDLIST([data array get @COMMANDLIST $cmd])
#putlog [effects DISPATCH:10 11,12 bold]:CMD($cmd):ABBR($abbr)

	# User-adjusted command level?
	set newlevel $cmdinfo(level)
	foreach a $cmdinfo(extra) { if [isnum -integer $a] { set newlevel $a } }
	set cmdinfo(level) $newlevel
#putlog [effects DISPATCH:10.1 11,12 bold]:CMD($cmd):ABBR($abbr)

	#######################################################################
	# Fixing a forever-old problem: if common hostmask, is this an authed #
	# user (so we can figure out which handle to assign)? Since all PROCs #
	# draw the $HANDLE from here (and WHOIS checks by nick first), we can #
	# fix this in one place: here                                         #
	#######################################################################

	# 2014-12-18 14:20:00 -0800: Moved to up here because MACRO may need this clarified before we go any further ....

	set h [sb7 auth find nick $nick] ; # Make sure we use the logged-in handle, not the mask-matched handle!
	if [notempty h] { set handle $h }

#putlog [effects DISPATCH:10.2 11,12 bold]:CMD($cmd):ABBR($abbr)
	# Valid command?

	if [isempty cmdinfo(cmd)] {
#putlog [effects DISPATCH:10.1 11,12 bold]
		print -error $nick "\[SB7\] Unknown command: $cmd"
		return 1
	}

	# Target channel?
	# 1/Check if channel is specified
	# 2/If not: did the user use a command (targetting a channel) recently?
	# 3/If not: is command channel-exempt? (-INVALID:OK)
	# 4/If not: use $HOME / [HOME]
	# 5/At some stage, $TCHAN should be occupied; it should never be empty

	set tchan [lindex $arg 1]
	if { [lsearch -exact [explode [data array value -default CONFIG CHAN:VALID #]] [left $tchan 1]] != -1 } {
		set arg [lreplace $arg 1 1] ; # This or LRANGE?
	} {
		set tchan $chan
	}
#msghome @COMMANDLIST([data array value @COMMANDLIST $cmd])

	# Store reference data (needed for PRINT)
	if [string eq * $tchan] { set tchan [data array get @LAST:CHAN $nick] } ; # set tchan [data array get @LAST:CHAN $nick $chan]
	if [string eq # $tchan] { set tchan [data array get @LAST:CHAN $nick] }
	if [string eq ## $tchan] { set tchan [home] }
	data array set @LAST:CHAN $nick $tchan
	# -DEFAULT is needed because the PUB bind, which comes straight here, isn't set into @LASTBIND like the others are
	data array set @OUTPUT $nick [list $nick $host $handle $tchan [data get -default @LASTBIND pub]]
#putlog "\[SB7:DISPATCH\] NICK($nick):HOST($host):HANDLE($handle):CHAN($chan):TCHAN($tchan):LASTBIND([data get @LASTBIND]):@OUTPUT([data array get @output $nick])"
	# ALL OUTPUT MUST COME AFTER THIS POINT!!

# if not -CHANSPECIFIC, ignore "*" as a chan (used for commands like LOGIN & REHASH)
	if [string eq * $tchan] {
		if { [lsearch -exact $cmdinfo(flags) -chanspecific] != -1 } { print -help -return $nick "\[SB7\] You must specify a channel in which to apply the $cmdinfo(cmd) command." }
	} {
		if { ![validchan $tchan] && ( [lsearch -exact $cmdinfo(flags) -ok:invalid] == -1 ) } { print -help -return $nick "\[SB7\] Illegal channel: $tchan" }
	}
# Why? -- set tchan [iff ![validchan $tchan] [data array value @OUTPUT LAST:$nick] $tchan]
	set chan $tchan
	data array lset @OUTPUT $nick 3 $tchan
	data array set @OUTPUT LAST:$nick $chan

#putlog [effects DISPATCH:11 11,12 bold]
	# Suspended command?
	if { [lsearch -exact [string tolower $cmdinfo(extra)] [string tolower SUSPENDED:GLOBAL]] != -1 } { print -error $nick "\[SUSPEND\] [string toupper $cmd] is globally suspended." ; return 0 }
	if { [lsearch -exact [string tolower $cmdinfo(extra)] [string tolower SUSPENDED:$chan]] != -1 } { print -error $nick "\[SUSPEND\] [string toupper $cmd] is suspended on ${chan}." ; return 0 }

#putlog [effects DISPATCH:12 11,12 bold]
	# User logged in?
	set access [access higher $handle $chan]
	if { $cmdinfo(level) > 0 } {
		if { [lsearch -exact $cmdinfo(flags) -logout:ok] == -1 } {
			if ![is authed $handle $nick $host] { print -private -error $nick "\[SB7\] Who are you?" ; return 0 }
			if { $cmdinfo(level) > $access } { print -private -error $nick "\[SB7\] You don't have access to the [string toupper $cmd] command." ; return 0 }
		}
	}

#putlog [effects DISPATCH:13 11,12 bold]:CMD($cmdinfo(cmd)):LEVEL($cmdinfo(level))
	#  =1000 command: user o-authed?
	if { $cmdinfo(level) >= 1000 } {
		if { [lsearch -exact $cmdinfo(flags) ok:logout] == -1 } {
			if ![is oauthed $handle $nick $host] { print -private -error $nick "\[SB7\] Who are you?" ; return 0 }
		}
	}

#putlog [effects DISPATCH:14 11,12 bold]
	# If authcmd, obfuscate $ARG
	if { [lsearch -exact $cmdinfo(flags) AUTHCMD] != -1 } {
		# Logging will be done later, after the command is thrown;
		# obfuscate there, not here
	}

#putlog [effects DISPATCH:15 11,12 bold]
	# -AUTHCMDCONT flag deprecated

#putlog [effects DISPATCH:16 11,12 bold]
	# Parse control flags ( --force:chan , et al )
	set valid_dispatch_flags [list notice msg chan burst mute]
	empty dispatch_flags
	# "--?" is needed because of FLAGS ("--" is being stripped here!)
	set check_flags [lsearch -all -glob $arg --?*]
#putlog "\[SB7:DISPATCH\] CHECK_FLAGS($check_flags)"
	if [notempty check_flags] {
		foreach a [lsort -int -dec $check_flags] {
			set check_flag [lindex $arg $a]
#putlog "\[SB7:DISPATCH\] CHECK_FLAG($check_flag):STL/TRIM([string tolower [string trimleft $check_flag -]])"
			if { [lsearch -exact [string tolower $valid_dispatch_flags] [string tolower [string trimleft $check_flag -]]] == -1 } continue
			lappend dispatch_flags [lindex $arg $a]
			set arg [lreplace $arg $a $a]
		}
	}
#putlog DISPATCH_FLAGS($dispatch_flags)
	data array set @OUTPUT:OVERRIDE $nick $dispatch_flags

#putlog [effects DISPATCH:20 11,12 bold]:ARG($arg)
	# Dispatch command
	set time(1) [clock clicks -milliseconds]
#putlog [effects DISPATCH:20.1:TIME\[1\]($time(1)) 11,12 bold]:CMDINFO\[PROC\]($cmdinfo(proc)):NICK($nick):HOST($host):HANDLE($handle):CHAN($chan):ARG($arg)
	set error [ catch { $cmdinfo(proc) $nick $host $handle $chan $arg } uh_oh ]
#debug error $cmdinfo(proc)
	set time(2) [clock clicks -milliseconds]
#putlog [effects DISPATCH:20.2:TIME\[2\]($time(2)) 11,12 bold]:ERROR($error):UH_OH($uh_oh)
	set time(3) [ expr $time(2) - $time(1) ]

	# Is command an original Eggdrop command that was rebound?
	if [string eq -nocase HELP [lindex $arg 1]] {
		if [sb7:bind rebound $cmd] {
			if { [lsearch -exact [list msg dcc] [data get -lower @LASTBIND]] != -1 } {
				switch -exact -- [data get -lower @LASTBIND] {
					msg { print $nick "\[[string toupper $cmd]\] The [string toupper $cmd] command is a core Eggdrop command\; the original command has been rebound to \"/${cmd}\" for /MSG commands: /msg $::botnick /${cmd} <args>" }
					dcc { print $nick "\[[string toupper $cmd]\] The [string toupper $cmd] command is a core Eggdrop command\; the original command has been rebound to \"./${cmd}\" for .DCC commands: ./${cmd} <args>" }
					not { # Do Nothing # }
					pub { # Do Nothing # }
					pubm { # Do Nothing # }
					default { # Do Nothing # }
				}
			}
		}
	}

	data set @LASTBIND

#putlog [effects DISPATCH:21 11,12 bold]
	# If authcmd, obfuscate $ARG
	if { [lsearch -exact $cmdinfo(flags) -authcmd] != -1 } { set arg "$cmd <password redacted>" }

#putlog [effects DISPATCH:22 11,12 bold]
	# If logging, ... log it!
	if [data array value -normalize CONFIG LOG] {
		set name ${::config}.command.global.log
		set append_error [ catch { set append [open $name a] } append_uh_oh ] 
		catch { puts $append [list $time(0) $time(3) $nick $host $handle $chan $cmd $arg $error $uh_oh] }
		catch { close $append }
		if $append_error { msghome "\[DISPATCH\] Error while writing command log ${name}: $append_uh_oh" }
		# Even if error, continue processing the command (error message?)
	}

#putlog [effects DISPATCH:23 11,12 bold]
	if [data array value -normalize CONFIG:$chan LOG] {
		set name ${::config}.command.${chan}.log
		set append_error [ catch { set append [open $name a] } append_uh_oh ] 
		catch { puts $append [list $time(0) $time(3) $nick $host $handle $chan $cmd $arg $error $uh_oh] }
		catch { close $append }
		if $append_error { msghome "\[DISPATCH\] Error while writing command log ${name}: $append_uh_oh" }
		# Even if error, continue processing the command (error message?)
	}

	# Account for normal RETURN codes of "" "0" & "1" (not really an error)
	if $error { if [info exists uh_oh] { if { [lsearch -exact [list {} 0 1] $uh_oh] != -1 } { zero error } } }

	#####
	# Do NOT exit the dispatcher prematurely: output overrides need to be
	# cleared (immediately before the final exit). Don't use ERROR here!
	#####

#putlog [effects DISPATCH:24 11,12 bold]
	if $error {
#putlog [effects DISPATCH:24.1 11,12 bold]:ERROR($error):UH_OH($uh_oh)
		set debuglevel [data array value -normalize CONFIG DEBUGLEVEL]
#putlog [effects DISPATCH:24.2:DEBUGLEVEL($debuglevel) 11,12 bold]
		switch -exact -- $debuglevel {

			0 {
				print $nick "An error occurred in the command [string toupper $cmd]: $uh_oh"
			}

			1 {
				print $nick "An error occurred in the command [string toupper $cmd]:"
				print $nick "Syntax: [iff {[lsearch -exact $cmdinfo(flags) AUTHCMD] == -1} $original "<password blocked>"]"
				print $nick "Time: [clock format $time(0) -format [data array value -default CONFIG FORMAT %c]] Length: $time(3)"
				print $nick "Error message: $uh_oh"
			}

			2 {
				print $nick "An error occurred in the command [string toupper $cmd]:"
				print $nick "Syntax: [iff {[lsearch -exact $cmdinfo(flags) AUTHCMD] == -1} $original "<password blocked>"]"
				print $nick "Error message: $uh_oh"
				print $nick "Time:[clock format $time(0) -format %Y%m%d%H%M%S] Length:$time(3)"
				print $nick "Please include the following debug information when reporting this bug (the coder will need all these details):"
				print $nick "SBV:[data get @VERSION] WWW:[data get @DISTRO] N:${::nick} BN:${::botnet-nick} AN:${::alt-nick} BH:[nick2hand $::botnick $chan] C:$chan EDV:[lindex $::version 0] NL:${::nick-len} HL:$::handlen TCLV:[info patchlevel] OS:$::tcl_platform(os) OSPLAT:$::tcl_platform(platform) OSV:$::tcl_platform(osVersion) OSBO:$::tcl_platform(byteOrder) OSTH:$::tcl_platform(threaded) OSM:$::tcl_platform(machine) OSWS:$::tcl_platform(wordSize) OSU:$::tcl_platform(user) LIB:$::tcl_library PWD:[pwd]"
				print $nick "NH:${nick}!$host H:$handle SC:$::server BO:[getopsymbol $nick $chan] AUTHED:[noyes [is authed $handle $nick $host]] ACCESS:[SB:accesslevel $handle]/[SB:accesslevel $handle $chan]"
				print $nick "LT:$::lasttrigger LB:$::lastbind CSL:[sb7:issuspended cmd $cmd $chan] CSG:[sb7:issuspended cmd $cmd] USL:[sb7:issuspended user $handle $chan] USG:[sb7:issuspended user $handle]"
				print $nick "CIL:$cmdinfo(level) CIF:[join $cmdinfo(flags) ,] CIF:[join $cmdinfo(flags) ,] CIE:[join $cmdinfo(extra) ,]"
			}

			default {
				# Do NOT exit the dispatcher here; need to clear the outptu overrides first!
				print -help -error $nick "\[DISPATCH\] Unknown DEBUGLEVEL value: $debuglevel"
			}

		}
#putlog [effects DISPATCH:24.3 11,12 bold]
	}
	data array set @OUTPUT:OVERRIDE $nick

#putlog [effects DISPATCH:30 11,12 bold]
	# We're done.
	return 0
}

proc sb7:dispatch:msg { nick host handle      arg } { data set @LASTBIND MSG ; sb7:dispatch $nick $host $handle * "$::lastbind $arg" ; return 0 }
proc sb7:dispatch:dcc {           handle idx  arg } { data set @LASTBIND DCC ; sb7:dispatch $idx * $handle [lindex [console $idx] 0] "$::lastbind $arg" ; return 0 }

# NOTC requires special handling 
proc sb7:dispatch:not { nick host handle arg { target "" } } {
	if ![string eq -nocase $target $::botnick] { return 0 }
	set 0 [lindex [lsearch -all -inline -not -exact [split $arg] ""] 0]
	if ![sb7 command check $0] { return 0 }
	data set @LASTBIND NOT
	sb7:dispatch $nick $host $handle * $arg
	return 0
}

# PUBM requires special checks
proc sb7:dispatch:pubm { nick host handle chan arg } {
	lassign [data array value CONFIG PUBCMD] active shortcut
	if ![normalize $active] return
	if [isempty shortcut] return
	set len [len $shortcut]
	set arg [string trimleft $arg]
	set 0 [join [lindex [lsearch -all -inline -not -exact [split $arg] ""] 0]]
	if ![left $0 $len $shortcut] return
	set test [mid $0 [ expr $len + 1 ]]
#debug active shortcut len 0 test

## 2015-01-07 00:56:58 -0800: Just let the main dispatcher worry about valid command / aliases / macros.
#	if ![sb7 command check [sb7 abbr check $test]] return
#	if { ![sb7 command check [sb7 abbr check $test]] && ![macro check $handle $test] } return

	data set @LASTBIND PUBM
	sb7:dispatch $nick $host $handle $chan [mid $arg [incr len]]
	return 0 ; # "RETURN 0" is needed to allow PUB BIND to trigger on this
}

# Delete me?
proc `sb7:macro_process { nick host handle chan cmd arg { var_cmd "" } { var_arg "" } } {
	if [notempty var_cmd] { upvar 1 $var_cmd new_cmd }
	if [notempty var_arg] { upvar 1 $var_arg new_arg }
	# Let local (user) macros override global ones.
	# 2014-12-18 14:32:00 -0800: How will "blank" (defined) macros survive CONCAT? (Test = "{}" used in proper places; the array integrity will hold)
	array set macros [concat [data array get macros *global] [data array get macro $handle]] ; # "*global" permitted in DATA to be saved to data file
	if ![info exists macros([string tolower $cmd])] {
		set new_cmd $cmd
		set new_arg $arg
		return
	}
	set macro $macros([string tolower $cmd])
	foreach a [list nick host handle chan cmd] { regsub -all "\\\$$a" $macro [set $a] macro }
	set macro [split $macro] ; # REGSUB will be placing LIST elements in to place; anticipate this
	foreach r [regexp -all -inline -nocase -- {[<\[](\d+\-*|\-*\d+|\d+)[\]>]} $macro] {
		if [regexp -- {^[<\[][^\]>]+[\]>]$} $r] {
			set original $r
			set r [mid $r 2 -1]
			switch -regexp -- $r {
				{^\d+$} { set:all begin end $r }
				{^\d+\-$} { set begin [left $r -1] ; set end [llength $arg] }
				{^\-\d+$} { set begin 1 ; set end [mid $r 2] }
				{^\d+\-\d+$} { lassign [split $r -] begin end }
				default { error "\[SB7:MACRO_PROCESS\] Illegal atom: <${r}>" }
			}
#debug original r begin end
			incr begin -1
			incr end -1
			set replace [lrange [split $arg] $begin $end]
			if [string match <*> $original] { if [isempty replace] { error "\[SB7:MACRO_PROCESS\] No data at position $r (${original}) in \$ARG: $arg" } }
			regsub -all [escape $original] $macro [lrange [split $arg] $begin $end] macro
#debug macro
		}
		if [regexp -- {^Q\[[^\]]+\]$} $r] {
			set original $r
			set r [mid $r 2 -1]
			switch -regexp -- $r {
				{^\d+$} { set:all begin end $r }
				{^\d+\-$} { set begin [left $r -1] ; set end [llength $arg] }
				{^\-\d+$} { set begin 1 ; set end [mid $r 2] }
				{^\d+\-\d+$} { lassign [split $r -] begin end }
				default { error "\[SB7:MACRO_PROCESS\] Illegal atom: \[${r}\]" }
			}
debug original r begin end
			incr begin -1
			incr end -1
			set replace [lrange [split $arg] $begin $end]
			regsub -all [escape $original] $macro [lrange [split $arg] $begin $end] macro
debug =\[\] r macro
		}
	}
	set macro [join $macro]
	set new_cmd [join [lindex [split $macro] 0]]
	set new_arg $macro
}

proc sb7:check_data_command_integrity args {
#logme
	# BOOTSTRAP: timered command (UTIMER 0 after boot)
	# Because "DATA" is such a common word for a bot, or any program,
	# did any other script overwrite it? Is it worth the "simplicity"
	# to risk using a common word susceptible to being overwritten?
	# Let's find out ....

	set bad 0
	set error [ catch { set value [data test] } crap ]
	if $error {
		set bad 1
	} {
		if ![info exists value] {
			set bad 1
		} {
			if ![string eq -nocase SB7 $value] {
				set bad 1
			}
		}
	}

	if $bad {# Stupid false matches on "PROC ..." when debugging ....
		set message "\00304,01\[SB7\] My data management command has been overwritten by another script! I can NOT function at all without this command! Check your other scripts for a line beginning with: \"proc \144ata \"\003\017"
		quit $message
		putlog $message
		after 1000
		die $message
	}
	return 0
}

proc sb7:setup_xtra_check args {
	set changed 0
	foreach user [userlist -b] {
		set data [getuser $user xtra]
		foreach pair $data {
			lassign $pair item value
			set stl [string tolower $item]
			if ![string eq $item $stl] {
				set changed 1
				set datum [getuser $user xtra $item]
				setuser $user xtra $item
				setuser $user xtra $stl $datum
			}
		}
	}
	if $changed { saveme user }
}

proc sb7:setup_userlevel args {
	foreach user [userlist -b] {
		sb7 fixlevel $user
		foreach chan [channels] { sb7 fixlevel $user $chan }
	}
	return 0
}

proc sb7:setup args {
#logme
	# Security clean-up
#	foreach var [info vars ::_*] { if [regexp -nocase -- {^(\:\:)?[a-f]+[1-9]$} $var] { unset $a } } ; # $VAR not $A
	foreach var [info vars ::_*] { if [regexp -nocase -- {^(\:\:)?_[a-f]+[1-9]$} $var] { unset $var } }

	global sb7

 	# Check all CONFIG:BLAH options; convert to DATA ARRAY
	# Check all CONFIG:BLAH options; convert to DATA ARRAY
	# MUST COME =BEFORE= DATA FILE LOAD!
	foreach a [array names sb7 config:*] {
		if ![string eq "" $sb7($a)] { data array set CONFIG [string range $a 7 end] $sb7($a) }
		unset sb7($a)
	}

	# LOGOUT is needed to remove stale logins (bot crash) & RESTART, but,
	# will be inconvenient on a REHASH. ):
	# Solution: check boot type! (:
	if ![data get -boolean @BOOT:TYPE] { sb7:logout * }

	# Check for USERINFO names
	@userinfo:check_valid

	# Load beads
	sb7:loadbeads * true

	# Read DATA file (this MUST override SB7.TCL settings!)
	# Must also override default command levels (thus, comes after LOADBEADS)
	data load

	# Process command level adjustments, put into command active level stream
	foreach a [data array names CMDLEVEL] {
		set data [data array value @COMMANDLIST $a]
		set level [data array value CMDLEVEL $a]
		lset data 1 $level
		data array set @COMMANDLIST $a $data
	}

	# Default setup variables (need to assure SOME values before we process)

	# Set these even if the CONFIG value is NOT empty (do always)!
	# if [data array value -isempty CONFIG SHORTCUT:ALL] { # }
		empty list
		#lappend list [string toupper [left $::nick 1][right $::nick 1]]
		# 1399779872: NO! Don't always want this! (SA / Santana)
		foreach a [list botnick botnet-nick alt-nick] {
			if [info exists ::${a}] { if ![string eq "" [set ::$a]] { lappend list [set ::$a] } }
		}
		data array set CONFIG shortcut:all [concat [data array value CONFIG shortcut:all] $list]
	# { # }

	if [data array value -isempty CONFIG GMT] { data array set CONFIG GMT [gmt:format decimal [clock format [clock seconds] -format %z]] }
	if [data array value -isempty CONFIG TZ ] { data array set CONFIG TZ  [clock format [clock seconds] -format %Z] }

	# Special default value checks: 0 / 1 (default to 0) (NOZERO will fix this :)
	foreach a [list bind:not give_p] { data array set CONFIG $a [data array value -normalize CONFIG $a] }

	# Special default value checks: 0 / 1 (default to 1)
	foreach a [list bind:pub bind:msg bind:dcc] {
		if [data array value -isempty CONFIG $a] { 
			data array set CONFIG $a 1 
		} { 
			data array set CONFIG $a [data array value -normalize CONFIG $a] 
		} 
	}

	# Special default value checks: USERLEVEL (just use the default definition)
	if [data get -isempty USERLEVELS] {
		set userlevels {
			unknown,-1,<0,|,unknown user
			noaccess,0,=0,|,no access
			chanfriend,1,1-49,|+f,Channel Friend
			chanvoice,50,50-74,+p|+fv,Channel Voice
			chanhalfop,75,75-99,+p|+fvl,Channel HalfOp
			chanop,100,100-299,+p|+fvlo,Channel Op
			chansuperop,300,300-399,+p|+fvlo,Channel SuperOp
			chanmaster,400,400-499,+p|+fvlom,Channel Master
			chanowner,500,=500,+p|+fvlomn,Channel Owner
			globalfriend,501,501-549,+fp,%GROUP% Helper
			globalvoice,550,550-574,+vfp,%GROUP% Global Voice
			globalhalfop,575,575-599,+lvfp,%GROUP% Global HalfOp
			globalop,600,600-699,+olvfp,%GROUP% Global Op
			globaltech,700,700-799,+tolvfp,%GROUP% Botnet Technician
			globalmaster,800,800-899,+mtolvfp,%GROUP% Co-Admin
			globalowner,900,900-999,+nmtolvfp,%GROUP% Admin
			botowner,1000,=1000,+jxnmtolvfp,%GROUP% Permanent Bot Owner
			primaryowner,1001,>1000,+heujxnmtolvfp,%GROUP% Primary Permanent Bot Owner
		}
	} {
		empty userlevels
	}

	## Process values if necessary
	## Should we BLANK all binds prior to re-assigning (given the advice in
	## SB7.TCL about changing these value)?
	# For now: yes
	foreach a [binds sb7:dispatch] { unbind [lindex $a 0] [lindex $a 1] [lindex $a 2] [lindex $a 4] }

	# Let's clean up the shortcut variables here
	foreach a [data array find CONFIG shortcut:*] { data array set CONFIG $a [lsort -unique -increasing -dictionary [data array value CONFIG $a]] }
	if [data array value -normalize CONFIG BIND:PUB] {
		foreach a [lsort -uni [data array value CONFIG shortcut:all]] { bind pub - [string tolower $a] sb7:dispatch }
		foreach a [lsort -uni [data array value CONFIG SHORTCUT:GLOBAL]] { bind pub vlotmn [string tolower $a] sb7:dispatch }
		foreach a [lsort -uni [data array value CONFIG SHORTCUT:OWNER ]] { bind pub n [string tolower $a] sb7:dispatch }
	}
	# CONFIG:BIND:MSG, CONFIG:BIND:DCC, & CONFIG:BIND:NOT will be handled by "COMMAND ADD"

	if [data array value -isempty CONFIG OUTPUT] {data array set CONFIG OUTPUT notice}

	if [data array value -isempty CONFIG PRECISION] {data array set CONFIG PRECISION 12}
	set ::tcl_precision [data array value CONFIG PRECISION]

	#%#
	foreach { a b } [data get abbr] {
		#
	}

	# Any changes to this will probably require a change in ADDUSER*
	set group [data array value CONFIG GROUP]
	if [notempty userlevels] {
		data set USERLEVELS
		foreach a [split $userlevels \n] {
			set a [trim $a]
			if [isempty a] continue
			foreach regexp [regexp -all -inline -- {[ ]+,|,[ ]+} $a] {
				regsub -all -- $regexp $a , a
			}
			if [isempty group] {
				regsub -all -nocase -- " %GROUP% " $a " " $a
				regsub -all -nocase -- "%GROUP% " $a " " $a
			} {
				regsub %GROUP% $a $group a
			}
			data lappend USERLEVELS [trim [split [trim $a] ,]]
		}
	}

	# Run all "setup" PROCs for each command
	foreach a [info procs sb7:setup_*] { eval $a } ; # (: I love TCL! :)
	# Done!
	return 0
}

#############################################################################

proc data args {
#logme *
	global sb7
	lassign $args cmd arg1 arg2 arg3 arg4 arg5 arg6; # Past this, we may need to extract flags

	switch -exact -- [string tolower $cmd] {

		test { return SB7 }

		file - filename { if [string eq -nocase TAIL $arg1] { return ${::nick}.data } { return [pwd]/${::nick}.data } }

		names { set p * ; if [notempty arg1] { set p [string tolower $arg1] } ; return [array names sb7 $p] }

		fetch - val - value - get {
			set arg2 [string tolower $arg2]
			# UNIQUEFLAGS will not cause a problem for -NOZERO vs -NORMALIZE
			# because -NOTEMPTY already causes it. :p

			flags -simple [lrange $args 1 end] [list -array -isnull -isempty -notempty -upper -lower -title -sentence -length -llength -normalize -nozero -join -split -boolean -default] text flags

			# Too many things use -NORMALIZE (DEBUG, MSGHOME, ....)
			# Change it to -BOOLEAN?
			if [validflag -normalize] { lappend flags -boolean ; lremove flags -normalize }
			lassign [string tolower $text] name arraycheck
			set name [lindex [string tolower $text] 0]
			if { ![validflag -default] || [validflag -array] } {
				if [notempty arraycheck] { return [eval data array value $flags $name $arraycheck] }
			}

			if [validflag -nozero] { lappend flags -normalize }
			# Some flags will conflict. For now, let the chaos reign! :)
#putlog ARGS($args):TEXT($text):FLAGS($flags):NAME($name)
			if [validflag -isnull]   { return [isnull   sb7($name)] }
			if [validflag -isempty]  { return [isempty  sb7($name)] }
			if [validflag -notempty] { return [notempty sb7($name)] }

			if [info exists sb7($name)] { set data $sb7($name) } { empty data }
			if [validflag -length]    { return [string length $data] }
			if [validflag -llength]   { return [llength $data] }
			if [validflag -upper]     { return [string toupper $data] }
			if [validflag -lower]     { return [string tolower $data] }
			if [validflag -title]     { return [stt $data] }
			if [validflag -sentence]  { return [sts $data] }
			# Need to avoid this -- if [validflag -normalize] { return [normalize $data] }
			if [validflag -boolean]   { return [isnum -boolean $data] } ; # Can't use IFF here
			if [validflag -join]      { return [join $data] }
			if [validflag -split]     { return [split $data] }
			if [validflag -default]   { return [iff [isempty data] $arg3 $data] }

			# Otherwise ....
			return $data
		}

		isempty { return [data get -isempty $arg1] }

		notempty { return [data get -notempty $arg1] }

		eq - = - == {
			flags -simple [lrange $args 1 end] -nocase text flags
			lassign $text arg1 arg2
#putlog EQ:ARGS($args):TEXT($text):FLAGS($flags):ARG1($arg1):ARG2($arg2)
			set arg1 [string tolower [lindex $arg1 0]]
			if ![info exists sb7($arg1)] { error "\[DATA NEQ\] No such element: $arg1" }
			set data $sb7($arg1)
			if [validflag -nocase] { return [string eq -nocase $data $arg2] } { return [string eq $data $arg2] }
		}

		neq - <> - != {
			flags -simple [lrange $args 1 end] -nocase text flags
			lassign $text arg1 arg2
#putlog NEQ:ARGS($args):TEXT($text):FLAGS($flags):ARG1($arg1):ARG2($arg2)
			set arg1 [string tolower [lindex $arg1 0]]
			if ![info exists sb7($arg1)] { error "\[DATA NEQ\] No such element: $arg1" }
			set data $sb7($arg1)
			if [validflag -nocase] { return ![string eq -nocase $data $arg2] } { return ![string eq $data $arg2] }
		}

		array:set - aset { return [data array set $arg1 $arg2 $arg3] }
		array:get - aget { return [data array get $arg1 $arg2 $arg3] }

		set {
			flags -simple [lrange $args 1 end] -array text flags
			lassign $text arg1 arg2 arg3
			if { [notempty arg3] || [validflag -array] } { return [data array set $arg1 $arg2 $arg3] }
			set sb7([string tolower $arg1]) $arg2 
			if [isempty arg2] { unset sb7([string tolower $arg1]) } 
			return $arg2
		}

		empty { set sb7([string tolower $arg1]) "" ; unset sb7([string tolower $arg1]) ; return "" }

		incr { if ![info exists sb7([string tolower $arg1])] { set sb7([string tolower $arg1]) 0 } ; return [incr sb7([string tolower $arg1]) [none $arg2 1]] }

		decr { if ![info exists sb7([string tolower $arg1])] { set sb7([string tolower $arg1]) 0 } ; return [incr sb7([string tolower $arg1]) [none $arg2 -1]] }

		append { set data [data get $arg1] ; append data $arg2 ; data set $arg1 $data ; return $data }

		prepend { set data [data get $arg1] ; set data ${arg2}${data} ; data set $arg1 $data ; return $data }

		length { return [len [data get $arg1]] }

		lset { set data [data get $arg1] ; data set $arg1 [lset data $arg2 $arg3] ; return $data }

		lindex { set data [data get $arg1] ; return [lindex $data $arg2] }

		lrange { set data [data get $arg1] ; return [lrange $data $arg2 $arg3] }

		concat { set data [data get $arg1] ; debug args =[join [lrange $args 2 end]] ; foreach a [join [lrange $args 2 end]] { lappend data $a } ; data set $arg1 $data ; return $data }

		lappend { set data [data get $arg1] ; set data [concat $data [lrange $args 2 end]] ; data set $arg1 $data ; return $data }

		lprepend { set data [data get $arg1] ; set data [concat [lrange $args 2 end] $data] ; data set $arg1 $data ; return $data }

		lsearch { return [lsearch -exact [data get $arg1] $arg2] }

		llength { return [llength [data get $arg1]] }

		lremove { set data [data get $arg1] ; lremove data $arg2 ; data set $arg1 $data ; return $data }

		in - inlist {
			flags -simple [lrange $args 1 end] -nocase text flags
			lassign $text arg1 arg2
			if [validflag -nocase] {
				set m [lsearch -exact [data get -lower $arg1] [string tolower $arg2]]
			} {
				set m [lsearch -exact [data get $arg1] $arg2]
			}
			return [expr ( $m != -1 ) ? 1 : 0]
		}

		find {# 2-element LIST: scalar, array?
			if [isempty arg1] { return "" }
			return [lsort -inc -dict -uni [array names sb7 [string tolower $arg1]]]
		}

		list {# 2-element LIST: scalar, array?
			if [isempty arg1] { return "" }
			empty list
			foreach a [lsort -inc -uni -dict [array names sb7 [string tolower $arg1]]] { lappend list [list $a [data get $a]] }
			return $list
		}

		search {# Search for text WITHIN the data streams
			if [isempty arg1] { return "" }
			if ![is wildcard $arg1] { set arg1 "*${arg1}*" }
			set arg1 [string tolower $arg1]
			empty list
			foreach a [array names sb7] {
				if [string match $arg1 [data get $a]] { lappend list $a }
			}
			return $list
		}

		ren - rename {
			set arg1 [string tolower $arg1]
			set arg2 [string tolower $arg2]
			if [info exists sb7($arg1)] { set sb7($arg2) $sb7($arg1) } { unset -nocomplain sb7($arg2) }
			unset -nocomplain sb7($arg1)
			return
		}

		def - default - defaults {
			set arg1 [string tolower $arg1]
			if [info exists sb7($arg1)] { set data $sb7($arg1) } { set data "" }
			set data [ldefault $data $arg2]
			set sb7($arg1) $data
			return $data
		}

		unset { set sb7($arg1) "" ; unset sb7($arg1) ; return }

		clear {
			set list [array names sb7 [none [string tolower $arg1] *]]
			if [isempty list] { return "" }
			foreach element $list { unset sb7($element) }
			return $list
		}

		load { # RETURN value: count of records processed (0 if fail)
			set filename ${::nick}.data
  			if [notempty arg1] { set filename $arg1 }
			if [is pathchange $filename] { return 0 }
			if ![file exists $filename] { return 0 }
			set count 0
			set data [readfile $filename \n]
			foreach line $data {
				if [isempty line] continue
				lassign $line cmd name data
				switch -exact -- [string tolower $cmd] {

					{#} { # Do nothing # }

					version {
						lassign $line - version key
						lassign [split $version .] version encrypt_mode
					}

					info {
						incr count
						switch -exact -- $version {

							2 { data set $name $data }

							3 {
								if [notempty key] { set data [decrypt ${::nick}:${key} [decrypt $encrypt_mode $line]] }
								data set $name $data
							}

						}
						lassign $line - name data
					}

					default { error "\[DATA LOAD\] Unknown instruction: [lindex $line 0]" }

				}
			}
			return $count
		}

		save {
#foreach a [stack:trace] { putcmdlog "\[STACK:TRACE\] $a" }
			# Invoke the "timer" version of saves by default!
			# Called by: "SAVEME DATA"
			putlog "Writing data file ...."
			set filename ${::nick}.data
			if [notempty arg1] {
				if [string eq * $arg1] {
					append filename ~[clock format [clock seconds] -format "%Y%m%d-%H%M%S"]
				} {
					#append filename ~$arg1
					set filename $arg1
				}
			}
			if [is pathchange $filename] { return 0 }
			set error [ catch {
				set w [open $filename w]
				puts $w "# StormBot.TCL v[data get @VERSION stormbot] data file for: ${::nick} -- [clock format [clock seconds] -format "%Y-%m-%d %H:%M:%S %Z (%z)"] #"
				puts $w "Version 2"

				# SB7 change: allow "#" for channel-based config data ....
				foreach a [lsort -dictionary -increasing [data names]] {
#					if ![regexp -- {^[\!\@\$\%\^\&\*]} $a] { puts $w [list info $a [data get $a]] }
					if ![regexp -- {^[\@\$\%\^\&]} $a] { puts $w [list info $a [data get $a]] } ; # Allow "!" "#" "*" (macro needs "*")

				}
			} crap ]
			flush $w
			close $w
			if $error return { return $filename }
		}

		backup {
			set filename [file dirname [file normalize $::config]]/${::nick}.data
			if [file exists ${filename}~bak] { file delete ${filename}~bak }
			set error [ catch { file copy $filename ${filename}~bak } crap ]
			if $error { error "\[DATA BACKUP\] Unable to save data file backup: ${filename}~bak" ; return }
			return 0
		}

		array {

			switch -exact -- [string tolower $arg1] {

				names {
					set name [string tolower $arg2]
					if [isempty name] { error "\[DATA ARRAY NAMES\] Missing array name" }
					if [info exists sb7($name)] { array set temp $sb7($name) } { array set temp "" }
					set matchme *
					if [notempty arg3] { set matchme [string tolower $arg3] }
					return [array names temp $matchme]
				}

				fetch - val - value - get {
					#set arg2 [string tolower $arg2]
					#set arg3 [string tolower $arg3]
					flags -simple [lrange $args 2 end] [list -isnull -isempty -notempty -upper -lower -title -sentence -length -llength -normalize -nozero -join -split -boolean -default] text flags
					if [validflag -normalize] { lappend flags -boolean ; lremove flags -normalize }
					lassign [string tolower $text] name element default
					if [isempty name] { error "\[DATA ARRAY [string toupper $arg1]\] Missing array name" }
					if [isempty element] { error "\[DATA ARRAY [string toupper $arg1]\] Missing element name (for array \"${name}\")" }

					if [validflag -nozero] { lappend flags -normalize }
					# Some flags will conflict. For now, let the chaos reign! :)
#putlog ARGS($args):TEXT($text):FLAGS($flags):NAME($name):ELEMENT($element)

					if [info exists sb7($name)] { array set temp $sb7($name) } { array set temp "" }
					if [validflag -isnull] { return [isnull temp($element)] }
					if [validflag -isempty] { return [isempty temp($element)] }
					if [validflag -notempty] { return [notempty temp($element)] }

					# Since any further functions upon the variable would cause an
					# exception, process -ISNULL / -ISEMPTY then bail out if null
#putlog 1:NAME($name):ELEMENT($element):EXISTS([info exists temp($element)])
					if [isnull temp($element)] { empty data } { set data $temp($element) }
#putlog 2:NAME($name):ELEMENT($element):EXISTS([info exists temp($element)]):DATA($data):ARG1($arg1):ARG2($arg2):ARG3($arg3):ARG4($arg4)
					if [validflag -length]    { return [string length $data] }
					if [validflag -llength]   { return [llength $data] }
					if [validflag -upper]     { return [string toupper $data] }
					if [validflag -lower]     { return [string tolower $data] }
					if [validflag -title]     { return [stt $data] }
					if [validflag -sentence]  { return [sts $data] }
					# Need to avoid this -- if [validflag -normalize] { return [normalize $data] }
					if [validflag -boolean]   { if [isnum -boolean $data] { return [ expr $data ? 1 : 0 ] } { return 0 } } ; # Can't use IFF here
					if [validflag -join]      { return [join $data] }
					if [validflag -split]     { return [split $data] }
					if [validflag -default]   { return [iff [isempty data] $default $data] }

					# Otherwise ....
					return $data
				}

				isempty { return [data array value -isempty $arg2 $arg3] }

				notempty { return [data array value -notempty $arg2 $arg3] }

				data - array:get {
					if [notempty 3] { putcmdlog [effects "\[DATA GET\] You meant DATA VALUE don't you? (ARG2($arg2):ARG3($arg3) GET returns \[LIST: <name> <value>\]" 4,5] }
					#set arg2 [string tolower $arg2]
					#set arg3 [string tolower $arg3]
					flags -simple [lrange $args 2 end] [list -isnull -isempty -notempty -upper -lower -title -sentence -length -llength -normalize -nozero -join -split] text flags
					lassign [string tolower $text] name element
					if [isempty name] { error "\[DATA ARRAY GET\] Missing array name" }
					#if [isempty element] { error "\[DATA ARRAY GET\] Missing element name (for array \"${name}\")" }

					if [validflag -nozero] { lappend flags -normalize }
# putlog ARGS($args):TEXT($text):FLAGS($flags):NAME($name):ELEMENT($element)

					if [info exists sb7($name)] { array set temp $sb7($name) } { array set temp "" }
					if [validflag -isnull] { return [isnull temp($element)] }
					if [validflag -isempty] { return [isempty temp($element)] }
					if [validflag -notempty] { return [notempty temp($element)] }

					# Since any further functions upon the variable would cause an
					# exception, process -ISNULL / -ISEMPTY then bail out if null
#putlog 1:NAME($name):ELEMENT($element):EXISTS([info exists temp($element)])
					if [isempty element] { return [array get temp ] } { return [array get temp $element] }
				}

				set {
#putlog \00304,01ARGS($args):ARG1($arg1):ARG2($arg2):ARG3($arg3):ARG4($arg4)\003
					# Is there a way to detect a concurrent scalar version?
					# No: arg1 only = unset scalar
					# arg1 arg2 = unset array (not set scalar).
					set arg2 [string tolower $arg2]
					set arg3 [string tolower $arg3]
					if [info exists sb7($arg2)] { array set data $sb7($arg2) } { array set data "" }
					set data($arg3) $arg4
					if [isempty arg4] { unset data($arg3) }
					if [string eq "" [array names data]] { set sb7($arg2) "" ; unset sb7($arg2) } { set sb7($arg2) [array get data] }
					return $arg4
				}

				incr { set data [data array value $arg2 $arg3] ; if [isempty data] { set data 0 } ; incr data ; data array set $arg2 $arg3 $data ; return $data }

				decr { set data [data array value $arg2 $arg3] ; if [isempty data] { set data 0 } ; incr data -1 ; data array set $arg2 $arg3 $data ; return $data }

				append {
					set data [data array value $arg2 $arg3]
					append data $arg4
					data array set $arg2 $arg3 $data
					return $data
				}

				concat {
					set data [data array value $arg2 $arg3]
					set data2 [join [lrange $args 4 end]]
					set data [concat $data $data2]
					data array set $arg2 $arg3 $data
					return $data

				}

				lset {
					if ![isnum -integer $arg4] { error "\[DATA ARRAY LSET\]: received \"${arg4}\" instead of an integer" }
					set data [data array value $arg2 $arg3]
					#if { ( $arg4 < 0 ) || ( $arg4 >= [llength $data] ) } { error "\[DATA ARRAY LSET\] Index out of range: $arg4" }
					lset data $arg4 $arg5
					data array set $arg2 $arg3 $data
					return $data
				}

				lappend {
					set data [data array value $arg2 $arg3]
					lappend data $arg4
					data array set $arg2 $arg3 $data
					return $data
				}

				lindex {
					#if ![isnum -integer $arg4] { error "\[DATA ARRAY LINDEX\]: received \"${arg4}\" instead of an integer" }
					set data [data array value $arg2 $arg3]
					#if { ( $arg4 < 0 ) || ( $arg4 >= [llength $data] ) } { error "\[DATA ARRAY LINDEX\] Index out of range: $arg4" }
					set error [ catch { set lindex [lindex $data $arg4] } uh_oh]
					if $error { error "\[DATA ARRAY LINDEX\] $uh_oh" }
					return $lindex
				}

				lrange {
					#if ![isnum -integer $arg4] { error "\[DATA ARRAY LRANGE\]: received \"${arg4}\" instead of an integer" }
					set data [data array value $arg2 $arg3]
					#if { ( $arg4 < 0 ) || ( $arg4 >= [llength $data] ) } { error "\[DATA ARRAY LRANGE\] Index out of range: $arg4" }
					#if { ( $arg5 < 0 ) || ( $arg5 >= [llength $data] ) } { error "\[DATA ARRAY LRANGE\] Index out of range: $arg4" }
					set error [ catch { set lrange [lrange $data $arg4 $arg5] } uh_oh]
					if $error { error "\[DATA ARRAY LRANGE\] $uh_oh" }
					return $lrange
				}

				len - length { return [string length [data array value $arg2 $arg3]] }

				lremove { set data [data array get $arg2 $arg3] ; lremove data $arg4 ; data array set $arg2 $arg3 $data ; return $data }

				llength { return [llength [data array value $arg2 $arg3]] }

				getall - get:all {
					# $ARG2 contains a list of data elements
					# $ARG3~ contains a list of elements to assign to variables
					set arg2 [string tolower $arg2]
					if [info exists sb7($arg2)] { array set data $sb7($arg2) } { array set data "" }
					foreach a [string tolower $arg3] b [lrange $args 4 end] {
						# 2014-11-26 14:49:40 -0800: Global variables (::*) need to be UPVAR #0; all others should be UPVAR 1
						if [left $b 2 ::] { upvar #0 [mid $b 3] local } { upvar 1 $b local }
						if [info exists data($a)] { set local $data($a) } { set local "" }
					}
					return
				}

				setall - set:all {
					# Do we want to change the syntax to:
					# data array set:all <array> element1 $VAR1 element2 $VAR2 element3 $VAR3 ... elementN $VARN
					# ?????

					# $ARG2 contains a list of data elements
					# $ARG3~ contains a list of elements to assign to variables
					set arg2 [string tolower $arg2]
					if [info exists sb7($arg2)] { array set data $sb7($arg2) } { array set data [list] }
					foreach a [string tolower $arg3] b [lrange $args 4 end] { set data($a) $b ; if [isempty b] { unset -nocomplain data($a) } }
					set sb7($arg2) [array get data]
					return
				}

				find {
					set list ""
					set arg2 [string tolower $arg2]
					set arg3 [string tolower $arg3]
					if ![info exists sb7($arg2)] { return "" }
					array set data $sb7($arg2)
					foreach loop [lsort -inc -uni -dict [array names data [none $arg3 *]]] { lappend list $loop }
					return $list
				}

				list {
					set list ""
					set arg2 [string tolower $arg2]
					if ![info exists sb7($arg2)] { return "" }
					array set data $sb7($arg2)
					set arg3 [none [string tolower $arg3] *]
					foreach loop [lsort -uni -dict -inc [array names data $arg3]] { lappend list [list $loop $data($loop)] }
					return $list
				}

				search {
					flags:simple [lrange $args 2 end] [list -list -nocase -glob -exact -regexp] text flags
					set matchme [lassign $text arg2 arg3]
					set arg2 [none [string tolower $arg2] *]
					set arg3 [none [string tolower $arg3] *]
					set list ""
					set type glob
					foreach a [list regexp exact glob] { if [validflag -$a] { set type $a } }
					foreach a [array names sb7 $arg2] {
						array set data $sb7($a)
						foreach b [array names data $arg3] {
							if [validflag -list] {
								# -GLOB -EXACT -REGEXP only matter here
								if [validflag -nocase] {
									set m [lsearch -${type} [string tolower $data($b)] [string tolower $matchme]]
								} {
									set m [lsearch -${type} $data($b) $matchme]
								}
								if { $m != -1 } { lappend list [list $a $b] }
							} {
								if [validflag -nocase] {
									if [string match -nocase $matchme $data($b)] { lappend list [list $a $b] }
								} {
									if [string match $matchme $data($b)] { lappend list [list $a $b] }
								}
							}
						}
						unset data
					}
					return $list
				}

				ren - rename {
					set arg2 [string tolower $arg2]
					set arg3 [string tolower $arg3]
					set arg4 [string tolower $arg4]
					array set data $sb7($arg2)
					if [info exists data($arg3)] { set data($arg4) $data($arg3) } { unset data($arg4) }
					unset -nocomplain data($arg3)
					set sb7($arg2) [array get data]
					return
				}

				clear {
					set arg2 [string tolower $arg2]
					set arg3 [string tolower $arg3]
					if ![info exists sb7($arg2)] { return "" }
					array set data $sb7($arg2)
					set list [array names data [none $arg3 *]]
					foreach element $list { unset data($element) }
					if [string eq "" [array names data]] {
						set sb7($arg2) ""
						unset sb7($arg2)
					} {
						set sb7($arg2) [array get data]
					}
					return $list
				}

				def - default - defaults {
					set data [data array value $arg2 $arg3]
					set data [ldefault $data $arg4]
					data array set $arg2 $arg3 $data
					return $data
				}

				unset {
					set arg2 [string tolower $arg2]
					set arg3 [string tolower $arg3]
					if ![info exists sb7($arg2)] { return "" }
					array set data $sb7($arg2)
					if [string eq "" $arg3] { unset sb7($arg2) ; return "" }
					if ![info exists data($arg3)] { return "" }
					set data($arg3) ""
					unset data($arg3)
					if [string eq "" [array names data]] {
						set sb7($arg2) ""
						unset sb7($arg2)
					} {
						set sb7($arg2) [array get data]
					}
					return $arg3
				}

				default { error "\[DATA\] Unknown option for ARRAY: [string toupper $arg1]" }

			}
		}

		default { error "\[DATA\] Unknown option: [string toupper $cmd]" }
	}
}

proc saveme { cmd { now false } } {
#foreach a [stack:trace] { putcmdlog "\[STACK:TRACE\] $a" }
	# Using !ISFALSE below allows variants like "-now" or "now" to be used, in case I forget the syntax. :p
	set do_auto_backup [string eq -nocase ALWAYS [data array get config backup]]
	switch -exact -- [string tolower $cmd] {

		* - all { # This will cause a DOUBLE backup call if CONFIG -GLOBAL BACKUP == "always" (once for user, once for data)
			saveme user $now
			saveme data $now
			return 0
		}

		"" - user {
			sb7 killtimer -quiet save
			if ![isfalse $now] save { utimer 0 save }
			if $do_auto_backup { if ![isfalse $now] { sb7:backup user } { utimer 0 [list sb7:backup user] } }
			return 0
		}

		chan - chans {
			sb7 killtimer -quiet savechannels
			if ![isfalse $now] savechannels { utimer 0 savechannels }
			if $do_auto_backup { if ![isfalse $now] { sb7:backup chan } { utimer 0 [list sb7:backup chan] } }
			return 0
		}

		data {
			sb7 killtimer -quiet [list data save]
			if ![isfalse $now] { data save } { utimer 0 [list data save] }
			if $do_auto_backup { if ![isfalse $now] { sb7:backup data } { utimer 0 [list sb7:backup data] } }
			return 0
		}

		backup {
			sb7 killtimer -quiet backup
			if ![isfalse $now] { backup ; data backup } { utimer 0 backup; utimer 0 [list data backup] }
			return 0
		}

		clear {
			sb7 killtimer -quiet [list data *]
			sb7 killtimer -quiet save
			sb7 killtimer -quiet savechannels
			sb7 killtimer -quiet backup
			return 0
		}

		default { error "\[SAVEME\] Unknown option: $cmd" ; return }

	}
	return $cmd
}

#############################################################################

proc sb7:logout args {
#logme
	if [string eq * $args] { set args [userlist] }
	empty successful
	foreach user $args {
		if ![validuser $user] continue
		sb7 auth LOGOUT $user
		lappend successful $user
	}
	return $successful
}

proc sb7:setup_home args {
	if [data array value -isnull CONFIG HOME] {
		if ![data array value -normalize CONFIG HOME:QUIET] {
			set home [lindex [channels] 0]
			data array set CONFIG HOME $home
		}
	} {
		set home [data array value config CONFIG HOME]
	}
	return $home
}

proc home { { chan "" } } {
	set home [data array value CONFIG HOME]
	if [notempty chan] { return [string eq -nocase $home $chan] }
	return $home
}

proc sb7:setup_version args {
#logme
	data set @VERSION ; # Clear previous values (code upgrade had a single value, caused "list != even # of elements" error)
	data array set @VERSION stormbot 7.0
	data array set @VERSION DISTRO http://www.stormbot.org/
	if [file exists ./scripts/sb7/sb7.tcl] {
		data array set @VERSION timestamp [file mtime ./scripts/sb7/sb7.tcl]
	} {
		data array set @VERSION timestamp [file mtime $::config]
	}
	if [file exists ./scripts/sb7/sb7_version.txt] {
		set data [readfile ./scripts/sb7/sb7_version.txt]
		lassign $data ver ts www
#putlog DATA($data):TS($ts):VER($ver):WWW($www)
		regsub -all -nocase -- {stormbot|.tar.gz} $ver "" ver
#putlog DATA($data):TS($ts):VER($ver):WWW($www)
		data array set @VERSION stormbot $ver
		data array set @VERSION TIMESTAMP $ts
		data array set @VERSION DISTRO $www
	}
}

proc sb7:loadbeads { { match "*" } { force false } } {
#logme *
	# Using $DIRS allows for bot-specific scripts addendums:
	# <bot dir>/scripts/sb7/<lowercase: bot's nick>/

	set dirs [list [file dirname [file normalize $::config]]/scripts/sb7 [file dirname [file normalize $::config]]/scripts/sb7/[string tolower $::nick]]
#putlog "\[SB7:LOADBEADS\] DIR($dir):BEADS($beads)"
#putlog <LOOP>
	foreach dir $dirs {
		empty matched
		set beads [glob -n ${dir}/*.sb7]
		foreach bead $beads {
			set tail [file tail $bead]
			set root [file rootname $bead]
			data set @BEAD $bead
			data get @BEAD:SOURCE $tail
#debug bead tail root
			set mtime [file mtime $bead]
			if [string match -nocase $match $tail] {
				if { [istrue $force] || [data array value -isempty @BEADS TIME:$tail] || ( $mtime > [data array value @BEADS TIME:$tail] ) } {
					lappend matched $root ; # $TAIL
					data array set @BEADS TIME:$tail [file mtime $bead]
#debug =Loading: =[data get @BEAD]
#putlog "\[SB7:LOADBEADS\] Loading: [data get @BEAD]"
					set error [ catch { uplevel #0 { source [data get @BEAD] } } uh_oh]
#debug error uh_oh
if $error { putlog "\[SB7:LOADBEADS\] Error while loading [file tail [data get @BEAD]]: $uh_oh" }
				}
			}
		}
	}
	data set @BEAD
	data set @BEAD:SOURCE
#putlog </loop>
	return $matched
}

proc sb7:bind { do cmd { what "" } } {
#putlog SB7:BIND--DO($do):CMD($cmd):WHAT($what)
	set char /
	switch -exact -- [string tolower $do] {

		rename {
			set binds [binds $cmd]
			foreach bind $binds {
#putlog BIND($bind)
				if [left [lindex $bind 4] 1 *] {
#putlog BIND/4([lindex $bind 4])
					lassign $bind method flags command triggered proc
#putlog METHOD($method):FLAGS($flags):COMMAND($command):PROC($proc)
#putlog LIST([list $what * $cmd * ?$what:$cmd])
#putlog MATCH([string match -nocase [list $what * $cmd * ?${what}:$cmd] $bind])
					if [string match -nocase [list $what * $cmd * ?${what}:*] $bind] {
#putlog MATCH!
						catch { unbind $method $flags $command $proc }
						bind $method $flags ${char}$command $proc
						if ![data inlist -nocase @REBOUND $cmd] { data lappend @REBOUND $cmd }
					}
				}
			}
			return
		}

		restore {
			set binds [binds /$cmd]
			foreach bind $binds {
				if ![left [lindex $bind 4] 1 *] { # }
					if [string match -nocase [list $what * ${char}$cmd * *] $bind] {
						unbind $what [lindex $bind 1] [mid [lindex $bind 2] 2] [lindex $bind 4]
					}
				# { # }
			}
			return 
		}

		rebound { return [data inlist -nocase @REBOUND $cmd] }

		default { error "\[SB7:BIND\] Unknown command: $do" }

	}
	?
}

proc sb7:command {command args} {
logme *
rawhome "\[SB7:COMMAND\] Why is this being used ?!?!? COMMAND($command):ARGS($args)"
	# Note: @COMMANDLEVEL:$CMD will be checked during dispatching (overrides
	# the value assigned here). Do =NOT= alter this data with changed levels

	lassign $args cmd arg1 name
	switch -exact -- [string tolower $command] {

		add {
			sb7:getcmdargs $args cmd level flags abbr
			data array set @COMMANDLIST $cmd [list $cmd $level $flags $abbr]
#putlog DATA/SET:CMD($cmd):@COMMANDLIST([data array value @commandlist $cmd])
			if [data array get -normalize CONFIG BIND:MSG] { bind MSG  - $cmd sb7:dispatch:msg }
			if [data array get -normalize CONFIG BIND:DCC] { bind DCC  - $cmd sb7:dispatch:dcc }
			if [data array get -normalize CONFIG BIND:NOT] { bind NOTC - $cmd sb7:dispatch:not }
			return [list $cmd $level $flags $abbr]
		}

		del {
			if [data array isempty @COMMANDLIST $cmd] { return 0 ; # Exit quietly }
			data array unset @COMMANDLIST $cmd
			if [data get -nozero CONFIG:BIND:MSG] { catch { unbind MSG  - $cmd sb7:dispatch:msg } }
			if [data get -nozero CONFIG:BIND:DCC] { catch { unbind DCC  - $cmd sb7:dispatch:dcc } }
			if [data get -nozero CONFIG:BIND:NOT] { catch { unbind NOTC - $cmd sb7:dispatch:not } }
			return $cmd
		}

		list { return [data array names @COMMANDLIST] }

		info { return [data array fetch @COMMANDLIST $cmd] }

		get {
			set data [data array value @COMMANDLIST $cmd]
			switch -exact -- [string tolower $arg1] {
				cmd     { return [lindex $data 0] }
				level   { return [lindex $data 1] }
				flags   { return [lindex $data 2] }
				abbr    { return [lindex $data 3] }
				default { error "\[SB7:COMMAND\] Unknown type for GET: [string toupper $arg1]" }
			}
		}

		clear {
			data unset @COMMANDLIST
			foreach a [binds sb7:dispatch:*] { catch { unbind [lindex $a 0] [lindex $a 1] [lindex $a 2] [lindex $a 4] } }
			return ""
		}

		rebind {
			# Recheck all binds: PUB / MSG / NOT
		}

		default { error "\[SB7:COMMAND\] Unknown option: [string toupper $command]" }

	}
}

proc sb7:getcmdargs {arg var_cmd var_level var_flags var_abbr} {
#$args cmd level flags abbr
	set validflags [list -none -badchan:ok -logout:ok -suspended:ok -auth -chanspecific -locklevel -corecommand -redirect]
	switch -exact -- [string tolower [lindex $arg 0]] {

		add {

			if {[lsearch -exact $arg -level] != -1} {
				# Flag version (we will =NOT= be allowing abbreviations to be set here!)
				# Syntax: sb7:command add dccme -level=50 -proc=sb7:proc_dccme -abbreviations=1,2,3,4,5
			} {
				# SB5 / SB6 version
				upvar 1 $var_cmd $cmd $var_level level $var_flags flags $var_abbr abbr
				set cmd [lindex $arg 0]
				set level [lindex $arg 1]
				flags -simple [lrange $arg 2 end] $validflags abbr flags
#putlog COMMAND/ADD:CMD($cmd):LEVEL($level):ABBR($abbr):FLAGS($flags)
			}
		}
	}
}

# --- Variable manipulation ---

proc empty   args { foreach var $args { if [string match ::* $var] { upvar #0 [mid $var 3] local } { upvar 1 $var local } ; unset -nocomplain local; set local "" } }
proc negone  args { foreach var $args { if [string match ::* $var] { upvar #0 [mid $var 3] local } { upvar 1 $var local } ; unset -nocomplain local; set local -1 } }
proc zero    args { foreach var $args { if [string match ::* $var] { upvar #0 [mid $var 3] local } { upvar 1 $var local } ; unset -nocomplain local; set local  0 } }
proc one     args { foreach var $args { if [string match ::* $var] { upvar #0 [mid $var 3] local } { upvar 1 $var local } ; unset -nocomplain local; set local  1 } }
proc set:all args { set value [lindex $args end] ; foreach var [lrange $args 0 end-1] { if [string match ::* $var] { upvar #0 [mid $var 3] local } { upvar 1 $var local } ; unset -nocomplain local; set local $value } }

proc emptyarray args {
	foreach var $args {
		if [string match ::* $var] { upvar #0 [mid $var 3] local } { upvar 1 $var local }
		if [info exists local] { unset local }
		array set local [list]
	}
	return 0

	# Old method
	set ::SB_emptyvars $args
	uplevel 1 {
		foreach ::SB_empty $::SB_emptyvars {
			if [info exists $::SB_empty] {unset $::SB_empty}
			array set $::SB_empty ""
		}
	}
	foreach a [info vars ::SB_empty*] {unset $a}
	return 0
}

proc isempty var {
	if [string match ::* $var] { upvar #0 [mid $var 3] local } { upvar 1 $var local }
	if { ![info exists local] || [string eq "" $local] } { return 1 }
	return 0
}

proc isnull var {
	if [string match ::* $var] { upvar #0 [mid $var 3] local } { upvar 1 $var local }
	if [info exists local] { return 0 } { return 1 }
}

proc notempty var { upvar 1 $var local ; expr ![isempty local] }
proc notnull var { if [string match ::* $var] { upvar #0 [mid $var 3] local } { upvar 1 $var local } ; expr ![isnull  local] }

proc isarray var { upvar 1 $var local ; array exists local }
proc notarray var { upvar 1 $var local ; not [array exists local] }

proc prepend { var value } { if [string match ::* $var] { upvar #0 [mid $var 3] local } { upvar 1 $var local } ; set local ${value}${local} }

proc none { value { none "" } { some "" } } { 
	if [string eq "" $value] { return $none }
	if ![string eq "" $some] { return $some }
	return $value
}

proc decr { var { decrement 1 } } { if [string match ::* $var] { upvar #0 [mid $var 3] local } { upvar 1 $var local }; incr local [ expr -1 * $decrement ] }

proc int args {
	# LEFT & MID calls INT, which causes an infinite loop! (15 May 2010)

	if [isnum -real $args] {
		if [isscinot $args] { set args [noscinot $args] }
		return [lindex [split $args .] 0]
	} ; # Allow literal INT

	zero reset
	if [string eq -nocase -RESET [lindex $args 0]] { one reset ; set args [lreplace $args 0 0] }

	foreach var $args {
		if [string match ::* $var] { upvar #0 [mid $var 3] local } { upvar 1 $var local }
		if $reset {
			set local 0
		} {
			if [isempty local] {set local 0}
	
			# Error conditions: fake decimal, & non-digits #
			if [string match *.*.* $local] {set local 0; return 0}
			if [regexp -- {^[\+\-]?\d(\.\d+)?[Ee][\+\-]?\d+$} $local] { return $local }
			if [info exists local] {set local [lindex [split $local .] 0]} { set local 0 }
			if [string match +* $local] { set local [string range $local 1 end] }
			# Do =NOT= NOZERO here: will endlessly loop!
		}
	}
	return $local
}


proc mant args {
	if [isnum -real $args] {
		if [isscinot $args] { set args [noscinot $args] }
		return [lindex [split $args .] 1]
	} ; # Allow literal MANTISSA

	zero reset
	if [string eq -nocase -RESET [lindex $args 0]] { one reset ; set args [lreplace $args 0 0] }

	foreach var $args {
		if [string match ::* $var] { upvar #0 [mid $var 3] local } { upvar 1 $var local }
		if $reset {
			set local .0
		} {
			if [isempty local] { set local .0 }
	
			# Error conditions: fake decimal, & non-digits #
			if [string match *.*.* $local] { set local .0 ; return .0 }
			if [regexp -- {^[\+\-]?\d(\.\d+)?[Ee][\+\-]?\d+$} $local] { return $local }
			if [info exists local] { set local .[lindex [split $local .] 1] } { set local .0 }
			if [string match +* $local] { set local [string range $local 1 end] }
			# Do =NOT= NOZERO here: will endlessly loop!
		}
	}
	return $local
}

proc swap { var1 var2 } {
	if [string match ::* $var1] { upvar #0 [mid $var1 3] local_var1 } { upvar 1 $var1 local_var1 }
	if [string match ::* $var2] { upvar #0 [mid $var2 3] local_var2 } { upvar 1 $var2 local_var2 }

	# If both are unset, ... DO NOTHING!
	if {![info exists local_var1] && ![info exists local_var2]} { return "" }

	# If either is unset, dump the (resultant) unset variable:
	if [isnull local_var1] { if [array exists local_var2] { array set local_var1 [array get local_var2] } { set local_var1 $local_var2 } ; unset local_var2; return "" }
	if [isnull local_var2] { if [array exists local_var1] { array set local_var2 [array get local_var1] } { set local_var2 $local_var1 } ; unset local_var1 ; return "" }
	if { [array exists local_var1] && ![array exists local_var2] } { error "\[SWAP\] $var1 is an array but $var2 is scalar" }
	if { ![array exists local_var1] && [array exists local_var2] } { error "\[SWAP\] $var1 is scalar but $var2 is an array" }
	if [array exists local_var1] {# Doesn't matter which one we test
		array set temp [array get local_var2]
		unset local_var2; array set local_var2 [array get local_var1]
		unset local_var1; array set local_var1 [array get temp]
		return ""
	} {
		set temp $local_var2
		set local_var2 $local_var1
		set local_var1 $temp
		return ""
	}
	?
}

# --- STRING manipulation ---

proc falsetrue value { lindex [list false true] [istrue $value] }

proc noyes     value { lindex [list no    yes ] [istrue $value] }

proc offon     value { lindex [list off   on  ] [istrue $value] }

proc singlespace text { set singlespace [regsub -all -- { [ ]+} $text " "] }

#%# 1386457200 Is this still needed? Only BOTSEND was using it but has been replaced by IFF
proc boolean:value { off on value } { lindex [list $off $on] [istrue $value] }

proc boolean args {
	flags:simple $args [list -integer -onoff -offon -truefalse -falsetrue -yesno -noyes] value flags
	if [isnum -real $value] { set value [ expr $value ? 1 : 0 ] } 
	if [istrue $value] { set value 1 } { set value 0 } ; # ISFALSE won't work here
	if [validflag -integer] { return [istrue $value] }
	if [validflag -offon -onoff] { return [offon [istrue $value]] }
	if [validflag -noyes -yesno] { return [noyes [istrue $value]] }
	if [validflag -falsetrue -truefalse] { return [falsetrue [istrue $value]] }
	istrue $value
}

proc escape { text { extra "" } } { 
	regsub -all -- {\\|\{|\}|\[|\]|\$|\;|\"} $text {\\&} text
	if [notempty extra] { regsub -all -- "\\[join [split $extra ""] |\\]" $text {\\&} text }
	return $text
}

proc unescape text {
	set error [ catch { set unescape [join $text] } crap ]
	if !$error { return $unescape }

	set temp \000\002\004\006\010BACKSLASH\010\006\004\002\000
	regsub -all -- \\\\\\\\ $text $temp text
	regsub -all -- \\\\ $text "" text
	regsub -all -- $temp $text \\ text
	return $text
}

proc null args return ; # (:

proc space { { count 1 } } { string repeat " " $count }

proc plural args {
	set args [lassign $args word]
	flags -simple $args [list -show -comma] text flags
	lassign $text count ending replacement
	if [validflag -comma] { lappend flags -show }

	set plural $word
	if { $count != 1 } {
		if [right $plural 1 y] {set plural [left $plural -1]ie}
		if [right $plural 3 ium] {set plural [left $plural -2]a}
		if [notempty replacement] {set plural $replacement} { append plural [iff [isempty ending] s $ending] }
	}
	if [validflag -show] {
		set num $count
		if [validflag -comma] { set num [comma $num] }
		prepend plural "${num}[space]"
	}
	return $plural
}

proc ajl args {
	if [string eq $args -] { return - }
	flags:simple $args [list -nosort -strip -empty -unique -increasing -decreasing] list flags
	if [validflag -empty] { lassign $list list comma and oxford_comma } { lassign [ldefault $list [list $list ", " & false]] list comma and oxford_comma }
	set direction increasing
	if [validflag -decreasing] { set direction decreasing }
	if ![validflag -nosort] {  # -command -lsort:strip
		if [validflag -strip] { set list [lsort -command lsort:strip -$direction -dictionary $list] } { set list [lsort -$direction -dictionary $list] }
		# if [validflag -unique] { set list [lunique $list] } ; # Moved below out of -NOSORT check
	}
	if [validflag -unique] { set list [lunique $list] }
	switch -exact -- [llength $list] {

		0 { return "" }

		1 { return [join $list] }

		2 { return [join $list " $and "] ; # Sorting done above now // -command -lsort:strip }

		default {
			# CONCAT to preserve STRING / LIST rule integrity

			# It's okay to do this: we're gonna convert LIST -> STRING via JOIN anyway
			set oxford ""
			if [boolean $oxford_comma] {
				set oxford [iff $oxford_comma $comma]
				if [right $oxford 1 " "] { set oxford [left $oxford -1] }
			}
			set list [lreplace $list end end "[iff ![string eq "" ${and}$oxford] "${and}$oxford "][lindex $list end]"]
			set list [join $list $comma]
			return $list
		}
	}
	?
}

proc aj { list { join ", " } { and & } } {
	if [isempty list] return 
	switch -exact -- [llength $list] {

		1 { return $list }

		2 { return [join $list " $and "] }

		default {
			set list [lreplace $list end end "$and [lindex $list end]"]
			set list [join $list $join]
			return $list
		}

	}
	? aj
}

proc comma number {
	if ![regexp -- {^[\+\-]?\d+(\.\d+)?$} $number] { return $number }
	if [instr $number ,] { return $number } ; # Or strip & recaulcuate?
	if [regexp -- {^[\+\-]} $number] { set sign [left $number 1] ; set number [mid $number 2] } { empty sign }
	set s [split $number .]
	lset s 0 ${sign}[join [reverse [regexp -inline -all -- {\d{1,3}} [reverse [lindex $s 0]]]] ,]
	join $s .
}

# Replacement PROC 2014-09-06 18:13:02 -0700
# By Peter Spjuth, from http://wiki.tcl.tk/526, modified by Pixelz
proc comma value { regsub -all {\d(?=(\d{3})+($|\.))} $value {\0,} }

proc nocomma { number { comma , } } { regsub -all -nocase -- $comma $number "" }

proc iscomma { number { comma , } } { string match -nocase *${comma}* $number }

proc reverse text {
	set error [ catch { set reverse [string reverse $text] } ]
	if $error { empty reverse ; foreach a [split $text ""] { prepend reverse $a } }
	return $reverse
}

proc explode args {
	set args [join $args]
	flags -simple $args [list -reverse -uppercase -lowercase -nospaces] text flags
	# Do operations on pre-exploded text: less characters (spaces) to process
	if [validflag -uppercase] { set text [string toupper $text] }; # Whichever is last wins
	if [validflag -lowercase] { set text [string tolower $text] }; # Whichever is last wins
	if [validflag -nospaces ] { regsub -all -- " " $text "" text }
	if [validflag -reverse  ] { set text [reverse $text] }
	split $text ""
}

proc implode args {
	set args [join $args]
	flags -simple $args [list -reverse -uppercase -lowercase -nonulls] text flags
	if [validflag -uppercase] { set text [string toupper $text] }; # Whichever is last wins
	if [validflag -lowercase] { set text [string tolower $text] }; # Whichever is last wins
	if [validflag -nonulls  ] { set text [lsearch -all -inline -not $text ""] }
	if [validflag -reverse  ] { set text [reverse $text] }
	join [join $text] ""
}

proc alphabet args {
	flags:simple $args [list -split -reverse -uppercase -lowercase] - flags
	set alphabet abcdefghijklmnopqrstuvwxyz
	if [validflag -split    ] { set alphabet [split $alphabet ""] }
	if [validflag -uppercase] { set alphabet [string toupper $alphabet] }; # Whichever is last wins
	if [validflag -lowercase] { set alphabet [string tolower $alphabet] }; # Whichever is last wins
	if [validflag -reverse  ] { set alphabet [reverse $alphabet] }
	return $alphabet
}

proc nocolor text { regsub -all -- {\003([0-9]{1,2}(,[0-9]{1,2})?)?} $text "" }

proc sgn { value { mode "" } } {
	# Enter with: sgn value [pos | neg | both | all / force (zero also gets "+")]
	if [string eq "" $value] { return 0 }
	if [regexp -- {^[\+\-]?0*(\.0)*$} $value] {
		# Must check this first ... damned negative-zero
		set compare 0
	} {
		if [string eq [string index $value 0] -] {
			set compare -1
		} {
			set compare 1
		}
	}
	#set compare [string compare [string trimleft $value +] 0]
	if [isempty mode] { return $compare }
	switch -glob -- [string tolower $mode] {
		p* { set list [list "" ""  +] }
		n* { set list [list -  "" ""] }
		b* { set list [list -  ""  +] }
		a* - f* { set list [list -  +   +] }
		default { return "" }
	}
	return [lindex $list [incr compare]]
}

proc normalize args {
	# Be careful: DEBUG uses MSGHOME which invokes DATA ARRAY LEVEL -*NORMALIZE* 
	flags:simple [join $args] [list -boolean -falsetrue -truefalse -noyes -yesno -offon -onoff -scinot -hexadecimal -octal -binary -roman] value flags
	if [isempty value] { set value 0 }
	if [istrue $value] { set value 1 } ; # Allow TRUE / YES / ON to survive
	# We're missing conversion flags below! -- if [regexp -- {^[\+\-]?(0|[1-9]\d*)$} $value] { return $value } ; # Let base-10 INTs escape
	if [regexp -nocase -- {^(#|#$|0x|\\x|\\0x|\\u)[0-9A-F]+$} $value] { set value [ expr 0x[ lindex [ regexp -inline -all -nocase -- {[^0-9A-F]*([0-9A-F]+)} $value ] end ] ] } ; # Hexadecimal
	if [regexp -nocase -- {^(&|0o|0)[0-7]+$} $value] { set value [ expr 0[ lindex [ regexp -inline -all -nocase -- {[^0-7]*([0-7]+)} $value ] end ] ] } ; # Octal
	if [regexp -nocase -- {^(%|0b)[01]+$} $value] { set value [ expr 0b[ lindex [ regexp -inline -all -nocase -- {[^01]*([01]+)} $value ] end ] ] } ; # Binary
	if [regexp -nocase -- {^[MDCLXVI]+$} $value] { set value [roman $value] } ; # Roman
	if [regexp -nocase -- {^\d(\.\d+)?E[\+\-]\d+$} $value] { set value [noscinot $value] }

	# Now that the conversions are done, do we have a valid number?
	if ![regexp -nocase -- {^[\+\-]?(\d+|\d+\.\d*|\d*\.\d+)$} $value] { return 0 }

	set value [number:clean $value]

	if [validflag -boolean] { return [ expr $value ? 1 : 0 ] }
	if [validflag -offon -onoff] { return [offon [ expr $value ? 1 : 0 ]] }
	if [validflag -noyes -yesno] { return [noyes [ expr $value ? 1 : 0 ]] }
	if [validflag -falsetrue -truefalse] { return [falsetrue [ expr $value ? 1 : 0 ]] }

	if [validflag -scinot     ] { return [scinot $value] }
	if [validflag -hexadecimal] { return [hex $value] }
	if [validflag -octal      ] { return [oct $value] }
	if [validflag -binary     ] { return [bin $value] }
	if [validflag -roman      ] { return [roman $value] }

	return $value
}

proc number:clean number {
	if [regexp -- {^[\+\-]} $number] { set sign [left $number 1] ; set number [mid $number 2] } { set sign "" }
	lassign [split $number .] int mant
	set int [string trimleft $int 0]
	if [isempty int] { set int 0 }
	set mant [string trimright $mant 0]     
	if [isempty mant] { return ${sign}$int } { return ${sign}${int}.$mant }
}

proc st= a { return $a } ; # Just a forced non-change so that flags / variables can keep their position (against others like STL)

proc stt a {# TITLE case (every word is Capped) 
	set a [split $a]
	# May cause 2nd level, redundant SPLIT, which is okay.

	set b ""
	foreach c $a {lappend b [string totitle $c]}
	join $b
}

proc str a {
	# Note: since digits & punctuation are not considered a 'letter,' all of
	#	  those characters will fail =BOTH= IS UPPER & IS LOWER. Therefore,
	#	  just test for one or the other [STL / STU won't affect these
	#	  characters anyway].

	set temp ""
	foreach char [split $a ""] {
		if [string is upper $char] { append temp [string tolower $char] } { append temp [string toupper $char] }
	}
	return $temp
}

proc st^ {a b} {
	if ![regexp -- {^[\+\-]?\d+$} $b] {error "Illegal value in ST^: $b (expecting integer)"}
	set temp [string toupper [string range $a 0 [ expr $b - 1 ]]]
	incr b
	append temp [mid [string tolower $a] $b]
	return $temp
}

proc sil a { string is lower -strict $a }
proc siu a { string is upper -strict $a }
proc sis a { regexp -- {^[A-Z][^A-Z]*$} $a }
proc sit a {
	set sit 1
	foreach b [split $a] { if ![ regexp -- {^[A-Z][^A-Z]*$} $b ] { zero sit } }
	return $sit
}

proc jstl  a    { join [string tolower $a] }
proc jstu  a    { join [string toupper $a] }
proc jsts  a    { join [string totitle $a] }
proc jstt  a    { join [stt $a] }
proc jstr  a    { join [str $a] }
proc jst^ {a b} { join [st^ $a $b] }

proc left args {
	lassign $args a b 
	set args [lreplace $args 0 1]
	empty flags
	while { [lsearch -glob [string tolower [lindex [split $args] 0]] -n*] != -1 } {
		lappend flags [lindex [split $args] 0]
		set args [lreplace $args 0 0]
	}
	set flags [uniquematch -nocase $flags]
	lassign $args c
	if [string match -nocase "-*" $b] {set b [ expr [ len $a ] + $b ]}
	int b; # Some procs might pass a floating-point. STRING RANGE will choke.
	incr b -1
	set left [string range $a 0 $b]
	if [string eq "" $c] { return $left }
	if [validflag -nocase] { string eq -nocase $left $c } { string eq $left $c }
}

proc right args {
	lassign $args a b
	int b; # Some procs might pass a floating-point. STRING RANGE will choke.
	set left [ expr [ string length $a ] - $b ]
	if [string match -nocase "-*" $b] {set left [expr abs( $b ) ]}
	set args [lreplace $args 0 1]
	empty flags
	while { [lsearch -glob [string tolower [lindex [split $args] 0]] -n*] != -1 } {
		lappend flags [lindex [split $args] 0]
		set args [lreplace $args 0 0]
	}
	set flags [uniquematch -nocase $flags]
	lassign $args c
	set right [string range $a $left end]
	if {$c == ""} {return $right}
	if [validflag -nocase] { string eq -nocase $right $c } { string eq $right $c }
}

proc pos { a b c } {
	int b c; # Some procs might pass a floating-point. STRING RANGE will choke.
	string range $a [incr b -1] [incr c -1]
}

proc mid args {
	lassign $args a b c
	set args [lreplace $args 0 2]
	if [isempty c] { set c 2147483647 }
	if [string match -nocase e* $b] { set b [len $a] }; # "End"
	if [string match -nocase e* $c] { set c [len $a] }; # "End"
	int b c; # Some procs might pass a floating-point. STRING RANGE will choke.
	if ![regexp -- {^\d+$} $b] {return ""}
	if ![regexp -- {^[\+\-]?\d+$} $c] {return ""}
	if !$b return
	if !$c return
	incr b -1; # Counter offset [INDEX vs position]
	set mid [left [string range $a $b end] $c]
	if [isempty args] { return $mid }
	empty flags
	while { [lsearch -glob [string tolower [lindex [split $args] 0]] -n*] != -1 } {
		lappend flags [lindex [split $args] 0]
		set args [lreplace $args 0 0]
	}
	set flags [uniquematch -nocase $flags]
	lassign $args d
	if [validflag -nocase] { string eq -nocase $mid $d } { string eq $mid $d }
}

proc instr { text value { start 1 } } {
	if [string eq "" text] { return 0 }
	decr start
	set instr [string first $value $text $start]
	incr instr
}

proc instrrev { text value { start "" } } {
	if [string eq "" text] { return 0 }
	if [isempty start] { set start [len $text] }
	decr start
	set instr [string last $value $text $start]
	incr instr
}

proc len text { string length $text }

proc addzero { number pad } {
	lassign [split $number .] integer mantissa
	if {$pad < [string length $integer]} { return $number }
	set addzero [right [string repeat 0 $pad]$integer $pad]
	if [notempty mantissa] {append addzero .$mantissa}
	return $addzero
}

proc fixmath args {
	set arg [string tolower [join $args]]
	
	# Process "PI" first
	set pi 3.14159265358979323846264338
	if {[info commands pi:value] != ""} { set pi [pi:value] }
	regsub -all -nocase -- PI $arg $pi arg

	empty new
	zero count

	foreach a [regexp -all -inline -- {[^\d]\.\d+[^\d]} $arg] {
		set b [left $a 1]0[mid $a 2] 
		regsub -all -- $a $arg $b arg
	}

	# Can we use REGEXP (\d*)\.?\d{1,} (or something else) and predict which
	# match will give us the full number?

	while { [notempty arg] } {
		incr count ; if { $count > 1024 } { error "> FIXMATH: We got up to a ridiculous loop count: \$COUNT ($count)" }

		if ![regexp -- {[0-9\.]} [left $arg 1]] {
			append new [left $arg 1]
			set arg [mid $arg 2]
			continue
		}

		# Otherwise, you're a digit ....

		set prelim [lindex [regexp -inline -nocase -- {^[+\-]?([0-9]+[\.]?[0-9]*|[0-9]*[\.]?[0-9]+)(e[+\-]?[0-9]+)?} $arg] 0]
		set num $prelim; # Need to preserve original # for string index shifting!
		if ![instr $num e] { if ![instr $num .] { append num .0 } }
		append new $num
		set arg [mid $arg [ expr [ len $prelim ] + 1 ]]
	}
	expr $new
}

proc trim { string { trim_me "" } } { if [isempty trim_me] { string trim $string } { string trim $string $trim_me } }

proc istrue  value { string is true -strict $value }
proc isfalse args  {
	flags:simple $args -strict value flags
	if [validflag -strict] { string is false -strict $value } { string is false $value }
}

if ![iseggcorecmd iff] {
	proc iff { condition true { false "" } } {
		# UPLEVEL because someone might do {$m > 0} instead of just "$m > 0"
		set ::_IFF_CONDITION $condition
	
		# MUST USE "!= 0" check: otherwise, values of "2" won't trigger TRUE (due to -STRICT).
		uplevel 1 { set ::_IFF_ANSWER [ expr ( $::_IFF_CONDITION != 0 ) ] }
		set iff $::_IFF_ANSWER
		unset ::_IFF_CONDITION ::_IFF_ANSWER
		if [string is true -strict $iff] { return $true } { return $false }
	}
}

proc isnum args {
	flags -simple $args [list -boolean -positive -negative -nonnegative -integer -real -even -odd -prime -square -binary -octal -decimal -hexadecimal -positive -negative -scinot] number flags
	if [regexp -- {^\.\d+$} $number] { prepend number 0 }

	# Check SCINOT first
	if [validflag -scinot] { return [isscinot $number] }
	set number [noscinot $number] ; # Will return non-SCINOT number literally

	#if ![regexp -- {^[\+\-]?\d+(\.\d+)?$} $number] { return 0 }
	if [validflag -boolean]     { return [expr ( [lsearch -exact [list 0 1 no yes off on false true] [string tolower $number]] == -1 ) ? 0 : 1] }
	if [validflag -real]        { return [regexp -- {^[+\-]?(\d*\.\d+|\d+(\.(\d+)?)?)([Ee][+\-]?\d+)?$} $number] }
	if [validflag -integer]     { return [regexp -- {^[\+\-]?\d+$} $number] }
	if [validflag -even ]       { int number ; return [regexp -- {[02468]$} $number] }
	if [validflag -odd  ]       { int number ; return [regexp -- {[13579]$} $number] }
	if [validflag -negative]    { return [ expr ( $number <  0 ) ? 1 : 0 ] }
	if [validflag -positive]    { return [ expr ( $number >  0 ) ? 1 : 0 ] }
	if [validflag -nonnegative] { return [ expr ( $number >= 0 ) ? 1 : 0 ] }
	if [validflag -prime]       { return [isprime $number] }
	if [validflag -square]      { return [issquare $number] }
	if [validflag -binary]      { return [regexp -- {^(%|0b|0B)?[01]+$} $number] }
	if [validflag -octal]       { return [regexp -- {^([&\\]?[0-7]+|0[0-7]+|0o[0-7]+)$} $number] }
	if [validflag -decimal]     { return [regexp -- {^[#\+\-]*([^0]+[0-9]+|0)$} $number] }
	if [validflag -hexadecimal] { return [regexp -- {^((#)?\$|0[xX]|\\0?x)?[0-9A-Fa-f]+$} $number] }
	regexp -- {^[\+\-]?\d+?(\.\d+)?$} [noscinot $number]
}

proc scinot { number { precision 0 } } {
	if $precision {
		set p $precision
		decr p
	} {
		set p $::tcl_precision
		if !$p { set p 15 } ; # Using "16" we're gonna do 1 (int) + 15 (mant)
	}
	set s [split [format %1.${p}E $number] E]
	lset s 0 [number:clean [lindex $s 0]]
	lset s 1 [number:clean [lindex $s 1]]
	join $s e
}

proc isscinot number { regexp -- {^[+\-]?\d(\.\d+)?[Ee][+\-]?\d+$} $number }

proc noscinot number {
	if ![isscinot $number] { return $number }
	if [left $number 1 +] { set number [mid $number 2] }
	set sign [sgn $number]; if [left $number 1 -] { set number [mid $number 2] }
	set number [stu $number]
	if ![instr $number .] { regsub -- E $number .0E number }
	lassign [split $number E] number e
	set e [normalize $e] ; # Damned "E-05" exponents!
	set r [ expr [string trimleft $e -] + 1] 
	set number [string repeat 0 $r]${number}[string repeat 0 $r]
	set i [instr $number .]
	incr i [expr $e - 1] ; # Compensate for about-to-be-removed decimal point
	regsub {\.} $number "" number
	set number [left $number $i].[mid $number [expr $i + 1]]
	if { $sign < 0 } { prepend number - }
	normalize $number
}

# --- LIST manipulation ---

if ![iseggcorecmd lassign] {
	proc lassign { list args } {
		# Return unused elements as a new list
		# Any unused variables should be set to "" (this is different than REGEXP)
		# Use only raw TCL commands (no SB extensions)
if { [string eq *** $list] || [string eq *** $args] } { putlog "\[LASSIGN\] LIST($list):ARGS($args)" }
		set leftovers [list]
		foreach a $list b $args {
			upvar 1 $b local_$b
			if [string eq "" $a] { set local_$b "" ; continue }
			if [string eq "" $b] { lappend leftovers $a ; continue }
			set local_$b $a
		}
		return $leftovers
	}
}

if ![iseggcorecmd lreverse] {
	proc lreverse list {
		set new ""
		foreach a $list { set new [linsert $new 0 $a] }
		return $new
	}
}

if ![iseggcorecmd lrepeat] {
	proc lrepeat { list iterations } { # iterations args
		if { ![isnum -integer $iterations] && [isnum -integer $list] } { swap iterations list }
		if ![isnum -integer $iterations] { error "\[LREPEAT\] Expected integer but got: $iterations" }
		# No: allow creation of null lists - if [isempty list] {return ""}
		if { $iterations <  1 } return
		if { $iterations == 1 } { return $list }
	
		empty lrepeat
		for { zero iteration } { $iteration < $iterations } { incr iteration } {
			lappend lrepeat $list
		}
		return $lrepeat
	}
}

proc lassign:array { name list args } {
	upvar 1 $name array
	set leftovers [list]
	array set array [list]
	foreach a $list b $args {
		if [isempty a] { set array($b) "" ; continue }
		if [isempty b] { lappend leftovers $a ; continue }
		set array($b) $a
	}
	return $leftovers
}

proc lassign:count { name list { start 0 } } {
	upvar 1 $name array
	set leftovers [list]
	array set array [list]
	set b [expr $start - 1]
	foreach a $list {
		incr b
		if [isempty a] { set array($b) "" } { set array($b) $a }
	}
	return [array get array]
}

proc lsort:priority { list { priority "" } { last "" } } {
	# Replaces SB6/LSORT:LIST
	# Set the pecking order (at least, for the beginning) 
	#! Removed -DICT flag from LSORT (let user decide if it's needed)
	set new [lsort -increasing -unique $list]
	if [notempty priority] { set new [lunique [concat $priority $new]] }
	if [notempty last] { set new [lunique -last [concat $new $last]] }
	set _sort $new ; # Needed by LSORT:PRIORITY2
	set list [lsort -uni -inc -command lsort:priority2 $list]
	return $list
}

proc lsort:priority2 { 1 2 } {
	#! Removed -NOCASE from STRING, STL from $PRIORITY / $1 / $2 in LSEARCHes (let -DICTIONARY handle it)
	set sc [string compare $1 $2]
	upvar 1 _sort priority
	set m [lsearch -glob $priority $1]
	set n [lsearch -glob $priority $2]
	if {($m <  0) && ($n <  0)} {return $sc}
	if {($m <  0) && ($n > -1)} {return +1}
	if {($m > -1) && ($n <  0)} {return -1}
	if {($m > -1) && ($n > -1)} {
		if {$m < $n} {return -1}
		if {$m > $n} {return +1}
		return $sc
	}
	return $sc
}

proc list:map args {
	# -NOCASE (usual meaning)
	# -FULL   (only match if the whole element matches)
	flags:simple $args [list -nocase -full] arg flags
	lassign $arg map list

	# Sanity check
	if ![isnum -even [llength $map]] { error "\[LIST:MAP\] Map must be an even number of elements" }

	# Default situations
	if [isempty list] return
	if [isempty map] { return $list }

	# I'm worried: REGSUB might SPLIT the text ....
	set list_map [list]
	set nocase [validflag -nocase]
	set full   [validflag -full]

	set replace_last [list]
	set step 0

	foreach { point replace } $map {
		incr step; # Strep ?! :p

		#set replace_temp \014\011\023\024[binary format c $step]\024\023\011\014
		# Using either \014 or \011 causes {braces} to be invoked (requiring
		# JOIN to be used later).
		set replace_temp \023\024[binary format c $step]\024\023

		lappend replace_last $replace_temp $replace
		if $full {
			set m [lsearch -all -exact [list:tolower $list] [string tolower $point]]
		} {
			set m [lsearch -all -glob  [list:tolower $list] [string tolower *${point}*]]
		}

		foreach k $m {
			set index [lindex $list $k]
			if $nocase {
				regsub -all -nocase $point $index $replace_temp index
			} {
				regsub -all         $point $index $replace_temp index
			}
			lset list $k $index
		}
	}
debug =final list
	foreach { a b } $replace_last { regsub -all $a $list $b list }
	return $list

	# Version 1
	if $full {
		foreach a $list {
			foreach { point replace } $map {
				if $nocase {
					# I know, I know, I know, ....
					if ![string compare -nocase $a $point] { set a $replace }
				} {
					if ![string compare         $a $point] { set a $replace }
				}
			}
			lappend list_map $a
		}
	} {
		# Can this be re-written using LSEARCH / LINDEX / LSET with REGSUB?
		foreach a $list {
			foreach { point replace } $map {
				if $nocase {
					regsub -all -nocase -- $point $a $replace a
				} {
					regsub -all         -- $point $a $replace a
				}
			}
			lappend list_map $a
		}
	}
	return $list_map
}

# Note: LMATCH (interp alias) -> "LDESTROY -NOT"

# 2014-09-13 18:05:00 -0700: Replaced with new (fresh re-write) version, specifically to add new function: -COMMON
proc `ldestroy args {
	# Use -COUNT to test number of results (0 = no matches)
	flags:simple $args [list -debug -count -common -all -not -unique -both -increasing -decreasing -nonulls -glob -regexp -exact -nocase -multiple -replacewith -keepnulls] temp flags
	set debug [validflag -debug]
	if ![validflag -keepnulls] { lremove flags -keepnulls -nonulls ; lappend flags -nonulls }

	lassign $temp list text replacewith
	if [validflag -multiple] { set total $text ; lappend flags -all } { set total [list $text] }

	set match glob ; # Default (order (lowest-to-highest: regexp, exact, glob)
	foreach a [list regexp exact glob] { if [validflag -$a] { set match $a } }
#debug *
	set _ $list
	if [validflag -nocase] { set _ [string tolower $_] ; set total [string tolower $total] }

	if [validflag -common] {
		empty common fail1 fail2
		foreach item [lunique [concat $_ $total]] {
			if [validflag -all] {
				if [validflag -nocase] {
					set __1 [lsearch -all -${match} [string tolower $_] $item]
					set __2 [lsearch -all -${match} [string tolower $total]  $item]
				} {
					set __1 [lsearch -all -${match} $_ $item]
					set __2 [lsearch -all -${match} $total $item]
				}
			} {
				if [validflag -nocase] {
					set __1 [lsearch -${match} [string tolower $_] $item]
					set __2 [lsearch -${match} [string tolower $total] $item]
				} {
					set __1 [lsearch -${match} $_ $item]
					set __2 [lsearch -${match} $total $item]
				}
			}
if [validflag -debug] { debug __1 __2 }
			empty _i _k
			foreach i $__1 { lappend _i [lindex $list $i] } ; swap _i __1
			foreach k $__2 { lappend _k [lindex $list $k] } ; swap _k __2
			switch -exact -- [string eq "" $__1][string eq "" $__2] {
				11 { error "\[LDESTROY -COMMON\] This should not be possible: \"${item}\" doesn't match either list!" }
				10 { set fail2 [concat $fail2 $item] }
				01 { set fail1 [concat $fail1 $item] }
				00 { set common [concat $common $item] }
			}
if [validflag -debug] { debug item common fail1 fail2 }
		}

		if [validflag -nonulls] { foreach a [list common fail1 fail2] { set $a [lsearch -all -inline -not -exact [set $a] ""] } }
		if [validflag -unique] { foreach a [list common fail1 fail2] { set $a [lunique [set $a]] } }
		if [validflag -increasing -decreasing] {
			set direction increasing
			if [validflag -decreasing] { set direction decreasing }
			foreach a [list common fail1 fail2] { set $a [lsort -$direction [set $a]] }
		}
		if [validflag -replacewith] { set common $replacewith }
		if [validflag -both] { if [validflag -count] { return [list [llength $common] [llength $fail1] [llength $fail2]] } { return [list $common $fail1 $fail2] } }
		if [validflag -not] { if [validflag -count] { return [llength [concat $fail1 $fail2]] } { return [concat $fail1 $fail2] } }
		if [validflag -count] { return [llength $common] }
		return $common
	}
	empty temp_list temp_not temp_list_ temp_not_

if [validflag -debug] { debug _ total }
	# Version 4
	set collected ""
set mm [lsearch -exact $flags -nocase] ; if { $mm != -1 } { set flags [lreplace $flags $mm $mm] }
	lassign [split [lindex [list : {[string tolower :]}] [validflag -nocase]] :] header footer
if [validflag -debug] { debug =0 header footer }
	foreach tot $total {
		set __ [eval lsearch [lindex [list "" -all] [validflag -all]] -${match} \"${header}${_}${footer}\" \"${header}${tot}${footer}\"]
		set collected [concat $collected $__]
if [validflag -debug] { debug __ tot }
	}
if [validflag -debug] { debug collected }

	set temp_list ""
	set temp_not ""
	for { set x 0 } { $x < [llength $list] } { incr x } { lappend temp_[lindex [list temp not] [expr ( [lsearch -exact $collected $x] != -1 ) ? 0 : 1]] [lindex $list $x] }
if [validflag -debug] { debug temp_list temp_not }
if 0 {
	# First pass
	set collected ""
	foreach item $total {
		if [validflag -all] {
			set __ [lsearch -all -${match} $_ $item] 
		} { 
			set __ [lsearch -${match} $_ $item] 
		}
		# Remember: we're trying to REMOVE entries by default, not keep them.
		if { ( $__ != -1 ) && ![string eq "" $__] } { set collected [concat $collected $__] }
#if $debug { debug collected item _ __ match =-all([lsearch -all -inline -${match} $_ $item]) =-single([lsearch -inline -${match} $_ $item]) temp_list_ temp_not_ }
	}
if [validflag -debug] { debug collected list _ }
	# Second pass
	set do_second_pass 1
	if $do_second_pass {
if $debug { debug temp_list_ temp_not_ _ }
		foreach a $collected { lappend temp_not [lindex $list $a] }
		foreach a [lsort -dec $collected] { set list [lreplace $list $a $a] }
		set temp_list $list
	}
if $debug { debug _ collected temp_list temp_not }
}
	if [validflag -nonulls] { foreach a [list temp_list temp_not] { set $a [lsearch -all -inline -not -exact [set $a] ""] } }
	if [validflag -unique] { foreach a [list temp_list temp_not] { set $a [lunique [set $a]] } }
	if [validflag -increasing -decreasing] {
		set direction increasing
		if [validflag -decreasing] { set direction decreasing }
		foreach a [list temp_list temp_not] { set $a [lsort -$direction [set $a]] }
	}
	if [validflag -replacewith] { set temp_list $replacewith }

if $debug { debug temp_list temp_not }
	if [validflag -both] { if [validflag -count] { return [list [llength $temp_list] [llength $temp_not]] } { return [list $temp_list $temp_not] } }
	if [validflag -not] { if [validflag -count] { return [llength $temp_not] } { return $temp_not } }
	if [validflag -count] { return [llength $temp_list] }
	return $temp_list
}

# FIX THIS: -NOCASE returns lower-case items!
proc ldestroy args {
	# Use -COUNT to test number of results (0 = no matches)
	flags:simple $args [list -debug -count -common -all -not -unique -both -increasing -decreasing -nonulls -glob -regexp -exact -nocase -multiple -replacewith -keepnulls] temp flags
	set debug [validflag -debug]
	if ![validflag -keepnulls] { lremove flags -keepnulls -nonulls ; lappend flags -nonulls }
 
	lassign $temp list text replacewith
	if [validflag -multiple] { set total $text ; lappend flags -all } { set total [list $text] }
 
	set match glob ; # Default (order (lowest-to-highest: regexp, exact, glob)
	foreach a [list regexp exact glob] { if [validflag -$a] { set match $a } }
#debug *
	set _ $list
	if [validflag -nocase] { set _ [string tolower $_] ; set total [string tolower $total] }
 
	if [validflag -common] {
		empty common fail1 fail2
		foreach item [lunique [concat $_ $total]] {
			if [validflag -all] {
				if [validflag -nocase] {
					set __1 [lsearch -all -inline -${match} [string tolower $_] $item]
					set __2 [lsearch -all -inline -${match} [string tolower $total]  $item]
				} {
					set __1 [lsearch -all -inline -${match} $_ $item]
					set __2 [lsearch -all -inline -${match} $total $item]
				}
			} {
				if [validflag -nocase] {
					set __1 [lsearch -inline -${match} [string tolower $_] $item]
					set __2 [lsearch -inline -${match} [string tolower $total] $item]
				} {
					set __1 [lsearch -inline -${match} $_ $item]
					set __2 [lsearch -inline -${match} $total $item]
				}
			}
			switch -exact -- [string eq "" $__1][string eq "" $__2] {
				11 { error "\[LDESTROY -COMMON\] This should not be possible: \"${item}\" doesn't match either list!" }
				10 { set fail2 [concat $fail2 $item] }
				01 { set fail1 [concat $fail1 $item] }
				00 { set common [concat $common $item] }
			}
#debug item common fail1 fail2
		}
 
		if [validflag -nonulls] { foreach a [list common fail1 fail2] { set $a [lsearch -all -inline -not -exact [set $a] ""] } }
		if [validflag -unique] { foreach a [list common fail1 fail2] { set $a [lunique [set $a]] } }
		if [validflag -increasing -decreasing] {
			set direction increasing
			if [validflag -decreasing] { set direction decreasing }
			foreach a [list common fail1 fail2] { set $a [lsort -$direction [set $a]] }
		}
		if [validflag -replacewith] { set common $replacewith }
		if [validflag -both] { if [validflag -count] { return [list [llength $common] [llength $fail1] [llength $fail2]] } { return [list $common $fail1 $fail2] } }
		if [validflag -not] { if [validflag -count] { return [llength [concat $fail1 $fail2]] } { return [concat $fail1 $fail2] } }
		if [validflag -count] { return [llength $common] }
		return $common
	}
	empty temp_list temp_not temp_list_ temp_not_
 
	# First pass
	set collected ""
	foreach item $total {
		if [validflag -all] { 
			set __ [lsearch -all -inline -${match} $_ $item] 
		} { 
			set __ [lsearch -inline -${match} $_ $item] 
		}
		# Remember: we're trying to REMOVE entries by default, not keep them.
		if [notempty __] {
			#set temp_list_ [concat $temp_not_ $__]
			set collected [concat $collected $__]
		} { 
			#set temp_not_ [concat $temp_list_ $__] ; # $item
		}
if $debug { debug collected item _ __ match =-all([lsearch -all -inline -${match} $_ $item]) =-single([lsearch -inline -${match} $_ $item]) temp_list_ temp_not_ }
	}
 
	# Second pass
	set do_second_pass 1
	if $do_second_pass {
if $debug { debug temp_list_ temp_not_ _ }
		foreach a $collected {
			set m [lsearch -exact $_ $a]
			if { $m != -1 } { lappend temp_not $a ; set _ [lreplace $_ $m $m] }
		}
		set temp_list $_
	}
if $debug { debug _ collected temp_list temp_not }
 
	if [validflag -nonulls] { foreach a [list temp_list temp_not] { set $a [lsearch -all -inline -not -exact [set $a] ""] } }
	if [validflag -unique] { foreach a [list temp_list temp_not] { set $a [lunique [set $a]] } }
	if [validflag -increasing -decreasing] {
		set direction increasing
		if [validflag -decreasing] { set direction decreasing }
		foreach a [list temp_list temp_not] { set $a [lsort -$direction [set $a]] }
	}
	if [validflag -replacewith] { set temp_list $replacewith }
 
if $debug { debug temp_list temp_not }
	if [validflag -both] { if [validflag -count] { return [list [llength $temp_list] [llength $temp_not]] } { return [list $temp_list $temp_not] } }
	if [validflag -not] { if [validflag -count] { return [llength $temp_not] } { return $temp_not } }
	if [validflag -count] { return [llength $temp_list] }
	return $temp_list
}

proc lcommon args {
	if { [llength $args] < 2 } { error "\[LCOMMON\] requires 2 or more params: <list#1> <list#2> \[... list#N\]" }
	empty lcommon full
	foreach list $args { set full [concat $full $list] }
	foreach item [lsort -inc -uni -dict $full] {
		one ok
		foreach list $args { if { [lsearch -exact $list $item] == -1 } { zero ok } }
		if $ok { lappend lcommon $item }
	}
	return $lcommon
}

proc lunique args { 
	# Replaces SB6/NODUP
	# Replaces SB6/NODUP_LAST
	flags:simple $args [list -nocase -regexp -exact -glob -last -reverse] list flags
	set list [join $list]
	set mode exact ; # In THIS situation only, default to "exact" matching mode
	foreach a [list regexp exact glob] { if [validflag -$a] { set mode $a } }
	empty lunique
	if [validflag -last] { set list [lreverse $list] }
	foreach a $list {
		if [validflag -nocase] { set m [lsearch -$mode [string tolower $lunique] [string tolower $a]] } { set m [lsearch -$mode $lunique $a] }
		if { $m == -1 } { lappend lunique $a }
	}

	# -LAST is the replacement for NODUP_LAST and requires a list reversal at the beginning and again here
	# -REVERSE is independent of this (a simple reversal). There MIGHT be reason to use both.
	# Instead of coding a series of IF/THENs, let's just let both survive as-is.
	if [validflag -last] { set lunique [lreverse $lunique] }
	if [validflag -reverse] { set lunique [lreverse $lunique] }
	return $lunique
}

proc lprepend { var text } { upvar 1 $var local ; set local [linsert $local 0 $text] }

proc ldefault args {
	flags:simple $args -count text flags
	lassign $text list defaults
	if [validflag -count] { set defaults [lrepeat "" $defaults] }

	lpad list [llength $defaults]
	for { set x 0 } { $x < [llength $defaults] } { incr x } {
		set index [lindex $list $x]
		if [isempty index] { set index [lindex $defaults $x] }
		lset list $x $index
	}
	return $list
}

proc lpad { var_list llength { pad_with "" } } {
	upvar 1 $var_list list
	if ![info exists list] { empty list }
	if ![isnum -integer $llength] { error "\[LPAD\] Expected integer but got: $llength" }
	if { $llength < 0 } return
	set ll [llength $list]
	if { $ll >= $llength } return
	for { set x $ll } { $x < $llength } { incr x } { lappend list $pad_with }
	return
}

proc lloop args {
	# Enter with: loop "text %# %#,#" [list 1] [list 2] [list 3] ... [list N]
	# %1 pulls the current count element from [LIST 1]
	# %1,2 pulls from [list 1] the second element (regardless of loop position). Example: %2,3 [list 1 2 3] [list 3 4 5] [list 5 6 7] --> "5"
	# loop "%1 %2 %1,3" [list 1 2 3] [list 11 12 13] [list 21 22 23] --> "{1 2} {11 12} {21 22}"
	empty loop
	set template [lindex $args 0]
	set lists [lreplace $args 0 0]
	set max 0 ; foreach a $lists { if { [llength $a] > $max } { set max [llength $a] } }
	if !$max { return [regsub -all -- {%\d+%} $template ""] }

	for { set index 0 } { $index < $max } { incr index } {
		set temp $template
		foreach regexp [concat [regexp -inline -all -nocase -- {%\d+,\d+} $temp] [regexp -inline -all -nocase -- {%\d+} $temp] ] {
			if [regexp -- {^%\d+,\d+} $regexp] {
				lassign [split [mid $regexp 2] ,] r1 r2
				set target [expr $r1 - 1]
				set x [normalize $r2]
			} {
				empty r1 r2
				set target $index
				set x [normalize [mid $regexp 2]]
			}
#debug regexp r1 r2 target x
			regsub -all -- $regexp $temp [lindex $lists $target [expr $x - 1]] temp
		}
		lappend loop $temp
		
	}
	return $loop
}

# Call with: LREMOVE <variable reference> <item_to_remove1> <item_to_remove2> <item_to_remove3> ... <item_to_removeN> 
proc lremove { var args } {
	if [string eq "" $args] return
	upvar 1 $var local
	if ![info exists local] { error "can't read \"${var}\": no such variable" }
	if [string eq "" $local] return

	zero matches
	foreach arg $args {
#		set m [lsearch -all -glob [stl $local] [stl $arg]]
		set m [lsearch -all -glob [string tolower $local] [string tolower $arg]]
		foreach n [lsort -int -dec -uni $m] {
			incr matches
			set local [lreplace $local $n $n]
		}
	}
	return $matches
}

# Enter with: LCANCEL <LIST> ( list = { +1 +2 +3 -2 -3 -4 +5 +4 -5 +5 cow } --> { +1 +5 cow } )
proc lcancel args {
	flags:simple $args [list -cleanup -nocase -replace -annihilate] list flags
	set list [join $list]
	# Process -CLEANUP first: parse through and handle duplicates, baed on other flags
	if [validflag -cleanup] {
		set cleanup [lindex $list 0]
		set temp_list [lrange $list 1 end]
		lremove flags -cleanup ; # Might cause a weird endless loop? (Let's just play it safe ....)
		while { ![string eq "" $temp_list] } {
			set cleanup [eval [lcancel $flags $cleanup $temp_list]]
			set temp_list [lreplace $temp_list 0 0]
		}
		set list $cleanup
	}

	# -REPLACE (CHANSET logic: +1 & -1 replace each other) -- DEFAULT!
	# -ANNIHILATE = remove both (+1 and -1 annihilate (self-destruct), leaving neither behind)
	set replace [not [validflag -annihilate]]
	empty lcancel
	while { ![string eq "" $list] } {
		set element [lindex $list 0]
		if [regexp -- {^[\+\-][^\+\-]} $element] { 
			set polarity [left $element 1 +]
			set anti [lindex [list + -] $polarity][string range $element 1 end]
			if [validflag -nocase] { set m [lsearch -exact [string tolower $list] [string tolower $anti]] } { set m [lsearch -exact $list $anti] }
			if { $m == -1 } {
				lappend lcancel $element
			} {
				set list [lreplace $list $m $m]
				if $replace { lappend lcancel $anti }
			}
		} {
			lappend lcancel $element
		}
#debug flags list element name polarity anti m lcancel
		set list [lreplace $list 0 0]
	}
	return $lcancel
}

proc inlist args {
	if [string eq "" $args] { return 0 }

	# No INLINE replies: only indicate if item(s) are in the original list, given the various flags
	flags:simple $args [list -any -all -nocase -glob -exact -regexp] temp flags
	lassign $temp list checkme
	set match glob
	foreach a [list regexp exact glob] { if [validflag -$a] { set match $a } }
	zero all any
	if [validflag -any] { one any ; lremove flags -all } { one all ; lremove flags -any }
	set inlist 1 ; # Default to 1, we'll "short-circuit" for any failures (depending on -ANY)
	# Because we're not going to return any list values, we can "corrupt" the list if necessary
	if [validflag -nocase] { set list [list:tolower $list] ; set checkme [list:tolower $checkme] }
	if [string eq "" $checkme] { return 0 }
	foreach check $checkme {
		set m [lsearch -$match $list $check]
		if { $m == -1 } { set inlist 0 ; if $all break } { set inlist 1 ; if $any break }
	}
	return $inlist
}

proc highlight:list args {
	flags:simple $args [list -nocase -glob -exact -regexp] text flags
	set effects [lassign $text list match]
	if [isempty effects] { set effects reverse }
	if ![validflag -glob -exact -regexp] { lappend flags -glob }

	empty m
	if [validflag -nocase] { set t [string tolower $list] ; set c [string tolower $match] } { set t $list ; set c $match }
	foreach i $c {
		if [validflag -glob] {
			set m [concat $m [lsearch -all -glob   $t $i]]
		} elseif [validflag -regexp] {
			set m [concat $m [lsearch -all -regexp $t $i]]
		} else {
			set m [concat $m [lsearch -all -exact  $t $i]]
		}
	}
	set m [lsort -inc -unique $m]

	foreach a $m {
		lset list $a [effects [lindex $list $a] $effects]
	}
	return $list
}

proc sequence args {
	set extra [lassign $args 0 1 2 3]
	if [notempty extra] { error "\[SEQUENCE\] Extra arguments: $extra" }

	set reverse 0
	if [isempty 1] {
		return $0
	} {
		if [isempty 3] {
			if [isempty 2] {
				if [string eq .. $1] {
					error "\[SEQUENCE\] Illegal pattern: $0 .. (infinite series)"
				} {
					if [string eq .. $0] { set start 1 ; set end $1 ; set step 1 } { return [list $0 $1] }
				}
			} {
				if [string eq .. $0] {
					if { $2 < $1 } { error "\[SEQUENCE\] Illegal format: $0 $1 $2 (infinite series count-down)" } { set start 1 ; set end $2 ; set step 1 }
				} {
					if [string eq .. $1] { set start $0 ; set end $2 ; set step 1 } { if [string eq .. $2] { error "\[SEQUENCE\] Illegal format: $0 $1 $2 (infinite series)" } { if { ( $2 - $1 ) == ( $1 - $0 ) } { set start $0 ; set end $2 ; set step [expr $1 - $0] } { return [list $0 $1 $2] } } }
				}
			}
		} {
			if [string eq .. $0] { error "\[SEQUENCE\] Illegal format: $args" }
			if [string eq .. $1] {
				if { $2 > $3 } { if { $0 < $2 } { error "\[SEQUENCE\] Illegal format: $args (null list)" } { set reverse 1 ; set start $3 ; set end $0 ; set step [expr $2 - $3] } } { if { $0 > $2 } { error "\[SEQUENCE\] Illegal format: $args (null list)" } { set start $0 ; set end $3 ; set step [expr $3 - $2] } }
			} {
				if [string eq .. $2] {
					if { $0 < $1 } { if { $3 < $1 } { error "\[SEQUENCE\] Illegal format: $args (null list)" } { set start $0 ; set end $3 ; set step [expr $1 - $0] } } { if { $3 > $1 } { error "\[SEQUENCE\] Illegal format: $args (null list)" } { set reverse 1 ; set start $0 ; set end $3 ; set step [expr $0 - $1] } }
				} {
					if [string eq .. $3] { error "\[SEQUENCE\] Illegal format: $args (infinite series)" } { return [list $0 $1 $2 $3] }
				}
			}
			
		}
	}

	if { [lsearch -exact [list $start $end $step] ..] != -1 } { error "\[SEQUENCE\] Syntax error (improper use of \"..\"): $args" }
	if !$step { error "\[SEQUENCE\] The increment must be non-zero" }
	if { $start > $end } { set reverse 2 }
	empty sequence
	switch -exact -- $reverse {
		2 { zero c ; for { set x $start } { $x >= $end } { decr x $step } { lappend sequence $x ; incr c ; if { $c > 1024 } { error LOOP! } } }
		1 { zero c ; for { set x $start } { $x >= $end } { incr x $step } { lappend sequence $x ; incr c ; if { $c > 1024 } { error LOOP! } } ; set sequence [lreverse $sequence] }
		0 { zero c ; for { set x $start } { $x <= $end } { incr x $step } { lappend sequence $x ; incr c ; if { $c > 1024 } { error LOOP! } } }
	}
	return $sequence
}

proc sequence:asc args {
	set range ""
	if [regexp -- {^\S+$} $args] { return $args }
	if [regexp -- {^\S+ \S+$} $args] { lassign $args 1 2 ; scan $1 %c 1 ; scan $2 %c 2 ; for { ; } { $1 <= $2 } { incr 1 } { lappend range [format %c $1] } ; return $range }
	if [regexp -- {^\S+ \.\. \S+$} $args] { lassign $args 1 - 2 ; scan $1 %c 1 ; scan $2 %c 2 ; for { ; } { $1 <= $2 } { incr 1 } { lappend range [format %c $1] } ; return $range }
	if [regexp -- {^\S+ \S+ \.\. \S+$} $args] { lassign $args 1 3 - 2; scan $1 %c 1 ; scan $2 %c 2 ; scan $3 %c 3 ; set step [expr $3 - $1] ; for { ; } { $1 <= $2 } { incr 1 $step } { lappend range [format %c $1] } ; return $range }
	return $args
}

# --- LIST helpers ---

proc list:tolower list {
	# Loop used to protect LIST integrity
	set list_tolower [list]
	foreach element $list { lappend list_tolower [string tolower $element] }
	return $list_tolower
}

proc list:toupper list {
	# Loop used to protect LIST integrity
	set list_toupper [list]
	foreach element $list { lappend list_toupper [string toupper $element] }
	return $list_toupper
}

proc list:totitle list {
	# Loop used to protect LIST integrity
	set list_totitle [list]
	foreach element $list { lappend list_totitle [stt $element] }
	return $list_totitle
}

proc list:tosentence list {
	# Loop used to protect LIST integrity
	set list_tosentence [list]
	foreach element $list { lappend list_tosentence [string totitle $element] }
	return $list_tosentence
}

proc lsort:strip { 1 2 } { 
	set 1 [effects $1 strip]
	set 2 [effects $1 strip]
	if { $1 < $2 } { return -1 }
	if { $2 < $1 } { return +1 }
	return 0
	#string compare [effects $1 strip] [effects $2 strip] ; # <--- isn't working
}

proc lsort:len { 1 2 } { set 1l [len $1] ; set 2l [len $2] ; if { $1l < $2l } { return -1 } ; if { $1l > $2l } { return 1 } ; return 0 }

proc lsort:alpha { 1 2 } { regexp -nocase -- {^[^A-Z]*(.+)$} $1 -- _1 ; regexp -nocase -- {^[^A-Z]*(.+)$} $2 -- _2 ;  string compare $_1 $_2 }

proc lsort:chanop { 1 2 } {
	regexp -nocase -- {^([\~\&\@\%\+]*)(.+)$} $1 - _1s _1n ;
	regexp -nocase -- {^([\~\&\@\%\+]*)(.+)$} $2 - _2s _2n ;
	set _1p [lsearch -exact [list ~ & @ % + ""] [left $_1s 1]]
	set _2p [lsearch -exact [list ~ & @ % + ""] [left $_2s 1]]
	if { $_1p < $_2p } { return -1 }
	if { $_1p > $_2p } { return +1 }
	string compare $_1n $_2n
}

# --- Math helpers ---

proc simplify args {# Get FACTORS of both number, get the common ones, run through a FOREACH of them.

	set expression [join $args ""]

	lassign [split $expression /] e1 e2



	# Syntax checks

	if ![regexp -- {^[\+\-]?\d+?(\.\d+)?$} $e1] { return $expression }

	# Don't check $E2: this disallows "0.24" to be passed through



	# Upgrade fraction if numerator or denominator is a decimal 

	# Can this be collected, choose the highest-needed push, apply to both?

	if [instr $e1 .] { set e1 [dec2frac $e1] }

	if [instr $e2 .] { set e2 [dec2frac $e2] }



	# A lonely floating point number was given

	if [isempty e2] { lassign [split $e1 /] e1 e2 }



	# Let's do a cheat first: can the fraction be directly reduced?



	set d1 [ expr $e2 / $e1 ]; # Will truncate to integer answer

	set d2 [ fixmath $e2 / $e1 ]; # Will force the mantissa is necessary

	if { $d1 == $d2 } { return 1/[ expr $e2 / $e1 ] }



	# Reduce fraction by GCM

	zero c

	do {incr c ; if {$c > 1000} { error "\[SIMPLIFY\] Endless loop: C($c)" }

		set f1 [factors $e1] 

		set f2 [factors $e2]

		if [isempty f1] break

		if [isempty f2] break

		set gcm [le [lcommon $f1 $f2]]

		if [isempty gcm] break

		set e1 [ expr $e1 / $gcm ]

		set e2 [ expr $e2 / $gcm ]

	}



	if { $e2 == 1 } { set simplify $e1 } { set simplify ${e1}/$e2 }

	return $simplify

}

proc dec2frac number {
	lassign [split $number .] i m
	if [notempty m] {
		set m [normalize $m]
		set l [len $m]
		set p 1[string repeat 0 $l]
		set number [expr ( [normalize $i] * $p ) + $m]/$p
	}
	return $number
}

proc factors { args } {

	set arg "* [join $args]"

	flags:simple $args [list -prime -square -all] 1 flags



	if { $1 > 1e12 } { error "\[FACTORS\] Error in FACTORS: value ($misc1) exceeds 999,999,999,999 limit (based on default 12-bit TCL precision)." }

	if ![isnum -integer $1] { error "\[FACTORS\] I need an integer, not: $1" }



	zero neg

	if [left $1 1 -] { one neg ; set 1 [mid $1 2] }



	empty f

	set s [expr $1 / 2] ; # set s [sqr $1]

	for { set loop 2 } { $loop <= $s } { incr loop } {

		if { [lsearch -exact $f $loop] != -1 } continue

		set d [normalize [expr $1 / ${loop}.0]]

		if [isnum -integer $d] { lappend f $loop $d }

	}

	set f [lsort -int -inc $f]

	if [validflag -prime] {

		empty t

		foreach a $f { if [isprime $a] { lappend t $a } }

		set f $t

	}

	if [validflag -square] {

		empty t

		foreach a $f { if [issquare $a] { lappend t $a } }

		set f $t

	}



	if [validflag -all] { lappend f 1 $1 } ; # Must precede $NEG check

	if $neg { foreach a $f { lappend f -$a } }

	set f [lsort -unique -real -increasing $f]

	return $f

}

proc isprime { number } {

	if ![regexp -- {^[\+\-]?\d+$} $number] { error "\"${number}\" is not an integer: ISPRIME <integer>" }

	if { $number < 2 } { return 0 }

	float number



	set half [ expr ( $number / 2 ) ]

	int half

	incr half; # These lines needed to evade "integer too large" error.



	one list

	for {set i 2} {$i < $half} {incr i} {

		if { ( $number / $i ) == int( $number / $i ) } { lappend list $i }

	}

	int number

	lappend list $number

	string eq -n "1 ${number}" $list

}

proc issquare number {
	set s [sqr $number]
	if [right $s 2 .0] { set s [left $s -2] }
	regexp -- {^(\+\-)?\d+$} $s
}

proc sqr { value } { normalize [expr pow( $value , 0.5 )] }

# --- User information ---

proc alias args {
	flags:simple $args [list] text flags
	lassign $text cmd 1 2 3 4 5
	switch -exact -- [string tolower $cmd] {

		set {}

		add {}

		del {}

		list {}

		check {# ALIAS check ME nick handle
			if [string eq -nocase ME $1] { return [casehandle $3] }
			set 1 [string tolower $1]
			foreach user [userlist] {
				set data [userinfo get $user ALIAS]
				if { [lsearch -exact [string tolower $data] $1] != -1 } { return $user }
			}
			return
		}

		default { error "\[ALIAS\] Unknown command: $cmd" }

	}
	?
}

proc validhost { host handle } {
	if ![validuser $handle] { return 0 }
	if [instr $host @] { if ![instr $host !] { prepend host *! } }
	foreach h [getuser $handle hosts] { if [string match -nocase $h $host] { return 1 } }
	return 0
}

proc validutimer text {
	foreach utimer [utimers] { if [string match -nocase $text [lindex $utimer 1]] { return 1 } }
	return 0
}

proc validtimer text {
	foreach timer [timers] { if [string match -nocase $text [lindex $timer 1]] { return 1 } }
	return 0
}

proc findtimer text {
	empty findtimer
	foreach timer [timers] { if [string match -nocase $text [lindex $timer 1]] { lappend findtimer [lindex $timer 2] } }
	foreach utimer [uimers] { if [string match -nocase $text [lindex $utimer 1]] { lappend findtimer [lindex $utimer 2] } }
	return $findtimer
}

proc gettimer text {
	empty gettimer
	foreach a [concat [utimers] [timers]] {
		if { [string eq -nocase $text [lindex $a 1]] || [ string eq -nocase $text [lindex $a 2]] } { lappend gettimer $a }
	}
	return $gettimer
}

proc userlist:level { { range "1-1001" } { chan "" } } {
	array set userlist ""
	empty list
	foreach user [userlist -b] { lappend userlist([access get $user $chan]) $user }
	foreach level [lsort -integer -decreasing [array names userlist]] {
		if [inrange $range $level] { set list [concat $list [lsort -inc -dict $userlist($level)]] }
	}
	return $list
}

proc nph { nick handle { join " " } { open "(" } { close ")" } } {
	set nick [casenick $nick]
	set handle [casehandle $handle]
	if [isempty nick] { return $handle }
	if [isnum -integer $nick] { return $handle }
	if [isempty handle] { return $nick }
	if ![validuser $handle] { return $nick }
	if [string eq -nocase $nick $handle] { return $handle }
	return ${nick}${join}${open}${handle}${close}
}

proc access args {
	set handle [lindex $args 1] ; # Most sections will use this
	set default [expr [validuser $handle] ? 0 : -1]
	set cmd [lindex $args 0]
	switch -exact -- [string tolower $cmd] {

		get {
			lassign $args - handle chan
			if ![validuser $handle] { return -1 }
			access fix $handle $chan
			if [isempty chan] { set chan global }
			set level [userinfo get $handle access:$chan]
			if [isempty level] { set level 0 }
			if [notempty level] { return $level }
		}

		fix { # 1399256400: Do =NOT= call ACCESS GET: infinite loop!
			lassign $args - handle chan
			if [isempty chan] {
				# PermOwner
				if [is permowner $handle] { 
					set level 1000
					regsub -all -- {,| [ ]+} $::owner " " o
					if { [llength $o] > 1 } {
						if [string eq -nocase $handle [lindex $o 0]] { set level 1001 }
						# Either way (: if { [lsearch -exact [string tolower $o] [string tolower $handle]] == 0 } { set level 1001 }
					}
					userinfo set $handle access:global $level
					return $level
				}
				# Global
				## WTF??
				##if [isnum -integer [string eq "" [userinfo get $handle access:global]]] return
				if ![string eq "" [userinfo get $handle access:global]] return
				foreach { flag value } [list n 900 m 800 t 700 o 600 l 575 v 550 f 501] {
					if [userflag check $handle $flag] { putcmdlog "\[ACCESS FIX\] Automatically adjusting user level: $handle (global -> $value)" ; userinfo set $handle access:global $value ; return $value }
				}
				userinfo set $handle access:global 0
				return 0
			} {
				# Local
				## WTF??
				##if [isnum -integer [string eq "" [userinfo get $handle access:$chan]]] return
				if ![string eq "" [userinfo get $handle access:$chan]] return
				foreach { flag value } [list n 500 m 400 o 100 l 75 v 50 f 1] {
					if [userflag check $handle $flag $chan] { putcmdlog "\[ACCESS FIX\] Automatically adjusting user level: $handle ($chan -> $value)" ; userinfo set $handle access:$chan $value ; return $value }
				}
				userinfo set $handle access:$chan 0
				return 0
			}
		}

		owner:check {
			set handle [lindex $args 1]
			if ![is owner $handle] return
			set o [get owners]
			set m [lsearch -exact [string tolower $o] [string tolower $handle]]
			if { $m == -1 } return ; # Shouldn't happen (IS OWNER would fail above)
			if [is owner $handle 1001] { set target 1001 } { set target 1000 }
			return $target
		}

		check {
			lassign $args - level handle chan
			if [isempty chan] { set chan global }
		
			set user_global [none [access get $handle] 0]
			set user_local [none [access get $handle $chan] 0]
			if { $user_global > $user_local } { set user $user_global } { set user $user_local }
			expr ( $user >= $level ) ? 1 : 0 
		}

		compare {
			flags -simple [lrange $args 1 end] [list -equal:ok -self:ok] text flags
			lassign $text handle1 handle2 chan
			if [string eq -nocase $handle1 $handle2] { if [validflag -equal:ok -self:ok] { return 1 } }
			if [isempty chan] { set chan global }
		
			# User 1
			set user1_global [access get $handle1]
			set user1_local [access get $handle1 $chan]
			if { $user1_global > $user1_local } { set user1 $user1_global } { set user1 $user1_local }

			# Cheat for highest-level (global vs local)
			if [isempty handle2] { return $user1 }
		
			# User 2
			set user2_global [access get $handle2]
			set user2_local [access get $handle2 $chan]
			if { $user2_global > $user2_local } { set user2 $user2_global } { set user2 $user2_local }
		
			# Obvious situations ....
			if { $user1 == 1001 } { return 1 }
			if { $user1 == 1000 } { if { $user2 != 1001 } { return 1 } }
		
			if [validflag -equal:ok] { set control >= } { set control > }
			expr ( $user1 $control $user2 ) ? 1 : 0
		}

		high - higher - highest - either {
			lassign [lrange $args 1 end] handle chan
			if ![validuser $handle] { return -1 }
			if [isempty chan] {
				return [none [access get $handle] $default]
			} {
				return [lindex [lsort -decreasing -integer [list [none [access get $handle] $default] [none [access get $handle $chan] $default]]] 0]
			}
		}

		set {
			lassign $args - handle level chan
			if ![validuser $handle] { return -1 }
			if ![inrange -1-1001 $level] { return -1 }
			if [string eq -nocase GLOBAL $chan] { empty chan }
			if [notempty chan] {
				if { $level > 500 } { error "\[ACCESS SET\] Illegal channel $chan access level \"${level}\" (channel level must be between 1 and 500)" }
				if ![validchan $chan] { error "\[ACCESS SET\] Illegal channel: $chan" }
				if $level {
					userinfo set $handle access:$chan $level
				} {
					userinfo set $handle access:$chan
				}
			} {
				if { $level < 501 } { error "\[ACCESS SET\] Illegal global access level \"${level}\" (global level must be between 501 and 1001)" }
				if { $level >= 1000 } {
					if ![is owner $handle] { error "\[ACCESS SET\] Illegal user being given $level access: $handle (not listed in \$OWNER configuration variable)" }
					if { $level == 1001 } {
						if ![is owner $handle 1001] { error "\[ACCESS SET\] Illegal user being given $level access: $handle (not listed in first position in \$OWNER configuration variable)" }
					}
				}
				if $level {
					userinfo set $handle access:global $level
				} {
					userinfo set $handle access:global
				}
			}
			return $level
		}

		default { return [eval access [concat get $args]] }
	}
}

proc userflag { cmd handle flags { chan "" } } {
	switch -exact -- [string tolower $cmd] {

		check {
			if ![validuser $handle] { return 0 }
			lassign [split [chattr $handle $chan] |] global local
			foreach flag [split $flags ""] {
				if [isempty chan] { if { [string first $flag $global] != -1 } { return 1 } } { if { [string first $flag $local] != -1 } { return 1 } }
			}
			return 0
		}

		set { # Sets flags *TO* the value (not add / remove)
			if ![validuser $handle] { return 0 }
			while { [regexp -- {^[\+\-]} $flags] } { set flags [string range $flags 1 end] }
			if [isempty chan] { chattr $handle -[chattr $handle] ; chattr $handle $flags } { chattr $handle -[lindex [split [chattr $handle $chan] |] 1] $chan ; chattr $handle |$flags $chan }
			return
		}

		get {
			if ![validuser $handle] return
			if [isempty chan] { return [chattr $handle $flags] } { return [lindex [split [chattr $handle |$flags $chan] |] 1] }
			return
		}

		clear {
			if ![validuser $handle] return
			if [isempty chan] { chattr $handle -[chattr $handle] } { chattr $handle |-[lindex [split [chattr $handle $chan] |] 1] }
			return
		}

		add {
			if ![validuser $handle] return
			if [isempty chan] { 
				chattr $handle $flags 
			} { 
				if ![string match *|* $flags] { set flags |+$flags }
				chattr $handle +$flags $chan 
			}
			return
		}

		del {
			if ![validuser $handle] return
			foreach flag [split $flags ""] { if [isempty chan] { chattr $handle -$flag } { chattr $handle |-$flag $chan } }
			return
		}

		default { error "\[FLAG\] Unknown option: $cmd" }

	}
	?
}

proc whois { who { who2 "" } { chan "" } } {
	# RETURN: [LIST online?:-1(unknown)/0(known:offline)/1(known:online)/2(DCCONLY)/3(BOT:LINKED)/4(unknown handle/nick matches known handle) nick host handle chan authed:handle authed:nick] ("" for any unknowns)
	if [string eq * $chan] { empty chan }

	# Bot?
	if { [isbotnick $who] || [isbotnick $who2] } { return "1 [findonchans nick $::botnick] {} {}" }

	# Me?
	if [notempty who2] {
		if [string eq -nocase ME $who] {
			if [string eq -nocase ME $who2] { error "\[WHOIS\] Invalid data: both monikers are \"me\"" }
			set who $who2
		}
	}

	# Hosts?
	if [string match *@* $who] {
		set mask $who
		if ![instr $mask !] { prepend mask *! }
		# Let's try the userlist
		foreach user [userlist] {
			set found 0
			foreach host [getuser $user HOSTS] {
				if [string match -nocase $mask $host] {
					if [isempty who2] { ; } { ; }
					set who \$$user
					set who2 \$$user
					set found 1
				}
			}
			if $found break
		}
	}

	if [string match *@* $who] {
		# Let's try all current channels
		if [notempty chan] {
			set chanlist [channels]
		} {
			set chanlist $chan
		}
		foreach ch $chanlist {
			#
		}
	}

	# Aliases?
	foreach user [string tolower [userlist -b]] { # STRING / LIST violation!
		set aliases [string tolower [userinfo get $user ALIAS]]
		if { [lsearch -exact $aliases $who] != -1 } { set who $user ; break }
	}

	# DCC index?
	if [isnum -integer $who] {
		if [valididx $who] {
			lassign [findonchans handle [idx2hand $who]] nick host handle chan
			if [isempty nick] {
				return [list 0 "" "" $handle "" "" ""]
			} {
				set authed_nick [sb7 auth find handle $nick]
				set authed_hand [sb7 auth find nick $handle]
				return [list 2 $nick $host $handle $chan $authed_nick $authed_hand]
			}
		}
	}

	# Unless a $-forced handle is specified, check for NICK MATCH FIRST!
	# This is required by dispatcher (for handle lookup override).
	if [left $who 1 $] {
		# A forced HANDLE consideration marker (which just bypasses the nick check)
		set who [mid $who 2]
		lassign [findonchans nick $who] foc_nick foc_host foc_handle foc_chan
		if [isempty foc_handle] { return [list 0 "" "" $who "" "" ""] }
		lassign [whois $foc_nick] _online _nick _host _handle _chan _auth_handle _auth_nick
		return [list $_online $_nick $_host $_handle $_chan $_auth_handle $_auth_nick]
	} {
		# Let's try this as an online nick first
		lassign [findonchans nick $who] nick host handle chan
		if [notempty handle] {
			set authed_nick [sb7 auth find handle $nick]
			set authed_hand [sb7 auth find nick $handle]
			return [list 1 $nick $host $handle $chan $authed_nick $authed_hand]
		}
	}

	# Let's look for a user by handle (whose nick is online)
	lassign [findonchans handle $who] nick host handle chan
	if [notempty nick] {
		set authed_nick [sb7 auth find handle $nick]
		set authed_hand [sb7 auth find nick $handle]
#debug -log 7 who nick host handle chan authed_nick authed_hand
		return [list 1 $nick $host $handle $chan $authed_nick $authed_hand]
	}

	# Let's look for a user whose nick matches a known handle, but, doesn't mask-match it (user needing an ADDMASK)
	lassign [findonchans nick $who] nick host handle chan
#debug -log 7 who nick host handle chan
	if { [isempty handle] && [validuser $who] } {
#debug who host handle chan authed_nick authed_hand
		empty authed_nick authed_hand
		return [list 4 $who $host "" $chan $authed_nick $authed_hand]
	}

	# Let's try a handle
	lassign [findonchans handle $who] nick host handle chan
	if [notempty nick] {
		set authed_nick [sb7 auth find handle $nick]
		set authed_hand [sb7 auth find nick $handle]
		return [list 1 $nick $host $handle $chan $authed_nick $authed_hand]
	}

	# Maybe the userlist? (When the user isn't on any common channels)
	if [validuser $who] {
		set authed_nick [sb7 auth find nick $who]
		set authed_hand [sb7 auth find handle $who]
		return [list 0 [casehandle $who] "" [casehandle $who] "" [iff [notempty authed_nick] $who] [iff [notempty authed_nick] $who]]
	}

	# Is it a bot on the botnet?
	# If $::BOTNET-NICK != $::NICK, we're gonna have a problem here ....
	if [islinked $who] { return [list 3 [casebot $who] "" [casebot $who] "" "" ""] }

	# I have no fucking clue who this is ....
	return [list -1 $who "" "" "" "" ""] ; # RETURN as a nick only
}

proc nick who {
	# ONLY RETURN a value if the nick is found online ; blank otherwise
	# If user is floating off channels where the bot can't see: "oh well!"

	# DCC index?
	if [isnum -integer $who] {
		if ![valididx $who] { error "\[NICK\] Invalid IDX: $who" }
		set who [idx2hand $who]
	}

	# Is this a handle that's logged-in? Let's default to that possibility first ....
	set auth [sb7 auth find nick $who]
	if [notempty auth] { return $auth }

	# Check if is a valid handle. If so, find nick online.
	lassign [whois $who $who] online wn wm wh wc
	switch -exact -- $online { -1 - 0 return 1 - 2 - 3 { return $wn } default ? }
	?
}

proc handle who {
	# DCC index?
	if [isnum -integer $who] {
		if ![valididx $who] { error "\[NICK\] Invalid IDX: $who" }
		return [idx2hand $who] ; # No need to go any further (as opposed to NICK)
	}

	# Check if this is a mask ....
	if [string match *@* $who] {
		set mask $who
		if ![instr $mask !] { prepend mask *! }
		foreach user [userlist] {
			foreach host [getuser $user HOSTS] {
				if [string match -nocase $host $mask] { return $user }
			}
		}
	}

	# Is this a handle that's logged-in? Let's default to that possibility first ....
	set auth [sb7 auth find handle $who]
	if [notempty auth] { return [casehandle $auth] }

	# Well, let's just default to a literal handle
	if [validuser $who] { return [casehandle $who] }
	return $who
}

if ![iseggcorecmd idx2nick] {
	proc idx2nick idx {
		if ![isnum -integer $idx] { return $idx }
		lassign [whois $idx] code nick host handle chan authed_hand authed_nick
		if { $code != 2 } return ; # Code "2" is DCC-only reference
		# Is this a smart second-default?
		return [iff [notempty $authed_nick] $authed_nick $nick]
		# Returns "" on an unknown user (caught by $CODE != 2 above)
	}
}

proc findonchans { mode who } {
	foreach chan [channels] {
		set chanlist [chanlist $chan]
		set userlist [userlist]
		switch -exact -- [string tolower $mode] {

			nick {
				if [onchan $who $chan] {
#					set nick [lindex $chanlist [lsearch -exact [stl $chanlist] [stl $who]]]
					set nick [lindex $chanlist [lsearch -exact [string tolower $chanlist] [string tolower $who]]]
					set handle [nick2hand $who $chan]
					if ![validuser $handle] { empty handle }
					return [list $nick [getchanhost $who $chan] $handle $chan]
				}
			}

			handle {
				if [handonchan $who $chan] {
#					set handle [lindex $userlist [lsearch -exact [stl $userlist] [stl $who]]]
					set handle [lindex $userlist [lsearch -exact [string tolower $userlist] [string tolower $who]]]
					set nick [hand2nick $who $chan]
					return [list $nick [getchanhost $nick $chan] $handle $chan]
				}
			}

			host {
				if [instr $who !] { set who [lindex [split $who !] 1] }
				foreach nick $chanlist {
					set host [getchanhost $nick $chan]
					if [string eq -nocase $who $host] { return [list $nick $host [nick2hand $nick $chan] $chan] }
				}
			}

			default { error "\[FINDONCHANS\] Unknown option: $mode" }

		}
	}
	return ""
	# Fail ):
}

proc maskmatches { nick host handle } {
	# Use "*" for $NICK to allow all possible matches by default
	if ![validuser $handle] return
	set mask ${nick}!$host
	empty list
	foreach h [getuser $handle HOSTS] { if [string match -nocase $h $mask] { lappend list $h } }
	return $list
}

proc @userinfo:check_valid args {
	set core [list botfl botaddr hosts laston info xtra comment email e-mail url handle pass]
	set who  [lindex [userlist] 0]
	if [isempty who] return ; # Can't conduct tests without a test subject!
	foreach test $core {
		set error [ catch { getuser $who $test } ]
		if $error { lremove core $test }
	}
	return $core
}

proc userinfo { cmd handle args } {
	lassign $args type arg

	# There seems to be some differences in what Eggdrop versions handle what info
	if [data get -isempty @USERINFO:VALID] @userinfo:check_valid
	set core [data get @USERINFO:VALID]
	set m [lsearch $core [string tolower $type]]

	switch -exact -- [string tolower $cmd] {

		set {
			# Note special format:
			# userinfo set HANDLE ALL [list type1 data1 type2 data2 ...]
			if ![validuser $handle] { return "" }
			set data $arg
			if [string eq -nocase ALL $type] {
				# Reflexive!
				foreach { a b } $data { userinfo $handle set $a $b }
				return $data
			}
			zero error
			set type [string tolower $type]
			if { $m != -1 } { set error [ catch { setuser $handle $type $data } uh_oh ] }
			if { $error || ( $m == -1 ) } { setuser $handle XTRA $type $data }
			return $data
		}

		get {
			if ![validuser $handle] { return "" }
			if { [string eq all $type] || [string eq * $type] } {
				empty userinfo
				foreach a $core { lappend userinfo [list $a [getuser $handle $a]] }
				set userinfo [concat $userinfo [getuser $handle xtra]]
				return [string tolower $userinfo] ; # Breaks STRING / LIST rules!
			}
			set type [string tolower $type]
			zero error
			if { $m != -1 } { set userinfo [getuser $handle $type] } { set userinfo [getuser $handle XTRA $type] }
			return $userinfo
		}

		list {
			set list $core
			if [string eq * $handle] {
				foreach user [userlist -b] { set list [concat $list [get first [getuser $user xtra]]] }
			} {
				set list [concat $list [get first [getuser $handle xtra]]]
			}
			if [notempty type] { set list [ldestroy -not -all -nocase -multiple -glob -- $list $type] }
			return [string tolower [lsort -uni -dict -uni $list]] ; # Breaks STRING / LIST rule
		}

	}
	?
}

# --- Channel Commands ---

proc chanrank { op nick chan { above false } } {
	set above [istrue $above] ; # Assure boolean
	set op [ldestroy -replacewith $op admin prot]
	set list [list voice halfop op prot own]
	set m [lsearch -exact [string tolower $list] [string tolower $op]]
	if { $m == -1 } { return 0 }

	set chanrank 0
	for { set x $m } { $x < [llength $list] } { incr x } {
		set cmd [lindex $list $x]
		if [validcmd is$cmd] { set chanrank [ expr $chanrank | [is$cmd $nick $chan] ] }
		if !$above break
	}
	return $chanrank
}

proc chanmode { cmd chan mode { arg "" } } {
#debug cmd chan mode arg
	switch -exact -- [string tolower $cmd] {

		add { # "ADD" just means: add on to, or remove from, the list (+ or - respectively)
			lassign [split $mode ""] polarity mode
			if [isempty arg] { error "\[MODECHANGE\] Missing target for: ${polarity}$mode" }
#debug cmd chan polarity mode arg
			if [string eq + $polarity] {
				data array lappend @chanmode ${chan}:$mode $arg
			} {
				data array lremove @chanmode ${chan}:$mode $arg
			}
#debug =RETURN
			return [data array get @chanmode ${chan}:$mode]
		}

		set { # "SET" just means: create the mode (with a value of "1" or the included value) or clear it (remove it completely)
			lassign [split $mode ""] polarity mode
			if [string eq + $polarity] {
				data array set @chanmode ${chan}:$mode [none $arg 1]
			} {
				data array set @chanmode ${chan}:$mode ""
			}
			return [data array get @chanmode ${chan}:$mode]
		}

		get { return [data array get @chanmode ${chan}:$mode] }

		default { error "\[MODECHANGE\] Unknown command type: CMD($cmd). Should be: add or set" }

	}
	?
}

proc ischanowner args { flags:simple $args -only text flags ; lassign $text user chan ; ischanmode $user $chan q ; # -ONLY flag is irrelevant here }
proc ischanadmin args { flags:simple $args -only text flags ; lassign $text user chan ; if [validflag -only] { return [ischanmode $user $chan a] } { return [ischanmode $user $chan qa] } }
proc ischanop args { flags:simple $args -only text flags ; lassign $text user chan ; if [validflag -only] { return [ischanmode $user $chan o] } { return [ischanmode $user $chan qao] } }
proc ischanhalfop args { flags:simple $args -only text flags ; lassign $text user chan ; if [validflag -only] { return [ischanmode $user $chan h] } { return [ischanmode $user $chan qaoh] } }
proc ischanvoice args { flags:simple $args -only text flags ; lassign $text user chan ; if [validflag -only] { return [ischanmode $user $chan v] } { return [ischanmode $user $chan qaohv] } }
proc ischanmode { user chan modes } { 
	foreach mode [explode $modes] { if [expr ( [lsearch -exact [string tolower [data array get @chanmode ${chan}:${mode}]] [string tolower $user]] == -1 ) ? 0 : 1] { return 1 } }
	return 0
}

proc chanowners args { flags:simple $args -only chan flags ; return [chanstaff $chan q] ; # -ONLY flag is irrelevant here }
proc chanadmins args { flags:simple $args -only chan flags ; return [chanstaff $chan [iff [validflag -only] a qa]] }
proc chanops args { flags:simple $args -only chan flags ; return [chanstaff $chan [iff [validflag -only] o qao]] }
proc chanhalfops args { flags:simple $args -only chan flags ; return [chanstaff $chan [iff [validflag -only] h qaoh]] }
proc chanvoices args { flags:simple $args -only chan flags ; return [chanstaff $chan [iff [validflag -only] v qaohv]] }
proc chanstaff { chan modes } {
	empty chanstaff
	foreach mode [explode $modes] { set chanstaff [concat $chanstaff [data array get @chanmode ${chan}:$mode]] }
	return $chanstaff
}

# --- Output commands ---

proc raw { type { target "" } { message "" } } {
	# Preferred order (based on Eggdrop versions): putnow putrawdcc quick-queue
	set text $type
	set max 360
	if {$target != ""} {set text "$type $target :$message"}

	#%# How to "read" the log timestamp format and duplicate it here?
	# Note: if raw logging ($::RAW-LOG) is off, this will throw an exception
	catch { putloglev v * "\[[clock format [clock seconds] -format %H:%M]\] \[r->\] $text" }
	set error [ catch { putraw $text } ] ; if !$error return ; # Eggdrop 1.8
	set error [ catch { putnow $text -oneline } ] ; if !$error return ; # Eggdrop 1.6.20
	set error [ catch { putnow $text } ] ; if !$error return ; # Eggdrop 1.6.20
	set error [ catch { putdccraw 0 [len ${text}\n] ${text}\n } ] ; if !$error return ; # Eggdrop 1.6.19:compat.tcl
	set error [ catch { putquick $text -next } ] ; if !$error return ; # Eggdrop 1.4
	set error [ catch { putquick $text } ] ; if !$error return ; # Eggdrop 1.3
	set error [ catch { putserv $text -next } ] ; if !$error return ; # Eggdrop 1.1.5
	set error [ catch { putserv $text } ] ; if !$error return ; # Eggdrop 1.0
	error "\[RAW\] I have =NO= idea how to do raw output on this bot!"
	return
}

proc isdccnick nick { regexp -- {^\d+$} $nick }

proc fixcolor code {
	if [string eq "" $code] { return 99,99 }
	set s [split $code ,]
	set new ""
	foreach a $s {
		if [string eq "" $a] { set a 99 }
		if { [string first . $a] != -1 } { set a [lindex [split $a .] 0] }
		if { $a < 10 } { set a [string range 00${a} end-1 end] }
		lappend new $a
	}
	join $new ,
}

proc color { { code "" } } { return \003[fixcolor $code] }

proc effects { text args } {
	# KILL = forcibly remove all codes ($0x0F)
	# STRIP = remove all codes from current sequence only (keep pre-existing ones)
	if [isempty args] { return "" }
	set args [join $args] ; # In case this is called from another command
	set original $text

	# DON'T SORT / UNIQUE any of the flags: the order may be important!
	empty open close

	foreach a $args {
		# Let DEFAULT handle the error ....
		switch -regexp -- [string tolower $a] {

			{^\d{1,2}(,\d{1,2})?$} { append open [color $a] ; prepend close [color] }

			^b$ - bold { append open \002 ; prepend close \002 }

			^u$ - underline { append open \037 ; prepend close \037 }

			^r$ - reverse { append open \026 ; prepend close \026 }

			^k$ - kill { set open \017 ; set close "" ; set text [effects $original strip] ; # Recursive! }

			^up$ - stu - upper { set text [string toupper $text] }

			^lo$ - stl - lower { set text [string tolower $text] }

			stt - title { set text [stt $text] }

			sts - sent - sentence { set text [string totitle $text] }

			e - esc - escape { set text [escape $text] }

			join { set text [join $text] }

			split { set text [split $text] }

			nocolor { regsub -all -- {\003([0-9]{1,2}(,[0-9]{1,2})?)?} $text "" text }

			^x$ - strip { empty open close ; regsub -all -- {\003([0-9]{1,2}(,[0-9]{1,2})?)?|\002|\037|\026|\017} $text "" text }

			^hex\:from$ {
				if [regexp -nocase -- {^0[xX]} $text] { set text [mid $text 3] }
				regsub -all -- [space] $text "" text
				if [isnum -odd [len $text]] { error "\[EFFECTS\] Hexadecimal string must have an even number of characters" }
				if ![isnum -hex $text] { error "\[EFFECTS\] Illegal hexadecimal characters: [regexp -inline -all -nocase -- {[^0-9A-F]} $text]" }
				set text [binary format H* $text]
			}

			^hex\:to$ { binary scan $text H* text ; set text [string toupper $text] }

			utf8 - ^utf\-8$ { set text [encoding convertto utf-8 $text] }

			default { error "\[EFFECTS\] Unknown command code: $a" }

		}
	}
	return ${open}${text}${close}
}

proc msg    { target message } { putserv "PRIVMSG $target :$message" ; return $message }
proc notice { target message } { putserv "NOTICE $target :$message" ; return $message }

proc msghome message {
	## Be careful: DEBUG calls this
	# Not anymore
	if [data array value -normalize CONFIG MUTE] { return 0 }
	set home [home]
	empty open close
	if ![data array value -normalize CONFIG COLOR:BLOCK] { 
		set open [data array value CONFIG COLOR]
		if [notempty open] { set close [color] }
	}
	msg $home ${open}${message}${close}
	return $message
}

proc rawhome text { rawprint "PRIVMSG [home] :$text" ; return }

proc printctcp { nick ctcp { message "" } } { print -ctcp -strip $nick "\001[string toupper $ctcp] ${message}\001" }
proc printctcr { nick ctcp { message "" } } { print -ctcr -strip $nick "\001[string toupper $ctcp] ${message}\001" }

# All-inclusive PRINT command (including line-splitting)
proc print args {
	set validflags [list -none -strip -mute -ctcp -ctcr -reset -debug -error -help -nouserdata -quick -burst -raw -noraw -normal -channel -private -msg -notice -next -header -header:short -short -return -wallops -home -keepdcc -dummy]
	# -DUMMY is for a variable to hold a "-private" or "-dummy" flag (based on need) within a variable
	flags -simple $args $validflags text flags
	if [validflag -none] { empty flags } ; # For use with NOTE and others that need to swap "-private" with "-none"
	set debug [validflag -debug]
	if [validflag -raw] { set flags [ldestroy -all -replacewith -- $flags -raw -burst] }; # Synonym
	if [validflag -noraw] { set flags [ldestroy -all -replacewith -- $flags -noraw -noburst] }; # Synonym
	lassign $text target message
if $debug { debug =-1 target message }
	empty open close
	set tab 5

	if [validflag -home] {
		# Abandon the previous $MESSAGE value
		if [isempty message] { set message $target }
		set target [home]
	}

	# Blank message?
	if { [isempty message] && [notempty target] } { set message " " }

	set output $target
if $debug { debug =0 flags target }

	# Inject override flags (yes, trump: BURSTALL -noburst -NORMAL)
	set override [data array value @OUTPUT:OVERRIDE $target]
	if $debug { debug =0.5 override }
	if [notempty override] {
		regsub -all -- {\-\-} $override - override
		foreach o $override {
			set um [uniquematch $validflags $o]
			if [notempty um] { lappend flags $um }
		}
	}

if $debug { debug =1 flags }
	# Removed burst-related code here; now handled by the SWITCH below ....

if $debug { debug =2 flags target }
	empty pre post
	set user_setting_burst 0
	if [validchan $target] {
		set type PRIVMSG
		set intended_user 0
		set chan $target
		#if [data array value -normalize CONFIG:$target MUTE] { return "" }
		# Only needed for MSGHOME!
		if ![data array value -normalize CONFIG:$target COLOR:BLOCK] { set open [data array value CONFIG:$target COLOR] }
		set output $target
		#foreach loop [list -notice -msg] { set flags [lsearch -all -inline -exact -not $flags $loop] }
		# OPMSG / WALLOPS might need this :p
	} {
		# Valid user

		# No output? Bail out! (Only for user output ~ do NOT affect output from direct-output commands like SAY)
		if [validflag -mute] return

		# What if it's the user's first command? (Trac #10)
		if [data array get -isempty @output $target] {
			data array set @output $target [list $target * $target [home] PUB]
			set type NOTICE 
		} {
			set type [data array value -default CONFIG OUTPUT NOTICE]
		}
		set intended_user 1
		lassign [data array value @OUTPUT $target] - host handle chan
		if [string eq * $chan] { set chan [data array get @output last:$target] }
#putlog "\[PRINT\] TARGET($target):CHAN($chan)"
#		if { [lsearch -exact [list "" *] $chan] != -1 } { set flags [ldestroy -replacewith $flags -msg -notice] ; set:all temp temp_type notice ; set output $target }
		if { [lsearch -exact [list "" *] $chan] != -1 } { set chan [lindex [concat [home] [channels]] 0] ; set flags [ldestroy -replacewith $flags -msg -notice] ; set:all temp temp_type notice ; set output $target }
		if [validuser $handle] {
			if [boolean -integer [userinfo get $handle BURST]] { set:all burst_ok user_setting_burst 1 ; lprepend flags -burst }
			set pre [userinfo get $handle prefix]
			set post [userinfo get $handle postfix]
			set temp_type [userinfo get $handle OUTPUT]
			switch -glob -- [string tolower $temp_type] {

				notice { set output $target ; set type NOTICE }

				msg { set output $target ; set type PRIVMSG }

				chan { set output $chan ; set type PRIVMSG }

				"#*" { set output $temp ; set type PRIVMSG }

				same {
					# set flags [ldestroy -replace [list -notice] -notice -msg] 
					switch -exact -- [string tolower [lindex [data array get @output $target] 4]] {

						pub - pubm { set output $chan ; set type PRIVMSG }

						msg { set output $target ; set type PRIVMSG }

						not - notc { set output $target ; set type NOTICE }

						dcc { set output $target ; set type dcc }

						default { error "\[PRINT\] User $target has an output setting of \"SAME\" but I can't tell where to send the output! @OUTPUT([data array get @output $target])" }

					}
					#putlog "\[PRINT\] SAME:FLAGS($flags):@OUTPUT([lindex [data array get @output $target] 4]):OUTPUT($output):TYPE($type)"
				}

				default { set output $target }

			}

		}
		set color [userinfo get $handle COLOR]
		if ![validflag -nouserdata] { if [notempty color] { set open $color } }
	}

if $debug { debug =3 flags target output }
	if [validflag -reset] {
		# Set ALL items to default values: no colour, notice, to user
		set output $target
		set type NOTICE
		empty open close
		# Presume -RESET was before other flags that should be the only survivors
		set m [lsearch -exact $flags -reset]
		set flags [lreplace $flags 0 $m]
	}

if $debug { debug =4 flags target output }
	if [validflag -noburst] { set flags [ldestroy -all -nonulls -multiple $flags [list -noburst -burst]] } ; # Supercede -burst
	if [validflag -normal] { set flags [ldestroy -all -nonulls -multiple $flags [list -normal -help -quick -burst -private -msg -notice -next]] ; set output $target ; set type NOTICE }
	if [validflag -channel] { set output $chan ; set type PRIVMSG }

if $debug { debug =5 flags target =CONFIG:BURST([data array get config burst]) burst_user_setting }
	switch -exact -- [string tolower [data array get config burst]] {

		all - force { set burst_ok 1 }

		user - users { set burst_ok $user_setting_burst }

		"" - none - off { if $user_setting_burst { set burst_ok 0 } { set burst_ok [validflag -burst] ; # Respects -NOBURST } }

		blocked - block { set burst_ok 0 }

		default { error "\[PRINT\] Unknown CONFIG -GLOBAL BURST mode: [data array get config burst]" }

	}
if $debug { debug burst_ok user_setting_burst =CONFIG:BURST([data array get config burst]) target =USER:SETTING([userinfo get $target burst]) message }
	# In priority order ....
	empty remote

	if [validflag -error] { lprepend flags -return }
	if [instr $target @] { lassign [split $target @] target remote }
if $debug { debug =6 flags }

	# Consideration: if --NOTICE --CHANNEL are both used, NOTICE wins. Is
	# there a situation where we would want to /NOTICE the channel with
	# data, outside of a WALLOPS situation? If so, we need to repeat a
	# -CHANNEL check within the -NOTICE check (around / bypass
	# $INTENDED_USER). Or, will the new -WALLOPS flag cover it? It
	# might be a better solution because of how unique the situation is.

	if [validflag -msg] { set type PRIVMSG ; if $intended_user { set output $target } }
	if [validflag -notice] { set type NOTICE ; if $intended_user { set output $target } }
	if [validflag -wallops] { set type NOTICE ; set output $chan }

	if [validflag -ctcp] { set type PRIVMSG ; set output $target }
	if [validflag -ctcr] { set type NOTICE ; set output $target }

	set queue serv
	if [validflag -error] { set queue help ; prepend message "[effects {[ERROR]} reverse] " }
	if [validflag -help] { set queue help }
	if [validflag -normal] { set queue serv }
	if [validflag -quick] { set queue quick }

	prepend queue put

	set speedflag normal
	if [validflag -next] { set speedflag next }

	# Override
	if [validflag -private] { lappend flags -notice ; set type NOTICE ; set output $target } ; # Will still pipe to DCC if appropriate

	if [validflag -short] { lappend flags -header -header:short }
	if [validflag -header:short] { lappend flags -header }
	if [validflag -header] {
		# Can we make use of "sb7 header 1" here?
		set header [lindex [info level [expr [info level] - 1]] 0]
		if [validflag -header:short] {
			if [left $header 8 sb7:cmd_] { set header [mid $header 9] }
			if [left $header 1 @] { set header [mid $header 2] }
		}
		prepend message "\[[string toupper $header]\] "
	}

if $debug { debug =7 flags }
	# Create the output buffer ....
	if [isempty message] { set message " " } ; # "Blank" line
	regsub -all -- \t $message [space $tab] message

	empty buffer
	# 1399240763: Tests indicate that the most the test bed's IRCd (UnrealIrcd) can handle is: 490 chars-per-line (including header)
	# This includes all "control codes" used (bold, reverse, color, et cetera)

	# What about NOTICE [vs PRIVMSG] (or just sacrifice the single character?)
	# Reduced to 484 to account for 6-characters color codes ("\00399,99")
	set limit 475 ; # Let's just be safe (RSS keeps clipping characters if we try to hit the limit exactly)
	set max_length [expr $limit - ([string length $::botname] + 1 + [string length "PRIVMSG $target :"]) ]

	set original $message
	foreach message [split $original \n] {
		# Enforce color here
		if [notempty open] {
			regsub -all -nocase -- \00399,99 $message &[color $open] message
			foreach a [regexp -inline -all -nocase -- \003. $message] {
				if ![regexp -- {\003\d} $a] { regsub $a $message \003[color $open][string index $a end] message }
			}
		}

		if [notempty pre] { incr max_length -[expr [string length $pre] + 1]}
		if [notempty post] { incr max_length -[expr [string length $post] + 1]}
		if [isempty message] {
			lappend buffer " "
			#lappend buffer [string trim "$pre $post"]
		} elseif [string eq " " $message] {
			lappend buffer " "
			#lappend buffer [string trim "$pre $post"]
		} {
			while { [notempty message] } {
				if { [len $message] <= $max_length } {
					set break [len $message]
					set skip 0
				} else {
					set break [instrrev $message " " $max_length]
					if !($break) { set break $max_length ; set skip 0 } { decr break ; set skip 1 ; # We're still "on" the space! }
				}
				set line [left $message $break]
				if [notempty pre] { prepend line "$pre " }
				if [notempty post] { append line " $post" }
				if [notempty open] { prepend line [color $open] }
				append line \017
				lappend buffer $line
				incr break $skip
				set message [mid $message [incr break]]
			}
		}
	}

if $debug { debug nick handle chan flags output queue type speedflag }
	foreach line $buffer {
		if [validflag -strip] { regsub -all -nocase -- {\003([0-9][0-9]?(,[0-9][0-9]?)?)?|[\000\002-\010\013-\014\016-\037]+} $line "" line }
		if [isempty line] { set line [space] }
		if [notempty remote] { putbot $remote [list SB7:REMOTEIDX $target $line] ; continue }
		if [isdccnick $target] { putdcc $target $line ; continue }

		set buffer_line "$type $output :$line"
		set buffer_full [expr ( [string length "$type $output :$line"] >= $max_length ) ? 1 : 0]
		set buffer_spacer "$type $output :"
		if $burst_ok { rawprint $buffer_line ; if $buffer_full { rawprint $buffer_spacer } ; continue }
		set error [ catch { 
			$queue $buffer_line -$speedflag 
			if $buffer_full { $queue $buffer_spacer -$speedflag }
		} ]
		if $error { $queue $buffer_line ; if $buffer_full { $queue $buffer_spacer } }
	}
	if [validflag -return] { return -code return $message }
	return $message
}

proc format:date args {
	flags:simple $args [list -gmt -cliptz] text flags
	lassign $text time handle
	set default %c
#	set default "%a %b %d %H:%M:%S %Y %z" ; # ISO compliant (not to ISO-8601!)
	set default "%Y-%m-%d %H:%M:%S %z"
	set offset 0

	if [string eq * $handle] {
		set format "%Y-%m-%d %H:%M:%S %z"
	} elseif [string eq -nocase @SHORT $handle] {
		set format "%Y-%m-%d %H:%M"
	} elseif [string eq -nocase @DATE $handle] {
		set format "%Y-%m-%d"
	} elseif [string eq -nocase @TIME $handle] {
		set format "%H:%M"
	} elseif [validuser $handle] {
		set format [userinfo get $handle DATETIME]
		set gmt [userinfo get $handle GMT]
		set tz [userinfo get $handle timezone]
		if [notempty gmt] { set offset [gmt:format decimal $gmt] }
		if [isempty format] { set format $default } 
		if [validflag -cliptz] {
			#regsub -all -- %z|%Z $format " " format
			regsub -all -- {[ ]?[<\(\[\{ ]?%[zZ][ \}\]\)>]?[ ]?} $format " " format
			set format [singlespace $format]
		} {
			if [notempty gmt] {
				set offset [gmt:format decimal $gmt]
				regsub -all -- %z $format [gmt:format integer $gmt] format
				regsub -all -- %Z $format $tz format
			} {
				set offset 0 ; # Forcibly force "unknown timezones" to GMT
				regsub -all -- %z $format +0000 format
				regsub -all -- %Z $format GMT format
			}
		}
		set bot [gmt:format decimal [get bot:gmt]]
		if ![validflag -gmt] { set time [expr $time + ( ( $offset - $bot ) * 3600 )] }
	} {
		set format $default
	}

	# Separately because of unknown user / default mode
	if [validflag -cliptz] {
		#regsub -all -- %z|%Z $format " " format
		regsub -all -- {[ ]?[<\(\[\{ ]?%[zZ][ \}\]\)>]?[ ]?} $format " " format
		set format [trim [singlespace $format]]
	}
	if [instr $format %Q] {# Stardate corrections
		# Version 2
		# Earliest date: -2147483648 / 1901-12-13 20:45:52 +0000
		# Start of EGGDROP's Stardate: -757353600 / 1946-01-01 00:00:00 +0000
		#set diff [datediff "1/1/1946 00:00:00" [clock seconds] s]
		#set offset [expr ${diff}.0 / 31536]

		# Version 1
		set Q [clock format $time -format %Q]
		if [regexp -- {\d\d1000\.} $Q] { regsub 1000 $Q 999 Q }
		if [regexp -- {\d\d002\.} $Q] { regsub 002 $Q 001 Q }
		regsub %Q $format $Q format
	}
	if [instr $format %q] {# Commonwealth years
		set q "CY [expr 4926 + [clock format $time -format %Y]]"
		regsub %q $format $q format
	}
	set gmtflag [boolean -truefalse [validflag -gmt]]
	if { [validflag -gmt] && [validflag -cliptz] } { append format " +0000" }
	clock format $time -format $format -gmt $gmtflag
}

proc format:percent args {
	flags:simple $args [list -leading_zero -trim] text flags
	lassign [concat $text 1] value decimal
	format %[iff [validflag -leading_zero] 0][expr ( 3 - ( [validflag -trim] * 2 ) ) + ( ( $decimal > 0 ) ? $decimal + 1 : 0 ) ].${decimal}f $value
}

proc using args {
	if [isempty args] return
	lassign $args format text extra
	if [isempty text] return
	#if [isempty format] { error "\[USING\] No format control string provided" }
	# Nah. If no format field, presume some kind of simple conversion: kick out result

	array set mode [list < justify:left > justify:right = justify:center @ justify:full # field . decimal J join B bytes _ convertwhitespace N nbsp R trim P paragraph L case:lower U case:upper S case:sentence T case:title + sign:positive - sign:negative , comma {$} sign:currency Z zero I integer M mantissa ^ scinot X noscinot % sign:percent]
	array set control [list]
	foreach { a b } [array get mode] { zero control($a) control($b) }
	empty control(field) 

	# +/-/$ = float it with number; ++/--/$$ = stick at far left end of formatting field
	while { [notempty format] } {
		set 1 [left $format 1]
		if [string eq " " $1] { set 1 # }
		switch -exact -- [string toupper $1] {

			< { one control(<) ; zero control(>) control(=) control(@) }
			> { one control(>) ; zero control(<) control(=) control(@) }
			= { one control(=) ; zero control(<) control(>) control(@) }
			@ { one control(@) ; zero control(<) control(>) control(=) }
			# { append control(field) # }
			. { if [instr $control(field) .] { #error "\[USING\] Wrong number of decimal points" } ; append control(field) . }
			B { error "\[USING\] B (bytes) option has been deprecated. Use BYTES command (\[bytes <value>\]) on data before sending to USING." }
			P { error "\[USING\] P (auto-paragraph breaks) option has been deprecated." }
			_ { one control(convertwhitespace) }
			N { one control(nbsp) }
			R { one control(trim) }
			`P { one control(paragraph) }
			L { one control(case:lower) ; zero control(case:upper) control(case:sentence) control(case:title) }
			U { one control(case:upper) ; zero control(case:lower) control(case:sentence) control(case:title) }
			S { one control(case:sentence) ; zero control(case:lower) control(case:upper) control(case:title) }
			T { one control(case:title) ; zero control(case:lower) control(case:upper) control(case:sentence) }
			J { one control(join) }
			, { one control(comma) }
			Z { one control(zero) }
			I { one control(integer) ; zero control(mantissa) }
			M { one control(mantissa) ; zero control(integer) }
			^ { one control(scinot) ; zero control(noscinot) }
			X { one control(noscinot) ; zero control(scinot) }

			+ { incr control(sign:positive) ; zero control(sign:negative) }
			- { incr control(sign:negative) ; zero control(sign:positive) }
			$ { incr control(sign:currency) ; zero control(sign:percent) }
			% { incr control(sign:percent) ; zero control(sign:currency) }

			* {
				set format [mid $format 2] 
				set l [regexp -inline -- {^\d+} $format] 
				if { $l < 1 } { error "\[USING\] Missing parameter for \"*\" format string length option" }
				append control(field) [string repeat # $l]
				set i [len $l] 
				set format [mid $format $i] ; # Loop will force move to next char
			}

			/ {
				set format [mid $format 2] 
				set l [regexp -inline -- {^\d+} $format] 
				if { $l < 1 } { error "\[USING\] Missing parameter for \"*\" format string length option" }
				append control(field) .[string repeat # $l]
				set i [len $l] 
				set format [mid $format $i] ; # Loop will force move to next char
			}

			default { error "\[USING\] Unknown format specifier: \"[string toupper $1]\"" }

		}
		set format [mid $format 2]
	}

	debug control

	if [isnum -real $text] {
		set text [normalize $text]
		if { !$control(sign:positive) && !$control(sign:negative) } { one control(sign:negative) }

		# Do all numeric formatting first, then treat as text later
		if $control(integer) { int text }
		if $control(mantissa) { mant text }
		if $control(^) { if ![isscinot $text] { set text [scinot $text] } }
		if $control(X) { if [isscinot $text] { set text [noscinot $text] } }
		if $control(comma) { set text [comma $text] }

		empty prep1 prep2
		if $control(sign:positive) {
			set sign [lindex [list - "" +] [expr [sgn $text] + 1]]
			if { $control(sign:positive) > 1 } {
				set prep1 $sign
				set control(field) [mid $control(field) 2]
			} {
				prepend text $sign
			}
		}
		if $control(sign:negative) {
			set sign [lindex [list -] [expr [sgn $text] + 1]]
			if { $control(sign:positive) > 1 } {
				set prep1 $sign
				set control(field) [mid $control(field) [expr [len $sign] + 1]]
			} {
				prepend text $sign
			}
		}
		if $control(sign:currency) {
			if { $control(sign:currency) > 1 } {
				append prep1 $
				set control(field) [mid $control(field) 2]
			} {
				prepend text $
			}
		}
		if $control(sign:percent) {
debug =1 text
			set text [expr $text * 100]
debug =2 text
			if { $control(sign:currency) > 1 } {
				set prep2 %
				set control(field) [mid $control(field) 2]
			} {
				append text %
			}
		}
		
		if $control(zero) {
			regsub -all -- # $control(field) 0 control(field)
		} {
			regsub -all -- # $control(field) " " control(field)
		}

		# Deal with decimal point (ONLY if necessary)
		if [instr $control(field) .] {
			empty temp
			set s_f [split $control(field) .]
			set s_t [split $text .]
			foreach a $s_f b $s_t { # Forces full length of $S_F to be looped-through
				lappend temp [left $b$control(field) [len $a]]
			}
			set text [join $temp .]
		}

		set control(field) ${prep1}${control(field)}${prep2}

	}
debug text control
	# Do regular text formatting now

	# Force text into format (or allow simple conversions and fall-through)
	if [isempty control(field)] {
		set using $text
	} {
		if $control(<) { set using [left ${text}${control(field)} [len $control(field)]] }
		if $control(>) { set using [right ${control(field)}${text} [len $control(field)]] }
		if $control(=) {
			set space [expr int( ( ( [len $control(field)] - [len $text]) / 2 ) + 0.5 ) ]
			set using [left $control(field) $space]${text}[right $control(field) [expr [len $control(field)] - $space]]
		}
		if $control(@) {
		}
	}
	return $using
}

proc _using { format text { extra "" } } {
	set mode 0 ; # STRING
	if [regexp -nocase -- {[%BITZNR0123456789\.\+\-]} $format] { one mode }
	if $mode {
		# Numeric processing
		if ![isnum -real $text] { error "\[USING\] Illegal value (non-numeric): $text" }
		array set format [list underline 0 comma 0 sign 0 currency 0 bytes 0 pad 0 integer 0 output 0 whitespace 0 + 0 - 0 trim 0]
		foreach a [explode -upper $format] {
			switch -exact -- $a {

				% #
				B { one format(bytes) }
				I { one format(integer) }
				T { one format(trim) }
				Z { one format(pad) }
				N { one format(underline) }
				# - . { append format(output) $a }
				+ { one format(sign) }
				- { negone format(sign) }

				default { error "\[USING\] Unknown numeric format marker: $a" }

			}
		}

		# Align decimal
		set text [normalize $text]
		lassign [split $text .] int mant
		lassign [split $format(output) .] f_int f_mant
		if { ( [len $int] > [len $f_int] ) || ( [len $text] > [len $format(output)] ) } { return [string repeat * [len $format(output)]] }
		set t_int [right $int [len $f_int]]
		set t_mant [left $mant [len $f_mant]]
		if $format(pad)
	} {
		# String processing
	}
	return $using
}

# --- Internet commands ---

proc www { url { query "" } } {
	set http_package_error [ catch { package require http } ]
	if $http_package_error { error "\[WWW\] HTTP package is unavailable" }
	set code 0
	set error [catch {
		::http::config -useragent "Mozilla/4.76 (Windows NT 5.1; U) Opera 12.12 \[en\]"; # Google hates the default of "Tcl http client package 2.4."
		# Allow it 10 seconds to connect
		if [isempty query] {
			set token [::http::geturl $url -timeout 10000 -headers {Accept-Language: en-us,en;q=0.5}]
		} {
			empty q
			foreach {a b} $query { if [notempty q] { append q & } ; append q [::http::formatQuery $a]=[::http::formatQuery $b] }
			if ![isnum -even [llength $query]] { error "\[WWW\] Query (POST) variables must be in even pairs: name-then-definition (failure in: $query)" }
			set token [::http::geturl $url -query $q -timeout 10000 -headers {Accept-Language: en-us,en;q=0.5} ]
		}
		set www [::http::data $token]
		array set temp [array get $token]
		array set temp $temp(meta); # Pull all META data into variables
		set code [lindex $temp(http) 1]
		::http::cleanup $token
	} ohshit]
	if $error { set www "I'm unable to get the information from $url at this time. Please try again later. (Error: $ohshit)" }
#debug error www code temp
	return [list $error $www $code [array get temp]]
}

proc www:headers url {
	set http_package_error [ catch { package require http } ]
	if $http_package_error { error "\[WWW:HEADERS\] HTTP package is unavailable" }
	set token [::http::geturl $url]
	array set temp [array get $token]
	array set temp $temp(meta); # Pull all META data into variables
	::http::cleanup $token
	return [array get temp]
}

proc pingpong { { time "" } } {
	# IRCd
	if [string eq "" $time] { set time [clock seconds] }
	raw "PING :$time"

	# Botnet
	foreach data [dcclist bot] { putidx [lindex $data 0] pi }
	return
}

# --- Shell commands ---

proc shell:findcmd cmd {
	# 2015-02-23 19:27:00 -0800: WHICH should work just fine; CygWin returns "/blah/cmd.exe" for WHEREIS
	set which [shell:which $cmd]
	if [notempty which] { return [lindex $which 0] }
	set whereis [shell:whereis $cmd]
	if [notempty whereis] { return [lindex $whereis 0] }
	return
}

proc shell:which cmd { 
	set error [ catch { set which [lindex [exec which [string tolower $cmd]] 0] } null ]
	if $error return
	lremove which *.gz *.man *.1
	return $which
}

proc shell:whereis cmd { 
	set error [ catch { set whereis [exec whereis $cmd] } null ]
	if $error return
	if [string eq ${cmd}: [lindex $whereis 0]] {set whereis [lreplace $whereis 0 0]}
	lremove whereis *.gz *.man *.1
	lindex $whereis 0
}

# --- Other commands ---

proc hexdump { text { size 16 } } {
	set max [string length [format %X [string length $text]]]
	set length [expr $size * 2] ; # Double it due to HEX
	set hexdump ""
	binary scan $text H* hex 
	set count 0
	while { ![string eq "" $hex] } {
		set line [string range $hex 0 [expr $length - 1]]
		set hex [string range $hex $length end]
		empty chars
		set temp "\[[format %0${max}X $count]\] "
		for { set x 0 ; set y 1 } { $x < $length } { incr x 2 ; incr y 2 } {
			set char [string index $line $x][string index $line $y]
			if [isempty char] { append temp "-- " ; append chars " " ; continue }
			append temp "[string toupper $char] " ; # Space!
			regsub -- {^[\000-\037\200-\377]$} [binary format H* $char] . char
			append chars $char
		}
		append temp " | " $chars
		lappend hexdump $temp
		incr count $size
	}
	return [join $hexdump \n]
}

proc path:relative { path { root . } { use_home false } } {
	set path [file normalize $path] ; # /a/b/c/d/e.txt
	set root [file normalize $root] ; # /a/b/c
	if [string eq [file dirname $path] $root] { return ./[file tail $path] }
#debug path root home
	if $use_home {
		set home [file normalize ~/] ; # /a/b
		if [left $path [len $home] $home] { return ~[string range $path [string length $home] end] }
	}

	if [left $path [len $root] $root] { return .[string range $path [string length $root] end] }

	# Back-step?
	# 1: /home/user/dir1/dir2/file.txt
	# 2: /home/user/dir1/dir3/dir4
	set s1 [file split $path]
	set s2 [file split $root]
	set count 0
	while 1 {
		if { ( $count > [llength $s1] ) || ( $count > [llength $s2] ) } { incr count 1 ; break }
		if ![string eq [lrange $s1 0 $count] [lrange $s2 0 $count]] { incr count -1 ; break }
		incr count
	}
	# Nothing in common ... dammit ....
	# FILE SPLIT uses "/" as the first LIST element, so if *only* that matches, don't calculate a bunch of "../"s for it; just return the path
#	if { $count < 1 } { return $path } 
#debug =S2([lrange $s2 0 $count]) =S1([lrange $s1 [expr $count + 1] end])
	set return [string repeat ../ [expr [llength $s2] - $count - 1]][join [lrange $s1 [expr $count + 1] end] /]
#debug return
	return $return
}

proc random args {
	flags:simple $args [list -all -nospaces -lower -upper -explode -implode -reverse] text flags
	set text [join $text]
	if [validflag -nospaces] { set text [lsearch -all -inline -not $text " "] }
	if [validflag -implode] { set text [join $text ""] }
	if [validflag -explode] { set text [split $text ""] }
	if [validflag -lower] { set text [string tolower $text] }
	if [validflag -upper] { set text [string toupper $text] }
	if [validflag -reverse] { set text [reverse $text] }
	empty random
	while { [notempty text] } {
		set index [rand [llength $text]]
		lappend random [lindex $text $index]
		set text [lreplace $text $index $index]
		if ![validflag -all] break
	}
	return $random
}

proc stack:trace args {
	if [isnum -integer $args] { set deep $args } { set deep [info level] }
	empty o
	for { set x $deep } { $x > 0 } { incr x -1 } { lappend o "\[STACK:TRACE - level #${x}\] [info level $x]" }
	return $o
}

proc os { { checkme "" } } {
	if [string eq -nocase VERSION $checkme] {
		set os $::tcl_platform(os)
		switch -glob -- [string tolower $os] {
			cygwin_nt-* {return [array set temp [list 1.0 "Windows 1.0" 2.0 "Windows 2.0" 3.0 "Windows 3.0" 3.1 "Windows 3.1" 3.11 "Windows 3.11" 4.0 "Windows NT4" 5.0 "Windows 2000" 5.1 "Windows XP" 6.0 "Windows Vista" 7.0 "Windows 7" 8.0 "Windows 8" 8.1 "Windows 8.1"]]$temp([right $os 3])}
			cygwin*     {return [string trimleft [mid $os 7] _]}
			windows*    {return $os}
			default     {return $os}
		}
	}

	if [regexp -nocase -- {^[\*]?n[i\?]x$} $checkme] { set checkme *n?x }

	set platform $::tcl_platform(platform)
	if [string eq -nocase UNIX $platform] {
		switch -glob -- [string tolower $::tcl_platform(os)] {
			unix - linux - *bsd { set os unix }
			windows { set os windows }
			cygwin* { set os cygwin }
			mac* - darwin* { set os mac }
			default { error "\[SB:os\] what OS am I? platform ($::tcl_platform(platform)), os ($::tcl_platform(os))" }
		}
	} {
		set os unix
	}
	if [isempty checkme] { return $os }

	# Specific OS check
	if ![is wildcard $checkme] { append checkme * }
	string match -nocase $checkme $os
}

proc rglob {dirpath patterns { recursive 0 } { sizes 0 } } {
	# Custom-written by thommey [1347803875] :: http://paste.tclhelp.net/?id=bn7

	set rlist {}
	if [isempty patterns] { set patterns * }
	catch { foreach pattern $patterns { set rlist [concat $rlist [glob -nocomplain -types f -directory ${dirpath} -- $pattern]] } }
	catch { foreach pattern $patterns { set rlist [concat $rlist [glob -nocomplain -types [list f hidden] -directory ${dirpath} -- $pattern]] } }

	if !$recursive {
		if !$sizes { return [lsort -inc $rlist] }
		float total
		foreach a $rlist { add total [file size $a] }
		return [list [lsort -inc $rlist] $total]
	}

	# Note the recursive call to RGLOB
	# Recurse into directories
	if [notempty dirpath] {
		set dirs [concat [glob -nocomplain -types d -directory ${dirpath} -- *] [glob -nocomplain -types [list d hidden] -directory ${dirpath} -- *]]
		lremove dirs . .. */. */..
		foreach dir $dirs { catch { set rlist [concat $rlist [rglob ${dir} ${patterns} $recursive]] } }
	}

	if $sizes {
		float total; # MUST BE FLOATING-POINT (stupid 32-bit issues)
		zero c
		set ll [llength $rlist]
		foreach file $rlist {
			incr c 
			if ![file isdirectory $file] { catch { add total [file size $file] } }
		}
		return [list $rlist $total [array get size_list]]
	}
	return ${rlist}
}

proc dglob { { dirpath . } { recursive true } } {
	# Custom-written by thommey [1347803875] :: http://paste.tclhelp.net/?id=bn7
	# Modified by ... me ....

	set dlist {}
	catch { set dlist [concat $dlist [glob -nocomplain -types d -directory ${dirpath} -- *]] }
	catch { set dlist [concat $dlist [glob -nocomplain -types {d hidden} -directory ${dirpath} -- *]] }
	lremove dlist . .. */. */..
	if !($recursive) { return $dlist }

	# Note the recursive call to DGLOB
	# Recurse into directories
	if $recursive {
		set dirs [concat [glob -nocomplain -types d -directory ${dirpath} -- *] [glob -nocomplain -types [list d hidden] -directory ${dirpath} -- *]]
		lremove dirs . .. */. */..
		foreach dir $dirs { catch { set dlist [concat $dlist [dglob ${dir} $recursive]] } }
	}
	return ${dlist}
}

proc datediff { date_from { date_to "" } { time_granularity * } } {
	if [isnum -integer $date_from] { set scan1 $date_from } { set scan1 [clock scan $date_from] }
	if [isempty date_to] { set scan2 [clock seconds] } { if [isnum -integer $date_to] { set scan2 $date_to } { set scan2 [clock scan $date_to] } }
	
	set diff [expr $scan1 - $scan2]

	if [string eq * $time_granularity] {
		set sign [sgn $diff] ; set diff [string trimleft $diff -]
		empty datediff
		set list [list l .001 n .01 t .1 s 1 m 60 h 3600 d 86400 w 604800 y 31536000 e 315360000 c 315360000]
		foreach { multiple marker } [lreverse $list] {
			if { $diff >= $multiple } {
				set int [expr $diff / $multiple] ; # Integer math
				lappend datediff ${int}${marker}
				set diff [expr $diff - ( $int * $multiple ) ]
			}
		}
		return [sgn $sign neg]$datediff
	}

	switch -glob -- [string tolower $time_granularity] {
		l* { set div 0.001 }
		n* { set div 0.01 }
		t* { set div 0.1 }
		s* { set div 1 }
		m*  { set div 60 }
		h*  { set div 3600 }
		d*  { set div 86400 }
		w*  { set div 604800 }
		y*  { set div 31536000 }
		e - decade* { set div 315360000 }
		c - centur* { set div 3153600000 }
	}
	normalize [expr ${diff}.0 / $div]
}

proc not value { isfalse $value }

proc integer value { int value ; return $value }

proc mantissa value { mant value ; return $value }

proc maskformat args {
	# "Numeric mode" will override flags-based formatting (this allows me to
	# defer to Eggdrop1.8 options by default)

	flags:simple $args [list -full -static -domain -nick] text flags
	lassign $text mask mode
	if [isempty mask] return
	if ![instr $mask !] { prepend mask *! } 

	# Numeric mode first
	if [isnum -integer $mode] {
		if ![is within $mode 0 29] { return *!*@* } ; # Trigger internal response
		set error [catch { set mask [maskhost $mask $mode] } ]
		if $error {
			# Custom routine (emulating Eggdrop 1.8 syntax)
		
			set ok [regexp -nocase -- {^([^\!]+)\!([^@]+)@(.+)$} $mask - tn ti td]
			set split [split $td .]
			set llength [llength $split]
			if { $llength < 3 } {
				set td_wc $td
			} elseif { $llength == 3 } {
				set td_wc [join [concat * [lrange $split end-1 end]] .]
			} else {# 4 or more elements
				set td_wc [join [concat * [lrange $split end-2 end]] .]
			}

			# Note: tests prove the $IDENT length is =NOT= truncated to 10
			switch -exact -- $mode {
				0 - 5 { set mask ${ti}@$td }
				1 - 6 { set mask *${ti}@$td }
				2 - 7 { set mask *@$td }
				3 - 8 { set mask *${ti}@$td_wc }
				4 - 9 { set mask *@$td_wc }
				10 - 15 - 20 - 25 { set mask ${ti}@[regsub -all -- {[0-9]} $td ?]}
				11 - 16 - 21 - 26 { set mask *${ti}@[regsub -all -- {[0-9]} $td ?] }
				12 - 17 - 22 - 27 { set mask *@[regsub -all -- {[0-9]} $td ?] }
				13 - 18 - 23 - 28 { set mask *${ti}@[regsub -all -- {[0-9]} $td ?] }
				14 - 19 - 24 - 29 { set mask *@[regsub -all -- {[0-9]} $td ?] }
			}
			if [is within $mode 20 29] { regsub -all -- {\?} $mask * mask }
			if [is within [right $mode 1] 5 9] { prepend mask ${tn}! } { prepend mask *! }
		}
		return $mask
	}

	# Next: flags mode
	if [isempty flags] { return [maskhost $mask] }

	# Process flags
	lassign [split *.*.* .] nick ident domain
	set ok [regexp -nocase -- {^([^\!]+)\!([^@]+)@(.+)$} $mask - tn ti td]
	if !$ok { error "\[MASKFORMAT\] Unknown formatted mask: $mask (must be in *!*@* format)" }

	if [validflag -full] { lappend flags -nick -static }

	# Do this first: -STATIC needs to be able to override it
	if [validflag -domain] { set domain $td ; set ident * } { set ident $ti }
	if ![validflag -nick] { set nick * } { set nick $tn }
	if [validflag -static] { set ident $ti ; set domain $td }

	if ![validflag -static -nickmask -domain] {
		set split [split $td .]
		set llength [llength $split]
		if {$llength < 3} {
			set domain $td
		} elseif {$llength == 3} {
			set domain [join [concat * [lrange $split end-1 end]] .]
		} else {# 4 or more elements
			set domain [join [concat * [lrange $split end-2 end]] .]
		}
	}
	set maskformat ${nick}!${ident}@${domain}
	return $maskformat
}


# Move to within IS?
proc inrange { range value { floor -1 } { ceiling 9999 } } {
	set a [getrange $range -2147483648 2147483647]
	if [string eq $a -1] {
		lassign $a 0 1
		if { $0 < $floor } { set floor $0 } 
		if { $1 < $ceiling } { set ceiling $1 } 
	}
	set g [getrange $range $floor $ceiling]
	if [isempty g] { return 0 }
	lassign $g gr0 gr1
	expr ( ( $gr0 <= $value ) && ( $value <= $gr1 ) ) ? 1 : 0
}

# Move to within GET?
proc getrange { range { floor -1 } { ceiling 1001 } } {
	switch -regexp -- $range {
		{^[\=]?[\-]?\d+$} { set range [string trimleft $range =] ; set 0 $range ; set 1 $range }
		{^\-(\-1|\d+)$} { set 0 $floor ; set 1 [mid $range 2] }
		{^(\-1|\d+)[+\-]$} { set 0 [left $range -1] ; set 1 $ceiling }
		{^(\-1|\d+)\+\d+$} { lassign [split $range +] 0 1 ; incr 1 $0 }
		{^<(\-1|\d+)$} { set 0 $floor ; set 1 [expr [mid $range 2] - 1] }
		{^(<=|=<)(\-1|\d+)$} { set 0 $floor ; set 1 [mid $range 3] }
		{^>(\-1|\d+)$} { set 0 [expr [mid $range 2] + 1] ; set 1 $ceiling }
		{^(>=|=>)(\-1|\d+)$} { set 0 [mid $range 3] ; set 1 $ceiling }
		{^(\-1|\d+)\-(\-1|\d+)$} {
			set s [split $range -]
			set l [llength $s]
			switch -exact -- $l {
				2 {lassign $s 0 1}
				3 { if [left $range 1 -] { set 0 -[lindex $s 1] ; set 1 [lindex $s 2] } { set 0 [lindex $s 0] ; set 1 -[lindex $s 2] } }
				4 { set 0 -[lindex $s 1] ; set 1 -[lindex $s 3] }
			}
		}
		default { return "" }
	}
	if { $0 > $1 } { swap 0 1 }
	if { ( $0 < $floor ) && ( $1 < $floor ) } return
	if { ( $0 > $ceiling ) && ( $1 > $ceiling ) } return
	if { $0 < $floor } { set 0 $floor } { if { $0 > $ceiling } { set 0 $ceiling } }
	if { $1 < $floor } { set 1 $floor } { if { $1 > $ceiling } { set 1 $ceiling } }
	return [list $0 $1]
}

proc longip { a } {
	if [instr $a .] {
		if {[llength [split $a .]] != 4} { return -1 }
		lassign [split $a .] a b c d
		return [format %u [expr { ( $a << 24 ) + ( $b << 16 ) + ( $c << 8 ) + $d } ]]
	}    
	return [ expr { $a >> 24 & 255 } ].[ expr { $a >> 16 & 255 } ].[ expr { $a >> 8 & 255 } ].[ expr { $a & 255 } ]
	# Credit: LionPing at egglist@undernet.nu from EggHead list 2003-03-14
}

proc convert { number { from 10 } { to 10 } } {
	# OK to use NORMALIZE (as long as there's no flag-based base convertion)
	set string "0123456789abcdefghijklmnopqrstuvwxyz"
	#Not yet! - if ![isnum -integer $from] { error "\[CONVERT\] Illegal base: $from" }
	if ![isnum -integer $to] { error "\[CONVERT\] Illegal base: $to" }
	if { $number == 0 } { return 0 }
	set number [string tolower $number]

	if [regexp -nocase -- {^[ivxlcdm]+$} $number] { set from 10; set number [unroman $number] }
	set number [string trimleft $number \\]

	switch -glob -- $number {
		#$* - 0x* - 0u* { set from 16 ; set number [mid $number 3] }
		$* - x* - u* { set from 16 ; set number [mid $number 2] }
		#* { set from 10 ; set number [mid $number 2] }
		&* { set from 8 ; set number [mid $number 2] }
		%* { set from 2 ; set number [mid $number 2] }
		0b* { set from 2 ; set number [mid $number 3] }
		0o* { set from 8 ; set number [mid $number 3] }
		0r* { set from 10; set number [unroman [mid $number 3]] }
		0* { set from 8 ; set number [mid $number 2] }
	}
	if ![isnum -integer $from] { error "\[CONVERT\] Illegal base: $from" }
#debug =1 $number from to
	# Middle-man: convert to decimal first ....
	if { $from == 10 } {
		set total $number
	} {
		set e [expr [len [lindex [split $number .] 0]] - 1]
		set total 0.0 ; # Needs to be a FLOAT
	
		foreach a [split $number ""] {
			if [string eq . $a] continue
			set v [expr [instr $string $a] - 1]
			if { $v >= $from } { error "\[CONVERT\] Illegal digit for base ${from}: $a" }
			set total [expr $total + ( $v * pow( $from , $e ) )]
			decr e
		}
	}
#debug =2 $number from to

	# Are we there yet?
	if { $to == 10 } { return [normalize $total] }

	set h [= int( log($total) / log($to) ) + 1] ; # Will overshoot sometimes
	empty new
	for { set exp $h } { ( $total > 0 ) || ( $exp >= 0 ) } { decr exp } {
		if { $exp == -1 } { append new . }
		set byte [ expr pow( $to , $exp ) ]
		set factor [ expr int( $total / $byte ) ]
		set char [string index $string $factor]

		append new [string toupper $char]
		set total [ expr $total - ( $factor * $byte ) ]
#debug new total byte factor char exp
	}

	# Why are we avoiding NORMALIZE?
	## Because it re-converts the number back to decimal!
	set new [string trimleft $new 0]
	if [left $new 1 .] { prepend new 0 }
	if [instr $new .] { set new [string trimright $new 0] }
	if [right $new 1 .] { set new [left $new -1] ; # append new 0 }
	return $new
}

proc hex number { convert $number 10 16 }
proc dec number { convert $number 10 10 }
proc oct number { convert $number 10  8 }
proc bin number { convert $number 10  2 }
proc rom number { roman $number }

proc roman value {
	if ![isnum -integer $value] { error "\[ROMAN\] Illegal value: ROMAN can only convert 1 - 4999 (integers only)" }
	if ![is within $value 1 4999] { error "\[ROMAN\] Illegal value: ROMAN can only convert 1 - 4999 (integers only)" }
	array set list [list 4,1 m 3,9 cm 3,5 d 3,4 cd 3,1 c 2,9 xc 2,5 l 2,4 xl 2,1 x 1,9 ix 1,5 v 1,4 iv 1,1 i]
	empty roman
	zero tens
	foreach digit [explode -reverse -lower $value] {
		incr tens
		if [info exists list(${tens},$digit)] {
			prepend roman $list(${tens},$digit)
		} {
			empty packet
			if { $digit >= 5 } {
				set packet $list(${tens},5)
				decr digit 5
				# Offset by 5 ... & fall-through ....
			}
			append packet [string repeat $list(${tens},1) $digit]
			prepend roman $packet
		}
	}
	string toupper $roman     
}

proc unroman value {
	if ![regexp -nocase -- {^[mdclxvi]+$} $value] { return 0 }
	set value [string tolower $value]
	set list [list cm 900 cd 400 xc 90 xl 40 ix 9 iv 4 m 1000 d 500 c 100 l 50 x 10 v 5 i 1] 
	foreach { a b } $list { regsub -all -nocase -- $a $value " $b " value } 
	expr [join $value +]
}

proc caseall    data { return [casealias [casebot [casehandle [casenick $data]]]] }
proc casenick   data { empty u ; foreach c [channels] { set u [concat $u [chanlist $c]] } ; set casenick [lindex $u [lsearch -exact [string tolower $u] [string tolower $data]]] ; if [isempty casenick] { set casenick $data } ; return $casenick }
proc casehandle data { if [validuser $data] { return [getuser $data handle] } { return $data } }
proc casechan   data { set c [channels] ; set casechan [lindex $c [lsearch -exact [string tolower $c] [string tolower $data]]] ; if [isempty casechan] { set casechan $data } ; return $casechan }
proc casebot    data { set b [bots] ; set casebot [lindex $b [lsearch -exact [string tolower $b] [string tolower $data]]] ; if [isempty casebot] { set casebot $data } ; return $casebot }
proc casealias  data { foreach a [userlist -b] { set alias [userinfo get $a alias] ; set m [lsearch -exact [string tolower $alias] [string tolower $data]] ; if { $m != -1 } { return [lindex $alias $m] } ; } ; return $data }

proc get { cmd args } {
	set args_ $args
	set args [join $args]
	switch -exact -- [string tolower $cmd] {

		bot:gmt { return [clock format [clock seconds] -format %z] }

		bot:tz { return [clock format [clock seconds] -format %Z] }

		long - longest { return [string length [lindex [lsort -dec -command lsort:len $args] 0]] }

		short - shortest { return [string length [lindex [lsort -inc -command lsort:len $args] 0]] }

		long:word - longest:word { return [lindex [lsort -dec -command lsort:len $args] 0] }

		short:word - shortest:word { return [lindex [lsort -inc -command lsort:len $args] 0] }

		highest { return [lindex [lsort -dec -unique -real $args] 0] }

		lowest { return [lindex [lsort -inc -unique -real $args] 0] }

		first { empty get ; foreach arg $args { lappend get [lindex $arg 0] } ; return $get }

		second { empty get ; foreach arg $args { lappend get [lindex $arg 1] } ; return $get }

		third { empty get ; foreach arg $args { lappend get [lindex $arg 2] } ; return $get }

		last { empty get ; foreach arg $args { lappend get [lindex $arg end] } ; return $get }

		owner - owners { if [isempty ::owner] { return "" } { return [regsub -all -- {,|[ ]{2,}} $::owner " "] } }

		permowner - permowners { set o $::owner ; if [isempty o] return ; return [lindex [split $o ", "] 0] ; # Why only index #0? }

		op - ops - opsymbol {
			lassign $args nick chan
			empty ops
			if [ischanowner  $nick $chan] { append ops ~ }
			if [ischanadmin  $nick $chan] { append ops & }
			if [ischanop     $nick $chan] { append ops @ }
			if [ischanhalfop $nick $chan] { append ops % }
			if [ischanvoice  $nick $chan] { append ops + }
			return $ops
		}

		level:range - level {
			# Enter with: (1) numeric to get word, or (2) word to get numeric
			set 0 [lindex $args 0]
			set default [iff [isnum -integer $0] "" 0]
			foreach userlevel [data get USERLEVELS] {
				lassign $userlevel word level range flags name
				if [isnum -integer $0] { # Have numeric ; return word
					if { $0 == $level } { return $word }
					# Keep or move to LEVEL:RANGE?
					if [inrange $range $0 -1 1001] { return $word }
				} { # Have word ; return numeric / range
					if [string eq -nocase $0 $word] {
						if [string eq -nocase LEVEL:RANGE $cmd] { return $range } { return $level }
					}
				}
			}
			return $default
		}

		token - tokens {
			lassign $args token chan
			set u [data get userlevels]
			set token [list:tolower $token]
			if [left $token 1 \$] { set token [mid $token 2] }
			if [right $token 1 s] { set token [left $token -1] }
			if [string eq -nocase    PERMOWNER $token] { return [get permowner] }
			if [string eq -nocase BOTPERMOWNER $token] { return [get permowner] }
			foreach a [get first $u] b [get third $u] {
				if [string eq -nocase $a $token] {
					if [left $token 6 global] { return [userlist:level $b] } { return [userlist:level $b $chan] }                         
				}
			}
			error "\[GET TOKEN\] Unknown token: $token" 
		}

		access { return [access get [lindex $args 0] [lindex $args 2]] ; # Just a pipe to ACCESS }

		index - range {
			# Version 3
			# Special consideration: when processing a negative integer within a range that includes negative numbers,
			# use "=" in front of it ("=-25") to protect the integer. Otherwise, an ambiguity exists: negative 25, or,
			# range:$LO -> 25? If the range doesn't call for negative numbers, ... do we auto-compensate to the range
			# option, or, return the error condition (blank)? Nah, don't compensate; let the rule stand to allow for
			# SOME SORT of consistency.

			lassign $args_ list lo hi combine
			set combine [istrue $combine]
			if [isempty list] { error "List?" }
			if [isempty lo] { error "Lo?" }

			# Allow for just the "HI" to be specified
			if [isempty hi] { if [notempty lo] { set hi $lo ; set lo 1 } { error "Hi?" } }
			empty numbers
			for { set x $lo } { $x <= $hi } { incr x } { lappend numbers $x } ; # SEQUENCE fails on negative $LO
			set ll [llength $numbers]

			# Alter $LIST to force delimiters to become new LIST elements of their own accord
			if [regexp -- {^[\<\>\=]?[0-9 \+\-]+$} $list] {
				set list [split $list { ,:;}]
			} {
				if { ![regexp -- , $list] && ( [llength $list] > 2 ) } {
					error "If you're going to use any keywords, you must use delimiters like commas (,), colons (:), or semi-colons (\;)."
				}
				set list [split $list { ,:;}] ; # 1408864315: Was missing the space! "1-5, 11-15; 22 24" was splitting into "1-5 { 11-15} { 22 24}"
			}

			empty final
			lremove list ""
			for { set main_index 0 } { $main_index < [llength $list] } { incr main_index } {
				set element [string trim [string tolower [lindex $list $main_index]]]
				if [isempty element] continue
				switch -glob -- $element {

					all { set final $numbers }

					start* { if [string eq -nocase START $element] { lappend final [lindex $numbers 0] } { if [string match -nocase START+* $element] { set position [mid $element 7] ; lappend final [lindex $numbers $position] } } }

					end* { if [regexp -nocase -- {^END(\-\d+)?$} $element] { lappend final [lindex $numbers $element] } }

					first* { 
						if [string eq -nocase FIRST $element] {
							if { $main_index == ( [llength $list] - 1 ) } {
								lappend final [lindex $numbers 0]
							} {
								incr main_index
								set range [string trim [string tolower [lindex $list $main_index]]]
								set final [concat $final [lrange $numbers 0 [expr $range - 1]]]
							}
						} {
							regexp -nocase -- {FIRST[ \+\-]?(\d+)} $element - range
							if { $range > 0 } {
								for { set index 0 } { $index < $range } { incr index } { 
									lappend final [lindex $numbers $index] 
								} 
							} 
						} 
					}

					last* { 
						if [string eq -nocase LAST $element] {
							if { $main_index == ( [llength $list] - 1 ) } { 
								lappend final [lindex $numbers end] 
							} {
								incr main_index
								set range [string trim [string tolower [lindex $list $main_index]]]
								set final [concat $final [lrange $numbers end-[expr $range - 1] end]]
							}
						} {
							regexp -nocase -- {LAST[ \+\-]?(\d+)} $element - range
							if { $range > 0 } { 
								for { set index 1 } { $index <= $range } { incr index } { 
									lappend final [lindex $numbers [expr $ll - $index]] 
								} 
							}							
						} 
					}

					default {
						# Digit(s) , or , range of numbers
						# Overflow / underflow: error or just return blanks (ignore errors)?
						if ![regexp -nocase -- {^[<>]?[=]?[0-9 \+\-]+$} $element] return
						set loop_lo $lo
						set loop_hi $hi
						if [regexp -- {^[\=]?([\-]?\d+)$} $element - item] { set loop_lo $item ; set loop_hi $item } 
						if [regexp -- {^\-([\-]?\d+)$} $element - item] { set loop_hi $item }
						if [regexp -- {^([\-]?\d+)\-$} $element - item] { set loop_lo $item }
						if [regexp -- {^([\+\-]?\d+)\-([\+\-]?\d+)$} $element - item1 item2] { if { $item1 > $item2 } { error "Reversed range: $element" } ; set loop_lo $item1 ; set loop_hi $item2 }
						if [regexp -- {^([\-]?\d+)\+(\d+)$} $element - item1 item2] { set loop_lo $item1 ; set loop_hi [expr $item1 + $item2] }
						if [regexp -- {^<(\d+)$} $element - item1] { set loop_hi [expr $item1 - 1] }
						if [regexp -- {^<=(\d+)$} $element - item1] { set loop_hi $item1 }
						if [regexp -- {^>(\d+)$} $element - item1] { set loop_lo [expr $item1 + 1] }
						if [regexp -- {^>=(\d+)$} $element - item1] { set loop_lo $item1 }

						if { $loop_lo < $lo } { set loop_lo $lo }
						if { $loop_hi > $hi } { set loop_hi $hi }
						for { set index $loop_lo } { $index <= $loop_hi } { incr index } {  lappend final $index }
					}
				}
			}
			# Sanity check
			empty range
			foreach a $final { if { ( $a >= $lo ) && ( $a <= $hi ) } { lappend range $a } }
			set range [lsort -increasing -real -unique $range]
			if $combine { set range [process numlist $range] }
			return $range
		}

		date - time {
			# Syntax: GET TIME <text> [byref: var_time] [byref: var_remaining_text] --> returns $TIME
			flags -param -force $args_ [list -past 0 -negative 0 -future 0 -positive 0 -time 1] arg_ flags
			if $flags(-negative) { set flags(-past) 1 }
			if $flags(-positive) { set flags(-future) 1 }
			if [notempty flags(-time)] { set current_time $flags(-time) } { set current_time [clock seconds] }
			lassign $arg_ arg var_time var_text
#debug arg_ arg var_time var_text
			set arg [escape $arg]

			if [notempty var_time] { upvar 1 $var_time time }
			if [notempty var_text] { upvar 1 $var_text text }

			empty time
			set _ [lindex $arg 0]

			# Unix time stamp?
			if [isnum -integer $_] {
				set time $_
				set text [join [lreplace $arg 0 0]]
				return $time
			}

			set timeval [timeval $_ s]
			if [notempty timeval] {
				set sign -
				if $flags(-past) { set sign - ; set timeval [expr abs($timeval)] }
				if $flags(-future) { set sign + ; set timeval [expr abs($timeval)] }
				set time [expr $current_time $sign $timeval]
				set text [join [lreplace $arg 0 0]]
				return $time
			} 

			# Guess'timates show that a valid timestamp can consist of SIX elements!
			# today
			# yesterday yesterday yesterday yesterday yesterday 11:00 (5 days ago, at 11:00)
			# 2014-08-30 17:50:00
			# 08/30/2014 17:50:00 -0700
			# Saturday, August 30, 2014 17:50:00 -0700

#debug =time_check arg
			set ll [llength $arg]
			for { set count 0 } { $count < $ll } { incr count } {
#debug =LRANGE([lrange $arg 0 $count])
				set error [ catch { set time [clock scan [lrange $arg 0 $count]] } ]
#debug error time 
				if $error { # Return the last valid value
					if [isempty time] { set text [join $arg] } { set text [join [lreplace $arg 0 [expr $count - 1]]] }
					return $time
				}
			}
			if [notempty time] { return $time }

			# Fail (we shouldn't get here any more)
			set time ""
			set text [join $arg]
			return $time
		}

		numberlist - numlist {
			lassign $args_ data low high
			if [isempty low] { set low 1 }
			if [isempty high] { set high 1073741824 }
			regsub -all -- {,|\;} $data " " data
			empty list
			foreach item $data {
				if [regexp -- {^\d+\-\d+$} $item] {
					lassign [split $item -] 1 2
					if { $1 > $2 } continue ; # Illogical! ("25-5")
					for { } { $1 <= $2 } { incr 1 } { lappend list $1 }
				} elseif [regexp -- {^\-\d+$} $item] {
#debug =<-#>
					set item [string range $item 1 end]
					for { set 1 $low } { $1 <= $item } { incr 1 } { lappend list $1 }
#debug =</-#>
				} elseif [regexp -- {^\d+\-$} $item] {
#debug =<#->
					set item [string range $item 0 end-1]
					for { } { $item <= $high } { incr item } { lappend list $item }
#debug =</#->
				} elseif [isnum -integer $item] {
					# This must come AFTER the "-#" check or this
					# number would process as a negative number
#debug =<#>
					if { ( $item >= $low ) && ( $item <= $high ) } { lappend list $item }
#debug =</#>
				} {
					error "\[GET [string toupper $cmd]\] Unknown value: $item"
				}
			}
#debug list
			return [lsort -increasing -dictionary -integer $list]
		}

		decode {
			# Create encode from this code
			set text [lassign $args mode]
#debug mode text args
			switch -exact -- [string tolower $mode] {

				uri - url {
					set r [regexp -all -inline -nocase -- {%[0-9A-F]{2}} $text]
					foreach a $r { regsub -all -nocase -- $a $text [binary format H* [string range $a 1 end]] text }
					return $text
				}

				html {
					array set html [list lt < gt > amp {\&}]
					set r [regexp -all -inline -nocase -- {&[^\;]+\;} $text]
					foreach a $r {
						set base [string range $a 1 end-1]
						if [info exists html($base)] {
							regsub -all -nocase -- "\\&${base}\\;" $text $html($base) text
						}
					}
					return [join $text]
				}

				default { ? get decode $mode }
			}

		}

		case {
			set who [lindex $args 0]
			# Logical order [most- to least-volatile]: custom, nick, handle, botnet
			if [notempty [lindex $args 1]] { set list [lindex $args 1] ; return [lindex $list [lsearch -exact [string tolower $list] [string tolower $who]]] }
			if [onchan $who] {
				empty list
				foreach chan [channels] { set list [concat $list [chanlist $chan]] }
				return [lindex $list [lsearch -exact [string tolower $list] [string tolower $who]]]
			}
			if [validuser $who] { set list [userlist] ; return [lindex $list [lsearch -exact [string tolower $list] [string tolower $who]]] }
			if [islinked $who] { set list [bots] ; return [lindex $list [lsearch -exact [string tolower $list] [string tolower $who]]] }
			return $who
		}

		default { ? get default }

	}
	? get
}

proc is { cmd args } {
	set args [join $args]
	switch -exact -- [string tolower $cmd] {

		path - pathchange { return [regexp -nocase -- {\/|\\} $args] }

		comp - component {
			set file [file rootname [file tail [lindex $args 0]]] 
			if [string eq -nocase SB7 $file] { set file core }
			if [string eq -nocase CORE [none $file core]] { return [file exists ./scripts/sb7/sb7.tcl] }
			regsub -nocase -- {^sb7_} $file "" file
			if [file exists ./scripts/sb7/sb7_${file}.tcl] { return 1 }
			if [file exists ./scripts/sb7/sb7_${file}.txt] { return 1 }
			if [file exists ./scripts/sb7/${file}.sb7] { return 1 }
			return 0
		}

		`own - `prot - `op - `halfop - `voice { lassign $args nick chan above ; return [chanrank $cmd $nick $chan $above] }

		own - prot - admin - op - halfop - voice { flags:simple $args -above text flags ; lassign $text nick chan ; return [chanrank $cmd $nick $chan [validflag -above]] }

		bot {
			set bot [lindex $args 0]
			if [isempty bot] { return 0 }
			if [string eq -nocase $bot $::botnick] { return 1 }
			if [matchattr $bot b] { return 1 }
			#if [islinked $bot] { return 1 } ; # Should this even BE here, Chandler?
			return 0
		}

		owner {
			set o [get owners]
			lassign $args checkme 1001
			if [isempty o] { return 0 }
			set m [lsearch [string tolower $o] [string tolower $checkme]]
			if { $1001 == 1001 } { return [expr (!$m && ( [llength $o] != 1 ) ) ? 1 : 0] }
			return [ expr ( $m == -1 ) ? 0 : 1 ]
		}

		perm - permowner { # Do =NOT= pipe into OWNER
			set o [get owners]
			if [isempty o] { return 0 }
			set checkme [lindex $args 0]
			if [isempty checkme] { return 0 }
			set 1001 [iff { [llength $o] == 1 } 1000 1001]
			return [ expr ( [lsearch -exact [string tolower $o] [string tolower $checkme]] != 0 ) ? 0 : $1001 ]
			# Allows for "placement" to check for single-entry (1000) or 1st position (1001)
		}

		cserv - cservice {
			set cserv [data array value CONFIG CSERVICE]
			if [isempty cserv] { return 0 }
			return [string eq -nocase [lindex $args 0] $cserv]
		}

		auth - authed {
			# Call with: is authed <handle> <nick> [host]
			lassign $args handle nick host

			# DCC users are ALWAYS considered as "logged-in"
			if [valididx $nick] { return [string eq -nocase [idx2hand $nick] $handle] }

			set data [data array get @AUTH $handle]
			if { [llength [join $data]] == 3 } {
				# Original SB7 format
				lassign [data array value @AUTH $handle] auth_handle auth_nick auth_host
				if ![string eq -nocase $nick $auth_nick] { return 0 }
				if [notempty host] { if ![string eq $host $auth_host] { return 0 } }
				return 1
			} {
				# New SB7 format (multi-nick authing)
				set authed 0
				foreach auth $data {
					lassign $auth _nick _host
					if [string eq -nocase $nick $_nick] {
						set authed 1
						if [notempty host] { # Make the auth check optional, if the requested host is ""
							set authed [string eq -nocase $host $_host]
							break
						}
					}
				}
				return $authed
			}
		}

		oauth - outhed - oauthed {
			# Call with: is Oauthed <handle> <nick>
			lassign $args handle nick host

			# DCC users are NEVER considered as "ologged-in" by default
			# Because the OLOGIN command must be used, even in DCC, we might have problem with storing hosts. So, we won't.
			
			if ![is authed $handle $nick $host] { return 0 } ; # Require normal LOGIN as well
			lassign [data array get @outh $handle] _handle _nick _host
			if ![string eq -nocase $handle $_handle] { return 0 } ; # Should be automatic
			if [isnum -integer $nick] { if [is dccnick $nick $handle] { return 1 } { return 0 } }
			return [string eq -nocase $nick $_nick]
		}

		dccnick { # Enter with: IS DCCNICK <idx> [handle to match (RETURNs boolean against handle match)]
			set dcclist [dcclist chat]
			lassign $args idx handle
			if [isempty handle] { set is [lsearch -glob $dcclist [list $idx * * * * *]] } { set is [lsearch -glob [string tolower $dcclist] [list $idx [string tolower $handle] * * * *]] }
			return [expr ( $is == -1 ) ? 0 : 1 ]
		}

		wildcard { return [regexp -- {[\*\?]} $args] }

		within {
			lassign $args value low high
			if { ![isnum -integer $value] || ![isnum -integer $low] || ![isnum -integer $high] } { return 0 }
			return [ expr ( ( $value >= $low ) && ( $value <= $high ) ) ? 1 : 0 ]
		}

		between {
			lassign $args value low high
			if { ![isnum -integer $value] || ![isnum -integer $low] || ![isnum -integer $high] } { return 0 }
			return [ expr ( ( $value > $low ) && ( $value < $high ) ) ? 1 : 0 ]
		}

		inrange {
			lassign $args value range
			lassign [getrange $range 1 9999] bottom top
			return [is within $value $bottom $top]
		}

		default {
			putcmdlog "\00301,00\[IS\] Unknown values: CMD($cmd):ARGS($args)\003"
			error "\[IS\] Unknown command value for IS: $cmd (see command log)"
		}

	}
	?
}

proc process { cmd args } {
	lassign $args 1 2 3 4
	switch -exact -- [string tolower $cmd] {

		level {
			# Enter with: (1) numeric to get word, or (2) word to get numeric
			foreach userlevel [data get USERLEVELS] {
				lassign $userlevel word level range flags name
				if [isnum -integer $1] {
					# Have numeric ; return word
					if []
				} {
					# Have word ; return numeric
				}
			}
		}

		rank {}

		numlist - numberlist {
			set crap $1
			set comma $2 ; if [isempty comma] { set comma , }
			set joiner $3 ; if [isempty joibner] { set joiner " " }
			# 1,2,3,5,9 -> 1-3, 5, 9

			regsub -all -- ",| ,|, |  " $crap " " crap
			regsub -all -- ";| ;|; |  " $crap " " crap
			set temp ""
			foreach a $crap {
				if [regexp -- {^[0-9]+\-[0-9]+$} $a] {
					# Double-dash ("--") or multiple dash ("1-2-3") will get caught
					# by the normal REGEXP below; therefore: ignore it for now.
					set n1 [string trim [lindex [split $a -] 0]]; # Handles "13 - 15"
				set n2 [string trim [lindex [split $a -] 1]]; # Handles "13 - 15"
					if {$n2 < $n1} {return ""}; # Won't cause an endless loop, BTW.
					for {set b $n1} {$b <= $n2} {incr b} {lappend temp $b}
				} elseif [regexp -- {^\-[\d]+$} $a] {# Handle negative numbers
					lappend temp $a
				} else {
					lappend temp $a
				}
			}
			set crap $temp
			if ![regexp -- {^[0-9 \-]+$} $crap] {return ""}
			set old $crap
			set crap [lsort -inc -int -uni $crap]
			set string ""
			set buffer ""
			set hold ""
			append crap " 999999999"
			foreach a $crap {
				if {$buffer == ""} {set buffer $a}
				if {$hold == ""} {set hold $a}
				if {[ expr $a - $buffer ] < 2} {
					set buffer $a
					continue
				}
				if {$string != ""} {append string $comma$joiner}
				set newitem $hold
				if {$buffer != $hold} {append newitem "-$buffer"}
				append string $newitem
				set buffer $a
				set hold $a
			}
			return $string
		}

		default { ? process default }

	}
}

proc debug args {
	empty o
	set l [info level]
	incr l -1
	if $l { set command [lindex [info level $l] 0] } { set command global }
	set loglevel ""
	set quiet 0

	set args [split $args] ; # Will rejoin after flag check
	while { [left [lindex $args 0] 1 -] } {
		set um [uniquematch [list -quiet -log] [lindex $args 0]]
		switch -exact -- [string tolower $um] {

			-log { # When using STRING MAP, put longest-length strings first to prevent false matches on smaller "in common" strings
				set loglevel [string map [list cmdlog c cmd c log o bots b bot b] [lindex $args 1]]
				set args [lreplace $args 0 1]
				set valid [list mpjkcobrxsdwvth123456789]
				if { [lsearch -exact [explode $valid] $loglevel] == -1 } { error "\[DEBUG\] Bad log level: \"-LOG ${loglevel}\" (valid = [explode $valid])" }
			}

			-quiet { set quiet 1 } 

			default break

		}
	}
	set args [join $args]

	set sort 0
	if [is wildcard $args] {
		if [string eq * $args] { set sort 1 }
		empty new
		foreach var $args {
			if [is wildcard $var] {
				set ::DEBUG_VARS_VARIABLE $var
				uplevel 1 { set ::DEBUG_VARS_UPLEVEL [info vars $::DEBUG_VARS_VARIABLE] }
				set new [concat $new $::DEBUG_VARS_UPLEVEL]
				unset ::DEBUG_VARS_VARIABLE ::DEBUG_VARS_UPLEVEL
			} {
				lappend new $var
			}
		}
		# Sort here (instead of setting $SORT): all non-wildcard vars will
		# stay in requested order this way.
		set args [lsort -increasing -dictionary $new]
	}

	if $sort { set args [lsort -inc -dict $args] } ; # NO -UNIQUE (in case multiple cases of the same word =DO= exist)!
	foreach arg $args {
		zero isarray
		if [left $arg 1 =] {
			empty v
			set error [ catch { set expr [ expr { [ mid $arg 2 ] } ] } ]
			if $error {
				set arg !ERROR(undefined:$arg)
			} {
				set arg =[ expr { [ mid $arg 2 ] } ]
			}
		} {
			upvar 1 $arg local
			if [info exists local] {
				if [array exists local] {
					#append arg \[\]
					one isarray
					set v [array get local]
				} {
					set v $local
				}
			} {
				#set v "\{UNDEFINED\}"
				#empty v
				unset -nocomplain v
			}
		}
		if [notempty o] { append o : }
		if [info exists v] {
			if $isarray { set data "\[${v}\]" } { set data "(${v})" }
		} {
			empty data
		}
		if { [instr $arg (] && ![left $arg 1 =] } {
			regsub -all -- {\(} $arg {[} arg
			regsub -all -- {\)} $arg {]} arg
		}
		append o ${arg}[iff ![instr $arg =] $data] ; # [iff ![isnull v] (${v})]
	}

	if [notempty loglevel] {
		prepend o "\026\[DEBUG\]:${loglevel}\026 \[[string toupper $command]\] "
		putloglev $loglevel * $o
	} {
		prepend o "\026\[DEBUG\]\026 \[[string toupper $command]\] "
		set home [home]
		if !$quiet {
			if [botonchan $home] { catch { print -home $o ; if 0 { msg $home $o } } } { putlog $o } ; # rawhome -> msg [home]
		}
	}
	return $o ; # -QUIET may be used just to get this RETURN value
}

proc procdef proc {
	if [string eq "" [info procs $proc]] { error "\[DEFPROC\] No such PROC: $proc" }
	empty args code
	foreach arg [info args $proc] {
		set def [info default $proc $arg a]
		if $def { lappend args " $arg \"${a}\" " } { lappend args $arg }
	}
	return "proc $proc \{ $args \} \{[info body $proc]\}"
}

proc findinprocs text {
	set list ""
	foreach proc [info commands *] {
		if [catch {set null [info args $proc]}] continue
		set body [info body $proc]
		zero count
		foreach line [split $body \n] {
			incr count
			if [instr $line $text] { lappend list "${proc}: line ${count}: $line" }
		}
	}
	return $list
}

proc validflag args {
	upvar 1 flags flags
	if [isempty flags] { return 0 }
	zero m
	foreach flag $args { if ![string eq "" [uniquematch $flags $flag]] { incr m } }
	return $m
}

proc isvalidflag { flags flag } { validflag $flag } ; # Allow use of a variable

proc flags args { # Have to allow several variations in order to unite everything
	# Be careful with debugs: DATA GET & DATA ARRAY GET/VALUE (and others)
	# call for a flag check, causing an endless loop.
	# MSGHOME / DEBUG also checks for colours (which calls DATA GET* also).

	# { text validflags { var_text "" } { var_flags "" } }
	# Options: -simple -params -force

	flags:simple $args [list -simple -params -force] args flags
	# Default to: -SIMPLE
	if [isempty flags] { set flags -simple }

	if [validflag -simple] {
		lassign $args text valid var_text var_flags
		if [notempty var_text] { upvar 1 $var_text new_text ; unset -nocomplain new_text }
		if [notempty var_flags] { upvar 1 $var_flags new_flags ; unset -nocomplain new_flags}
		if [isempty valid] {
			empty new_flags
			set new_text $text
		} {
			flags:simple $text $valid new_text new_flags
		}
		return [list $new_text $new_flags]
	}

	if [validflag -params] {
		# Note: flag params CAN BE STACKED! (:
		# "-flag1 text -flag2 blah -flag1 text2" will return
		# -flag1 {test test2} -flag2 blah
		# Whether or not this was intentional, I DON'T CARE!! (: (: (:

		# Example: flags -params "-flag0 -flag1 Text -flag5 {1 2 3 4 5} This is a sentence." [list -flag0 0 -flag1 1 -flag2 2 -flag5 5] text flags
		# The assigned variable (var_flags) will return an array:
		# $TEXT: this is a sentence.
		# $FLAGS[]: -flag0 {} flag1 Text -flag5 {1 2 3 4 5}
		# Any flags not used will be UNSET (not set) in the $FLAGS[] variable.

		# To override this and set blank values to unused flags, use the -force! (:
		# Zero-parameter flags will be forced to a value of "1" ($BLANK)
		# Example: flags -params "-flag0 -flag1 Text -flag5 {1 2 3 4 5} This is a sentence." [list -flag0 0 -flag1 1 -flag2 2 -flag5 5] text flags
		# The assigned variable (var_flags) will return an array:
		# $TEXT: this is a sentence.
		# $FLAGS[]: -flag0 1 flag1 Text -flag2 {} -flag5 {1 2 3 4 5}

		set blank_1 1
		set blank_0 0
		lassign $args text array var_text var_flags

		array set params ""
		foreach { a b } $array {
			if ![left $a 1 -] { prepend a - }
			if [string eq * $b] { set b 512 }
			set params($a) $b
		}

		# Sanity check!
		foreach { a b } [array get params] {
			if ![isnum -integer $b] { error "\[FLAGS -PARAMS\] Illegal array format: flags must be arrayed to how many arguments each will take: \[LIST -flag0 0 -flag1 1 -flag2 2\] would allow: -flag0 -flag1 text -flag2 test1 test2 -- This is a sentence." }
		}

		emptyarray processed

		while { [notempty text] } {
			set flag [lindex $text 0]
			set text [lreplace $text 0 0]
			if [string eq $flag --] break
			set um [uniquematch [array names params] $flag]

			if [isempty um] {
				lprepend text $flag; # Put it back!
				break
			}

			if [isnull processed($um)] { empty processed($um) }
			set skip $params($um)
			if $skip {
				# Worf?! *Bwahahahahaha*
				for { zero pass } { $pass < $skip } { incr pass } {
					set worf [lindex $text 0]
					if [left $worf 1 -] {
						#NO! break
						# Did we find a legit flag, or, does it just LOOK like one?
						set um [uniquematch [array names params] $worf]
						if [notempty um] { break ; # Don't swallow the next flag; process it! }
					}
					if [notempty worf] { lappend processed($um) $worf }
					set text [lreplace $text 0 0]
				}
			} {
				#lappend processed($um) $um
				lappend processed($um) 1
			}
		}

		# So no errors are thrown by flag-checked variables in the calling PROC ....
		# Is this a good idea?
		# No ~ flags with 0 (zero) elements will ALSO return blank ("") array elements by design.
		# Let's make this coder-controlled via the -FORCE flag
		if [validflag -force] {
			foreach { a - } $array {
				if [info exists processed($a)] {
					if [string eq "" $processed($a)] { if { $params($a) == 0 } { set processed($a) $blank_1 } }
				} {
					# The same thing! How to differentiate ....
					if { $params($a) == 0 } { set processed($a) $blank_0 } { set processed($a) "" }
				}
			}
		}

		if [notempty var_text] { upvar 1 $var_text local_text ; unset -nocomplain local_text ; set local_text $text }
		if [notempty var_flags] { upvar 1 $var_flags local_flags ; unset -nocomplain local_flags ; array set local_flags [array get processed] }
		return [list $text [array get processed]]
	}

	?
}

proc flags:simple { list valid { var_text "" } { var_flags "" } { flags2variables false } } {
	# Convert this to $ARGS: [-variables] {list} {valid} {var_text} {var_flags}
	empty flags
	if [notempty valid] {
		while { [notempty list] } {
			set 0 [lindex $list 0]
			if [string eq $0 --] { set list [lreplace $list 0 0] ; break }
			# We do not allow flags to have "? *" in them; can create false matches when processing text with wildcards that happend to match flags
			if [is wildcard $0] { 
				empty u
			} {
				if [string eq $0 -] { empty u } { set u [uniquematch $valid $0] }
			}
			if [isempty u] break
			lappend flags $u
			set list [lreplace $list 0 0]
		}
	}
	if [notempty var_text] { upvar 1 $var_text local_text ; set local_text $list }
	if [notempty var_flags] { upvar 1 $var_flags local_flags ; set local_flags $flags}
	if $flags2variables {
		foreach a $valid {
			set a [string trimleft $a -]
			upvar 1 $a local_$a
			set local_$a 0
		}
		foreach a $flags { set a [string trimleft $a -] ; set local_$a 1 }
	}
	return [list $list $flags]
}

proc flags:order args {
	# [-c -a] [-a -b -c]: -a
	# -ALL [-c -a] [-a -b -c]: -a -c
	flags:simple $args -all text flags
	lassign $text list priority low 
	set order [lsort:priority $list $priority $low]
	if [validflag -all] { return $order } { return [lindex $order 0] }
}

proc uniquematch { list matchme } {
	if [string eq "" $list] return
	if [string eq "" $matchme] return
	if [string eq * $matchme] { return $list }
	set list [lsort -uni -dict -inc $list] ; # Might be duplicates: if so, false negatives will occur ):
	set m [lsearch -all -exact [string tolower $list] [string tolower $matchme]]
	if { [llength $m] == 1 } { return [lindex $list $m] }
	set n [lsearch -all -glob [string tolower $list] [string tolower $matchme]*]
	if { [llength $n] == 1 } { return [lindex $list $n] }
	return ""
}

proc gmt:format { type gmt } {
	switch -exact -- [uniquematch [list decimal integer colon] $type] {

		? - help { return "[lindex [info level [info level]] 0] <type: integer | decimal> <value>" }

		decimal {
			if [regexp -- {^[\+\-]?\d{1,2}(\.\d{1,2})?$} $gmt] {return $gmt}
			if ![regexp -- {^[\+\-]?\d{3,4}$} $gmt] {error "\[SB:GMT_FORMAT\] illegal GMT format: \"${gmt}\" (should be in +\[0\]000 format)"}
			set sgn [sgn $gmt]   
			set sign [lindex [list - + +] [incr sgn]]
			set gmt [addzero [string trimleft $gmt +-] 4]
			set hour [normalize [left $gmt 2]]
			set minutes [mantissa [ fixmath [ right $gmt 2 ] / 60 ]]
			return [nozero ${sign}${hour}${minutes}]
		}

		colon - integer {
			if [regexp -- {^[\+\-]?\d{3,4}$} $gmt] {return $gmt}
			if ![regexp -- {^[\+\-]?\d{1,2}(\.\d{1,2})?$} $gmt] {error "\[SB:GMT_FORMAT\] illegal GMT format: \"${gmt}\" (should be in +00, +00.0, or +00.00 format)"}
			set sgn [sgn $gmt]
			set sign [lindex [list - + +] [incr sgn]]
			regsub -- {^[\+\-]} $gmt "" gmt
			if [instr $gmt .] {set mantissa [ expr int( 60 * [ mantissa $gmt ] ) ]} {zero mantissa}
			int gmt
			if [string eq -nocase colon $type] { set colon : } { empty colon }
			return ${sign}[addzero $gmt 2]${colon}[addzero $mantissa 2]
		}

		default {error "[sb7 header] Illegal option \"${type}\""}

	}
	return 0
}

proc logme args {
	set level [info level]
	incr level -1
	if [string eq "" $args] {
		set name [lindex [info level $level] 0]
		putlog "\[LOGME\] [string repeat " " [expr $level * 3]]${level}: [string toupper $name]"
	} {
		set name [info level $level]
		set 100 [left [lrange $name 1 end] 100]
		putlog "\[LOGME\] [string repeat " " [expr $level * 3]]${level}: [string toupper [lindex $name 0]] (${100}[iff { [len $100] > 100 } " ...."])"
	}
	return $name
}

proc file:read { name { split "" } { binary "" } } {
	set r [open $name r]
	if [istrue $binary] { set binary binary }
	if [notempty binary] { fconfigure $r -translation $binary }
	set data [read $r]
	close $r

	if {$split != ""} { set data [split $data $split] }
	return $data
}

proc _file:write { filename data { listmode false } { binary false } } {
	set w [open $filename w]
	set binary [string map [list 0 "" 1 binary false "" true binary] [string tolower $binary]]
	# This allows the user to specify "0" as usual, or specify the encoding [e.g.: utf-8].
	if {$binary != ""} {
		if ![string match -nocase *DOS* $binary] {
			fconfigure $w -translation $binary
			set listmode 0; # In binary write mode, do not LIST-SPLIT lines!
		}
	}
	if $listmode {
		foreach datum $data { puts $w ${datum}[lindex [list "" \r] [string match -nocase *DOS* $binary]] }
	} else {
		if [string eq "" $binary] { puts $w [regsub -all -- \n $data [lindex [list "" \r] [string match -nocase *DOS* $binary]]\n] } { puts -nonewline $w $data }
	}
	close $w
	return 0
}

proc file:write args {
	flags:simple $args [list -listmode -binary -dos -utf-8 -unicode -auto -cr -lf -crlf] arg flags true
	lassign $arg filename data
	set listmode [validflag -listmode]
	set dos [validflag -dos]
	set error [ catch {
		set w [open $filename w]
		if $cr {
			zero binary ; fconfigure $w -translation cr
		} elseif $lf {
			zero binary ; fconfigure $w -translation lf
		} elseif $crlf {
			zero binary ; fconfigure $w -translation crlf
		} elseif $auto {
			zero binary ; fconfigure $w -translation auto
		} elseif $binary {
			zero listmode; one binary; fconfigure $w -translation binary
		} elseif ${utf-8} {
			zero listmode; one binary; fconfigure $w -translation utf-8
		} elseif $unicode {
			zero listmode; one binary; fconfigure $w -translation unicode
		}
		if $listmode {
			foreach datum $data { puts $w ${datum}[lindex [list "" \r] $dos] }
		} else {
			if $binary { puts -nonewline $w $data } { puts $w [regsub -all -- \n $data [lindex [list "" \r] $dos]\n] }
		}
		flush $w
		close $w
	} crap]
	if $error { msghome "\[SB7\] File write error (${filename}): $crap" }
	return [not $error]
}

proc sb7:bugsiebug_check args {
	foreach dcc [dcclist chat] {
		lassign $dcc idx handle host - flags ts
		zero bugsie
		if ![validuser $handle] {
			set message "\[BUGSIE BUG CHECK\] Unknown user in DCC CHAT: $handle ($host) since: [clock format $ts -format "%Y-%m-%d %H:%M:%S [data get @CONFIG:TZ] ([gmt:format integer [data get CONFIG:GMT]])"]"
			killdcc $idx $message
			putlog $message
			msghome $message
		}
		if [info exists ::require-p] {
			if { ${::require-p} && ![matchattr $handle p] } {
				set message "\[BUGSIE BUG CHECK\] User in DCC CHAT without global +p flag: $handle ($host) since: [clock format $ts -format "%Y-%m-%d %H:%M:%S [data get @CONFIG:TZ] ([gmt:format integer [data get CONFIG:GMT]])"]"
				killdcc $idx $message
				putlog $message
				msghome $message
			}
		} {
			set ok 0
			foreach chan [channels] { if [matchattr $handle o|o $chan] { set ok 1 ; break } }
			if !$ok {
				set message "\[BUGSIE BUG CHECK\] User in DCC CHAT without global or local +o flag: $handle ($host) since: [clock format $ts -format "%Y-%m-%d %H:%M:%S [data get @CONFIG:TZ] ([gmt:format integer [data get CONFIG:GMT]])"]"
				killdcc $idx $message
				putlog $message
				msghome $message
			}
		}
	}
	return 0
}

proc @dcc:null { handle idx arg } {
	set n $::nick
	if [info exists ::botnet-nick] { set n ${::botnet-nick} }
	set chan [lindex [console $idx] 0]
	putdcc $idx "Hi, ${handle} @ ${n} ([access $handle][iff [validchan $chan] " / [access $handle $chan]:$chan"])"
	return 0
}

proc ? args {
	set level [info level]
	decr level
	set name [lindex [info level $level] 0]
	error "\[?\][iff [notempty args] [space][join $args]]: Something went wrong in the \"${name}\" command: I ended up defaulting to the \"?\" somehow!"
}

proc bytes { number { decimal * } { align_bytes "false" } } {
	set powers [list "" Ki Mi Gi Ti Pi Ei Zi Yi Xi Wi Vi]
	if $align_bytes { lset powers 0 "  " }
	for { set x [expr [llength $powers] - 1] } { $x >= 0 } { incr x -1 } {
		if { $number >= pow( 1024 , $x ) } {
			set multiple [fixmath $number / pow( 1024 , $x ) ]
			if { ( $multiple < 1 ) && ( $multiple >= 0.975 ) } { incr x }
			if [string eq * $decimal] { 
				if !$x { return "$number [lindex $powers $x]B" }
				set expr [ expr $number / pow( 1024 , $x ) ]
				if { $expr == int( $expr ) } { int expr }
				return "$expr [lindex $powers $x]B"
			}
			return "[format %.${decimal}f [ expr $number / pow( 1024 , $x ) ]] [lindex $powers $x]B"
		}
	}
	# How do we get here: if $NUMBER < 1 ? (otherwise, it would pass the "$number >= pow( 1024 , $x ) " test above even with $X = 0)
	# Respect $ALIGN_BYTES
	if [string eq * $decimal] { 
		if [isint $number] { int number }
		return "$number [lindex $powers 0]B" 
	}
	return "[format %.${decimal}f $number] [lindex $powers 0]B"
}

proc double { number { times_double "1" } } { 
	# Mathematical doubling without concern for "integer too large" errors.
	set error [ catch { set test [ expr $number * pow( 2 , $times_double ) ] } crap ]
	if !$error { if ![isscinot $test] { return [lindex [split $test .] 0] } }
	# INT() without that PESKY "integer too large" on TCL <= 8.4

	# Okay, we're going too large and will end-up in SCINOT. Let's do this
	# the hard way!
	set comma [iscomma $number] ; if $comma { set number [nocomma $number] }
	if ![isnum -real $number] { error "\[DOUBLE\] Illegal number: $number" }
	if [scinot $number] { set number [noscinot $number] }
	#set number [nozero $number]
	if { ( $times_double < 1 ) || ( $times_double > 100000000 ) } { error ""  }
	for { set i 1 } { $i <= $times_double } { incr i } {
		set double ""
		set carry 0
		foreach digit [split [reverse $number] ""] {
			if [string eq . $digit] { set double .$double ; continue }
			set new [expr ($digit * 2) + $carry]
			set carry 0
			if { $new > 9 } { set carry 1 ; set new [string index $new end] }
			set double ${new}$double
		}
		if $carry {
			set double 1$double
		} {
			if { [string eq . [string index $double 0]] } {
				set double 0$double
			}
		}
		set number $double
	}

	set double [nozero $double]
	if $comma { set double [comma $double] }
	return $number
}

proc percent { number total { decimal "" } } {
	if !$total { return 0% }
	set % [ fixmath 100 * ( $number / $total ) ]
	if ![string eq "" $decimal] { set % [format %.${decimal}f ${%}] }
	return ${%}%
}

proc timeval { value { convert_to s } } {
	array set mult [list l .001 n .01 t .1 s 1 m 60 h 3600 d 86400 w 604800 y 31536000 e 315360000 c 315360000]
	empty time marker
	regexp -nocase -- {^([\+\-]?)([\d\.]+)([lntsmhdwyec]?)$} $value - sign time marker
	if [isempty time] return
	if [isempty marker] { set marker s }
	if ![info exists mult($marker)] return
	set timeval [expr ${sign}$time * $mult($marker) ]
	if [notempty convert_to] {
		if ![info exists mult($convert_to)] return
		set timeval [expr ( ${timeval} / 1.0 ) / $mult($convert_to)]
	}
	normalize $timeval
}

proc urt { value { joiner "" } { limit 0 } } {
	set d [duration $value]
	foreach a [regexp -inline -all -nocase -- {\d+ [A-Z]} $d] { array set t [string tolower [split [lreverse $a]]] }
	if [info exists t(y)] { 
		if { $t(y) >= 10 } { 
			set t(e) [expr int( $t(y) / 10 )]
			set t(y) [expr int( $t(y) % 10 )] 
		} 
	}
	empty urt
	foreach a [list e y w d h m s] {  if [info exists t($a)] { lappend urt "$t($a)$a" }  }
	if $limit { set urt [lrange $urt 0 [expr $limit - 1]] }
	return [join $urt $joiner] 
}

proc angle args {
	flags:simple $args [list -dms -decimal -help] value flags
	set degree_mark \xC2\xB0
	if { [validflag -help] || [string eq -nocase HELP $value] } { return "-dms 0.0 \[or\] -decimal 0\[${degree_mark}\] 0\['\] 0\[\"\]" }
	if [validflag -decimal] {
		catch { set value [join $value] }
		set value [encoding convertfrom utf-8 $value]
		regsub % {^(\d+)[ %]+(\d+)[ ']+(\d+)[\"]*$} $degree_mark regexp
		if ![regexp -- $regexp $value - degrees minutes seconds] { error "\[ANGLE -DECIMAL\] expected degrees\[${degree_mark}\] minutes\['\] seconds\[\"\] -- got: $value" }

		return [expr {$degrees + ( $minutes / 60.0 ) + ( $seconds / 3600.0 ) } ]
	}
	if [validflag -dms] { return [concat [expr int( $value )]$degree_mark [clock format [expr round( $value * 3600 )] -format "%M' %S\"" -gmt true]] }
	return $value
}

proc addlog { filename text } {
	set a [open $filename a]
	puts $a "[clock format [clock seconds] -format "%Y-%m-%d %H:%M:%S %z]|$text"
	flush $a
	close $a
	putcmdlog "\[ADDLOG\] $text"
	return $filename
}

proc getbytesfree { { dir . } } {
	# Original code: http://wiki.tcl.tk/526
	switch -glob -- $::tcl_platform(os) {
		FreeBSD - Linux - OSF1 - SunOS - CYGWIN_NT-* {
			# Use end-2 instead of 3 because long mountpoints can make the output to appear in two lines. There is "df -k -P" to avoid this, but -P is Linux specific afaict
			return [lindex [lindex [split [exec df -a -B 1 $dir] \n] end] end-2]
		}
		HP-UX { return [lindex [lindex [split [exec bdf $dir] \n] end] 3] }
		{Windows 95} - {Windows 98} - {Windows NT} { return [getbytesfree:dos [regsub -all -- / $dir \\]] }
		default { error "\[GETBYTESFREE\] I don't know how to df on $::tcl_platform(os)" }
	}
	return 
}

proc getbytesfree:dos { { dir . } } {
	set dir [exec cmd /C dir /-C $dir]
	regsub -all -- { [ ]+} [string trimleft [string trimright [lindex [split $dir \n] end]]] " " last
	scan $last {%i %s %i bytes free} - - free
	return $free
}

# --- 005 (mode handler) ---
# VERSION trips 351 and 005 (use 351 to clear 005 data in advance so no leftovers / ghosts remain)

proc sb7:bind:raw:001 { server code arg } { 
	set me [lindex [split $arg] end]
	putlog "\[001\] $::botnick is: $me"
	return 0 ; # RAW requires "0" 
}

proc sb7:bind:raw:351 { server code arg } { 
	set arg [lreplace $arg 0 0]
	data unset @server *
	data array set @server ircd [lindex $arg 0] ; # Opposite of #004
	data array set @server server [lindex $arg 1] ; # Opposite of #004
	return 0 ; # RAW requires "0" 
}

proc sb7:bind:raw:353 { server code arg } {
	# 353 Santana * #BotHouse :Santana @Minerva &Demon|iDevice +Denora ~Demon|Sleep ~Demonicpagan +Puppy +Rodgrod +Finn ~shade Yong +Monica +Rachel 
	set users [lassign $arg me - chan]
#putlog "\[SB7:BIND:RAW:353\] ARG($arg)"
#putlog "\[SB7:BIND:RAW:353\] USERS($users)"
	foreach user [mid $users 2] {
		empty modes
		regexp -- {^([\~\&\@\%\+]*)(.+)$} $user - flags user
#putlog "\[SB7:BIND:RAW:353\] CHAN($chan):USER($user):FLAGS($flags)"
		if [notempty flags] {
			foreach flag [explode $flags] {
				set mode [lindex [list q a o h v] [lsearch [list ~ & @ % +] $flag]]
#debug flags flag user mode
#putlog "\[SB7:BIND:RAW:353\] USER($user):CHAN($chan):MODE(+$mode)"
				chanmode add $chan +$mode $user
			}
		}
	}
	return 0
}

proc sb7:bind:raw:381 { server code arg } {
	set modes [data array get @usermode Santana]
	set snomask [data array get @snomask Santana]
	putlog "\[381\] $::botnick is now: $arg ([none $modes + [data array get @usermode Santana]][none $snomask "" " ($snomask)"])"
	return 0
}

proc sb7:bind:raw:004 { server code arg } { 
	set arg [lreplace $arg 0 0]
	data unset @server *
	data array set @server ircd [lindex $arg 1] ; # Opposite of #351
	data array set @server server [lindex $arg 0] ; # Opposite of #351
	return 0 ; # RAW requires "0" 
}

proc sb7:bind:raw:005 { server code arg } {
	set arg [lreplace $arg 0 0]
	# Parse this?
	foreach item $arg {
		lassign [split $item =] name value
		switch -glob -- [string tolower $name] {
			uhnames - namesx - safelist - hcn { # Do nothing # }
			:* { break ; # Do nothing more # }
			wallchops - excepts - invex { data array set @server $name 1 }
			maxchannels - nicklen - channellen - topiclen - kicklen - awaylen - maxtargets - watch - watchopts - silence - modes - chantypes - network - casemapping - elist - statusmsg { data array set @server $name $value }
			chanlimit - maxlist {
				foreach s [split $value ,]  {
					lassign [split $s :] mode max
					data array set @server ${name}:$mode $max
				}
			}
			prefix {
				if [string match (*)* $value] {
					regexp -- {^\((.+)\)(.+)$} $value - modes flags
					foreach mode [split $modes ""] flag [split $flags ""] {
						data array set @server chanflag:$mode $flag
						data array set @server chanflag:$flag $mode
						if [string eq q $mode] { data array set @server chanowner 1 }
						if [string eq a $mode] { data array set @server chanadmin 1 }
						if [string eq o $mode] { data array set @server chanop 1 }
						if [string eq h $mode] { data array set @server chanhalfop 1 }
						if [string eq v $mode] { data array set @server chanvoice 1 }
					}
				} {
					data array set @server $name $value
				}
			}
			chanmodes {
				lassign [split $value ,] 1 2 3 4
				if [data array get -boolean @server chanowner] { append 1 q }
				if [data array get -boolean @server chanadmin] { append 1 a }
				if [data array get -boolean @server chanop] { append 1 o }
				if [data array get -boolean @server chanhalfop] { append 1 h }
				if [data array get -boolean @server chanvoice] { append 1 v }
				data array set @server chanmodes:1 [split $1 ""]
				data array set @server chanmodes:2 [split $2 ""]
				data array set @server chanmodes:3 [split $3 ""]
				data array set @server chanmodes:4 [split $4 ""]
				foreach a [list 1 2 3 4] { foreach b [split [set $a] ""] { data array set @server chanmode:$b $a } }
				data array set @server chanmodes [concat [split $1 ""] [split $2 ""] [split $3 ""] [split $4 ""] ]
			}
			cmds {
				set s [split $value ,]
				foreach a $s { data array set @server $a 1 }
				data array set @server cmds $s
			}
			extban {
				lassign [split $value ,] type flags
				foreach a $type { data array set @server extban:$a [split $flags ""] }
			}
			default { putlog "\[SB7:005\] NAME($name):VALUE($value)" }
		}
	}
	return 0 ; # Must be RETURN 0 (due to the BIND RETURN rules for RAW)
}

proc sb7:bind:raw:221 { server cmd arg } { set arg [lreplace $arg 0 0] ; data array set @usermode $::botnick $arg ; putlog "\[221\] $::botnick MODE: $arg" ; return 0 }

proc sb7:bind:raw:008 { server cmd arg } { regexp -nocase -- {([\+\-][a-zA-Z0-9]*)} $arg - snomask ; data array set @snomask $::botnick $snomask ; putlog "\[008\] $::botnick SNOMASK: $snomask" ; return 0 }

proc sb7:bind:raw:311 { server cmd arg } {
	# WHOIS reply = 311 379 378 319 312 313 310 671 320 317 318
	# [@] <server> 311 <target nick> <nick> <ident> <host> * :<gecos>
	set gecos [lassign $arg - nick ident host *]
	if ![string eq -nocase $nick $::botnick] { return 0 }
	putlog "\[311\] I am: ${nick}!${ident}@$host --> $::botname"
	after 1000 { putlog "\[311 (delayed)\] \$::BOTNAME = $::botname" }
	return 0
}


# --- Binds form BootStrap ---

proc sb7:raw:mode { server cmd arg } {
putloglev 5 * "[effects "\[SB7:RAW:MODE\] SERVER($server):CMD($cmd):ARG($arg)" 6 b]"
	set check_chan [join [lindex [split $arg] 0]]
	if ![validchan $check_chan] {
		if ![isbotnick $check_chan] { return 0 ; # Weird situation where the bot sees another user's mode change }
		putquick "MODE $::botnick" -next ; # Force mode re-evaluation (let's not try to calculate cancelling modes, shall we?)
		return 0
	}

	set targets [lassign $arg chan modes]
#debug chan modes targets
	set polarity ""
	foreach mode [explode $modes] {
		switch -exact -- $mode {

			+ - "-" { set polarity $mode }

			default {
				set type [data array search -list @server chanmodes:? $mode]
				set type [lindex [split [lindex $type 0 1] :] 1]
#debug chan polarity mode targets =@SERVER:CHANMODE([data array get @server chanmode:$mode]) type
				if { [lsearch -exact [list + -] $polarity] == -1 } { error "\[SB7:RAW:MODE\] Missing polarity for channel mode: $mode (+ or -?)" }
#[data array get @server chanmode:$mode]
				switch -exact -- $type {

					1 {
						if [isempty targets] { error "\[SB7:RAW:MODE\] Missing nick/host target for channel mode: ${polarity}$mode" }
						chanmode add $chan ${polarity}$mode [join [lindex [split $targets] 0]]
						set target [join [lreplace [split $targets] 0 0]]
					}

					2 {
						if [isempty targets] { error "\[SB7:RAW:MODE\] Missing target for channel mode: ${polarity}$mode" }
						chanmode add $chan ${polarity}$mode [join [lindex [split $targets] 0]]
						set target [join [lreplace [split $targets] 0 0]]
					}

					3 {
						if [string eq + $polarity] {
							set target [join [lindex [split $targets] 0] ]
							if [isempty target] { error "\[SB7:RAW:MODE\] Missing target for channel mode: ${polarity}$mode" }
							chanmode set $chan ${polarity}$mode $target
						} {
							chanmode set $chan ${polarity}$mode
						}
					}

					4 { chanmode set $chan ${polarity}$mode }

					default { error "\[SB7:RAW:MODE\] Unknown server data for channel mode: ${polarity}$a (what type is it?)" }

				}
			}

		}
	}
	return 0
}

proc sb7:bind:join { nick host handle chan } {
	if [isbotnick $nick] {
		data array clear @chanmode ${chan}:*
	} {
		# Nothing for now
		return 0
	}
	return 0
}

proc sb7:bind:part { nick host handle chan { reason "" } } {
	if [isbotnick $nick] {
		data array clear @chanmode ${chan}:*
	} {
		# Chanmodes
		foreach a [data array search -list -nocase @chanmode ${chan}:? $nick] {
			lassign $a root field
			data array lremove $root $field $nick
		}
	}
}

proc sb7:bind:sign { nick host handle chan { reason "" } } {
	# Nothing yet ....
}

proc sb7:bind:splt { nick host handle chan } {
	# Nothing yet ....
}

proc sb7:bind:rejn { nick host handle chan } {
	# Nothing yet ....
}

# --- Event binds ---

proc @event:sigterm args { 
	set die 0
	if [info exists ::die-on-sigterm] { if ${::die-on-sigterm} { set die 1 } }
	if $die { save ; data save ; print -home -raw "\[SIGTERM\] Shutting down ...." ; after 1000 ; die "SIGTERM received: shutting down ...." }
	set message "\[SIGTERM\] SIGTERM received (config variable set: NOT to die): RESTARTing instead ...."
	print -home -raw $message
	putlog $message
	after 2000
	restart
	return 0
}

proc @event:sigquit args { print -home -raw "\[SIGQUIT\] RESTARTing ...." ; restart }

proc @event:sighup  args {
	set die 0
	if [info exists ::die-on-sighup] { if ${::die-on-sighup} { set die 1 } }
	if $die { 
		set message "\[SIGHUP\] SIGHUP received: config variable requires me to DIE ...."
		save
		data save
		print -home -raw $message
		putlog $message
		after 2000
		die $message
	}
	set message "\[SIGHUP\] REHASHing ...."
	print -home -raw $message
	putlog $message
	rehash
	return 0
}

proc @event:sigill args {
	if 0 {
		set message "\[SIGILL\] SIGILL received: saving data and RESTARTing ...."
		print -home -raw $message
		save
		data save
		restart
	}

	catch { set home [home] }
	putquick "PRIVMSG $home :\[SIGILL\] Aggressive reset in progess ...." -next
	catch { after 1000; # Let msg to HOME be seen }

	# Remove BINDs (so nothing can be triggered)
putquick "PRIVMSG $home :\[SIGILL\] Removing BINDs ...." -next
	foreach bind [binds] { catch { unbind [lindex $bind 0] [lindex $bind 1] [lindex $bind 2] [lindex $bind 4] } }

	# Clear all queues
putquick "PRIVMSG $home :\[SIGILL\] Clearing all output queues ...." -next
	catch { clearqueue all }

	# Kill all LISTEN ports
putquick "PRIVMSG $home :\[SIGILL\] Killing all LISTEN ports ...." -next
	catch { foreach dcc [dcclist telnet] { catch { listen [lindex [lindex $dcc 4] 1] off } } }

	# Unlink all bots
putquick "PRIVMSG $home :\[SIGILL\] Unlinking all bots ...." -next
	catch { foreach dcc [dcclist bot] { catch { killdcc [lindex $dcc 0] } } }
	catch { unlink * }

	# Kill all other DCC connections
putquick "PRIVMSG $home :\[SIGILL\] Killing all DCC CHAT users ...." -next
	foreach dcclist [dcclist] {
		set dcc [lindex $dcclist 0]
		set hand ""
		catch { set hand [idx2hand $dcc] }
		if [is permowner $hand] {
			putquick "PRIVMSG $home :\[SIGILL\][space 5]Protecting DCC CHAT #$dcc with: [idx2hand $dcc]" -next
		} {
			catch { killdcc $dcc } 
		}
	}

	# Remove timers
putquick "PRIVMSG $home :\[SIGILL\] Killing all timers" -next
	catch { putlog "\[SIGILL\] \[U\]TIMERS killed: [sb7 killtimer *]" ; # NOT quiet! }

	# Remove all PROC commands
putquick "PRIVMSG $home :\[SIGILL\] Removing all PROCs from memory ...." -next
	set myproc [lindex [info level [info level]] 0]
	set myproc @event:sigill
	catch {
		foreach proc [info procs] {
			if { [lsearch -exact [list rehash return $myproc] $proc] < 0 } { rename $proc "" }
		}
	}

	##### From this point forward, use PUTQUICK only! #####

	# Remove all variables!
putquick "PRIVMSG $home :\[SIGILL\] Clearing all NAMESPACEs & variables ...." -next
	set namespaces [namespace children :: *]

	#lappend namespaces ""
	# [2010-07-18 14:12:12 -0700] <Dumbledore> [14:12] Tcl error [*dcc:tcl]: invalid command name "*dcc:tcl"
	# [2010-07-18 14:12:26 -0700] <Domino> mental note: never delete the global namespace!

	set var ""; set proc ""; set ::sb7(nocomplain) ""
	foreach namespace $namespaces {
#rawprint "PRIVMSG $home :NAMESPACE($namespace):VARS"
		catch { foreach var [info vars ${namespace}::*] { unset $var } }
#rawprint "PRIVMSG $home :NAMESPACE($namespace):PROCS"
		catch { foreach proc [info procs ${namespace}::*] { rename $proc "" } }
		catch { namespace delete ::$namespace }
	}

#rawprint "PRIVMSG $home :\[SIGILL\] Clearing all global variables!"
	#catch { foreach var [info vars ::*] {unset $var} }

	# --- We should just now have EGGDROP core commands left ---
	putquick "PRIVMSG $home :\[SIGILL\] RESTARTing!" -next

	catch { unset -nocomplain ::sb7 } ; # Not needed (local variables): home var proc

	##### Nothing left but C-coded TCL and EGGDROP core commands #####
	after 5000
	set error [ catch { after 500 restart } ohshit ]
	if $error restart
	return 0

}

# --- Command aliases ---

interp alias {} =  {} expr
interp alias {} == {} fixmath
interp alias {} stl {} string tolower
interp alias {} stu {} string toupper
interp alias {} sts {} string totitle ; # SENTENCE case! (misnomer; thanks, TCL people)
interp alias {} nozero {} normalize ; # PROC NOZERO
interp alias {} norm {} normalize
interp alias {} rawoutput {} raw
interp alias {} rawprint {} raw
interp alias {} casechannel {} casechan
interp alias {} sb7:auth {} sb7 auth
interp alias {} readfile {} file:read
interp alias {} dumpfile {} file:write
interp alias {} formattime {} format:date
interp alias {} formatdate {} format:date
interp alias {} format:time {} format:date
interp alias {} % {} percent
interp alias {} userflags {} userflag
interp alias {} getflags {} sb7 parseflags
interp alias {} FLAGS {} sb7 parseflags ; # Case sensitive!
interp alias {} lmatch {} ldestroy -not
interp alias {} path:rel {} path:relative 

# --- Deprecated commands ---

proc sb7:parseflags args { msghome "\[SB7\] PARSEFLAGS -- FROM(<--[info level [expr [info level] -1]]):ARGS($args) (Remove me!)" }

#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################

#####
# Bootstrap:

bind DCC  t dcc  *dcc:dccstat
bind DCC  - /w   *dcc:whois
bind DCC  - q    *dcc:quit
bind DCC  - ""   @dcc:null
bind join - *    sb7:bind:join
bind part - *    sb7:bind:part
bind kick - *    sb7:bind:kick
bind sign - *    sb7:bind:sign
bind splt - *    sb7:bind:splt
bind rejn - *    sb7:bind:rejn
bind pubm - *    sb7:dispatch:pubm
bind time - *    sb7:bugsiebug_check
bind raw  - 001  sb7:bind:raw:001
bind raw  - 004  sb7:bind:raw:004
bind raw  - 005  sb7:bind:raw:005
bind raw  - 311  sb7:bind:raw:311
bind raw  - 351  sb7:bind:raw:351
bind raw  - 353  sb7:bind:raw:353
bind raw  - 381  sb7:bind:raw:381
bind raw  - 221  sb7:bind:raw:221
bind raw  - 008  sb7:bind:raw:008
bind raw  - mode sb7:raw:mode
catch { bind evnt - sighup  @event:sighup }
catch { bind evnt - sigterm @event:sigterm }
catch { bind evnt - sigill  @event:sigill }
catch { bind evnt - sigquit @event:sigquit }

utimer 0 sb7:check_data_command_integrity ; # =MUST= be on a timer!

sb7:setup ; # Last thing to be executed!

# --- VERSION command ---

##### Install the VERSION command here (can't be a separate bead: file name match conflict with "sb7_version.txt") #####

sb7 command add VERSION 501 -none VER

proc @version { nick host handle chan arg } {
	if [string eq -nocase HELP [lindex [split $arg] 1]] {
		print -help $nick "\[VERSION\]:"
		print -help $nick "Syntax: $::botnick VERSION HELP"
		print -help $nick "Syntax: $::botnick VERSION"
		print -help $nick "VERSION displays the version information for StormBot.TCL on $::botnick"
		return
	}

	print -short $nick "StormBot.TCL v[data array get @VERSION stormbot] ([data get @VERSION distro]) on $::botnick (Eggdrop v[lindex $::version 0] running TCL v[info patchlevel])"
	return
}

#####

putlog "\[StormBot.TCL\] StormBot.TCL v[data array get @VERSION stormbot] (by Mai \"Domino\" Mizuno) loaded"


