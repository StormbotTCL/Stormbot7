# StormBot.TCL version 7.X series, add-on script for EGGDROP V1.3 - V1.8
# (C) 2012 - 2015: [V7.0      ] David P. Hansen, Mai Mizuno
# (C) 2009 - 2012: [V6.0      ] David P. Hansen, Mai Mizuno
# (C) 2003 - 2010: [V5.0 - 5.2] David P. Hansen, Mai Mizuno
# (C) 2000 - 2003: [V4.0 - 4.3] David P. Hansen, Mai Mizuno
# (C) 2002 - 2003: [V3.5      ] David P. Hansen, Mai Mizuno
# (C) 2000 - 2003: [V3.1b- 3.2] David P. Hansen, Mai Mizuno
# (C) 1998 - 2000: [V3.0 - 3.1] David P. Hansen, Dave Klein, Justin Hammond, & Mai "Domino" Mizuno
# (C) 1997 - 1998: [V1.0 - 2.0] David P. "Xone" Hansen, Dave "CyberMac" Klein, & Justin "ElriC" Hammond

# Requirements:
# Eggdrop 1.6.0 or above (recommended [1.6] 1.6.13 or [1.8] 1.8.0 / above )
# TCL8.4.1 or above (recommended: 8.5.10 / above)

# --- License ---

# This program is free software; you can redistribute it and / or  modify  it
# under the terms of the GNU General Public License as published by the  Free
# Software Foundation. We hope the SOFTWARE is useful, but provide it WITHOUT
# ANY WARRANTY; without even  the  implied  warranty  of  MERCHANTABILITY  or
# FITNESS FOR A PARTICULAR PURPOSE. See  the GNU General Public  License  for
# more details. You should have received a copy of  the  GNU  General  Public
# License along with this  program;  if  not,  write  to  the  Free  Software
# Foundation, Inc., 59   Temple  Place -  Suite  330,  Boston,  Massachusetts
# 02111-1307, USA. Or, you can read it at http://www.gnu.org/ or you may read
# the license on our website in the MISCELLANEOUS section, or the  text  file
# included in the tarball from which you extracted this file (usually called:
# sb7_license.txt).

# TRANSLATION: if you modify this script into  your  own  product,  you  must
# credit us as the source of the original source code. After  all,  we  wrote
# this, not you.

# --- EGGDROP information ---

# StormBot.TCL is an add-on script for use with the EGGDROP V1.*  IRC  robot. 
# This script is =NOT= a fully running bot; you MUST download, install, & run
# the EGGDROP code itself, separately. We have no  affiliation  with  EGGDROP
# core coders in any way. We can and will  assist  you  with  installing  and
# configuring the StormBot.TCL script for use with EGGDROP on request. To get
# EGGDROP (*nix & Windows), visit: http://www.GetEggdrop.com/ and follow  the
# appropriate links.

# And please: RTFM !! People write manuals for a reason; honour our  work  by
# reading them.

# To load SB7 into your bot, do the following:
#
# (1) put the tarball into your bot's HOME dir (<bot dir>) (this is a change!)
# (2) tar -zxvf <tarball>
# (3) place any bot specific-beads into a subfolder of sb7 by the bot's
#     nick (all lower case)
# (4) edit your bot's config file (watch for the DIE commands!)
# (5) make sure to set the $OWNER variable in your bot's config file
# (6) start / REHASH your bot!

# Sample directory tree (presuming user "tkr" and bot "SkyOne"):
# /users/home/tkr/eggdrop/SkyOne.conf
# /users/home/tkr/eggdrop/scripts/sb7/sb7.tcl
# /users/home/tkr/eggdrop/scripts/sb7/skyone/custom_modes_code_for_skyone.sb7
#
# The bot will load any command beads within the directory with its name. No
# other bot will load from this directory. This allows for bot-specific beads
# to be used without special coding / handling.

############################################################
# Script prep (find "#%#" below for user-settable options) #
############################################################

array set sb7 [list]

# --- Store pre-SB7 data ---
if ![info exists sb7(%pre:binds)    ] { set sb7(%pre:binds)     [binds]         }
if ![info exists sb7(%pre:commands) ] { set sb7(%pre:commands)  [info commands] }
if ![info exists sb7(%pre:variables)] { set sb7(%pre:variables) [info vars]     }
if ![info exists sb7(%pre:timers)   ] { set sb7(%pre:timers)    [timers]        }
if ![info exists sb7(%pre:utimers)  ] { set sb7(%pre:utimers)   [utimers]       }

# Ignore this: REHASH -NOSB7 flag check
if [info exists sb7(@norehash)] {
	if $sb7(@norehash) {
		putlog "\[REHASH\] SB7's -NOSB7 flag used: REHASH without script reload ...."
		unset sb7(@norehash)
		return 0
	}
}

if ![info exists sb7(@boot:type)] { set sb7(@boot:type) 0 ; # COLD }

# Ignore this: REHASH -TOTAL flag check
if [info exists sb7(@rehash:total)] {
	if $sb7(@rehash:total) {
		putlog "\[REHASH\] SB7's REHASH -TOTAL flag used: wiping all SB7 data from memory ...."
		foreach a [array names sb7] { unset sb7($a) }
		array set sb7 [list @boot:type 1]; # Can't do a REHASH -TOTAL on a cold boot!
	}
}

#############################################################################
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#
######################## --- BEGIN EDITING HERE! --- ########################
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#
#############################################################################

#####
# Note: CONFIG command
# Please note: once these default values are loaded into the bot,  they  will
# remain in place until changed via the "live" (online) CONFIG  command.  The
# CONFIG command's setting will then override these  and  become  "permanent"
# (until manually changed via the CONFIG command again). I've listed  "change
# via CONFIG command" where appropriate.
#####

##### Optional: SHORTCUT
# Set this to the bot's shortcut. A shortcut is how the bot will be given its
# commands ("LlamaBot say Hello!"). This bind  is  available  to  all  users,
# regardless of level. If you do not set  a  value  here,  StormBot.TCL  will
# automatically set this to the FIRST & LAST letters of your bot's nick ("LT"
# for "LlamaBot"). This can be changed via the CONFIG command when the bot is
# online.
#
# Example: set sb7(config:shortcut:all) "LB" ; # "LB say Hello!"
#
# There are THREE binds available: one for all ("all")  users,  one  for  all
# admin users ("global"), and one for owners only  ("owner").  Set  the  ones
# you wish to use; leave the others blank  if  unwanted.  There  will  be  no
# support for any other level-restricted binds in SB7 (a change from SB6).
#
# Options: (any valid characters)

set sb7(config:shortcut:all)    ""
set sb7(config:shortcut:global) "***"
set sb7(config:shortcut:owner)  ""

##### Optional: Version Binds
# Set this to determine if special binds should be automatically added to SB7
# that represent version numbers: Eggdrop, TCL,  &  Stormbot.TCL.  These  are
# useful for those who run bot services and need to control  groups  of  bots
# based on their versions.
#
# Eggdrop binds (example: 1.6.21): *E1* *E6* *E16* *E621* *E1621*
# TCL binds (example: 8.5.11): *T8* *T5*, *T85*, *T511*, *T8511*
# StormBot.TCL binds (example 7.0.1): *SB7*
#
# Options: boolean

set sb7(config:shortcut:version) true

##### Required: Output
# Set this to the default type of output for each user (each  user  can  then
# select an individual output type via the SET  command).  The  original  IRC
# protocol (RFC) requires this to be NOTICE; this will be honoured.  In  SB4,
# SB5, & SB6, there was an option to set this to "MSG" or  "MSG  #TargetChan"
# for users who had WebTV (or similar) ISPs. SB7 will no longer  support  the
# WebTV option, but, you can set  this for cosmetic reasons if desired.  This
# can be updated via the CONFIG command. Default value: "notice"
# Note: in SB7, to direct output to a specific channel at all times, just put
# the channel's name instead of "chan #channel" (this is an upgrade).
#
# Options: NOTICE, MSG, CHAN, #SPECIFIC_CHANNEL_NAME

set sb7(config:output) "notice"

##### Required: BIND:PUB, BIND:MSG, BIND:DCC, & BIND:NOT
# SB7 normally binds commands to: PUB (in-channel), MSG (commands sent in via
# "/MSG BOT <text>"), and DCC (party-line / .DCC CHAT). New to SB7:  the  bot
# will now respond to "/NOTICE" messages (normally discouraged). You can turn 
# any of these off if your bot needs to run under "specific conditions." Some
# commands can ONLY WORK  via  a  specific  method  (e.g.  LOGIN needs "/MSG"
# available). Set as required. StormBot.TCL is  setup  to  function  normally
# with PUB, MSG, & DCC "on" and NOT[ice] "off" (which are the defaults).
#
# Please note: /MSG & .DCC already have built-in Eggdrop  commands.  Some  of
# StormBot.TCL's commands use the same command words. In some cases, you  may
# want to retain the original version of the commands. If  you  activate  the
# /MSG or .DCC triggers, StormBot.TCL  will  "rename"  the  original  command
# binds, adding a solidus (slash or "/" character) to the  word,  making  the
# REHASH command into "/REHASH" instead. In this way, you can use  "./REHASH"
# in .DCC or "/MSG bot /REHASH" in /MSG to use the original  REHASH  command.
# In SB4, SB5, &, SB6, this was done with a solidus (forward-slash  or  "/"),
# but, I noticed some confusion with people not understanding that the syntax
# was otherwise the same as the raw Eggdrop installation. So I changed it  to
# a full-stop (period or "."), which created a "..REHASH" command in DCC. So,
# I changed it back. I can't make up my mind.
#
# Once you set these values, the only way to properly change them is  to  (1)
# change the values in this file, then (2) RESTART your bot. REHASH =DOES NOT
# RESTORE / REMOVE BINDS= as necessary.
#
# Options: boolean [0 (off), 1 (on)]

set sb7(config:bind:pub) true
set sb7(config:bind:msg) true
set sb7(config:bind:dcc) true
set sb7(config:bind:not) true ; # false

##### Required: GMT & TZ
# These variables help the bot understand what timezone it is in. By default,
# WinDrop / WinEggy can't figure out what zone it's in: the %z flag, used  by
# STRFTIME, isn't defined in the interaction with the earlier Windows host OS
# versions (up to Win2K, inclusive).
#
# Set "GMT" to your GMT / UTC offset, such as "-8" or  "+10.5"  (DO  NOT  use
# values like "-0800" and "+1030"). *nix users: if you leave the values blank
# the bot will TRY to calculate the info for you. If it can not, the bot will
# fatal-crash (intentionally) requiring you  to  manually  set  these  values
# before restarting your bot.
#
# Set "TZ" to your timezone  name  abbreviation  (in  your  home  country  or
# language), such as "PST" or "UTC" ("GMT"). Values can be  changed  via  the
# CONFIG command  (necessary  during  Summer  Time- / Daylight  Saving  Time-
# -related changes.
#
# Options (GMT): (-14.00 ~ +14.00)
# Options (TZ) : (any valid text, such as "PST")

set sb7(config:gmt) "-0800"
set sb7(config:tz) "PST"

##### Required: HOME (valid channel name)
# Set this variable to the channel that will be your "home" for your bot. For
# normal bot users, this may be nothing you  need.  For  bot-loaning  groups,
# this would be the channel that all the reports go to (LOGIN, BOTMON,  etc).
# Certain commands require output at all times to some "home" location; those
# reports, if this value is unset, will go to the first  channel  loaded,  in
# order of loading. This value can be changed via the CONFIG  command.  Also,
# the "output to home" text can be silenced by the CONFIG command.
#
# Options: (any valid channel name), or "" (blank)

set sb7(config:home) "#Home"

##### Optional: GROUP
# If this bot belongs to an organization / bot-loaning  group,  you  can  set
# the name here. This will only show up cosmetically in WHOIS-like  requests.
# Leave blank if not needed. This can be changed via the CONFIG command.
# Example: "Glee Club Bot Service"
#
# Options: (any valid text) or "" (pair of double quotes, or, blank)

set sb7(config:group) ""

##### Optional: GIVE_P
# Eggdrop allows a restriction for DCC CHAT  ability:  only  users  with  the
# global "+p" flag (party-line access)  can  enter  the  party-line.  If  the
# restriction is off, then any global user (and any user with +o or better on
# any channel) can DCC CHAT the bot. To cooperate with this setting, you  can
# set this variable to give all added users the +p flag  or  not,  SEPARATELY
# from any normal flags used by users and the bot. Standard behavior is:  (1)
# turn the restriction off, and, (2) give all users (at least, global  users)
# +p by default. See "USERLEVELS" setting below for further. If you are going
# to set custom flags (including +p) below, then turn THIS off ("0").
#
# Options: 0 (no), 1 (yes: global users only), 2 (yes: everyone)

set sb7(config:give_p) 0

##### Required: PRECISION
# Set this to the decimal precision you wish your bot to use for any math  it
# needs to perform. By default (as of the release of SB7), the  precision  is
# 12 (twelve digits of accuracy, ignoring the decimal point's position,  with
# rounding as necessary). Any number that is larger (more digits than can fit
# within this range) will be  automatically  truncated  (with  rounding),  or
# converted to scientific notation (e.g. 1.70141183E+38).
#
# Please note: using the maximum value  of  "17"  will  cause  IEEE  rounding 
# errors (a value of 0.1 will become "0.09999999999999" in some  calculations
# instead). It's not recommended to use 17; use "16" instead.

# Additional note: in TCL 8.6, a TIP is active to allow a new value "0" which
# will allow TCL to use whatever precision necessary to  internally  preserve
# perfect accuracy. As of this date (1364253709), I am unsure of its  status.
# For now, TCL defaults to "12" but I recommend "16"  instead.  This  can  be
# changed via the CONFIG command. Additionally, TCL 8.5, and  above,  have  a
# special ability to do: (1) 64-bit math, even on 32-bit systems,  &  (2)  do
# all integer math using "arbitrary-precision" (there are no  retrictions  as
# how large of a number the system can manipulate). As long as you keep  with
# integers,  you  can  produce  obscenely  large  numbers.  The  instant  you
# introduce a floating-point number  (decimal  or  scientific-notation),  you
# lose this ability; the normal rules of TCL's EXPR math are applied.
#
# Options: (integer: 0 , or , 1 ~ 17)

set sb7(config:precision) 16

##### Required: BOOT_FAIL_DIE
# If SB7 fails to load properly during the bot's cold-start sequence (due  to
# some error or glitch), the bot  would  immediately  crash-and-burn,  as  it
# would with any script load failure (on bootup). This option will allow  you
# to specify whether it should crash and  die  on  an  SB7  load  error,  or,
# otherwise, trap the error safely and continue loading  all  other  scripts.
# This setting will take effect on RESTART but will have NO EFFECT  on  warm-
# -starts, such as REHASH.

# Options: 0 / 1

set sb7(@boot_fail_die) true

#############################################################################
#############################################################################
######################### --- STOP EDITING HERE --- #########################
#############################################################################
#############################################################################

# An acknowledgement from Eggdrop core for a major recursive bug we found by
# way of the "UPDATE" command in SB6 -- from Eggdrop 1.6.21 release notes:
# ( http://cvs.eggheads.org/viewvc/eggdrop1.6/doc/Changes1.6?view=markup )
# 100    - do_restart is now reset before actually performing a rehash or restart to
# 101      ensure it doesn't try to do it again infinitely.
# 102      Found by: Domino / Patch by: thommey

# An acknowledgement from Eggdrop core for a bug report and a command 
# suggestion re: STICKY bans and related typoes:
# ( http://egg.lib.so:1352/cgi-bin/gitweb.cgi?p=eggcvs1.8;a=commitdiff;h=4548fef )
# 1.8.0 (CVS):
#
#+  - Fix error messages of the Tcl commands (un)stick(exempt/invite).
#+  - Add an alias for (un)stickban to correspond to them.
#+    Found by: Domino / Patch by: thommey

# Note: there are only 2 files now: CORE (sb7.tcl) and  TAIL  (sb7_tail.tcl).
# The DISP (dispatcher) & LOAD (loader) files  are  now  part  of  TAIL.  The
# default settings file (sb6_permsettings_renameme.tcl) has been deprecated.

# --- Boot it up! ---
set sb7(@fullfile) %SB7:FULLFILE% ; # Boolean (0/1)
if [string match %*% $sb7(@fullfile)] { set sb7(@fullfile) 0 }
if !$sb7(@fullfile) {
	set sb7(@boot_error) [ catch { uplevel #0 { source scripts/sb7/sb7_tail.tcl } } dammit ]
	if $sb7(@boot_error) {
		putcmdlog "\[SB7\] Boot up error while loading\n\[SB7\] $dammit"
		if [info exists sb7(@boot_fail_die)] {
			if $sb7(@boot_fail_die) { die "\[SB7\] Boot failure in SB7: $dammit" }
			return
		} {
			die "\[SB7\] Boot failure: $dammit"
		}
	}
}
# Error trapped to allow the bot to continue loading without SB7.

# --- Store post-SB7 data
if ![info exists sb7(%post:binds)    ] { set sb7(%post:binds)     [binds]         }
if ![info exists sb7(%post:commands) ] { set sb7(%post:commands)  [info commands] }
if ![info exists sb7(%post:variables)] { set sb7(%post:variables) [info vars]     }
if ![info exists sb7(%post:timers)   ] { set sb7(%post:timers)    [timers]        }
if ![info exists sb7(%post:utimers)  ] { set sb7(%post:utimers)   [utimers]       }

# --- End: transfer control back to EGGDROP ---
set sb7(@boot:type) 1 ; # WARM
if !$sb7(@fullfile) { return $sb7(@version) } ; # The FULL version will never load due to this ....


